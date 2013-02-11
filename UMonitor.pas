unit UMonitor;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Classes, Windows, SysUtils;

const
  MONITOR_FLAGS = FILE_NOTIFY_CHANGE_FILE_NAME or FILE_NOTIFY_CHANGE_DIR_NAME;

var
  Critical: TRTLCriticalSection;

type
  PInfoCallback = ^TInfoCallback;
  TInfoCallback = record
    fAction      : Integer; // FILE_ACTION_XXX constants
    fDrive       : String;  // Drive, where change occured
    fOldFileName : String;  // File name before rename
    fNewFileName : String;  // File name after rename
    fFolder      : String;  // Folder assigned to thread
  end;

  TWatchFileSystemCallback = procedure (pInfo: TInfoCallback);

  PFileNotifyInformation = ^TFileNotifyInformation;
  TFileNotifyInformation = record
    NextEntryOffset: DWORD;
    Action         : DWORD;
    FileNameLength : DWORD;
    FileName       : Pointer;
  end;

  TWFS = class(TThread)
  private
    fName          : String;
    fFilter        : Cardinal;
    fSubTree       : Boolean;
    fInfoCallback  : TWatchFileSystemCallback;
    fWatchHandle   : THandle;
    fWatchBuf      : array[0..4096] of Byte;
    fOverLapp      : TOverlapped;
    fPOverLapp     : POverlapped;
    fBytesWrite    : DWORD;
    fCompletionPort: THandle;
    fNumBytes      : Cardinal;
    fOldFileName   : String;
    fLockCounter   : Integer;
    function CreateDirHandle(ADir: String): THandle;
    procedure WatchEvent;
    procedure HandleEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(pName: String; pFilter: Cardinal; pSubTree: Boolean;
      pInfoCallback: TWatchFileSystemCallback);
    destructor Destroy; override;

    procedure IncLockCounter;
    procedure DecLockCounter;

    property Directory: String read fName write fName;
    property LockCounter: Integer read fLockCounter write fLockCounter;
  end;

procedure StartWatch(pName: String; pFilter: Cardinal; pSubTree: Boolean;
  pInfoCallback: TWatchFileSystemCallback);
procedure StopWatch(pName: String; AForce: Boolean = False); overload;
procedure StopWatch; overload;
procedure RenameWatch(const pOldName, pNewName: String);
procedure RealingArray(const Item: TWFS);

implementation

const
  FILE_LIST_DIRECTORY = $0001;

var
  WFSCount: Integer = 0;
  WFS: array of TWFS;

// -----------------------------------------------------------------------------
// Start monitoring of specified folder:
// -------------------------------------
// pName:    path to folder
// pFilter:  actions to monitor (FILE_NOTIFY_XXX)
// pSubTree: true, if recursively
// pInfoCallback: callback address
procedure StartWatch(pName: String; pFilter: Cardinal; pSubTree: Boolean;
  pInfoCallback: TWatchFileSystemCallback);
var
  I: Integer;
begin
  if DirectoryExists(pName) then
  begin
    { Check if already watching }
    for I := 0 to WFSCount - 1 do
      if WFS[I].Directory = pName then
      begin
        WFS[I].IncLockCounter; // Multiple requests to monitor the same folder
        Exit;
      end;
    SetLength(WFS, WFSCount + 1);
    WFS[WFSCount] := TWFS.Create(pName, pFilter, pSubTree, pInfoCallback);
    Inc(WFSCount);
  end;
end;

// -----------------------------------------------------------------------------
// Stop monitoring a folder
procedure StopWatch(pName: String; AForce: Boolean = False);
var
  I: Integer;
begin
  if pName = '' then
    Exit;
  for I := 0 to WFSCount - 1 do
    if Assigned(WFS[I]) and (WFS[I].Directory = pName) then
    begin
      if AForce or (WFS[I].LockCounter <= 0) then
      begin
        PostQueuedCompletionStatus(WFS[I].fCompletionPort, 0, 0, nil);
        WFS[I].Terminate;
      end
      else
        WFS[I].DecLockCounter;
    end;
end;

// -----------------------------------------------------------------------------
// Ultimately stop watch everything
procedure StopWatch;
var
  I: Integer;
begin
  for I := 0 to WFSCount - 1 do
    if Assigned(WFS[I]) then
    begin
      PostQueuedCompletionStatus(WFS[I].fCompletionPort, 0, 0, nil);
      try
        WFS[I].Terminate;
      except
      end;
    end;
end;

// -----------------------------------------------------------------------------
// Rename directory in watch pool if it's name gets changed
procedure RenameWatch(const pOldName, pNewName: String);
var
  I: Integer;
begin
  for I := 0 to WFSCount - 1 do
    if WFS[I].Directory = pOldName then
    begin
      WFS[I].Directory := pNewName;
      Break;
    end;
end;

// -----------------------------------------------------------------------------
// Realings array after deletion of thread
procedure RealingArray(const Item: TWFS);
var
  I, P, J: Integer;
begin
  P := -1;
  for I := 0 to WFSCount - 1 do
  begin
    if WFS[I] = Item then
    begin
      P := I;
      Break;
    end;
  end;
  if P > -1 then
  begin
    if P < WFSCount - 1 then
      for J := P + 1 to WFSCount - 1 do
        WFS[J - 1] := WFS[J];
    Dec(WFSCount);
    SetLength(WFS, WFSCount);
  end;
end;

// -----------------------------------------------------------------------------

{ TWatchFileSystem }

// -----------------------------------------------------------------------------

constructor TWFS.Create(pName: String; pFilter: Cardinal; pSubTree: Boolean;
  pInfoCallback: TWatchFileSystemCallback);
begin
  inherited Create(True);
  fWatchHandle := 0;
  fCompletionPort := 0;
  fLockCounter := 0;
  FreeOnTerminate := True;
  fName := pName;
  fFilter := pFilter;
  fSubTree := pSubTree;
  fOldFileName := EmptyStr;
  ZeroMemory(@fOverLapp, SizeOf(TOverLapped));
  fPOverLapp := @fOverLapp;
  ZeroMemory(@fWatchBuf, SizeOf(fWatchBuf));
  fInfoCallback := pInfoCallback;
  Resume;
end;

destructor TWFS.Destroy;
begin
  PostQueuedCompletionStatus(fCompletionPort, 0, 0, nil);
  CloseHandle(fWatchHandle);
  fWatchHandle := 0;
  CloseHandle(fCompletionPort);
  fCompletionPort := 0;

  EnterCriticalSection(Critical);
  try
    RealingArray(Self);
  finally
    LeaveCriticalSection(Critical);
  end;

  inherited Destroy;
end;

procedure TWFS.IncLockCounter;
begin
  Inc(fLockCounter);
end;

procedure TWFS.DecLockCounter;
begin
  if fLockCounter > 0 then
    Dec(fLockCounter);
end;

function TWFS.CreateDirHandle(ADir: string): THandle;
begin
  Result := CreateFile(PChar(ADir), FILE_LIST_DIRECTORY, FILE_SHARE_READ or FILE_SHARE_DELETE or FILE_SHARE_WRITE,
                       nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED, 0);
end;

procedure TWFS.Execute;
begin
  fWatchHandle := CreateDirHandle(fName);
  if (fWatchHandle = 0) or (fWatchHandle = ERROR_INVALID_HANDLE) then
    Terminate
  else
    WatchEvent;
end;

procedure TWFS.HandleEvent;
var
  FileNotifyInfo: PFileNotifyInformation;
  InfoCallback  : TInfoCallback;
  Offset        : Longint;
begin
  Pointer(FileNotifyInfo) := @fWatchBuf[0];
  repeat
    { Find position of next entry }
    Offset := FileNotifyInfo^.NextEntryOffset;

    { Get file name }
    SetLength(InfoCallback.FNewFileName, FileNotifyInfo^.FileNameLength shr 1);
    Move(FileNotifyInfo^.FileName, InfoCallback.FNewFileName[1],
      FileNotifyInfo^.FileNameLength);
    InfoCallback.FNewFileName := Trim(InfoCallback.FNewFileName);

    { Get drive name }
    InfoCallback.fDrive := ExtractFileDrive(InfoCallback.FNewFileName);

    { Get performed action }
    InfoCallback.FAction := FileNotifyInfo^.Action;
    case InfoCallback.FAction of
      FILE_ACTION_RENAMED_OLD_NAME: fOldFileName := InfoCallback.FNewFileName;
      FILE_ACTION_RENAMED_NEW_NAME: InfoCallback.FOldFileName := fOldFileName;
    end;

    { Set folder which caused callback }
    InfoCallback.fFolder := fName;

    { Call processing routine }
    if InfoCallback.fAction >= FILE_ACTION_RENAMED_OLD_NAME then
      if (InfoCallback.fOldFileName <> '') and (InfoCallback.fNewFileName <> '') then
      begin
        if DirectoryExists(InfoCallback.fFolder + InfoCallback.fNewFileName) then
        begin
          EnterCriticalSection(Critical);
          try
            RenameWatch(IncludeTrailingBackslash(InfoCallback.fFolder + InfoCallback.fOldFileName),
              IncludeTrailingBackslash(InfoCallback.fFolder + InfoCallback.fNewFileName));
          finally
            LeaveCriticalSection(Critical);
          end;
        end;
        fInfoCallback(InfoCallback);
      end
      else
    else
      fInfoCallback(InfoCallback);

    { Proceed to next entry }
    PByte(FileNotifyInfo) := PByte(FileNotifyInfo) + Offset;
  until
    (Offset = 0) or Terminated;
end;

procedure TWFS.WatchEvent;
var
 CompletionKey: Cardinal;
begin
  fCompletionPort := CreateIoCompletionPort(fWatchHandle, 0, Longint(Pointer(Self)), 0);
  ZeroMemory(@fWatchBuf, SizeOf(fWatchBuf));
  if not ReadDirectoryChanges(fWatchHandle, @fWatchBuf[0], SizeOf(fWatchBuf), fSubTree,
    fFilter, @fBytesWrite, @fOverLapp, nil)
  then
    Terminate
  else
    while not Terminated do
    begin
      GetQueuedCompletionStatus(fCompletionPort, fNumBytes, CompletionKey,
        fPOverLapp, INFINITE);
      if CompletionKey <> 0 then
      begin
        Synchronize(HandleEvent);
        ZeroMemory(@fWatchBuf, SizeOf(fWatchBuf));
        fBytesWrite := 0;
        ReadDirectoryChanges(fWatchHandle, @fWatchBuf, SizeOf(fWatchBuf), fSubTree, fFilter,
                             @fBytesWrite, @fOverLapp, nil);
      end
      else
        Terminate;
    end;
end;

initialization
  InitializeCriticalSection(Critical);

finalization
  DeleteCriticalSection(Critical);

end.
