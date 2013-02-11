(*

  Letterpress

  Windows is a piece of crap.

  Copyright 2009-2010, Garnet

*)

{$UNDEF DEBUG}

unit URoutine;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Registry, Math, ShellAPI, SHDocVw_TLB;

type

  { Output (stdout reader) thread events }
  TOutputBufferReady = procedure(Sender: TObject;
    const Data: UTF8String) of object; // Event for buffered output in HTML win
  TOuputCompleted = procedure(Sender: TObject;
    const Data: UTF8String) of object; // Event for complete output (all others)
  TOutputProcessFinished = procedure(Sender: TObject;
    const ExitCode: Cardinal) of object; // Output thread controls process termination

  { Input (stdin writer) thread events }
  TInputRequested = procedure(Sender: TObject;
    const Data: PUTF8String) of object;

  { stdout reader }
  TOutputCommandThread = class(TThread)
  private
    { General properties }
    fExitCode: Cardinal;
    fWaitForCompleteOutput: Boolean;
    fOutputStream, fProcess: THandle;

    { Buffer accumulator }
    fAccumulated: Integer;
    fOutputBuffer, fOutputAccumulator: UTF8String;

    { Events }
    fOnOutputBufferReady: TOutputBufferReady;
    fOnOutputCompleted: TOuputCompleted;
    fOnOutputProcessFinished: TOutputProcessFinished;

    { Sync methods }
    procedure FlushOutput;
    procedure OutputCompleted;
    procedure ProcessFinished;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended, WaitForCompleteOutput,
      FreeOnTerm: Boolean; OutStream, Process: THandle);

    property OnOutputBufferReady: TOutputBufferReady
      write fOnOutputBufferReady;
    property OnOutputCompleted: TOuputCompleted
      write fOnOutputCompleted;
    property OnOutputProcessFinished: TOutputProcessFinished
      write fOnOutputProcessFinished;
  end;

  TInputCommandThread = class(TThread)
  private

    { States }
    fHandleClosed,  // fInputStream pipe was closed
    fInputReady,    // Input prompt was issued and user entered data
    fCanceled,      // Halt command issued or input prompt was issued and user
                    // refused to enter anything
    fAwaitingInput, // Input prompt hs been issued and waiting for input data
    fWriteInitialInput: Boolean; // Thread was created with some initial data
                                 // to send in

    { Handles }
    fInputStream, fMonitorStream1, fMonitorStream2,
    fProcess: THandle;

    { Input buffers }
    fInput: PUTF8String;
    fInitialInput: RawByteString;

    { Input event }
    fOnInputRequested: TInputRequested;

    { Input event & state methods }
    procedure InputRequest;
    procedure GotInput;
    procedure Cancel;

    { Sync methods }
    procedure SetHandleClosed;
    procedure SetInputReady(Value: Boolean);
    procedure SetCanceled(Value: Boolean);
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended, FreeOnTerm: Boolean;
      InStream, MonStream1, MonStream2, Process: THandle;
      const InitialInput: RawByteString);
    destructor Destroy; override;

    property InputReady: Boolean write SetInputReady;
    property Canceled: Boolean write SetCanceled;
    property HandleClosed: Boolean read fHandleClosed;

    property OnInputRequested: TInputRequested write fOnInputRequested;
  end;

  function NtSuspendProcess(dwProcess: Cardinal): DWORD; stdcall; external 'ntdll.dll';
  function NtResumeProcess(dwProcess: Cardinal): DWORD; stdcall; external 'ntdll.dll';

  function LaunchPipedProcess(var g_hChildStd_OUT_Rd, g_hChildStd_IN_Wr,
    g_hChildStd_Inf1, g_hChildStd_Inf2, g_Proc: THandle;
    Command, Dir, CommandDir, Env: UnicodeString;
    Editor: TObject = nil): Boolean;

implementation

uses
  SynEdit, SynUniHighlighter, SynUnicode, SynEditMiscProcs,
  UConst, UInput, USettings, UProjectDrawer;

const
  nBufferSize = 4096;
  nDelayTime = 50;

{ URoutine.pas }

// -----------------------------------------------------------------------------

function InjectDLL(hProcess: THandle; DllPath: AnsiString): Boolean;
var
  kern32: HMODULE;
  hThread, Dummy: Cardinal;
  Addr: Pointer;
begin
  { Initialize }
  Result := False;

  { Get kernel32.dll handle }
  kern32 := GetModuleHandle('kernel32');

  { Allocate space for dll name in victim's address space }
  Addr := VirtualAllocEx(hProcess, nil, Length(DllPath) + 1, MEM_COMMIT, PAGE_READWRITE);
  if Addr = nil then
    Exit;

  { Write DLL name into newely allocated memory }
  WriteProcessMemory(hProcess, Addr, PAnsiChar(DllPath), Length(DllPath) + 1, Dummy);

  { Call LoadLibrary in victim's address space in order to load our DLL }
  hThread := CreateRemoteThread(hProcess, nil, 0,
    GetProcAddress(kern32, 'LoadLibraryA'), Addr, 0, Dummy);

  { Free memory used for DLL name }
  VirtualFreeEx(hProcess, Addr, Length(DllPath) + 1, MEM_RELEASE);

  { Check if CreateRemoteThread failed }
  if hThread = 0 then
    Exit;

  { Wait for thread to establish }
  WaitForSingleObject(hThread, INFINITE);

  { Get hModule of our DLL }
  GetExitCodeThread(hThread, Dummy);
  CloseHandle(hThread);
  if Dummy = 0 then
    Exit;

  { Success }
  Result := True;
end;

function LaunchPipedProcess(var g_hChildStd_OUT_Rd, g_hChildStd_IN_Wr,
  g_hChildStd_Inf1, g_hChildStd_Inf2, g_Proc: THandle;
  Command, Dir, CommandDir, Env: UnicodeString;
  Editor: TObject = nil): Boolean;
const
  cEnvironmentNull: AnsiChar = #0;
var
  g_hChildStd_OUT_Rd_Tmp, g_hChildStd_OUT_Wr,
  g_hChildStd_IN_Wr_Tmp, g_hChildStd_IN_Rd,
  g_hChildStd_Err: THandle;
  Directory: PChar;

  { Ensure environment variables ready }
  procedure RecreateEnvironmentVariables;
  var
    I, J, K: Integer;
    EnironmentList: TStringList;
    CurrLine, CurrChar: UnicodeString;
    CurrLineConverted: UTF8String;
    Runner: PAnsiChar;
  begin
    { Assemble all environment variables together }
    EnironmentList := TStringList.Create;
    try

      { Configure }
      with EnironmentList do
      begin
        NameValueSeparator := '=';
        LineBreak := #26;
        Text := Env;
        BeginUpdate;
      end;

      { Windows }
      with EnironmentList do
      begin
        Add('NUMBER_OF_PROCESSORS=' + GetEnvironmentVariable('NUMBER_OF_PROCESSORS'));
        Add('PROCESSOR_ARCHITECTURE=' + GetEnvironmentVariable('PROCESSOR_ARCHITECTURE'));
        Add('PROCESSOR_IDENTIFIER=' + GetEnvironmentVariable('PROCESSOR_IDENTIFIER'));
        Add('PROCESSOR_LEVEL=' + GetEnvironmentVariable('PROCESSOR_LEVEL'));
        Add('PROCESSOR_REVISION=' + GetEnvironmentVariable('PROCESSOR_REVISION'));
        Add('OS=' + GetEnvironmentVariable('OS'));
        Add('COMSPEC=' + GetEnvironmentVariable('COMSPEC'));
        Add('HOMEDRIVE=' + GetEnvironmentVariable('HOMEDRIVE'));
        Add('HOMEPATH=' + GetEnvironmentVariable('HOMEPATH'));
        Add('PATH=' + GetEnvironmentVariable('PATH'));
        Add('PATHEXT=' + GetEnvironmentVariable('PATHEXT'));
        Add('PROMPT=' + GetEnvironmentVariable('PROMPT'));
        Add('SYSTEMDRIVE=' + GetEnvironmentVariable('SYSTEMDRIVE'));
        Add('SYSTEMROOT=' + GetEnvironmentVariable('SYSTEMROOT'));
        Add('WINDIR=' + GetEnvironmentVariable('WINDIR'));
        Add('TEMP=' + GetEnvironmentVariable('TEMP'));
        Add('TMP=' + GetEnvironmentVariable('TMP'));
      end;

      { Global }
      with SetEnvironment.SetGlobal do
        for I := 0 to Pred(Count) do
          EnironmentList.Add(Names[I] + '=' + ExpandLetterpressVariables(ValueFromIndex[I], CommandDir));

      { Project (can override global) }
      if Assigned(FrmProjectDrawer) then
      begin
        with FrmProjectDrawer.Environment do
          for I := 0 to Pred(Count) do
          begin
            J := EnironmentList.IndexOfName(Names[I]);
            if J > -1 then
              EnironmentList.ValueFromIndex[J] := ValueFromIndex[I]
            else
              EnironmentList.Add(Names[I] + '=' + ValueFromIndex[I]);
          end;
        EnironmentList.Add('LETTERPRESS_SELECTED_FILE=' + FrmProjectDrawer.GetSelectedFileOrFolder);
        EnironmentList.Add('LETTERPRESS_SELECTED_FILES=' + FrmProjectDrawer.GetSelectedFilesOrFolders);
      end;

      { Grammar }
      if Assigned(Editor) and Assigned(TSynEdit(Editor).Highlighter) then
        with TSynUniSyn(TSynEdit(Editor).Highlighter).Environment do
          for I := 0 to Pred(Count) do
            EnironmentList.Add(Names[I] + '=' + ValueFromIndex[I]);

      { Editor }
      if Assigned(Editor) then
        with Editor as TSynEdit do
        begin
          CurrLine := LineText;
          if CaretX <= Length(CurrLine) then
            CurrChar := CurrLine[CaretX]
          else
            CurrChar := EmptyStr;
          CurrLineConverted := UnicodeStringToUTF8(CurrLine);
          with EnironmentList do
          begin
            Add('LETTERPRESS_CARET_X=' + IntToStr(CaretX));
            Add('LETTERPRESS_CARET_Y=' + IntToStr(CaretY));
            Add('LETTERPRESS_COLUMN=' + IntToStr(DisplayX));
            Add('LETTERPRESS_COLUMNS=' + IntToStr(RightEdge));
            Add('LETTERPRESS_CURRENT_SCOPE=' + CurrentScope);
            Add('LETTERPRESS_CURRENT_LINE=' + CurrLine);
            Add('LETTERPRESS_CURRENT_WORD=' + WordAtCursor);
            Add('LETTERPRESS_CURRENT_CHAR=' + CurrChar);
            Add('LETTERPRESS_LINE_INDEX=' + IntToStr(Max(Pred(CountUnicodeChars(CurrLineConverted, CaretX)), 0)));
            Add('LETTERPRESS_LINE_NUMBER=' + IntToStr(CaretY));
            Add('LETTERPRESS_TAB_SIZE=' + IntToStr(TabWidth));
(*
            if FileExists(FileName) then
            begin
              Add('LETTERPRESS_FILE_NAME=' + FileName);
              Add('LETTERPRESS_FILE_PATH=' + IncludeTrailingPathDelimiter(ExtractFilePath(FileName)));
            end;
            if eoTabsToSpaces in Options then
              Add('LETTERPRESS_SOFT_TABS=YES')
            else
              Add('LETTERPRESS_SOFT_TABS=NO');
*)
          end;
        end;

      { Letterpress }
      with EnironmentList do
      begin
        Add('LETTERPRESS_EXE=' + Application.ExeName);
        Add('LETTERPRESS_PATH=' + GetAppDir);
        Add('LETTERPRESS_DATA_PATH=' + GetSettingsDir + sCommandsDir);
        Add('LETTERPRESS_COMMAND_PATH=' + CommandDir);
        Add('LETTERPRESS_TEMP_PATH=' + GetTempDir);
        Add('LETTERPRESS_PID=' + IntToStr(GetCurrentProcessId));
      end;

      { Sort }
      with EnironmentList do
      begin
        Sort;
        EndUpdate;
      end;

      { Expand environment variables }
      for I := 0 to Pred(EnironmentList.Count) do
      begin
        Command := StringReplace(Command, '${' + EnironmentList.Names[I] + '}',
          EnironmentList.ValueFromIndex[I], [rfReplaceAll]);
        Dir := StringReplace(Dir, '${' + EnironmentList.Names[I] + '}',
          EnironmentList.ValueFromIndex[I], [rfReplaceAll]);
      end;

      { Assemble environment string. Count length of block }
      K := 1;
      for I := 0 to Pred(EnironmentList.Count) do
        Inc(K, Length(UnicodeStringToUTF8(EnironmentList[I])) + 1);

      { No environment variables }
      if K = 1 then
        Inc(K);

      { Copy to block }
      if Environment = nil then
        Environment := AllocMem(K)
      else
        Environment := ReallocMemory(Environment, K);
      Runner := Environment;
      if EnironmentList.Count > 0 then
        for I := 0 to Pred(EnironmentList.Count) do
        begin
          CurrLineConverted := UnicodeStringToUTF8(EnironmentList[I]);
          Move(CurrLineConverted[1], Runner^, Length(CurrLineConverted));
          Inc(Runner, Length(CurrLineConverted));
          Move(cEnvironmentNull, Runner^, 1);
          Inc(Runner, 1);
        end
      else begin
        Move(cEnvironmentNull, Runner^, 1);
        Inc(Runner, 1);
      end;
      Move(cEnvironmentNull, Runner^, 1);
    finally
      FreeAndNil(EnironmentList);
    end;
  end;

  { Launch target process }
  function CreateChildProcess: Boolean;
  var
    Start: TStartUpInfo;
    ProcessInfo: TProcessInformation;
  begin
    { Initialize }
    Result := True;

    { Prepare starup info }
    FillChar(Start, SizeOf(Start), #0);
    with Start do
    begin
      cb := SizeOf(Start);
      hStdError := g_hChildStd_Err;
      hStdOutput := g_hChildStd_OUT_Wr;
      hStdInput := g_hChildStd_IN_Rd;
      dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      wShowWindow := SW_HIDE;
    end;

    { Execute command }
    try

      { Get enbironment block }
      RecreateEnvironmentVariables;

      { Get working directory }
      if Dir <> EmptyStr then
        Directory := PChar(Dir)
      else if CommandDir <> EmptyStr then
        Directory := PChar(CommandDir);

      { Create child process }
      if CreateProcess(nil, PChar(Command), nil, nil, True,
        CREATE_NEW_CONSOLE, Environment, Directory, Start, ProcessInfo) then

      { Retrieve handles which will be closed after execution }
      begin

        { Get process handles }
        g_Proc := ProcessInfo.hProcess;

        { Close pipe handles (do not continue to modify the parent) }
        CloseHandle(g_hChildStd_OUT_Wr);
        CloseHandle(g_hChildStd_IN_Rd);
        CloseHandle(g_hChildStd_Err);

        { Close unneeded handles. Process handle will be used to retrieve
          exit code and to suspend entire child }
        CloseHandle(ProcessInfo.hThread);

        { Wait for process to start up }
        WaitForInputIdle(g_Proc, INFINITE);

        { Inject our DLL in process' space and load id }
        if not SetSystem.SetAlwaysPresentInputPrompt then
          if not InjectDll(g_Proc, UnicodeStringToUTF8(GetAppDir + 'binary.dll')) then
          begin
            CloseHandle(g_hChildStd_Inf1);
            g_hChildStd_Inf1 := INVALID_HANDLE_VALUE;
          end;
      end
(*
      { Output error message }
      else begin
        Msg(MainForm.Handle, SysErrorMessage(GetLastError), MB_ICONERROR or MB_OK);
        Result := False;
        Exit;
      end;
*)
    { Output critical error message }
    except
      on E: Exception do
      begin
//        Msg(MainForm.Handle, E.Message, MB_ICONERROR or MB_OK);
        Result := False;
        Exit;
      end;
    end;
  end;

var
  Security: TSecurityAttributes;
  Descriptor: TSecurityDescriptor;
  execCommand, execPath, srchPath, sUUID: UnicodeString;
  Dummy: PChar;
begin
  { Initialize }
  Result := False;

  { Check command format }
  if Command[1] <> '"' then
  begin

    { Get executable part }
    execCommand := Copy(Command, 1, Pred(Pos(#32, Command)));
    if execCommand = EmptyStr then
      execCommand := Command;

    { Fix command name }
    if ExtractFileExt(execCommand) = EmptyStr then
      execCommand := execCommand + '.exe';

    { See if relative path specified }
    if not FileExists(execCommand) then
    begin

      { Get search path }
      srchPath := EmptyStr;
      try
        with TRegistry.Create(KEY_QUERY_VALUE) do
        begin
          RootKey := HKEY_LOCAL_MACHINE;
          OpenKey('SYSTEM\CurrentControlSet\Control\Session Manager\Environment', False);
          srchPath := ReadString('Path');
          Free;
        end;
      finally
      end;

      { Get full executable path }
      SetLength(execPath, 1024);
      SetLength(execPath, SearchPath(PChar(srchPath), PChar(execCommand), nil,
        Length(execPath), PChar(execPath), Dummy));

      { Construct back }
      if execPath <> EmptyStr then
        Command := '"' + execPath + '"' + Copy(Command, Pos(#32, Command), MAXINT);
    end;
  end;

  { Fill in security info }
  with Security do
  begin
    nLength := SizeOf(Security);
    bInheritHandle := True;
  end;

  { Fill in security descriptor }
  InitializeSecurityDescriptor(@Descriptor, SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@Descriptor, True, nil, False);
  Security.lpSecurityDescriptor := @Descriptor;

  { Ensure our pipes will be unique system-wide for each command }
  sUUID := GenerateUUID;

  { Create reading (stdout) named pipe }
  g_hChildStd_OUT_Rd_Tmp := CreateNamedPipe(PChar('\\.\pipe\read ' + sUUID),
    PIPE_ACCESS_INBOUND, PIPE_READMODE_BYTE or PIPE_WAIT,
    1, 4096, 4096, 0, @Security
  );
  g_hChildStd_OUT_Wr := CreateFile(PChar('\\.\pipe\read ' + sUUID),
		{FILE_WRITE_DATA}2 or SYNCHRONIZE, 0, @Security,
    OPEN_EXISTING, // !important
		FILE_ATTRIBUTE_NORMAL, 0
  );

  { We are going to erdirect error messages to the same output }
  DuplicateHandle(GetCurrentProcess, g_hChildStd_OUT_Wr, GetCurrentProcess,
    @g_hChildStd_Err, 0, True, DUPLICATE_SAME_ACCESS);

  { Create writing (stdin) named pipe }
  g_hChildStd_IN_Rd := CreateNamedPipe(PChar('\\.\pipe\write ' + sUUID),
    PIPE_ACCESS_INBOUND, PIPE_TYPE_BYTE or PIPE_READMODE_BYTE or PIPE_WAIT,
    1, 4096, 4096, 0, @Security
  );
  g_hChildStd_IN_Wr_Tmp := CreateFile(PChar('\\.\pipe\write ' + sUUID),
		{FILE_WRITE_DATA}2 or SYNCHRONIZE, 0, @Security,
    OPEN_EXISTING, // !important
		FILE_ATTRIBUTE_NORMAL, 0
  );

  { Duplicate handles as says MSDN }
  DuplicateHandle(GetCurrentProcess, g_hChildStd_OUT_Rd_Tmp, GetCurrentProcess,
    @g_hChildStd_OUT_Rd, 0, False, DUPLICATE_SAME_ACCESS);

  DuplicateHandle(GetCurrentProcess, g_hChildStd_IN_Wr_Tmp, GetCurrentProcess,
    @g_hChildStd_IN_Wr, 0, False, DUPLICATE_SAME_ACCESS);

  { Close orphaned handles }
  CloseHandle(g_hChildStd_OUT_Rd_Tmp);
  CloseHandle(g_hChildStd_IN_Wr_Tmp);

  { Allow access to our pipes for child process for some obscure case }
  if Length(Env) > 0 then
    Env := Env + #26;
  Env := Env + 'LETTERPRESS_STDOUT_PIPE=\\.\pipe\read ' + sUUID + #26 +
    'LETTERPRESS_STDIN_PIPE=\\.\pipe\write ' + sUUID;

  { Create monitor pipes }
  if not SetSystem.SetAlwaysPresentInputPrompt then
  begin

    { Create monitor pipe1. This is for injected DLL.
      One monitoring pipe isn't enough because once we connect it on client side
      it becomes unavaialbe for other connections. I have no fucking idea why.
      It's Windows }
    g_hChildStd_Inf1 := CreateNamedPipe(PChar('\\.\pipe\monitor1 ' + sUUID),
      PIPE_ACCESS_DUPLEX, PIPE_TYPE_BYTE or PIPE_READMODE_BYTE or PIPE_WAIT,
      1, 4096, 4096, 0, nil
    );

    { Create monitor pipe2. This is for manual operation inside child }
    g_hChildStd_Inf2 := CreateNamedPipe(PChar('\\.\pipe\monitor2 ' + sUUID),
      PIPE_ACCESS_DUPLEX, PIPE_TYPE_BYTE or PIPE_READMODE_BYTE or PIPE_WAIT,
      1, 4096, 4096, 0, nil
    );

    { Allow access to our pipes for child process for some obscure case }
    if Length(Env) > 0 then
      Env := Env + #26;
    Env := Env + 'LETTERPRESS_MONITOR_DLL_PIPE=\\.\pipe\monitor1 ' + sUUID + #26 +
      'LETTERPRESS_MONITOR_PIPE=\\.\pipe\monitor2 ' + sUUID;
  end
  else begin
    g_hChildStd_Inf1 := INVALID_HANDLE_VALUE;
    g_hChildStd_Inf2 := INVALID_HANDLE_VALUE;
  end;

  { Try to create child process }
  Result := CreateChildProcess;
end;

{ TOutputCommandThread }

// -----------------------------------------------------------------------------

constructor TOutputCommandThread.Create(CreateSuspended, WaitForCompleteOutput,
  FreeOnTerm: Boolean; OutStream, Process: THandle);
begin
  { Prepare }
  inherited Create(CreateSuspended);
  FreeOnTerminate := FreeOnTerm;

  { Setup input / output streams }
  fOutputStream := OutStream;
  fProcess := Process;

  { Need to flush only complete results? }
  fWaitForCompleteOutput := WaitForCompleteOutput;
end;

// -----------------------------------------------------------------------------

procedure TOutputCommandThread.Execute;
var
  chBuf: RawByteString;
  dwRead, dwReaded, dwWritten: Cardinal;
  bSuccess: Boolean;

  { Finalize accumulating buffer }
  procedure QuitExecution;
  begin
    if fWaitForCompleteOutput then
    begin
      SetLength(fOutputAccumulator, fAccumulated);
      Synchronize(OutputCompleted);
    end;
  end;

begin
  { Prepare accumulator }
  if fWaitForCompleteOutput then
  begin
    fAccumulated := 0;
    SetLength(fOutputAccumulator, nBufferSize);
  end;

  { Read output from started process }
  try

    { Do it while possible }
    while True do
    begin

      { Check if there's information to read from the pipe }
      dwRead := 0;
      bSuccess := PeekNamedPipe(fOutputStream, nil, 0, nil, @dwRead, nil);
      if not bSuccess then
      begin
        {$IFDEF DEBUG}
        WriteLn('STDOUT: Output pipe died. ', SysErrorMessage(GetLastError), ' => ', IntToStr(GetLastError));
        {$ENDIF}
        QuitExecution;
        Terminate;
        Break;
      end

      { Child thread is either busy or awaiting for input }
      else if dwRead = 0 then
      begin
        Sleep(nDelayTime);
        Continue;
      end

      { Read }
      else begin

        { Prepare buffer and read }
        SetLength(chBuf, dwRead);
        bSuccess := ReadFile(fOutputStream, chBuf[1], dwRead, dwReaded, nil);

        { What's wrong? }
        if not bSuccess then
        begin

          { Flush }
          QuitExecution;

          { Some reading error? }
          if GetLastError <> ERROR_BROKEN_PIPE then
          begin
            Terminate;
            Exit;
          end

          { 109 is pipe end, normal behaviour }
          else
            QuitExecution;

          { Exit from loop }
          Break;
        end

        { Output read data }
        else begin

          { Accumulate data }
          if fWaitForCompleteOutput then
          begin
            if Length(fOutputAccumulator) < Integer(dwReaded) then
              SetLength(fOutputAccumulator, dwRead * 2);
            Move(chBuf[1], fOutputAccumulator[Succ(fAccumulated)], dwReaded);
            Inc(fAccumulated, dwReaded);
          end

          { Output immidiately }
          else begin
            fOutputBuffer := Copy(chBuf, 1, dwReaded);
            Synchronize(FlushOutput);
          end;
        end;
      end;
    end;

  { Free buffer }
  finally
    SetLength(chBuf, 0);
  end;

  { Wait for process to finish. At this stage both pipes are dead }
  dwRead := 0;
  bSuccess := GetExitCodeProcess(fProcess, dwRead);
  if bSuccess then
  begin

    { Still running? }
    if dwRead = STILL_ACTIVE then
    begin
      dwRead := 0;
      WaitForSingleObject(fProcess, INFINITE);
      {bSuccess := }GetExitCodeProcess(fProcess, dwRead);
    end;
  end;

  { Done }
  fExitCode := dwRead;
  Synchronize(ProcessFinished);
end;

procedure TOutputCommandThread.FlushOutput;
begin
  if fWaitForCompleteOutput then Exit;
  if Assigned(fOnOutputBufferReady) then
    fOnOutputBufferReady(Self, fOutputBuffer);
end;

procedure TOutputCommandThread.OutputCompleted;
begin
  if fWaitForCompleteOutput and Assigned(fOnOutputCompleted) then
    fOnOutputCompleted(Self, fOutputAccumulator);
end;

procedure TOutputCommandThread.ProcessFinished;
begin
  if Assigned(fOnOutputProcessFinished) then
    fOnOutputProcessFinished(Self, fExitCode);
end;

{ TInputCommandThread }

// -----------------------------------------------------------------------------

constructor TInputCommandThread.Create(CreateSuspended, FreeOnTerm: Boolean;
  InStream, MonStream1, MonStream2, Process: THandle;
  const InitialInput: RawByteString);
begin
  { Initialize }
  inherited Create(CreateSuspended);
  FreeOnTerminate := FreeOnTerm;
  fHandleClosed := False;

  { Assign handles }
  fInputStream := InStream;
  fMonitorStream1 := MonStream1;
  fMonitorStream2 := MonStream2;
  fProcess := Process;

  { Prepare input data }
  fInitialInput := InitialInput;
  fWriteInitialInput := fInitialInput <> EmptyAnsiStr;
  New(fInput);
end;

destructor TInputCommandThread.Destroy;
begin
  Dispose(fInput);
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TInputCommandThread.Execute;
var
  dwRead, dwReaded, dwWritten: Cardinal;
  bSuccess: Boolean;
  chBuff: RawByteString;
  Hnd: THandle;

  procedure DoWriteInput;
  begin

    { Need to write initial input? }
    if fWriteInitialInput then
    begin

      { Initial input written }
      fWriteInitialInput := False;

      { Do buffered write }
      dwReaded := 0;
      while dwReaded < Length(fInitialInput) do
      begin

        { Write what's left }
        if Length(fInitialInput) - dwReaded >= nBufferSize then
          dwRead := nBufferSize
        else
          dwRead := Length(fInitialInput) - dwReaded;

        { Do write }
        if not WriteFile(fInputStream, fInitialInput[1], dwRead, dwWritten, nil) then
        begin
          Terminate;
          Break;
        end;

        { Inc step }
        Inc(dwReaded, dwWritten);
      end;

      { Finalize input buffer }
      SetLength(fInitialInput, 0);
      try
        Synchronize(SetHandleClosed);
        CloseHandle(fInputStream);
        fInputStream := INVALID_HANDLE_VALUE;
      finally
      end;
      Terminate;
    end

    { Input stream dead? }
    else if fInputStream = INVALID_HANDLE_VALUE then
      Terminate

    { Don't need initial input, simple open input prompt }
    else
      Synchronize(InputRequest);
  end;

begin
  { Wait for client to connect }
  if not SetSystem.SetAlwaysPresentInputPrompt then
    if fMonitorStream1 <> INVALID_HANDLE_VALUE then
      ConnectNamedPipe(fMonitorStream1, nil)
    else if fMonitorStream2 <> INVALID_HANDLE_VALUE then
      ConnectNamedPipe(fMonitorStream2, nil)
    else begin
      Terminate;
      Exit;
    end;

  { Wait for input until pipe is destroyed / abandoned / etc }
  try
  while True do
  begin

    { Terminated? }
    if Terminated then
      Exit;

    { Awaiting input? }
    if fAwaitingInput then
    begin
      Sleep(nDelayTime);
      Continue;
    end;

    { Canceled? }
    if fCanceled then
    begin
      Terminate;
      Exit;
    end;

    { Input ready? }
    if fInputReady then
    begin
      Synchronize(InputRequest);
      Continue;
    end;

    { Choose the way we are working }
    if SetSystem.SetAlwaysPresentInputPrompt then
    begin

      { Wait for pipe to signal }
      dwRead := WaitForSingleObject(fInputStream, INFINITE);

      { Terminate on bad return }
      if (dwRead = WAIT_FAILED) or (dwRead = WAIT_ABANDONED) or
        (dwRead = WAIT_TIMEOUT) then
      begin
        Terminate;
        Exit;
      end;

      { Do write operation }
      DoWriteInput;
    end

    else begin

      { Check monitor pipe }
      dwRead := 0;
      if fMonitorStream1 <> ERROR_INVALID_HANDLE then
        Hnd := fMonitorStream1
      else
        Hnd := fMonitorStream2;
      bSuccess := PeekNamedPipe(Hnd, nil, 0, nil, @dwRead, nil);

      { Try to read from alternative monitor pipe }
      if bSuccess and (dwRead = 0) and (Hnd = fMonitorStream1) then
      begin

        { Check second monitor pipe }
        Hnd := fMonitorStream2;
        bSuccess := PeekNamedPipe(Hnd, nil, 0, nil, @dwRead, nil);

        { It might be closed }
        if not bSuccess then
        begin
          bSuccess := True;
          Hnd := fMonitorStream1;
        end;
      end;

      { Both monitor pipes are dead. No reason for us to exist anymore }
      if not bSuccess then
      begin
        Terminate;
        Exit;
      end

      { No input request so far? }
      else if dwRead = 0 then
      begin
        Sleep(nDelayTime);
        Continue;
      end

      { Input requested }
      else begin

        { Read that byte of data to make PeekNamedPipe() stop reporting it
          and report only on next written byte }
        dwReaded := 0;
        SetLength(chBuff, dwRead);
        ReadFile(Hnd, chBuff[1], dwRead, dwReaded, nil);

        { Small delay }
        Sleep(nDelayTime);

        { Do write operation }
        DoWriteInput;
      end;
    end;
  end;
  finally
    SetLength(chBuff, 0);
  end;
end;

// -----------------------------------------------------------------------------

procedure TInputCommandThread.InputRequest;
var
  bEOF: Boolean;
  nWritten: Cardinal;
begin

  { Wait for user input }
  if not fInputReady then
    if Assigned(fOnInputRequested) then
    begin
      fInputReady := False;
      fCanceled := False;
      fAwaitingInput := True;
      SetLength(fInput^, 0);
      fOnInputRequested(Self, fInput);
      Exit;
    end
    else
      Terminate;

  { Write to stream }
  if fInputReady then
  begin
    fInputReady := False;
    if Length(fInput^) = 0 then
      Terminate
    else begin

      { See if EOD char has been passed }
      bEOF := fInput^[Length(fInput^)] = #26;

      { Write data to stream }
      if not WriteFile(fInputStream, fInput^[1], Length(fInput^) - Ord(bEOF), nWritten, nil) then
        Terminate

      { EOD char sent? }
      else if bEOF then
      try
        SetHandleClosed;
        CloseHandle(fInputStream);
        fInputStream := INVALID_HANDLE_VALUE;
      finally
      end;
    end;
  end

  { Input canceled }
  else
    Terminate;
end;

// -----------------------------------------------------------------------------

procedure TInputCommandThread.GotInput;
begin
  fInputReady := True;
  fCanceled := False;
  fAwaitingInput := False;
end;

procedure TInputCommandThread.Cancel;
begin
  fInputReady := False;
  fCanceled := True;
  fAwaitingInput := False;
end;

// -----------------------------------------------------------------------------

procedure TInputCommandThread.SetHandleClosed;
begin
  fHandleClosed := True;
end;

procedure TInputCommandThread.SetInputReady(Value: Boolean);
begin
  if Value then Synchronize(GotInput);
end;

procedure TInputCommandThread.SetCanceled(Value: Boolean);
begin
  if Value then Synchronize(Cancel);
end;

end.
