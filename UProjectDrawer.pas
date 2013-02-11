(*
  Letterpress

  Provides project drawer functionality.

  Copyright 2009-2010, Garnet
*)

unit UProjectDrawer;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ShlObj, ShellApi, UxTheme, ActiveX, Menus, ImgList, ExtCtrls,
  ActnList, Math, StrUtils,

  { Letterpress }
  UMonitor,

  { Virtual TreeView }
  VirtualTrees;

const
  sProjectTag: UTF8String = 'project {';
  sEnvironmentTag: UTF8String = 'environment {';
  sDirectoryTag: UTF8String = 'directory {';
  sFileTag: UTF8String = 'file {';
  sUUIDParam: UTF8String = 'uuid = ''';
  sTitleParam: UTF8String = 'title = ''';
  sPathParam: UTF8String = 'path = ''';
  sTail = ''';';
  sEnd: UTF8String = '}';

type

  { This data record contains all necessary information about a particular file
    system object. This can be either a folder (virtual or real) or a file.
    When FullPath is blank, it's assumed that this item is a virtual folder.
    When FullPath isn't bkank, it is either file or folder. If it's a folder,
    Attributes field will contain SFGAO_FOLDER flag }
  PProjectItem = ^TProjectItem;
  TProjectItem = packed record
    FullPath,               // Real path in file system
    Display: UnicodeString; // Text to display for node
    Attributes: Cardinal;   // Attributes of corresponding object in file system
    OpenIndex, CloseIndex: Integer; // Image indices in system image list
  end;

  TFrmProjectDrawer = class(TForm)
    Resizer: TPanel;
    Pages: TNotebook;
    VtExplorer: TVirtualStringTree;
    ActionList: TActionList;
    actProjectAddFile: TAction;
    actProjectAddReference: TAction;
    actProjectOpen: TAction;
    actProjectBrowse: TAction;
    actProjectRefresh: TAction;
    actProjectAddFolder: TAction;
    actProjectDelete: TAction;
    actProjectRename: TAction;
    DlgAddFile: TOpenDialog;
    actProjectSave: TAction;
    actProjectNew: TAction;
    actProjectLoad: TAction;
    actProjectSaveAs: TAction;
    ppmProjectDrawer: TPopupMenu;
    ppmProjectreferencedFolder: TPopupMenu;
    ppmProjectSingle: TPopupMenu;
    ppmProjectMultiple: TPopupMenu;
    ppmProjectVirtualFolder: TPopupMenu;
    mAddFile: TMenuItem;
    mAddFolder: TMenuItem;
    mAddReference: TMenuItem;
    mSingleOpen: TMenuItem;
    mSingleBrowse: TMenuItem;
    spSingleBrowse: TMenuItem;
    mSingleDelete: TMenuItem;
    mSingleRename: TMenuItem;
    mMultipleOpen: TMenuItem;
    mVirtualAddFile: TMenuItem;
    mVirtualAddFolder: TMenuItem;
    mVirtualAddReference: TMenuItem;
    spVirtualAddReference: TMenuItem;
    mVirtualDelete: TMenuItem;
    mVirtualRename: TMenuItem;
    mRefBrowse: TMenuItem;
    spRefBrowse: TMenuItem;
    mRefDelete: TMenuItem;
    mRefRename: TMenuItem;
    spRefRename: TMenuItem;
    mRefRefresh: TMenuItem;

    { Form }
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

    { VtExplorer }
    procedure VtExplorerInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VtExplorerFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VtExplorerInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure VtExplorerGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VtExplorerCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VtExplorerGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VtExplorerNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure VtExplorerPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure VtExplorerDragAllowed(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure VtExplorerDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure VtExplorerDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure VtExplorerGetPopupMenu(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; const P: TPoint;
      var AskParent: Boolean; var PopupMenu: TPopupMenu);
    procedure VtExplorerDblClick(Sender: TObject);
    procedure VtExplorerEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure VtExplorerNodeCopying(Sender: TBaseVirtualTree; Node,
      Target: PVirtualNode; var Allowed: Boolean);
    procedure VtExplorerNodeMoving(Sender: TBaseVirtualTree; Node,
      Target: PVirtualNode; var Allowed: Boolean);
    procedure VtExplorerClick(Sender: TObject);
    procedure VtExplorerBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VtExplorerIncrementalSearch(Sender: TBaseVirtualTree;
      Node: PVirtualNode; const SearchText: string; var Result: Integer);

    { Actions }
    procedure actProjectAddFolderExecute(Sender: TObject);
    procedure actProjectDeleteExecute(Sender: TObject);
    procedure actProjectDeleteUpdate(Sender: TObject);
    procedure actProjectRenameExecute(Sender: TObject);
    procedure actProjectRenameUpdate(Sender: TObject);
    procedure actProjectRefreshExecute(Sender: TObject);
    procedure actProjectRefreshUpdate(Sender: TObject);
    procedure actProjectAddFileExecute(Sender: TObject);
    procedure actProjectAddReferenceExecute(Sender: TObject);
    procedure actProjectBrowseExecute(Sender: TObject);
    procedure actProjectBrowseUpdate(Sender: TObject);
    procedure actProjectOpenExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actProjectOpenUpdate(Sender: TObject);
    procedure actProjectSaveAsExecute(Sender: TObject);
    procedure actProjectNewExecute(Sender: TObject);
    procedure actProjectSaveExecute(Sender: TObject);
    procedure actProjectSaveUpdate(Sender: TObject);
    procedure actProjectLoadExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    fHideLockCount: Integer;
    fModified: Boolean;
    fFileName, fUUID, fTitle: UnicodeString;
    fTarget: PVirtualNode;

    { File & folder functions }
    function GetNewFullName(const AFullPath, ADisplay: String): String;
    function GetDisplayNameFromFullPath(const AFullPath: String): String;
    function GetParentName(const AFullPath: String): String;
    function GetIconIndex(const AFileName: String; AFlags: Cardinal): Integer;
    procedure GetOpenAndClosedIcons(const AFileName: String;
      var Open, Closed: Integer);
    function GetAttributes(const FileName: UnicodeString): Cardinal;
    function GetNodeByFullPath(const AFullPath: String): PVirtualNode;

    { Drag & Drop }
    procedure UpdatePathsInMoved(ACopiedParent: PVirtualNode);

    { OLE Drag & Drop }
    procedure AddFilesOrFolders(DataObject: IDataObject;
      Target: TVirtualStringTree; Mode: TVTNodeAttachMode);
    procedure InsertData(Sender: TVirtualStringTree;
      DataObject: IDataObject; Formats: TFormatArray;
      Effect: Integer; Mode: TVTNodeAttachMode);

    { Project open / save }
    procedure WriteProjectToFile(const AFileName: String);
    function SaveToFile(const FileName: String; SaveAs: Boolean = False): Boolean;
  public
    Environment: TStringList;

    { Project management }
    function AskProjectSave: Boolean;
    function AddToProject(const AFileName, ADisplay: String;
      AParent: PVirtualNode = nil;
      APlacement: TVTNodeAttachMode = amAddChildLast): PVirtualNode;
    function CloseProject: Boolean;

    function GetSelectedFileOrFolder: UnicodeString;
    function GetSelectedFilesOrFolders: UnicodeString;

    procedure NewProject;

    { These are required to be exposed to main form }
    procedure SelectFileInProject(const FileName: String);
    procedure LoadProjectFromFile(const AFileName: String);
    function GetFileInProject(const AFullPath: String): Boolean;

    { Hide restrictions }
    procedure LockHide;
    procedure UnLockHide;

    { Properties }
    property FileName: String read fFileName write fFileName;
    property Modified: Boolean read fModified write fModified;
    property HideLock: Integer read fHideLockCount;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
  end;

  procedure ProcessDirectoryChanges(pInfo: TInfoCallback);

var
  FrmProjectDrawer: TFrmProjectDrawer;

implementation

uses
  UConst, SynUnicode;

{$R *.dfm}

function MenuCallback(Wnd: HWND; Msg: UINT; WParam: WPARAM;
 LParam: LPARAM): LRESULT; stdcall;
var
  ContextMenu2: IContextMenu2;
begin
  case Msg of
    WM_CREATE:
    begin
      ContextMenu2 := IContextMenu2(PCreateStruct(lParam).lpCreateParams);
      SetWindowLong(Wnd, GWL_USERDATA, Longint(ContextMenu2));
      Result := DefWindowProc(Wnd, Msg, wParam, lParam);
    end;
    WM_INITMENUPOPUP:
    begin
      ContextMenu2 := IContextMenu2(GetWindowLong(Wnd, GWL_USERDATA));
      ContextMenu2.HandleMenuMsg(Msg, wParam, lParam);
      Result := 0;
    end;
    WM_DRAWITEM, WM_MEASUREITEM:
    begin
      ContextMenu2 := IContextMenu2(GetWindowLong(Wnd, GWL_USERDATA));
      ContextMenu2.HandleMenuMsg(Msg, wParam, lParam);
      Result := 1;
    end;
  else
    Result := DefWindowProc(Wnd, Msg, wParam, lParam);
  end;
end;

// Это для создания самого меню, как оконного элемента
function CreateMenuCallbackWnd(const ContextMenu: IContextMenu2): HWND;
const
  IcmCallbackWnd = 'ICMCALLBACKWND';
var
  WndClass: TWndClass;
begin
  FillChar(WndClass, SizeOf(WndClass), #0);
  WndClass.lpszClassName := PChar(IcmCallbackWnd);
  WndClass.lpfnWndProc := @MenuCallback;
  WndClass.hInstance := HInstance;
  Windows.RegisterClass(WndClass);
  Result := CreateWindow(IcmCallbackWnd, IcmCallbackWnd, WS_POPUPWINDOW, 0,
    0, 0, 0, 0, 0, HInstance, Pointer(ContextMenu));
end;

procedure GetProperties(Path: String; MousePoint: TPoint; WC: TWinControl);
var
  CoInit, AResult: HRESULT;
  CommonDir, FileName: String;
  Desktop, ShellFolder: IShellFolder;
  pchEaten, Attr: Cardinal;
  PathPIDL: PItemIDList;
  FilePIDL: array [0..1] of PItemIDList;
  ShellContextMenu: HMenu;
  ICMenu: IContextMenu;
  ICMenu2: IContextMenu2;
  PopupMenuResult: BOOL;
  CMD: TCMInvokeCommandInfo;
  M: IMAlloc;
  ICmd: Integer;
  CallbackWindow: HWND;
begin
  // Первичная инициализация
  ShellContextMenu := 0;
  Attr := 0;
  PathPIDL := nil;
  CallbackWindow := 0;
  CoInit := CoInitializeEx(nil, COINIT_MULTITHREADED);
  try
    // Получаем пути и имя фала
    CommonDir := ExtractFilePath(Path);
    FileName := ExtractFileName(Path);
    // Получаем указатель на интерфейс рабочего стола
    if SHGetDesktopFolder(Desktop) <> S_OK then
      RaiseLastOSError;
    // Если работаем с папкой
    if FileName = '' then
    begin
      // Получаем указатель на папку "Мой компьютер"
      if (SHGetSpecialFolderLocation(0, CSIDL_DRIVES, PathPIDL) <> S_OK) or
        (Desktop.BindToObject(PathPIDL,  nil,  IID_IShellFolder,
          Pointer(ShellFolder)) <> S_OK) then RaiseLastOSError;
      // Получаем указатель на директорию
      ShellFolder.ParseDisplayName(WC.Handle, nil, StringToOleStr(CommonDir),
        pchEaten, FilePIDL[0], Attr);
      // Получаем указатель на контектсное меню папки
      AResult := ShellFolder.GetUIObjectOf(WC.Handle, 1, FilePIDL[0],
        IID_IContextMenu, nil, Pointer(ICMenu));
    end
    else
    begin
      // Получаем указатель на папку "Мой компьютер"
      if (Desktop.ParseDisplayName(WC.Handle, nil, StringToOleStr(CommonDir),
        pchEaten, PathPIDL, Attr) <> S_OK) or
        (Desktop.BindToObject(PathPIDL, nil, IID_IShellFolder,
          Pointer(ShellFolder)) <> S_OK) then RaiseLastOSError;
      // Получаем указатель на файл
      ShellFolder.ParseDisplayName(WC.Handle, nil, StringToOleStr(FileName),
        pchEaten, FilePIDL[0], Attr);
      // Получаем указатель на контектсное меню файла
      AResult := ShellFolder.GetUIObjectOf(WC.Handle, 1, FilePIDL[0],
        IID_IContextMenu, nil, Pointer(ICMenu));
    end;

    // Если указатель на конт. меню есть, делаем так:
    if Succeeded(AResult) then
    begin
      ICMenu2 := nil;
      // Создаем меню
      ShellContextMenu := CreatePopupMenu;
      // Производим его наполнение
      if Succeeded(ICMenu.QueryContextMenu(ShellContextMenu, 0,
        1, $7FFF, CMF_EXPLORE)) and
        Succeeded(ICMenu.QueryInterface(IContextMenu2, ICMenu2)) then
          CallbackWindow := CreateMenuCallbackWnd(ICMenu2);
      try
        // Показываем меню
        PopupMenuResult := TrackPopupMenu(ShellContextMenu, TPM_LEFTALIGN or TPM_LEFTBUTTON
          or TPM_RIGHTBUTTON or TPM_RETURNCMD,
          MousePoint.X, MousePoint.Y, 0, CallbackWindow, nil);
      finally
        ICMenu2 := nil;
      end;
      // Если был выбран какой либо пункт меню:
      if PopupMenuResult then
      begin
        // Индекс этого пункта будет лежать в ICmd
        ICmd := LongInt(PopupMenuResult) - 1;
        // Заполняем структуру TCMInvokeCommandInfo
        FillChar(CMD, SizeOf(CMD), #0);
        with CMD do
        begin
          cbSize := SizeOf(CMD);
          hWND := WC.Handle;
          lpVerb := PAnsiChar(MakeIntResource(ICmd));
          nShow := SW_SHOWNORMAL;
        end;
        // Выполняем InvokeCommand с заполненной структурой
        AResult := ICMenu.InvokeCommand(CMD);
        if AResult <> S_OK then RaiseLastOSError;
       end;
    end;
  finally
    // Освобождаем занятые ресурсы чтобы небыло утечки памяти
    if FilePIDL[0] <> nil then
    begin
      // Для освобождения использем IMalloc
      SHGetMAlloc(M);
      if M <> nil then
        M.Free(FilePIDL[0]);
      M:=nil;
    end;
    if PathPIDL <> nil then
    begin
      SHGetMAlloc(M);
      if M <> nil then
        M.Free(PathPIDL);
      M:=nil;
    end;
    if ShellContextMenu <>0 then
      DestroyMenu(ShellContextMenu);
    if CallbackWindow <> 0 then
      DestroyWindow(CallbackWindow);
    ICMenu := nil;
    ShellFolder := nil;
    Desktop := nil;
    if CoInit = S_OK then CoUninitialize;
  end;
end;

// -----------------------------------------------------------------------------
// Processing pool for all file and directory changes
procedure ProcessDirectoryChanges(pInfo: TInfoCallback);

  { Synchronize project tree with changes in file system } 
  procedure SynchronizeTree(AParentNode: PVirtualNode);
  var
    NewNode: PVirtualNode;
    Runner, Mirror: PVirtualNode;
    Data, NewData: PProjectItem;
    Compare: String;
  begin
    with FrmProjectDrawer do
    begin
      { Insert new node for that item, happens only in referenced folders }
      if pInfo.fAction = FILE_ACTION_ADDED then
      begin
        Runner := VtExplorer.GetFirstChild(AParentNode);
        if Assigned(Runner) then
          repeat
            Data := VtExplorer.GetNodeData(Runner);

            { Happens only in referenced folders }
            if Data^.Attributes and SFGAO_FOLDER <> 0 then

              { Add new node if children have been initialized }
              if (Data^.FullPath = pInfo.fFolder) and
                not ((vsHasChildren in Runner.States) and
                (Runner.ChildCount = 0)) then
              begin

                { Insert node }
                NewNode := VtExplorer.AddChild(Runner);
                NewData := VtExplorer.GetNodeData(NewNode);
                with NewData^ do
                begin
                  FullPath := pInfo.fFolder + pInfo.fNewFileName;
                  if DirectoryExists(FullPath) then
                    FullPath := IncludeTrailingBackslash(FullPath);
                  Display := '';
                end;
                VtExplorer.ReinitNode(NewNode, False);

                { Sort parent referenced folder }
                VtExplorer.Sort(Runner, -1, sdAscending, False);

                { Update flag }
                fModified := True;
              end
              else
                SynchronizeTree(Runner)

            { Simply search for folder in found virtual folder }
            else
              SynchronizeTree(Runner);

            { Go to next node }
            Runner := VtExplorer.GetNextSibling(Runner);
          until
            Runner = nil;
      end

      { Update / Delete existing item }
      else begin
        Runner := VtExplorer.GetFirstChild(AParentNode);
        if Assigned(Runner) then
          repeat

            { Get data, remember node, do step }
            Mirror := Runner;
            Data := VtExplorer.GetNodeData(Mirror);
            Runner := VtExplorer.GetNextSibling(Runner);

            { Construct correct string for comparison }
            if pInfo.fAction = FILE_ACTION_REMOVED then
              if Data^.Attributes and SFGAO_FOLDER <> 0 then
                Compare := IncludeTrailingBackslash(pInfo.fFolder + pInfo.fNewFileName)
              else
                Compare := pInfo.fFolder + pInfo.fNewFileName
            else
              if Data^.Attributes and SFGAO_FOLDER <> 0 then
                Compare := IncludeTrailingBackslash(pInfo.fFolder + pInfo.fOldFileName)
              else
                Compare := pInfo.fFolder + pInfo.fOldFileName;

            { Is our case? }
            if Data^.FullPath = Compare then
              case pInfo.fAction of

                { The object has been renamed }
                FILE_ACTION_RENAMED_OLD_NAME, FILE_ACTION_RENAMED_NEW_NAME:
                with Data^ do
                begin
                  if Attributes and SFGAO_FOLDER <> 0 then
                  begin
                    FullPath := IncludeTrailingBackslash(pInfo.fFolder + pInfo.fNewFileName);
                    UpdatePathsInMoved(Mirror);
                  end
                  else
                    FullPath := pInfo.fFolder + pInfo.fNewFileName;
                  Display := GetDisplayNameFromFullPath(FullPath);

                  { Sort parent referenced folder (if it is) and update flag }
                  if Attributes = 0 then
                    fModified := True
                  else begin
                    if Mirror.Parent <> VtExplorer.RootNode then
                      NewData := VtExplorer.GetNodeData(Mirror.Parent)
                    else
                      NewData := nil;

                    if (NewData <> nil) and (NewData^.Attributes <> 0) then
                      VtExplorer.Sort(Mirror.Parent, -1, sdAscending, False)
                    else
                      fModified := True;
                  end;
                end;

                { The object has been deleted }
                FILE_ACTION_REMOVED:
                begin
                  VtExplorer.DeleteNode(Mirror);
                  fModified := True;
                end;
              end
            else
              SynchronizeTree(Mirror);
          until
            Runner = nil;
      end;
    end;
  end;

begin
  { We aren't interested in contents modification for project drawer }
  if pInfo.fAction <> FILE_ACTION_MODIFIED then
    SynchronizeTree(FrmProjectDrawer.VtExplorer.RootNode);
end;

// -----------------------------------------------------------------------------

{ TFrmProjectDrawer }

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    Style := WS_POPUP or WS_BORDER or WS_THICKFRAME;
    ExStyle := WS_EX_TOOLWINDOW;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.WMActivate(var Message: TWMActivate);
begin
  inherited;

  if csDestroying in ComponentState then
    Exit;

//  { We're being activated }
//  if Message.Active > WA_INACTIVE then
//  begin
//    Message.Result := 0;
//
//    { Place project drawer under MainForm }
//    SetWindowPos(Handle, GetNextWindow(MainForm.Handle, GW_HWNDNEXT), 0, 0, 0,
//      0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
//  end
//
//  { We're being deactivated to non-main form window }
//  else
//    if MainForm.HandleAllocated and (Message.ActiveWindow <> MainForm.Handle) then
//    begin
//      Message.Result := 0;
//      if HideLock = 0 then
//        ShowWindow(Handle, SW_HIDE);
//    end;
end;

// -----------------------------------------------------------------------------
// Forbids OS window resizing
procedure TFrmProjectDrawer.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  inherited;
  Msg.Result := HTNOWHERE; // Restrict Windows resize as it forces window on top
                           // above main form resulting in ugly behaviour
end;

// -----------------------------------------------------------------------------
// Update tree appearance on theme change
procedure TFrmProjectDrawer.WMThemeChanged(var Message: TMessage);
begin
  if UseThemes then
    VtExplorer.TreeOptions.PaintOptions := VtExplorer.TreeOptions.PaintOptions +
      [toThemeAware, toUseExplorerTheme]
  else
    VtExplorer.TreeOptions.PaintOptions := VtExplorer.TreeOptions.PaintOptions -
      [toThemeAware, toUseExplorerTheme];
end;

{ Private declarations }

// -----------------------------------------------------------------------------

function TFrmProjectDrawer.AskProjectSave: Boolean;
begin
  Result := True;
//  LockHide;
//  try
//    if fModified then
//      case Msg(MainForm.Handle, Format('Do you want to save changes to “%s”?',
//        [ExtractFileName(fFileName)]), MB_ICONQUESTION or MB_YESNOCANCEL)
//      of
//        ID_YES: if not SaveToFile(fFileName) then Result := False;
//        ID_NO: Result := True;
//        ID_CANCEL: Result := False;
//        else
//          Result := False;
//      end;
//  finally
//    UnLockHide;
//  end;
end;

// -----------------------------------------------------------------------------
// Assemble new full name. WARNING! For a reason, the result is without
// backslash for folders. Caller should add slash itself
function TFrmProjectDrawer.GetNewFullName(const AFullPath,
  ADisplay: String): String;
var
  I: Integer;
begin
  Result := ExcludeTrailingBackslash(AFullPath);
  I := LastDelimiter(PathDelim, Result);
  if I < Length(Result) then
    Result := Copy(Result, 1, I - 1) + PathDelim + ADisplay
  else
    Result := AFullPath;
end;

// -----------------------------------------------------------------------------
// Extract portion of full path to get node display text
function TFrmProjectDrawer.GetDisplayNameFromFullPath(const AFullPath: String): String;
var
  I: Integer;
begin
  Result := ExcludeTrailingBackslash(AFullPath);
  I := LastDelimiter(PathDelim, Result);
  if I = Length(Result) then
    Result := Copy(Result, 1, Length(Result) - 1)
  else
    Result := Copy(Result, I + 1, MAXINT);
end;

// -----------------------------------------------------------------------------
// Returns blank string if already top-level path
function TFrmProjectDrawer.GetParentName(const AFullPath: String): String;
var
  I: Integer;
begin
  Result := ExcludeTrailingBackslash(AFullPath);
  I := LastDelimiter(PathDelim, Result);
  if I > -1 then
    Result := Copy(Result, 1, I) // Copy with PathDelim
  else
    Result := '';
end;

// -----------------------------------------------------------------------------
// Returns the index of the system icon for the given file object
function TFrmProjectDrawer.GetIconIndex(const AFileName: String;
  AFlags: Cardinal): Integer;
var
  SFI: TSHFileInfo;
begin
  if SHGetFileInfo(PChar(AFileName), 0, SFI, SizeOf(TSHFileInfo), AFlags) = 0 then
    Result := -1
  else
    Result := SFI.iIcon;
end;

// -----------------------------------------------------------------------------
// Get open & close indeces in system image list for a given file
procedure TFrmProjectDrawer.GetOpenAndClosedIcons(const AFileName: String;
  var Open, Closed: Integer);
begin
  Open := GetIconIndex(AFileName, SHGFI_SYSICONINDEX or SHGFI_SMALLICON or
    SHGFI_OPENICON);
  Closed := GetIconIndex(AFileName, SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
end;

// -----------------------------------------------------------------------------
// Get file system attributes for a given file
function TFrmProjectDrawer.GetAttributes(const FileName: UnicodeString): Cardinal;
const
  SFGAO_CONTENTSMASK = $F0000000; // This value is wrongly defined in ShlObj
var
  Desktop: IShellFolder;
  Eaten: Cardinal;
  PIDL: PItemIDList;
  Malloc: IMalloc;
begin
  { Get the root folder of the shell name space }
  SHGetDesktopFolder(Desktop);

  { While parsing the name also the shell object's attributes are determined.
    These is what we are really interested in }
  Result := SFGAO_DISPLAYATTRMASK or SFGAO_CONTENTSMASK or SFGAO_COMPRESSED;
  Desktop.ParseDisplayName(0, nil, PChar(FileName), Eaten, PIDL, Result);

  { Don't forget to free the returned PIDL.
    The shell folder is released automatically }
  SHGetMalloc(Malloc);
  Malloc.Free(PIDL);
end;

// -----------------------------------------------------------------------------
// Search for a node by it's full path
function TFrmProjectDrawer.GetNodeByFullPath(const AFullPath: String): PVirtualNode;

  function SearchForMatch(AParentNode: PVirtualNode): PVirtualNode;
  var
    Runner: PVirtualNode;
    Data: PProjectItem;
  begin
    Result := nil;
    Runner := VtExplorer.GetFirstChild(AParentNode);
    if Assigned(Runner) then
      repeat
        Data := VtExplorer.GetNodeData(Runner);
        if Data^.FullPath = AFullPath then
        begin
          Result := Runner;
          Exit;
        end;

        { Do not look inside referenced folders }
        if Data^.Attributes = 0 then
          Result := SearchForMatch(Runner);

        if Result <> nil then
          Exit;

        Runner := VtExplorer.GetNextSibling(Runner);
      until
        Runner = nil;
  end;

begin
  Result := SearchForMatch(VtExplorer.RootNode);
end;

// -----------------------------------------------------------------------------
function TFrmProjectDrawer.GetFileInProject(const AFullPath: String): Boolean;

  function DoCheckFileInProject(AParent: PVirtualNode): Boolean;
  var
    Runner: PVirtualNode;
    Data: PProjectItem;
  begin
    Result := False;
    Runner := VtExplorer.GetFirstChild(AParent);
    repeat
      Data := VtExplorer.GetNodeData(Runner);
      if Data^.FullPath = '' then
        Result := DoCheckFileInProject(Runner)
      else if Data^.Attributes = 0 then
        Result := Data^.FullPath = AFullPath;
      if Result then
        Break;
      Runner := VtExplorer.GetNextSibling(Runner);
    until
      Runner = nil;
  end;

begin
  Result := DoCheckFileInProject(VtExplorer.RootNode);
end;

// -----------------------------------------------------------------------------
// Update real file system paths for items with renamed parent referenced folder
procedure TFrmProjectDrawer.UpdatePathsInMoved(ACopiedParent: PVirtualNode);
var
  CopiedChild: PVirtualNode;
  ParentNodeData, CopiedNodeData: PProjectItem;
begin
  CopiedChild := VtExplorer.GetFirstChild(ACopiedParent);
  if Assigned(CopiedChild) then
  begin
    ParentNodeData := VtExplorer.GetNodeData(ACopiedParent);
    repeat
      CopiedNodeData := VtExplorer.GetNodeData(CopiedChild);
      with CopiedNodeData^ do
      begin
        { Referenced items always have attributes }
        if Attributes and SFGAO_FOLDER <> 0 then
          FullPath := ParentNodeData^.FullPath + Display + PathDelim
        else
          FullPath := ParentNodeData^.FullPath + Display;
      end;
      UpdatePathsInMoved(CopiedChild);
      CopiedChild := VtExplorer.GetNextSibling(CopiedChild);
    until
      CopiedChild = nil;
  end;
end;

// -----------------------------------------------------------------------------
// Get files dropped from Windows Explorer on us. All dropped folders become
// references to existing ones in file system. All dropped files are put into
// root or existing virtual folders. (Root is a virtual folder by itself)
procedure TFrmProjectDrawer.AddFilesOrFolders(DataObject: IDataObject;
  Target: TVirtualStringTree; Mode: TVTNodeAttachMode);
var
  FormatEtc: TFormatEtc;
  Medium: TStgMedium;
  OLEData: PDropFiles;
  Files: PAnsiChar;
  UStr: UnicodeString;
  AStr: AnsiString;
  TargetNode: PVirtualNode;
  TargetNodeData: PProjectItem;

  procedure TryToAddFileOrFolder(AFileName: String);
  var
    NewNode: PVirtualNode;
    NewData: PProjectItem;
    IsReference: Boolean;
  begin
    { Check if already exists }
    if DirectoryExists(AFileName) then
    begin
      AFileName := IncludeTrailingBackslash(AFileName);
      IsReference := True;
    end;
    if GetNodeByFullPath(AFileName) <> nil then
      Exit;

    { Dragging on root }
    if Mode = amNowhere then
      if IsReference then
        AddToProject(AFileName, '')
      else
        AddToProject(AFileName, GetDisplayNameFromFullPath(AFileName))

    { Dragging on node, before or after }
    else begin
      NewNode := VtExplorer.InsertNode(VtExplorer.DropTargetNode, Mode);
      NewData := VtExplorer.GetNodeData(NewNode);
      with NewData^ do
      begin
        FullPath := AFileName;
        if IsReference then
          Display := ''
        else
          Display := GetDisplayNameFromFullPath(FullPath);
      end;
    end;
  end;

begin
  with FormatEtc do
  begin
    cfFormat := CF_HDROP;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
  if DataObject.QueryGetData(FormatEtc) = S_OK then
  begin
    if DataObject.GetData(FormatEtc, Medium) = S_OK then
    begin
      OLEData := GlobalLock(Medium.hGlobal);
      if Assigned(OLEData) then
      begin
        Target.BeginUpdate;
        TargetNode := Target.DropTargetNode;
        if TargetNode = nil then
          TargetNode := Target.FocusedNode;
        if Assigned(TargetNode) then
          TargetNodeData := Target.GetNodeData(TargetNode);
        try
          Files := PAnsiChar(OLEData) + OLEData^.pFiles;
          while Files^ <> #0 do
          begin
            if OLEData^.fWide then
            begin
              UStr := PChar(Files);
              Inc(Files, (Length(PChar(Files)) + 1) * SizeOf(Char));
              TryToAddFileOrFolder(UnicodeString(UStr));
            end
            else begin
              AStr := PAnsiChar(Files);
              Inc(Files, (Length(PAnsiChar(Files)) + 1)*SizeOf(AnsiChar));
              TryToAddFileOrFolder(UnicodeString(AStr));
            end;
          end;
        finally
          GlobalUnlock(Medium.hGlobal);
          Target.EndUpdate;
        end;
      end;
      ReleaseStgMedium(Medium);
    end;
  end;
end;

//------------------------------------------------------------------------------
// Process various types of data. Used in OLE drag & drop
procedure TFrmProjectDrawer.InsertData(Sender: TVirtualStringTree;
  DataObject: IDataObject; Formats: TFormatArray;
  Effect: Integer; Mode: TVTNodeAttachMode);
var
  I: Integer;
  FormatAccepted: Boolean;
begin
  FormatAccepted := False;
  for I := 0 to High(Formats) do
  begin
    case Formats[I] of
      { Windows Explorer drop }
      CF_HDROP:
        if not FormatAccepted then
        begin
          AddFilesOrFolders(DataObject, Sender as TVirtualStringTree, Mode);
          FormatAccepted := True;
        end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.LoadProjectFromFile(const AFileName: String);
var
  F: TextFile;
  S: UTF8String;
  P: PAnsiChar;
  Valid: Boolean;

  {$INCLUDE UFormatInput.inc}

  { Reads project uuid and title }
  procedure ReadProjectInfo;
  begin
    Valid := False;
    RdLn;
    if IsTagRead(sUUIDParam) then begin fUUID := UTF8ToUnicodeString(GetValue(sUUIDParam)); RdLn; end else Exit;
    if IsTagRead(sTitleParam) then begin fTitle := UTF8ToUnicodeString(GetValue(sTitleParam)); RdLn; end;
    Valid := True;
  end;

  { Reads environment {... }
  procedure ReadEnvironment;
  var
    sName, sValue: UTF8String;
  begin
    if IsTagRead(sEnvironmentTag) then
    begin
      RdLn;
      while not IsTagRead(sEnd) do
      begin
        sName := GetName;
        sValue := GetValue(sName + ' = ''');
        Environment.Add(UTF8ToUnicodeString(sName + #9 + sValue));
        RdLn;
      end;
      RdLn;
    end;
  end;

  { Reads directory {... }
  procedure ReadDirectory(Parent: PVirtualNode);
  var
    NewNode: PVirtualNode;
    NewData: PProjectItem;
    bPath: Boolean;
  begin
    Valid := False;
    RdLn;
    bPath := False;
    while not IsTagRead(sEnd) do
    begin

      { Read path }
      if not bPath then
      begin
        if IsTagRead(sPathParam) then
        begin
          bPath := True;
          NewData := VtExplorer.GetNodeData(Parent);
          with NewData^ do
          begin
            FullPath := UTF8ToUnicodeString(GetValue(sPathParam));

            { Referenced? }
            if FullPath[Length(FullPath)] = PathDelim then
              Display := EmptyStr

            { Virtual }
            else begin
              Display := FullPath;
              FullPath := EmptyStr;
            end;
          end;
          RdLn;
        end
        else if Parent <> VtExplorer.RootNode then Exit
        else begin bPath := True; NewNode := Parent; end;
      end

      { Read subdirectories }
      else if IsTagRead(sDirectoryTag) then
      begin
        NewNode := VtExplorer.AddChild(Parent);
        ReadDirectory(NewNode);
        if not Valid then Exit;
      end

      { Read files right here }
      else if IsTagRead(sFileTag) then
      begin
        RdLn;
        if not IsTagRead(sPathParam) then Exit;
        NewNode := VtExplorer.AddChild(Parent);
        NewData := VtExplorer.GetNodeData(NewNode);
        with NewData^ do
        begin
          FullPath := UTF8ToUnicodeString(GetValue(sPathParam));
          Display := GetDisplayNameFromFullPath(FullPath);
        end;
        RdLn;
        if not IsTagRead(sEnd) then Exit;
        RdLn;
      end
      else Exit;
    end;
    Valid := IsTagRead(sEnd);
    RdLn;
  end;

  { Reads project {... }
  procedure ReadProject;
  begin
    Valid := IsTag(sProjectTag); if not Valid then Exit;
    ReadProjectInfo; if not Valid then Exit;
    ReadEnvironment;
    Valid := IsTagRead(sDirectoryTag); if not Valid then Exit;
    ReadDirectory(VtExplorer.RootNode); if not Valid then Exit;
    Valid := IsTagRead(sEnd);
  end;

begin

  { Initialize }
  if not FileExists(AFileName) then
    Exit;

  { Clear tree }
  VtExplorer.Clear;

  { Try to read project }
  VtExplorer.BeginUpdate;
  AssignFile(F, AFileName);
  Reset(F);
  try
    ReadProject;
    fModified := False;
    fFileName := AFileName;
  finally
    CloseFile(F);
    VtExplorer.EndUpdate;
  end;
end;

procedure TFrmProjectDrawer.SelectFileInProject(const FileName: String);
var
  Node: PVirtualNode;
  Data: PProjectItem;
begin
  Node := VtExplorer.GetFirst;
  repeat
    Data := VtExplorer.GetNodeData(Node);
    if (Data^.Attributes and SFGAO_FOLDER) = 0 then
      if Data^.FullPath = FileName then
      begin
        VtExplorer.ClearSelection;
        VtExplorer.Selected[Node] := True;
        VtExplorer.ScrollIntoView(Node, True);
        Break;
      end;
    Node := VtExplorer.GetNext(Node);
  until
    Node = nil;
end;

// -----------------------------------------------------------------------------
// Use writer class to care about buffer
procedure TFrmProjectDrawer.WriteProjectToFile(const AFileName: String);
var
  I: Integer;
  F: TextFile;

  {$INCLUDE UFormatOutput.inc}

  { Writes project uuid and title }
  procedure WriteInfo;
  begin
    WrLn(sUUIDParam + UnicodeStringToUTF8(fUUID) + sTail);
    if fTitle <> EmptyStr then
      WrLn(sTitleParam + UnicodeStringToUTF8(fTitle) + sTail);
  end;

  { Writes environment {... }
  procedure WriteEnvironment;
  var
    J: Integer;
  begin
    if Environment.Count = 0 then Exit;
    WrLn(sEnvironmentTag);
    Inc(I);
      for J := 0 to Pred(Environment.Count) do
        WrLn(UnicodeStringToUTF8(Environment.Names[J] + ' = ''' + Environment.ValueFromIndex[J] + ''';'));
    Dec(I);
    WrLn(sEnd);
  end;

  { Writes directory {... }
  procedure WriteDirectory(Parent: PVirtualNode);
  var
    First: PVirtualNode;
    Data: PProjectItem;
  begin
    if Parent = VtExplorer.RootNode then
    begin
      WrLn(sDirectoryTag);
      Inc(I);
    end;
    First := Parent.FirstChild;
    while First <> nil do
    begin
      Data := VtExplorer.GetNodeData(First);

      { Virtual directory }
      if Data^.FullPath = EmptyStr then
      begin
        WrLn(sDirectoryTag);
        Inc(I);
          WrLn(sPathParam + UnicodeStringToUTF8(Data^.Display) + sTail);
          if First.FirstChild <> nil then
            WriteDirectory(First);
        Dec(I);
        WrLn(sEnd);
      end

      { Referenced directory }
      else if Data^.Attributes and SFGAO_FOLDER <> 0 then
      begin
        WrLn(sDirectoryTag);
        Inc(I);
          WrLn(sPathParam + UnicodeStringToUTF8(IncludeTrailingPathDelimiter(Data^.FullPath)) + sTail);
        Dec(I);
        WrLn(sEnd);
      end

      { File }
      else begin
        WrLn(sFileTag);
        Inc(I);
          WrLn(sPathParam + UnicodeStringToUTF8(Data^.FullPath) + sTail);
        Dec(I);
        WrLn(sEnd);
      end;

      { Next node}
      First := First.NextSibling;
    end;
    if Parent = VtExplorer.RootNode then
    begin
      Dec(I);
      WrLn(sEnd);
    end;
  end;

  { Writes grammar {... }
  procedure WriteGrammar;
  begin
    WrLn(sProjectTag);
    Inc(I);
      WriteInfo;
      WriteEnvironment;
      WriteDirectory(VtExplorer.RootNode);
    Dec(I);
    WrLn(sEnd);
  end;

begin
  AssignFile(F, AFileName);
  try
    Rewrite(F);
    I := 0;
    WriteGrammar;
  finally
    CloseFile(F);
  end;
end;

// -----------------------------------------------------------------------------

function TFrmProjectDrawer.SaveToFile(const FileName: String;
  SaveAs: Boolean = False): Boolean;
begin
  Result := False;
//  if FileExists(FileName) and not SaveAs then
//  begin
//    WriteProjectToFile(FileName);
//    fModified := False;
//    Result := True;
//  end
//  else with MainForm do
//  begin
//    DlgSaveProject.InitialDir := GetSettingsDir + sProjectDir;
//    if not DirectoryExists(DlgSaveProject.InitialDir) then
//      if not CreateDir(DlgSaveProject.InitialDir) then
//        Exit;
//    DlgSaveProject.FileName := ExtractFileName(fFileName);
//    LockHide;
//    try
//      if DlgSaveProject.Execute(Handle) then
//      begin
//        fFileName := DlgSaveProject.FileName;
//        WriteProjectToFile(fFileName);
//        fModified := False;
//        Result := True;
//      end;
//    finally
//      UnLockHide;
//    end;
//  end;
end;

// -----------------------------------------------------------------------------

{ Form }

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.FormCreate(Sender: TObject);
var
  Dummy: TSHFileInfo;
begin
  fModified := False;
  fTarget := nil;

  VtExplorer.NodeDataSize := SizeOf(TProjectItem);
  Environment := TStringList.Create;
  Environment.NameValueSeparator := #9;

  { Set proper font }
  DesktopFont := True;

  { Prepare system image list }
//  with MainForm.imagesExplorer do
//  begin
//    Handle := SHGetFileInfo('', 0, Dummy, SizeOf(Dummy),
//      SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
//    ShareImages := True;
//  end;

  SendMessage(Handle, WM_THEMECHANGED, 0, 0);
end;

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Environment);
  VtExplorer.Clear;
end;

procedure TFrmProjectDrawer.FormResize(Sender: TObject);
begin
  with Resizer do
  begin
    Left := 0;
    Top := 0;
    Height := Self.ClientHeight;
  end;
end;

// -----------------------------------------------------------------------------
// Custom implemented resize
procedure TFrmProjectDrawer.PanelMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  NewWidth: Integer;
begin
  { Left button down? }
//  if GetAsyncKeyState(VK_LBUTTON) = -32768 then
//    if (MainForm.Left - Mouse.CursorPos.X) > 64 then
//    begin
//      NewWidth := MainForm.Left - Mouse.CursorPos.X + GetSystemMetrics(SM_CXSIZEFRAME) + 2;
//      if NewWidth < Constraints.MaxWidth then
//      begin
//        Left := Mouse.CursorPos.X;
//        Width := NewWidth;
//      end;
//    end;
end;

{ Public declarations }

// -----------------------------------------------------------------------------
// Adds an item to the project
function TFrmProjectDrawer.AddToProject(const AFileName, ADisplay: String;
  AParent: PVirtualNode = nil;
  APlacement: TVTNodeAttachMode = amAddChildLast): PVirtualNode;
var
  NewData: PProjectItem;
begin
  if Assigned(AParent) then
    Result := VtExplorer.InsertNode(AParent, APlacement)
  else
    Result := VtExplorer.AddChild(VtExplorer.RootNode);
  NewData := VtExplorer.GetNodeData(Result);
  with NewData^ do
  begin
    if DirectoryExists(AFileName) then
      FullPath := IncludeTrailingBackslash(AFileName)
    else
      FullPath := AFileName;
    Display := ADisplay;

    { New virtual folders and files don't have children } 
    if Length(Display) > 0 then
      Exclude(Result.States, vsHasChildren);
  end;
  if VtExplorer.UpdateCount = 0 then
  begin
    VtExplorer.ReinitNode(Result, False);
    if (Length(ADisplay) > 0) and Assigned(AParent) then
      VtExplorer.Expanded[AParent] := True;
  end;
  Modified := True;
end;

//------------------------------------------------------------------------------

function TFrmProjectDrawer.CloseProject: Boolean;
begin
  if AskProjectSave then
    Result := True
  else
    Result := False;
end;

function TFrmProjectDrawer.GetSelectedFileOrFolder: UnicodeString;
var
  vNode: PVirtualNode;
  vData: PProjectItem;
begin
  Result := EmptyStr;
  vNode := VtExplorer.GetFirstSelected;
  if Assigned(vNode) then
  begin
    vData := VtExplorer.GetNodeData(vNode);
    if FileExists(vData^.FullPath) then
      Result := vData^.FullPath;
  end;
end;

function TFrmProjectDrawer.GetSelectedFilesOrFolders: UnicodeString;
var
  I: Integer;
  vNodes: TNodeArray;
  vData: PProjectItem;
begin
  Result := EmptyStr;
  vNodes := VtExplorer.GetSortedSelection(False);
  for I := 0 to High(vNodes) do
  begin
    vData := VtExplorer.GetNodeData(vNodes[I]);
    if FileExists(vData^.FullPath) then
      Result := Result + '"' + vData^.FullPath + '" ';
  end;
  Result := TrimRight(Result);
end;

procedure TFrmProjectDrawer.NewProject;
begin
  VtExplorer.Clear;
  fFileName := 'New Project.project';
  fTitle := 'New Project';
  fUUID := GenerateUUID;
  fModified := False;
end;

//------------------------------------------------------------------------------

procedure TFrmProjectDrawer.LockHide;
begin
  Inc(fHideLockCount);
end;

procedure TFrmProjectDrawer.UnLockHide;
begin
  Dec(fHideLockCount);
  if fHideLockCount < 0 then
    fHideLockCount := 0;
end;

{ Actions }

//------------------------------------------------------------------------------

procedure TFrmProjectDrawer.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  if not Active or (fHideLockCount > 0) then
  begin
    Handled := True;
    (Action as TAction).Enabled := False;
  end
  else
    Handled := False;
end;

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.actProjectNewExecute(Sender: TObject);
begin
  if AskProjectSave then
    NewProject;
end;

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.actProjectLoadExecute(Sender: TObject);
begin
(*
  if AskProjectSave then
  begin
    LockHide;
    try
      with MainForm do
      begin
        DlgOpenProject.InitialDir := GetSettingsDir + sProjectDir;
        if not DirectoryExists(DlgOpenProject.InitialDir) then
          if not CreateDir(DlgOpenProject.InitialDir) then
            DlgOpenProject.InitialDir := '';
        if DlgOpenProject.Execute(MainForm.Handle) then
        begin
          fFileName := DlgOpenProject.FileName;
          LoadProjectFromFile(fFileName);
        end;
      end;
    finally
      UnLockHide;
    end;
  end;
*)
end;

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.actProjectSaveExecute(Sender: TObject);
begin
  SaveToFile(fFileName);
end;

procedure TFrmProjectDrawer.actProjectSaveUpdate(Sender: TObject);
begin
  actProjectSave.Enabled := fModified = True;
end;

// -----------------------------------------------------------------------------
// Save project under new file name
procedure TFrmProjectDrawer.actProjectSaveAsExecute(Sender: TObject);
begin
  SaveToFile(fFileName, True);
end;

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.actProjectAddFileExecute(Sender: TObject);
var
  I: Integer;
  Data: PProjectItem;
  Placement: TVTNodeAttachMode;
begin
  { Get correct parent for node }
  Placement := amAddChildLast;
  if Assigned(VtExplorer.FocusedNode) then
  begin
    Data := VtExplorer.GetNodeData(VtExplorer.FocusedNode);
    if (Data^.Attributes = 0) and (Data^.FullPath <> '') then
      Placement := amInsertAfter;
  end;
(*
  { Do add }
  LockHide;
  try
    if DlgAddFile.Execute(MainForm.Handle) then
    begin
      VtExplorer.BeginUpdate;
      try
        if Placement = amNoWhere then
          for I := 0 to DlgAddFile.Files.Count - 1 do
            { Add directories as references }
            if DirectoryExists(DlgAddFile.Files[I]) then
              AddToProject(DlgAddFile.Files[I], '', VtExplorer.FocusedNode, Placement)

            { Put files into virtual folder }
            else
              AddToProject(DlgAddFile.Files[I], ExtractFileName(DlgAddFile.Files[I]),
                VtExplorer.FocusedNode, Placement)

        else
          for I := DlgAddFile.Files.Count - 1 to 0 do
            { Add directories as references }
            if DirectoryExists(DlgAddFile.Files[I]) then
              AddToProject(DlgAddFile.Files[I], '', VtExplorer.FocusedNode, Placement)

            { Put files into virtual folder }
            else
              AddToProject(DlgAddFile.Files[I], ExtractFileName(DlgAddFile.Files[I]),
                VtExplorer.FocusedNode, Placement);
      finally
        VtExplorer.EndUpdate;
      end;
    end;
  finally
    UnLockHide;
  end;
*)
end;

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.actProjectAddFolderExecute(Sender: TObject);
var
  Data: PProjectItem;
  Target: PVirtualNode;
begin
  Target := fTarget;
  if Assigned(Target) and (Target <> VtExplorer.RootNode) then
  begin
    Data := VtExplorer.GetNodeData(Target);
    if Data^.FullPath <> EmptyStr then Target := VtExplorer.RootNode;
  end;
  VtExplorer.EditNode(AddToProject(EmptyStr, 'New Folder', Target, amAddChildLast),
    VtExplorer.Header.MainColumn);
end;

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.actProjectAddReferenceExecute(Sender: TObject);
var
  lpItemID:    PItemIDList;   
  BrowseInfo:  TBrowseInfo;   
  DisplayName: array[0..MAX_PATH] of Char;
  TempPath:    array[0..MAX_PATH] of Char;
  Data: PProjectItem;
begin
(*
  LockHide;
  try 
    FillChar(BrowseInfo, SizeOf(TBrowseInfo), #0);
    with BrowseInfo do
    begin
      hwndOwner := MainForm.Handle;
      pszDisplayName := @DisplayName;
      lpszTitle := 'Please, specify a directory:';
      ulFlags := BIF_RETURNONLYFSDIRS or $0040 or BIF_EDITBOX or BIF_STATUSTEXT;
    end;

    lpItemID := SHBrowseForFolder(BrowseInfo);
    if Assigned(lpItemId) then
    begin
      SHGetPathFromIDList(lpItemID, TempPath);
      GlobalFreePtr(lpItemID);
      if DirectoryExists(TempPath) then
      begin
        if Assigned(VtExplorer.FocusedNode) then
          Data := VtExplorer.GetNodeData(VtExplorer.FocusedNode)
        else
          Data := nil;
        if (Data = nil) or ((Data^.Attributes = 0) and (Data^.FullPath = '')) then
          AddToProject(TempPath, '', VtExplorer.FocusedNode)
        else if Assigned(Data) and (Data^.Attributes = 0) and (Data^.FullPath <> '') then
          AddToProject(TempPath, '', VtExplorer.FocusedNode, amInsertAfter);
      end;
    end;
  finally
    UnLockHide;
  end;
*)
end;

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.actProjectOpenExecute(Sender: TObject);
var
  I: Integer;
  Nodes: TNodeArray;
  Data: PProjectItem;
begin
(*
  with MainForm do
    try
      EditorsUpdating := True;
      Nodes := VtExplorer.GetSortedSelection(False);
      if Length(Nodes) > 0 then
        for I := 0 to High(Nodes) do
        begin
          Data := VtExplorer.GetNodeData(Nodes[I]);
          if ((Data^.Attributes = 0) and (Data^.FullPath <> '')) or
            ((Data^.Attributes <> 0) and (Data^.Attributes and SFGAO_FOLDER = 0))
          then
            CreateBuffer(Data^.FullPath);
        end;
    finally
      EditorsUpdating := False;
      with pnTabs do
        if SelectedTab.Index < Tabs.Count - 1 then
          SelectedTab := Tabs[SelectedTab.Index + 1];
    end;
*)
end;

procedure TFrmProjectDrawer.actProjectOpenUpdate(Sender: TObject);
begin
  actProjectOpen.Enabled := VtExplorer.SelectedCount > 0;
end;

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.actProjectBrowseExecute(Sender: TObject);
var
  Data: PProjectItem;
begin
  Data := VtExplorer.GetNodeData(VtExplorer.FocusedNode);
  if Data^.Attributes = 0 then
    if Data^.FullPath <> '' then
      ExploreAndSelectFile(ExtractFilePath(Data^.FullPath), Data^.Display)
    else
  else if Data^.Attributes and SFGAO_FOLDER <> 0 then
    ExploreAndSelectFile(Data^.FullPath, Data^.Display)
  else
    ExploreAndSelectFile(ExtractFilePath(Data^.FullPath), Data^.Display);
end;

procedure TFrmProjectDrawer.actProjectBrowseUpdate(Sender: TObject);
begin
  actProjectBrowse.Enabled := Assigned(fTarget);
end;

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.actProjectDeleteExecute(Sender: TObject);

  function ConstructDeletionPaths: String;
  const
    sItemDelim: String = #0;
    sBufferEnd: String = #0#0;
  var
    I, Len: Integer;
    Nodes: TNodeArray;
    Data, ParentData: PProjectItem;
    CorrectPath: String;
  begin
    Result := '';
    SetLength(Nodes, 0);
    Nodes := VtExplorer.GetSortedSelection(True);
    if Length(Nodes) > 0 then
    begin
      Len := 0;
      SetLength(Result, 512);
      for I := 0 to High(Nodes) do
      begin
        Data := VtExplorer.GetNodeData(Nodes[I]);
        if Nodes[I].Parent <> VtExplorer.RootNode then
          ParentData := VtExplorer.GetNodeData(Nodes[I]);
        if (Data^.Attributes <> 0) and (Nodes[I].Parent <> VtExplorer.RootNode) and
          (ParentData^.Attributes <> 0) then
        begin
          { Folders are required to have  }
          if Data^.Attributes and SFGAO_FOLDER <> 0 then
            CorrectPath := GetNewFullName(Data^.FullPath, '\' + Data^.Display)
          else
            CorrectPath := Data^.FullPath;
          if Length(Result) < Len + Length(CorrectPath) + 1 then
            SetLength(Result, Len + Length(CorrectPath) + 512);
          Move(CorrectPath[1], Result[Len + 1], SizeOf(Char) * Length(CorrectPath));
          Inc(Len, Length(CorrectPath));
          if I < High(Nodes) then
          begin
            Move(sItemDelim[1], Result[Len + 1], SizeOf(Char));
            Inc(Len, 1);
          end;
        end
        else
          VtExplorer.DeleteNode(Nodes[I]); // Delete virtual items right here.
                                           // This will also stop referencing
                                           // top level referenced folders
      end;
      if Len > 0 then
      begin
        if Length(Result) < Len + Length(sBufferEnd) then
          SetLength(Result, Len + Length(sBufferEnd));
        Move(sBufferEnd[1], Result[Len + 1], SizeOf(Char) * 2);
        Inc(Len, 2);
      end;
    end;
    SetLength(Result, Len);
  end;

var
  pFromItems: String;
  Struct: TSHFileOpStruct;
begin
(*
  LockHide;
  try
    case Msg(MainForm.Handle, 'Are you sure you want to remove selected items from project?',
      MB_YESNO or MB_ICONQUESTION)
    of
      ID_YES:
      begin
        pFromItems := ConstructDeletionPaths;
        if pFromItems <> '' then
        begin
          with Struct do
          begin
            Wnd := Handle;
            wFunc := FO_DELETE;
            pFrom := PChar(pFromItems);
            pTo := nil;
            fFlags := FOF_ALLOWUNDO;
            fAnyOperationsAborted := True;
            hNameMappings := nil;
            lpszProgressTitle := nil;
          end;
          SHFileOperation(Struct);
          { Since here can occur erros, warnings or other questions from OS,
            user can cancel deletion of specific file even if he had choosen
            to delete all earlier. So entries from tree will be deleted through
            monitoring system: one by one }
        end;
      end;
      ID_NO: { do nothing };
    end;
  finally
    UnLockHide;
  end;
*)
end;

procedure TFrmProjectDrawer.actProjectDeleteUpdate(Sender: TObject);
begin
  actProjectDelete.Enabled := VtExplorer.SelectedCount > 0;
end;

// -----------------------------------------------------------------------------
// Renaming node
procedure TFrmProjectDrawer.actProjectRenameExecute(Sender: TObject);
begin
  with VtExplorer do
    EditNode(FocusedNode, -1);
end;

procedure TFrmProjectDrawer.actProjectRenameUpdate(Sender: TObject);
begin
  actProjectRename.Enabled := Assigned(fTarget);
end;

//------------------------------------------------------------------------------
// Refreshing referenced node
procedure TFrmProjectDrawer.actProjectRefreshExecute(Sender: TObject);
var
  Data: PProjectItem;
begin
  with VtExplorer do
  begin
    BeginUpdate;
    try
      { Get node data }
      Data := GetNodeData(FocusedNode);

      { Blank display so that initialization routine will treat node as
        referenced and required to be refilled with children info and
        attributes }
      Data^.Display := '';

      { Free existing children (if there are) }
      ChildCount[FocusedNode] := 0;

      { Reinit and expand }
      ReinitNode(FocusedNode, False);
      Expanded[FocusedNode] := True;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TFrmProjectDrawer.actProjectRefreshUpdate(Sender: TObject);
begin
  actProjectRefresh.Enabled := Assigned(fTarget);
end;

// -----------------------------------------------------------------------------

{ VtExplorer }

// -----------------------------------------------------------------------------
// Clear focused node if clicking on empty area
procedure TFrmProjectDrawer.VtExplorerClick(Sender: TObject);
var
  P: TPoint;
begin
  P := VtExplorer.ScreenToClient(Mouse.CursorPos);
  with VtExplorer, P do
    fTarget := GetNodeAt(X, Y);
end;

// -----------------------------------------------------------------------------
// Sorting is performed only inside referenced items
procedure TFrmProjectDrawer.VtExplorerCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PProjectItem;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  { Folder are always before files. Check if both are folders or both are
    non-folders, but not different }
  if ((Data1^.Attributes xor Data2^.Attributes) and SFGAO_FOLDER) <> 0 then
    { One of both is a folder the other is a file }
    if (Data1^.Attributes and SFGAO_FOLDER) <> 0 then
      Result := -1
    else
      Result := 1
  else
    { Both are of same type (folder or file). Just compare captions }
    Result := WideCompareText(Data1^.FullPath, Data2^.FullPath);
end;

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.VtExplorerDblClick(Sender: TObject);
var
  Data: PProjectItem;
begin
(*
  if not Assigned(VtExplorer.FocusedNode) or (fHideLockCount > 0) then
    Exit;

  Data := VtExplorer.GetNodeData(VtExplorer.FocusedNode);
  if ((Data^.Attributes = 0) and (Data^.FullPath <> '')) or
    ((Data^.Attributes <> 0) and (Data^.Attributes and SFGAO_FOLDER = 0))
  then
    MainForm.CreateBuffer(Data^.FullPath);
*)
end;

// -----------------------------------------------------------------------------
// Allow drag & drop
procedure TFrmProjectDrawer.VtExplorerDragAllowed(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := fHideLockCount = 0;
end;

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.VtExplorerDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);

  procedure DetermineEffect;
  begin
    if Shift = [] then
      Effect := DROPEFFECT_MOVE
    else if ssCtrl in Shift then
      Effect := DROPEFFECT_COPY
    else
      Effect := DROPEFFECT_MOVE;
  end;

  procedure CopyDataToCopied(ASourceParent, ACopiedParent: PVirtualNode;
    AResetAttributes: Boolean = False);
  var
    SourceChild, CopiedChild: PVirtualNode;
    SourceNodeData, CopiedNodeData: PProjectItem;
  begin
    SourceChild := VtExplorer.GetFirstChild(ASourceParent);
    CopiedChild := VtExplorer.GetFirstChild(ACopiedParent);
    if Assigned(SourceChild) and Assigned(CopiedChild) then
      repeat
        SourceNodeData := VtExplorer.GetNodeData(SourceChild);
        CopiedNodeData := VtExplorer.GetNodeData(CopiedChild);
        with CopiedNodeData^ do
        begin
          FullPath := SourceNodeData^.FullPath;
          Display := SourceNodeData^.Display;
          if AResetAttributes then
          begin
            if Attributes and SFGAO_FOLDER <> 0 then
              FullPath := '';
            Attributes := 0;
          end
          else
            Attributes := SourceNodeData^.Attributes;
          OpenIndex := SourceNodeData^.OpenIndex;
          CloseIndex := SourceNodeData^.CloseIndex;
        end;
        CopyDataToCopied(SourceChild, CopiedChild, AResetAttributes);
        SourceChild := VtExplorer.GetNextSibling(SourceChild);
        CopiedChild := VtExplorer.GetNextSibling(CopiedChild);
      until
        (SourceChild = nil) or (CopiedChild = nil);
  end;

var
  Attachmode: TVTNodeAttachMode;
  Nodes: TNodeArray;
  NewNode, DropTarget: PVirtualNode;
  ResultNodeData,
  SourceNodeData,
  DropTargetData,
  ParentNodeData: PProjectItem;
  I: Integer;
  SourceName,
  TargetName,
  RealSourceName,
  RealTargetName: String;
  Struct: TSHFileOpStruct;
begin
(*
  { Just to be sure }
  if fHideLockCount > 0 then
    Exit;

  { Determine attach mode }
  case Mode of
    dmAbove:  AttachMode := amInsertBefore;
    dmOnNode: AttachMode := amAddChildLast;
    dmBelow:  AttachMode := amInsertAfter;
  else
    AttachMode := amNowhere;
  end;

  { Internal drag }
  if Source = Sender then
  begin
    DetermineEffect;
    Nodes := VtExplorer.GetSortedSelection(True);
    DropTarget := VtExplorer.DropTargetNode;
    if (Mode = dmAbove) or (Mode = dmBelow) then
      DropTarget := DropTarget.Parent;
    if (DropTarget = VtExplorer.RootNode) or (DropTarget = nil) then
      DropTargetData := nil
    else
      DropTargetData := VtExplorer.GetNodeData(Sender.DropTargetNode);

    { Copying }
    if Effect = DROPEFFECT_COPY then
    begin
      for I := 0 to High(Nodes) do
      begin
        SourceNodeData := VtExplorer.GetNodeData(Nodes[I]);

        { Dragging into virtual folder }
        if (DropTargetData = nil) or
          (DropTargetData^.Attributes = 0) then
        begin
          { No reason for same virtual items }
          if SourceNodeData^.Attributes = 0 then
            Continue;

          { Add to virtual folder. DROPEFFECT_COPY is suitable to make user
            able to copy referenced folders to virtual without making copied
            nodes virtual (they will stay as referenced). Files, by the way,
            always loose their referenced attributes }
          NewNode := VtExplorer.CopyTo(Nodes[I], Sender.DropTargetNode,
            AttachMode, False);
          with NewNode^ do
          begin
            Include(States, vsInitialized);
            Exclude(States, vsSelected);
          end;

          ResultNodeData := VtExplorer.GetNodeData(NewNode);
          with ResultNodeData^ do
          begin
            FullPath := SourceNodeData^.FullPath;
            Display := SourceNodeData^.Display;
            if SourceNodeData^.Attributes and SFGAO_FOLDER <> 0 then
              Attributes := SourceNodeData^.Attributes
            else
              Attributes := 0;
            OpenIndex := SourceNodeData^.OpenIndex;
            CloseIndex := SourceNodeData^.CloseIndex;
          end;
          CopyDataToCopied(Nodes[I], NewNode);
        end

        { Dragging into referenced folder }
        else begin

          { Skip virtual folders }
          if SourceNodeData^.FullPath = '' then
            Continue;

          { Check if item is a folder and ensure path is correct }
          if SourceNodeData^.Attributes and SFGAO_FOLDER <> 0 then
            SourceName := GetNewFullName(SourceNodeData^.FullPath, PathDelim + SourceNodeData^.Display) + #0
          else
            SourceName := SourceNodeData^.FullPath + #0;
          if Mode = dmOnNode then
            TargetName := DropTargetData^.FullPath + #0
          else begin
            ParentNodeData := Sender.GetNodeData(Sender.DropTargetNode.Parent);
            TargetName := ParentNodeData^.FullPath + #0;
          end;

          { Do file operation }
          with Struct do
          begin
            Wnd := MainForm.Handle;
            wFunc := FO_COPY;
            pFrom := PChar(SourceName);
            pTo := PChar(TargetName);
            fFlags := FOF_ALLOWUNDO or FOF_RENAMEONCOLLISION;
            fAnyOperationsAborted := True;
            hNameMappings := nil;
            lpszProgressTitle := nil;
          end;
          LockHide;
          try
            SHFileOperation(Struct);
          finally
            UnLockHide;
          end;
          { Don't do anything. New nodes will appear through monitoring }
        end;
      end;
    end

    { Moving }
    else for I := 0 to High(Nodes) do
    begin
      SourceNodeData := VtExplorer.GetNodeData(Nodes[I]);

      { Dragging into virtual folder }
      if (DropTargetData = nil) or
        (DropTargetData^.Attributes = 0) then
      begin

        { Source node is from referenced folder (or is one). Copy nodes and
          make them virtual now }
        if SourceNodeData^.Attributes > 0 then
        begin
          ParentNodeData := nil;
          if Nodes[I].Parent <> Sender.RootNode then
            ParentNodeData := Sender.GetNodeData(Nodes[I].Parent);

          { Source node is referenced file. Will need to copy }
          if SourceNodeData^.Attributes and SFGAO_FOLDER = 0 then
          begin
            NewNode := VtExplorer.CopyTo(Nodes[I], Sender.DropTargetNode,
              AttachMode, False);

            ResultNodeData := VtExplorer.GetNodeData(NewNode);
            with ResultNodeData^ do
            begin
              if Attributes and SFGAO_FOLDER <> 0 then
                FullPath := ''
              else
                FullPath := SourceNodeData^.FullPath;
              Display := SourceNodeData^.Display;
              Attributes := 0;
              OpenIndex := SourceNodeData^.OpenIndex;
              CloseIndex := SourceNodeData^.CloseIndex;
            end;
            CopyDataToCopied(Nodes[I], NewNode, True);
          end

          { Source node is referenced folder. Will need to copy. This will
            create another referenced folder }
          else if (SourceNodeData^.Attributes and SFGAO_FOLDER <> 0)
            and (Nodes[I].Parent <> Sender.RootNode)
            and (ParentNodeData^.Attributes <> 0) then
          begin
            NewNode := VtExplorer.CopyTo(Nodes[I], Sender.DropTargetNode,
              AttachMode, False);

            { Copy data also, from source node and it's subnodes }
            ResultNodeData := VtExplorer.GetNodeData(NewNode);
            with ResultNodeData^ do
            begin
              FullPath := SourceNodeData^.FullPath;
              Display := SourceNodeData^.Display;
              Attributes := SourceNodeData^.Attributes;
              OpenIndex := SourceNodeData^.OpenIndex;
              CloseIndex := SourceNodeData^.CloseIndex;
            end;
            CopyDataToCopied(Nodes[I], NewNode);
          end

          { Source is top-level referenced folder. Can move }
          else begin
            Effect := DROPEFFECT_COPY;
            VtExplorer.MoveTo(Nodes[I], VtExplorer.DropTargetNode, AttachMode, False);
          end;
        end

        { Source node is from virtual folder or root. Can move }
        else begin
          Effect := DROPEFFECT_COPY;
          VtExplorer.MoveTo(Nodes[I], VtExplorer.DropTargetNode, AttachMode, False);
        end;
      end

      { Dragging into referenced folder. Can move }
      else begin
        { Move only referenced items too. Items from virtual folders and root
          shouldn't be touched }
        if SourceNodeData^.Attributes = 0 then
          Continue;

        { Get parent node data }
        if Mode = dmOnNode then
          ParentNodeData := Sender.GetNodeData(Sender.DropTargetNode)
        else
          ParentNodeData := Sender.GetNodeData(Sender.DropTargetNode.Parent);

        { Find source and destination paths }
        RealSourceName := SourceNodeData^.FullPath;
        if SourceNodeData^.Attributes and SFGAO_FOLDER <> 0 then
        begin
          SourceName := GetNewFullName(SourceNodeData^.FullPath, PathDelim + SourceNodeData^.Display) + #0;
          TargetName := ParentNodeData^.FullPath + SourceNodeData^.Display + PathDelim + #0;
          RealTargetName := ParentNodeData^.FullPath + SourceNodeData^.Display + PathDelim;
        end
        else begin
          SourceName := SourceNodeData^.FullPath + #0;
          TargetName := ParentNodeData^.FullPath + SourceNodeData^.Display + #0;
          RealTargetName := ParentNodeData^.FullPath + SourceNodeData^.Display;
        end;

        { If paths are different }
        if RealSourceName <> RealTargetName
        then begin

          { Do file operation }
          with Struct do
          begin
            Wnd := MainForm.Handle;
            wFunc := FO_MOVE;
            pFrom := PChar(SourceName);
            pTo := PChar(TargetName);
            fFlags := FOF_ALLOWUNDO or FOF_RENAMEONCOLLISION;
            fAnyOperationsAborted := True;
            hNameMappings := nil;
            lpszProgressTitle := nil;
          end;
          LockHide;
          try
            SHFileOperation(Struct);
          finally
            UnLockHide;
          end;
          { Node will be deleted through monitoring. The node will appear after
            actual moving in file system through monitoring }
        end;
      end;
    end;
  end

  { Dragging from document tab }
  else if Source = MainForm.pnTabs then
    if (FileExists(MainForm.Edit.FileName)) and
      (GetNodeByFullPath(MainForm.Edit.FileName) = nil)
    then
      AddToProject(MainForm.Edit.FileName, MainForm.pnTabs.SelectedTab.Caption,
        Sender.DropTargetNode, Attachmode)
    else

  { OLE drag & drop }
  else begin
    if Effect and DROPEFFECT_COPY <> 0 then
      Effect := DROPEFFECT_COPY
    else
      Effect := DROPEFFECT_MOVE;
    InsertData(Sender as TVirtualStringTree, DataObject, Formats, Effect,
      AttachMode);
  end;
*)
end;

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.VtExplorerDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);

  function IsNodeParent(AParent, ANode: PVirtualNode): Boolean;
  var
    NextParent: PVirtualNode;
  begin
    NextParent := AParent;
    repeat
      NextParent := NextParent.Parent;
    until
      (NextParent = Sender.RootNode) or (NextParent = nil) or
        (NextParent = ANode);
    Result := ANode = NextParent;
  end;

var
  I: Integer;
  Nodes: TNodeArray;
  Data, SourceData: PProjectItem;
begin
  { Restrict all drag operations during update }
  if fHideLockCount > 0 then
  begin
    Accept := False;
    Exit;
  end;

  { Require drop target if not OLE drag }
  if Source = Sender then
    if not Assigned(Sender.DropTargetNode) then
      Exit;

  Accept := True;

  { Get drop target data }
  if (Sender.DropTargetNode = Sender.RootNode) or
    (Sender.DropTargetNode = nil)
  then
    Data := nil
  else
    Data := Sender.GetNodeData(Sender.DropTargetNode);

  { Restrict dragging onto file }
  if Mode = dmOnNode then
    if Data <> nil then
      if ((Data^.Attributes > 0) and (Data^.Attributes and SFGAO_FOLDER = 0))
        or ((Data^.Attributes = 0) and (Data^.FullPath <> '')) then
      begin
        Accept := False;
        Exit;
      end;

  { Self drag & drop }
  if Source = Sender then
  begin

  { Get selected nodes }
  SetLength(Nodes, 0);
  if Data <> nil then
    Nodes := (Sender as TVirtualStringTree).GetSortedSelection(True);

  { There must be at least one }
  if Length(Nodes) > 0 then

    { Iterate through each node and do checks }
    for I := 0 to High(Nodes) do
    begin

      { Restrict dragging inside self }
      Accept := not IsNodeParent(Sender.DropTargetNode, Nodes[I])
        and not (Sender.DropTargetNode = Nodes[I]);
      if not Accept then
        Exit;

      { We will need selected node data for other checks }
      SourceData := Sender.GetNodeData(Nodes[I]);

      { Restrict copying items from virtual folders or root to virtual
        folders or root }
      if ssCtrl in Shift then
        if SourceData^.Attributes = 0 then
          if Data^.Attributes = 0 then
            Accept := False;
      if not Accept then
        Exit;

      { Restrict moving file from virtual folder or root into referenced
        folder }
      if not (ssCtrl in Shift) then
        if Data^.Attributes and SFGAO_FOLDER <> 0 then
          if SourceData^.Attributes = 0 then
            Accept := False;
      if not Accept then
        Exit;
    end;
  end

  { OLE drag & drop }
  else if Data <> nil then

    { Restrict dragging into referenced folder }
    if Data^.Attributes <> 0 then
      Accept := False;

end;

procedure TFrmProjectDrawer.VtExplorerEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := fHideLockCount = 0;
end;

// -----------------------------------------------------------------------------
// Free record resources and stop monitor for folder is needed
procedure TFrmProjectDrawer.VtExplorerFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data, ParentData: PProjectItem;
begin
  Data := Sender.GetNodeData(Node);

  { Freeing referenced folder }
  if Data^.Attributes and SFGAO_FOLDER <> 0 then
  begin
    if Node.Parent <> Sender.RootNode then
      ParentData := Sender.GetNodeData(Node.Parent)
    else
      ParentData := nil;
    if (ParentData = nil) or (ParentData^.Attributes = 0) then
      StopWatch(GetParentName(Data^.FullPath));
    StopWatch(Data^.FullPath);
  end

  { Freeing virtual folder or file }
  else if (Data^.Attributes = 0) and (Data^.FullPath <> '') then
    StopWatch(GetParentName(Data^.FullPath));

  { Free resources }
  Finalize(Data^);
end;

// -----------------------------------------------------------------------------
// Returns the proper node image which has been determined on initialization
// time. Also overlay images are used properly for shared folders
procedure TFrmProjectDrawer.VtExplorerGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PProjectItem;
begin
  Data := Sender.GetNodeData(Node);
  case Kind of
    ikNormal,
    ikSelected:
      begin
        if Sender.Expanded[Node] then
          ImageIndex := Data^.OpenIndex
        else
          ImageIndex := Data^.CloseIndex;
      end;
    ikOverlay:
      if (Data.Attributes and SFGAO_SHARE) <> 0 then
        ImageIndex := 0
      else
        if (Data.Attributes and SFGAO_LINK) <> 0 then
          ImageIndex := 1;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.VtExplorerGetPopupMenu(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const P: TPoint;
  var AskParent: Boolean; var PopupMenu: TPopupMenu);
var
  Data: PProjectItem;
begin
  { Clear focused node if clicking on empty area }
  with VtExplorer, P do
    fTarget := GetNodeAt(X, Y);

  { Empty area? }
  if not Assigned(Node) then
    PopupMenu := PpmProjectDrawer

  { Multiple items? } 
  else if Sender.SelectedCount > 1 then
    PopupMenu := PpmProjectMultiple

  { Single item }
  else begin
    Data := Sender.GetNodeData(Node);

    { Virtual fodler? }
    if (Data^.Attributes = 0) and (Data^.FullPath = '') then
      PopupMenu := PpmProjectVirtualFolder

    { Referenced folder? }
    else if Data^.Attributes and SFGAO_FOLDER <> 0 then
      PopupMenu := PpmProjectReferencedFolder

    { File (from whatever folder) }
    else begin
      if (Node.Parent = Sender.RootNode) then
        Data := nil
      else
        Data := Sender.GetNodeData(Node.Parent);
      
      { If above file from virtual folder or root and not selected, then
        show addition popup }
      if (Sender.SelectedCount = 0) and ((Data = nil) or (Data.Attributes = 0)) then
        PopupMenu := PpmProjectDrawer
        
      { Show file popup } 
      else
        PopupMenu := PpmProjectSingle;
    end;   
  end;
end;

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.VtExplorerGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PProjectItem;
begin
  Data := Sender.GetNodeData(Node);
  CellText := Data^.Display;
end;

// -----------------------------------------------------------------------------
// Simple file search
procedure TFrmProjectDrawer.VtExplorerIncrementalSearch(
  Sender: TBaseVirtualTree; Node: PVirtualNode; const SearchText: string;
  var Result: Integer);
var
  Data: PProjectItem;
  S1, S2: String;
begin
  if fHideLockCount > 0 then
  begin
    Result := 0;
    Exit;
  end;
  Data := Sender.GetNodeData(Node);
  S1 := WideLowerCase(SearchText);
  S2 := WideLowerCase(Data^.Display);
  Result := StrLComp(PChar(S1), PChar(S2), Min(Length(S1), Length(S2)));
end;

// -----------------------------------------------------------------------------
// InitNode / InitChildren are for referenced folders only as virtual folders
// and files are enirely initialized during loading of project file.
// Children items for referenced folders aren't filled instantly
procedure TFrmProjectDrawer.VtExplorerInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  Data,
  ChildData: PProjectItem;
  ChildNode: PVirtualNode;
  SR: TSearchRec;
begin
  Data := Sender.GetNodeData(Node);

  { Add sub items for referenced folder }
  if Data^.Attributes > 0 then
    if FindFirst(IncludeTrailingBackslash(Data^.FullPath) + '*.*', faAnyFile - faHidden - faSysFile,
      SR) = 0 then
    begin
      Screen.Cursor := crHourGlass;
      try
        repeat
          if (SR.Name <> '.') and (SR.Name <> '..') then
          begin
            ChildNode := Sender.AddChild(Node);
            ChildData := Sender.GetNodeData(ChildNode);
            with ChildData^ do
              if SR.Attr and faDirectory <> 0 then
                FullPath := IncludeTrailingBackslash(Data^.FullPath) + SR.Name + PathDelim
              else
                FullPath := IncludeTrailingBackslash(Data^.FullPath) + SR.Name;
            Sender.ReinitNode(ChildNode, False);
          end;
        until
          FindNext(SR) <> 0;
        ChildCount := Sender.ChildCount[Node];

        { Sort node }
        if ChildCount > 0 then
          Sender.Sort(Node, 0, TVirtualStringTree(Sender).Header.SortDirection,
            False);
      finally
        FindClose(SR);
        Screen.Cursor := crDefault;
      end;
    end

  { Need to do anything here? }
  else begin

  end;
end;

// -----------------------------------------------------------------------------
// InitNode / InitChildren are for referenced folders only as virtual folders
// and files are enirely initialized during loading of project file
procedure TFrmProjectDrawer.VtExplorerInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data, ParentData: PProjectItem;
begin
  Data := Sender.GetNodeData(Node);

  { Node with blank display denotes referenced item }
  if Data^.Display = '' then
  begin
    { Fill node data in }
    with Data^ do
    begin
      Display := GetDisplayNameFromFullPath(FullPath);
      GetOpenAndClosedIcons(FullPath, OpenIndex, CloseIndex);
      Attributes := GetAttributes(Data^.FullPath);
    end;

    { Check for subitems }
    if Data^.Attributes and SFGAO_FOLDER <> 0 then
    begin
      if GetHasChildren(Data^.FullPath) then
      begin
        Include(InitialStates, ivsHasChildren);
        Include(Node.States, vsHasChildren);
      end
      else begin
        Exclude(InitialStates, ivsHasChildren);
        Exclude(Node.States, vsHasChildren);
      end;

      { Start to monitor parent folder of that item, if it is a top level
        referenced folder }
      if ParentNode <> Sender.RootNode then
        ParentData := Sender.GetNodeData(ParentNode)
      else
        ParentData := nil;
      if (ParentData = nil) or (ParentData^.Attributes = 0) then
        StartWatch(GetParentName(Data^.FullPath), MONITOR_FLAGS,
          False, TWatchFileSystemCallback(@ProcessDirectoryChanges));

      { Start to monitor this folder }
      StartWatch(Data^.FullPath, FILE_NOTIFY_CHANGE_FILE_NAME or FILE_NOTIFY_CHANGE_DIR_NAME, False,
        TWatchFileSystemCallback(@ProcessDirectoryChanges));
    end
    else begin
      Exclude(InitialStates, ivsHasChildren);
      Exclude(Node.States, vsHasChildren);

      { Do not start monitor for parent folders of files. They already will have
        a monitor runnung for them after initialization of their parent folder }
    end;
  end

  { Item from virtual folder or root, or virtual folder itself.
    Retrieve image indices only and blank attributes }
  else with Data^ do
  begin
    { Always start monitor for files in virtual folders }
    if FileExists(FullPath) then
      StartWatch(GetParentName(Data^.FullPath),
        FILE_NOTIFY_CHANGE_FILE_NAME or FILE_NOTIFY_CHANGE_DIR_NAME,
        False, TWatchFileSystemCallback(@ProcessDirectoryChanges));
    GetOpenAndClosedIcons(FullPath, OpenIndex, CloseIndex);
    Attributes := 0;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.VtExplorerNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var
  Data, ParentData: PProjectItem;
  NewPath: String;
begin
  { File name can not be blank. Also, restrict blank virtual folder names } 
  if NewText = '' then
    Exit;

  Data := Sender.GetNodeData(Node);

  { Renaming item from referenced folder (or referenced folder itself) }
  if Data^.Attributes > 0 then
  begin
    NewPath := GetNewFullName(Data^.FullPath, NewText);

    { Update full paths of sub nodes }
    if RenameFile(Data^.FullPath, NewPath) then
    begin
      UpdatePathsInMoved(Node);

      { Do not update node data here.
        It will be updated through monitoring upon receivng update
        notification from OS }

      { Modify project if renamed top level referenced folder }
      if Node.Parent = Sender.RootNode then
        fModified := True
      else begin
        ParentData := Sender.GetNodeData(Node.Parent);
        if ParentData^.Attributes = 0 then
          fModified := True;
      end;
    end;
  end

  { Renaming file from virtual folder or root }
  else if FileExists(Data^.FullPath) then
  begin
    NewPath := GetNewFullName(Data^.FullPath, NewText);
    
    RenameFile(Data^.FullPath, NewPath);
    
    { Do not update node data here.
      It will be updated through monitoring upon receivng update
      notification from OS }
  end

  { Renaming virtual folder }
  else begin
    Data^.Display := NewText;
    fModified := True;
  end;
end;

procedure TFrmProjectDrawer.VtExplorerNodeCopying(Sender: TBaseVirtualTree;
  Node, Target: PVirtualNode; var Allowed: Boolean);
begin
  Allowed := fHideLockCount = 0;
end;

procedure TFrmProjectDrawer.VtExplorerNodeMoving(Sender: TBaseVirtualTree; Node,
  Target: PVirtualNode; var Allowed: Boolean);
begin
  Allowed := fHideLockCount = 0;
end;

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.VtExplorerPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  Data: PProjectItem;
begin
  { Get node data }
  Data := Sender.GetNodeData(Node);

  { Choose foreground color }
  with TargetCanvas.Font do
  begin
    if Data^.Attributes and SFGAO_FOLDER <> 0 then
      Style := [fsBold]
    else if (Data^.Attributes = 0) and (Data^.FullPath = '') then
      Style := [fsBold]
    else
      Style := [];
    if Data^.Attributes and SFGAO_COMPRESSED <> 0 then
      Color := clBlue
    else
      Color := clBlack;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFrmProjectDrawer.VtExplorerBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);

  function GetColorFromEditorList(const AFileName: String; Default: TColor): TColor;
  var
    I: Integer;
  begin
    Result := Default;
(*
    with MainForm do
    begin
      for I := 0 to EditCount - 1 do
        if Editor[I].FileName = AFileName then
        begin
          if Editor[I].Modified then
            Result := $00E1E1FF
          else
            Result := $00E1FFE1;
          Break;
        end;
    end;
*)
  end;

var
  Data: PProjectItem;
begin
  { We don't change cell bounds }
  if CellPaintMode <> cpmPaint then
    Exit;

  { Get node data }
  Data := Sender.GetNodeData(Node);

  { Choose background color }
  with TargetCanvas.Brush do
  begin
    if Data^.Attributes <> 0 then
    begin
      Color := $00F7F7F7;
      if Data^.Attributes and SFGAO_FOLDER = 0 then
        Color := GetColorFromEditorList(Data^.FullPath, Color);
    end
    else begin
      Color := clWhite;
      if Data^.FullPath <> '' then
        Color := GetColorFromEditorList(Data^.FullPath, Color);
    end;

    { Fill cell rect with that color }
    TargetCanvas.FillRect(CellRect);

    { Recreate control bitmaps with that color in backrground }
//    Sender.PrepareBitmaps(True, False, Color);
  end;
end;

end.
