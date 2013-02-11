(*

  Letterpress

  Global variables, constants and methods.

  Copyright 2009-2010, Garnet

*)

unit UConst;

interface

uses
  Windows, Forms, MSHTML_TLB;

  function PathCompactPathExW(lpszOut: PWideChar; lpszSrc: PWideChar;
    cchMax: Cardinal; dwFlags: DWORD): Boolean; stdcall; external 'shlwapi.dll';

var

  { Global variables }
  Portable: Boolean;
  Environment: Pointer;

  { Messages }
  LETTERPRESS_INSTANCE: Cardinal;

const

  { Application }
  sApp = 'Letterpress';
  sAppName = 'Letterpress text editor';
  sAppGUID = '{F1D13BAA-9FE3-4E13-970E-CF7AE94354ED}';
  sAppVersion = '0.05a';
  sAppWeb = 'http://quadro.drkb.ru/blog/';
  sAppKey = 'Software\Garnet\Letterpress\';
  sAppPortable = '.portable';

  { Messages }
  sMessageInstance = 'LETTERPRESS_INSTANCE';

  { Directories }
  sPackageDir = 'packages\';
  sProjectDir = 'projects\';
  sThemeDir   = 'themes\';
  sMacroDir   = 'macros\';
  sHelpDir    = 'manual\';
  sDictDir    = 'dictionaries\';
  sTemplateDir= 'templates\';
  sPaletteDir = 'palettes\';
  sCommandsDir = 'bundles\';
  sLibraryDir = 'library\';
  sSupportDir = 'support\';

  { Packages }
  sBundleConfig = 'bundle.config';
  sPackageConfig = 'package.config';
  sPackagePostfix = '.package\';
  sPackagePostfixWithoutSlash = '.package';
  sPackageIndex = 'packages.index';
  sUnknown = 'Unknown';
  sUnknownPackage = sUnknown + sPackagePostfix;

  { Postfixes }
  sProjectPostifx    = '.project';
  sStylesheetPostfix = '.colors';
  sConfigPostfix = '.config';
  sMacroPostifx      = '.macro';
  sDictionaryPostfix = '.dictionary';
  sGrammarPostfix = '.grammar';
  sLibraryPostfix = '.library';
  sSnippetPostfix = '.snippet';
  sCommandPostfix = '.command';
  sDragPostfix = '.drag';

  { Other }
  sUntitled = 'Untitled';
  sIncrementalSearch = 'Searching: %s';

  { Cursors }
  CUR_RARROW = 255;

  { Unicode encodings }
  UNICODE_UTF7 = 65000;    // Deprecated UTF-7
  UNICODE_UTF8 = 65001;    // UTF-8 without byte order mark
  UNICODE_UTF8BOM = 65010; // UTF-8 with byte order mark
  UNICODE_UTF16LE = 1200;  // UTF-16 Little Endian
  UNICODE_UTF16BE = 1201;  // UTF-16 Big Endian

  { Files }
  sHTMLVoidFile = sSupportDir + 'void.html';
  sHTMLTooltipFile = sSupportDir + 'tooltip.html';
  sHTMLWindowFile = sSupportDir + 'window.html';

  { Tags }
  sHTMLHead = 'HEAD';
  sHTMLLink = 'LINK';
  sHTMLScript = 'SCRIPT';
  sHTMLMeta = 'META';
  sHTMLBeforeEnd = 'BeforeEnd';

  { Constants }
  sHTMLLetterpressTitle = '<!-- LETTERPRESS_COMMAND_TITLE -->';

  { IDs }
  sHTMLLetterpressHeaderId = 'letterpress_header';
  sHTMLLetterpressStatusId = 'letterpress_status';
  sHTMLLetterpressToolbarId = 'letterpress_toolbar';
  sHTMLLetterpressOutputId = 'letterpress_output';
  sHTMLLetterpressFooterId = 'letterpress_footer';
  sHTMLLetterpressExitCodeId = 'letterpress_exitcode';

  { Special tags }
  sHTMLLetterpressHeaderTag = '<div id="' + sHTMLLetterpressHeaderId + '">';
  sHTMLLetterpressStatusTag = '<div id="' + sHTMLLetterpressStatusId + '">';
  sHTMLLetterpressToolbarTag = '<div id="' + sHTMLLetterpressToolbarId + '">';
  sHTMLLetterpressOutputTag = '<div id="' + sHTMLLetterpressOutputId + '">';
  sHTMLLetterpressFooterTag = '<div id="' + sHTMLLetterpressFooterId + '">';

  { Quick methods to obtain folder paths and ensure their existence }
  function GetCurrentUserName: UnicodeString;
  function GetAppDir: String;
  function GetSettingsDir: String;
  function GetTempDir: String;
  function GetPackageDir: String;
  function GetProjectDir: String;
  function GetThemeDir: String;
  function GetMacroDir: String;
  function GetHelpDir: String;
  function GetDictDir: String;
  function GetTemplateDir: String;
  function GetPaletteDir: String;
  function GetLibraryDir: String;

  { Exploration routines }
  procedure ExploreFile(const FileName: String);
  procedure ExploreAndSelectFile(const Path, FileName: String);
  procedure ExplorePath(const Path: String);

  { File name routines }
  function ExtractFileNameWithoutExt(const FileName: String): String;
  function ExtractFileExtWithoutDot(const FileName: String): String;
  function ExtractShortFileName(const FileName: String): String;

  { Other routines }
  function Msg(Wnd: HWND; const Text: String; const Flags: Integer = 0): Integer;
  function GetHasChildren(const Folder: String): Boolean;
  function GetFontExists(const FontName: String): Boolean;
  function ChooseBestMonospacedFont(const FontName: String): String;
  function IsANSIEncoding(Encoding: Cardinal): Boolean;
  function GenerateUUID: UnicodeString;
  function GenerateFileUUID: UnicodeString;
  function ExpandLetterpressVariables(const S, CommandPath: UnicodeString): UnicodeString;
  function RemoveTitleAmpersand(const S: UnicodeString): UnicodeString;
  function RemoveTitleAmpersandUTF8(const S: UTF8String): UTF8String;
  function GetElementById(const Doc: IDispatch; const Id: UnicodeString): IHTMLElement;

implementation

uses
  SysUtils, ShellApi, ShlObj, ActiveX, Math, USettings, UProjectDrawer,
  SynUniHighlighter, SynUnicode, RegularExpressions, Variants;

// -----------------------------------------------------------------------------

function GetCurrentUserName: UnicodeString;
const
  cMaxUserNameLen = 255;
var
  dwUserNameLen: Cardinal;
begin
  dwUserNameLen := cMaxUserNameLen - 1;
  SetLength(Result, cMaxUserNameLen);
  GetUserName(PChar(Result), dwUserNameLen);
  SetLength(Result, dwUserNameLen);
end;

// -----------------------------------------------------------------------------
// Retrieve application .exe path with PathDelim
function GetAppDir: String;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));

  { No need to call CreateDir() here }
end;

// -----------------------------------------------------------------------------
// Locate AppData\Roaming\Letterpress folder or simply redirect to GetAppDir if
// in portable mode
function GetSettingsDir: String;
begin
  { Redirect? }
  if Portable then
    Result := GetAppDir

  { Locate }
  else begin
    SetLength(Result, MAX_PATH);
    if SHGetFolderPath(0, CSIDL_APPDATA or CSIDL_FLAG_CREATE, 0, 0,
      PChar(Result)) = S_OK then
    begin
      SetLength(Result, Pos(#0, Result) - 1);
      Result := IncludeTrailingPathDelimiter(Result) + sApp + PathDelim;

      { Create if doesn't exist }
      if not DirectoryExists(Result) then

        { Redirect? }
        if not CreateDir(Result) then
          Result := GetAppDir;
    end

    { Redirect }
    else
      Result := GetAppDir;
  end;
end;

// -----------------------------------------------------------------------------
// Locate Windows temporary directory for current user
function GetTempDir: String;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetTempPath(MAX_PATH, PChar(Result)));

  { No need to call CreateDir() here }
end;

// -----------------------------------------------------------------------------

function GetPackageDir: String;
begin
  Result := GetSettingsDir + sPackageDir;
  CreateDir(Result);
end;

function GetProjectDir: String;
begin
  Result := GetSettingsDir + sProjectDir;
  CreateDir(Result);
end;

function GetThemeDir: String;
begin
  Result := GetSettingsDir + sThemeDir;
  CreateDir(Result);
end;

function GetMacroDir: String;
begin
  Result := GetSettingsDir + sMacroDir;
  CreateDir(Result);
end;

function GetHelpDir: String;
begin
  Result := GetSettingsDir + sHelpDir;
  CreateDir(Result);
end;

function GetDictDir: String;
begin
  Result := GetSettingsDir + sDictDir;
  CreateDir(Result);
end;

function GetTemplateDir: String;
begin
  Result := GetSettingsDir + sTemplateDir;
  CreateDir(Result);
end;

function GetPaletteDir: String;
begin
  Result := GetSettingsDir + sPaletteDir;
  CreateDir(Result);
end;

function GetLibraryDir: String;
begin
  Result := GetSettingsDir + sLibraryDir;
  CreateDir(Result);
end;

// -----------------------------------------------------------------------------

procedure ExploreFile(const FileName: String);
begin
  ShellExecute(0, 'open', 'explorer.exe', nil,
    PChar(ExtractFilePath(FileName)), SW_SHOWNORMAL);
end;

procedure ExploreAndSelectFile(const Path, FileName: String);
begin
  ShellExecute(0, 'open', 'explorer.exe', PChar('/select, ' + FileName),
    PChar(Path), SW_SHOWNORMAL);
end;

procedure ExplorePath(const Path: String);
begin
  ShellExecute(0, 'open', PChar(Path), nil, nil, SW_SHOWNORMAL);
end;

// -----------------------------------------------------------------------------

function ExtractFileNameWithoutExt(const FileName: String): String;
begin
  Result := ExtractFileName(FileName);
  Result := Copy(Result, 1, Max(LastDelimiter('.', Result), 1) - 1);
end;

function ExtractFileExtWithoutDot(const FileName: String): String;
var
  I: Integer;
begin
  I := LastDelimiter('.', FileName);
  if I < 1 then
    Result := ''
  else
    Result := LowerCase(Copy(FileName, I + 1, MAXINT));
end;

function ExtractShortFileName(const FileName: String): String;
var
  sExtension: String;
begin
  Result := ExtractFileName(FileName);
  if Length(Result) > 48 then
  begin
    sExtension := ExtractFileExt(Result);
    Result := TrimRight(Copy(Result, 1, 48 - Length(sExtension))) + #$2026 + sExtension;
  end;
end;

// -----------------------------------------------------------------------------
// Call configurable message box
function Msg(Wnd: HWND; const Text: String; const Flags: Integer = 0): Integer;
begin
  Result := MessageBoxEx(Wnd, PChar(Text), PChar(Application.Title),
    Flags or MB_APPLMODAL or MB_DEFBUTTON1, 0);
end;

// -----------------------------------------------------------------------------
// Determines, whether folder contains other files
function GetHasChildren(const Folder: String): Boolean;
var
  Fs: TSearchRec;
begin
  try
    Result := FindFirst(IncludeTrailingBackslash(Folder) + '*.*', faNormal, Fs) = 0;
  finally
    FindClose(Fs);
  end;
end;

// -----------------------------------------------------------------------------
// Tests if a given font of FontName name actually installed in the system.
// Also, enusres it is a monospaced font
function GetFontExists(const FontName: String): Boolean;

  function EnumFontProc(lpelf: PEnumLogFont; lpntm: PNewTextMetric;
    nFontType: integer; lParam: LParam): Integer; stdcall;
  begin
    PBoolean(LParam)^ := lpntm^.tmPitchAndFamily and 1 = 0;
    Result := 0;
  end;

var
  LF: LOGFONT;
  DC: HDC;
begin
  Result := False;
  DC := GetDC(GetDesktopWindow);
  try
    LF.lfCharSet := DEFAULT_CHARSET;
    lstrcpy(LF.lfFaceName, PChar(FontName));
    EnumFontFamiliesEx(DC, LF, @EnumFontProc, Integer(@Result), 0);
  finally
    ReleaseDC(GetDesktopWindow, DC);
  end;
end;

// -----------------------------------------------------------------------------
// Falls back if selected font found to be uninstalled or turned out
// to be variable width
function ChooseBestMonospacedFont(const FontName: String): String;
begin
  Result := FontName;
  if not GetFontExists(Result) then
  begin
    Result := 'Consolas';
    if not GetFontExists(Result) then Result := 'Courier New';
  end;
end;

// -----------------------------------------------------------------------------
// Tests if a given encoding identifier represents Unicode encoding
function IsANSIEncoding(Encoding: Cardinal): Boolean;
begin
  if (Encoding = UNICODE_UTF8) or (Encoding = UNICODE_UTF8) or
    (Encoding = UNICODE_UTF8BOM) or (Encoding = UNICODE_UTF16LE) or
    (Encoding = UNICODE_UTF16BE) or (Encoding = UNICODE_UTF7)
  then
    Result := False
  else
    Result := True;
end;

// -----------------------------------------------------------------------------

function GenerateUUID: UnicodeString;
var
  GUID: TGUID;
begin
  if CoCreateGUID(GUID) = S_OK then Result := GUIDToString(GUID)
  else Result := EmptyStr;
end;

function GenerateFileUUID: UnicodeString;
begin
  Result := LowerCase(StringReplace(Copy(GenerateUUID, 2, MaxInt), '-', '', [rfReplaceAll]));
  SetLength(Result, Length(Result) - 1);
  Result := Result + '.letterpress';
end;

function ExpandLetterpressVariables(const S, CommandPath: UnicodeString): UnicodeString;
begin
  Result := StringReplace(S, '${LETTERPRESS_EXE}', Application.ExeName, [rfReplaceAll]);
  Result := StringReplace(Result, '${LETTERPRESS_PATH}', GetAppDir, [rfReplaceAll]);
  Result := StringReplace(Result, '${LETTERPRESS_DATA_PATH}', GetSettingsDir, [rfReplaceAll]);
  Result := StringReplace(Result, '${LETTERPRESS_COMMAND_PATH}', CommandPath, [rfReplaceAll]);
  Result := StringReplace(Result, '${LETTERPRESS_TEMP_PATH}', GetTempDir, [rfReplaceAll]);
end;

// -----------------------------------------------------------------------------

function RemoveTitleAmpersand(const S: UnicodeString): UnicodeString;
var
  US: UTF8String;
begin
  US := TRegex.Replace(UnicodeStringToUTF8(S), '&(?!&)', EmptyAnsiStr);
  Result := UTF8ToUnicodeString(TRegex.Replace(US, '&&', '&'));
end;

function RemoveTitleAmpersandUTF8(const S: UTF8String): UTF8String;
begin
  Result := TRegex.Replace(S, '&(?!&)', EmptyAnsiStr);
  Result := TRegex.Replace(Result, '&&', '&');
end;

// -----------------------------------------------------------------------------
// JavaScript ported analog
function GetElementById(const Doc: IDispatch;
  const Id: UnicodeString): IHTMLElement;
var
  Document: IHTMLDocument2;     // IHTMLDocument2 interface of Doc
  Body: IHTMLElement2;          // Document body element
  Tags: IHTMLElementCollection; // All tags in document body
  Tag: IHTMLElement;            // A tag in document body
  I: Integer;                   // loops through tags in document body
begin
  { Initialize }
  Result := nil;

  { Check for valid document: require IHTMLDocument2 interface to it }
  if not Supports(Doc, IHTMLDocument2, Document) then
    Exit;

  { Check for valid body element: require IHTMLElement2 interface to it }
  if not Supports(Document.body, IHTMLElement2, Body) then
    Exit;

  { Get all tags in body element ('*' => any tag name) }
  Tags := Body.getElementsByTagName('*');

  { Scan through all tags in body }
  for I := 0 to Pred(Tags.length) do
  begin

    { Get reference to a tag }
    Tag := Tags.item(I, EmptyParam) as IHTMLElement;

    { Check tag's id and return it if id matches }
    if AnsiSameText(Tag.id, Id) then
    begin
      Result := Tag;
      Break;
    end;
  end;
end;

end.
