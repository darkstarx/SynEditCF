unit UImpress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Registry, ExtCtrls, ShellAPi, ActiveX, RegularExpressions,
  SHDocVw_TLB, MSHTML_TLB,

  { Letterpress }
  URoutine;

type
  TFrmImpress = class(TForm)
    TimerScroll: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerScrollTimer(Sender: TObject);
  private
    fBrowser: TWebBrowser;
    fDocument: IHTMLDocument2;
    fBody, fOutput: IHTMLElement;
    fHead: IHTMLDOMNode;
    fCommandTitle: UnicodeString;
    fCommandInputForm: TForm;
    fEditor: TObject;
    fInitializing, fWentAway: Boolean;

    fHeader,
    fFooter: UnicodeString;

    fAfterContent,
    fRunning,
    fSuspended: Boolean;

    fChildStdRd, fChildStdWr, fChildStdMon1, fChildStdMon2, fChildProc: THandle;
    fCommandOutputThread: TOutputCommandThread;
    fCommandInputThread: TInputCommandThread;

    procedure GetContentDiv(Sender: TObject; const pDisp: IDispatch;
      var URL: OleVariant);
    procedure DoOnBeforeNavigate2(ASender: TObject;
      const pDisp: IDispatch;
      var URL: OleVariant; var Flags: OleVariant;
      var TargetFrameName: OleVariant; var PostData: OleVariant;
      var Headers: OleVariant; var Cancel: WordBool);
    procedure DoOnTitleChange(ASender: TObject; const Text: WideString);

    procedure AttachStyle(const Location: UnicodeString);
    procedure AttachScript(const Location: UnicodeString);
    procedure AttachMeta(const Data: UTF8String);
    procedure AttachTitle(const Title: UnicodeString);

    procedure CommandInputRequested(Sender: TObject; const Data: PUTF8String);
    procedure CommandInputCompleted(Sender: TObject);
    procedure CommandOutputBufferReady(Sender: TObject; const Data: UTF8String);
    procedure CommandProcessCompleted(Sender: TObject; const ExitCode: Cardinal);
    procedure CommandCompleted(Sender: TObject);

    procedure TerminateCommandProcess(Closing: Boolean = False);
  public
    procedure Build(const Command, Directory, CommandDir, Title: UnicodeString;
      const Input: UTF8String);
    procedure RemoveInputForm;
    property Building: Boolean read fRunning;
    property Editor: TObject read fEditor write fEditor;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

var
  FrmImpress: TFrmImpress;

implementation

uses
  UConst, USettings, UInput,

  { SynEdit }
  SynUnicode;

{$R *.dfm}

{ TFrmImpress }

// -----------------------------------------------------------------------------

procedure TFrmImpress.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    ExStyle := ExStyle or WS_EX_APPWINDOW;
    WndParent := GetDesktopWindow;
  end;
end;

{ Form }

// -----------------------------------------------------------------------------

procedure TFrmImpress.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;

  { Terminate anything if still running }
  TerminateCommandProcess(True);

  { Remove command form right from here }
//  (fEditor as TSynEdit).RemoveCommandForm(Self);
end;

procedure TFrmImpress.FormCreate(Sender: TObject);
begin
  { Initialize }
  fRunning := False;
  fSuspended := False;
  DesktopFont := True;

  { Create embedded browser and put it onto form }
  fBrowser := TWebBrowser.Create(Self);
  with TWinControl(fBrowser) do
  begin
    Parent := Self;
    Align := alClient;
  end;

  { Make it as silent as possible }
  with fBrowser do
  begin
    Silent := True;
    Offline := False;
    RegisterAsBrowser := False;
    RegisterAsDropTarget := False;

    { Get content div upon loading complete }
    OnDocumentComplete := GetContentDiv;
    OnBeforeNavigate2 := DoOnBeforeNavigate2;
    OnTitleChange := DoOnTitleChange;
  end;
end;

procedure TFrmImpress.FormDestroy(Sender: TObject);
begin
  { Halt any running command }
  TerminateCommandProcess(True);

  { Finalize elements & browser }
  fHead := nil;
  fOutput := nil;
  fDocument := nil;
  FrmImpress := nil;
  FreeAndNil(fBrowser);
end;

procedure TFrmImpress.GetContentDiv(Sender: TObject; const pDisp: IDispatch;
  var URL: OleVariant);

  procedure NicerDisplay(Body: IHTMLElement);
  begin
    if Body <> nil then
    begin
      Body.style.border := 'none';
      Body.style.overflow := 'auto';
    end;
  end;

var
  QueryDispatch: IDispatch;
begin
  { Once the page is loaded, initialization is done }
  fInitializing := False;

  { Check if went away }
  if fWentAway then
  begin

    { Stop receiving output }
    fRunning := False;
    if fChildProc <> INVALID_HANDLE_VALUE then
      TerminateProcess(fChildProc, 0);

    { Make appearance nicer }
    NicerDisplay(fDocument.body);

    { Retreat }
    Exit;
  end;

  { Retrieve document and output element }
  fDocument := fBrowser.Document as IHTMLDocument2;

  { All capture output from command will go into body element }
  fBody := nil;
  fOutput := nil;
  fHead := nil;
  fBody := fDocument.body;
  NicerDisplay(fBody);
  fOutput := fDocument.body;

  { Retrieve head element as DOM object }
  QueryDispatch := IHTMLElementCollection(fDocument.all.tags(sHTMLHead)).item(0, 0);
  QueryDispatch.QueryInterface(IID_IHTMLDOMNode, fHead);
end;

procedure TFrmImpress.TimerScrollTimer(Sender: TObject);
var
  contentElem: IHTMLElement2;
begin
  { Disable timer }
  TimerScroll.Enabled := False;

  { Try to get scroll widths }
  if fBody <> nil then
  try
    if Supports(fBody, IID_IHTMLElement2) then
    begin
      fBody.QueryInterface(IID_IHTMLElement2, contentElem);
      contentElem.scrollTop := contentElem.scrollHeight;
      contentElem := nil;
    end;
  finally
  end;
end;

procedure TFrmImpress.DoOnBeforeNavigate2(ASender: TObject;
  const pDisp: IDispatch; var URL: OleVariant; var Flags: OleVariant;
  var TargetFrameName: OleVariant; var PostData: OleVariant;
  var Headers: OleVariant; var Cancel: WordBool);
const
  sLetterpressProtocol: UTF8String = 'letterpress://';
  sLetterpressProcessStart: UTF8String = 'javascript:letterpress.processStart()';
  sLetterpressProcessSuspend: UTF8String = 'javascript:letterpress.processSuspend()';
  sLetterpressProcessStop: UTF8String = 'javascript:letterpress.processStop()';
var
  jsCommand: UTF8String;
begin
  { Check }
  if fInitializing then
    Exit;

  { See if letterpress command issued }
  jsCommand := UnicodeStringToUTF8(URL);
  if Copy(jsCommand, 1, Length(sLetterpressProcessStop)) = sLetterpressProcessStop then
    TerminateCommandProcess
  else if Copy(jsCommand, 1, Length(sLetterpressProcessStart)) = sLetterpressProcessStart then
  begin
    if not fRunning then Exit;
    if not fSuspended then Exit;
    fSuspended := False;
    if Assigned(fCommandOutputThread) then
      fCommandOutputThread.Suspended := False;
    if Assigned(fCommandInputThread) then
      fCommandInputThread.Suspended := False;
    NtResumeProcess(fChildProc);
  end
  else if Copy(jsCommand, 1, Length(sLetterpressProcessSuspend)) = sLetterpressProcessSuspend then
  begin
    if not fRunning then Exit;
    if fSuspended then Exit;
    fSuspended := True;
    if Assigned(fCommandOutputThread) then
      fCommandOutputThread.Suspended := True;
    if Assigned(fCommandInputThread) then
      fCommandInputThread.Suspended := True;
    NtSuspendProcess(fChildProc);
  end
  else
    fWentAway := True;
  (*{ See if it's a Letterpress action URL and there's actual data }
  if (Copy(jsCommand, 1, Length(sLetterpressProtocol)) = sLetterpressProtocol)
    and (Length(jsCommand) > Length(sLetterpressProtocol)) then
  begin

    { Command execution URL }
    if jsCommand[Succ(Length(sLetterpressProtocol))] = '{' then
    begin
      jsCommand := Copy(jsCommand, Succ(Length(sLetterpressProtocol)), MaxInt);

      { Get options }
      nOptionsPos := Pos('/', jsCommand);
      if nOptionsPos > 0 then
        sOptions := Copy(jsCommand, Succ(nOptionsPos), MaxInt)
      else begin
        sOptions := EmptyStr;
        nOptionsPos := Length(jsCommand) + 1;
      end;
      if Pos('s', sOptions) > 0 then { load silently };

      { Get parameters environment variable }
      nParamsPos := Pos(':', jsCommand);
      if nParamsPos > 0 then
        sParams := Copy(jsCommand, Succ(nParamsPos), nOptionsPos - nParamsPos - 1)
      else
        sParams := EmptyStr;

      { Execute the command }
    end

    { File execution URL (allow only this }
    else
      Cancel := False;
  end;*)
end;

procedure TFrmImpress.DoOnTitleChange(ASender: TObject; const Text: WideString);
begin
  if Text <> EmptyStr then
    Caption := Text;
end;

// -----------------------------------------------------------------------------

procedure TFrmImpress.AttachStyle(const Location: UnicodeString);
var
  LinkElement: IHTMLElement;
  LinkElementReal: IHTMLLinkElement;
  LinkDOMElement: IHTMLDOMNode;
begin
  LinkElement := fDocument.createElement(sHTMLLink);
  LinkElement.QueryInterface(IID_IHTMLLinkElement, LinkElementReal);
  LinkElementReal.href := Location;
  LinkElementReal.type_ := 'text/css';
  LinkElementReal.rel := 'stylesheet';
  LinkElement.QueryInterface(IID_IHTMLDOMNode, LinkDOMElement);
  fHead.appendChild(LinkDOMElement);
end;

procedure TFrmImpress.AttachScript(const Location: UnicodeString);
var
  ScriptElement: IHTMLElement;
  ScriptElementReal: IHTMLScriptElement;
  ScriptDOMElement: IHTMLDOMNode;
begin
  ScriptElement := fDocument.createElement(sHTMLScript);
  ScriptElement.QueryInterface(IID_IHTMLScriptElement, ScriptElementReal);
  ScriptElementReal.src := Location;
  ScriptElement.QueryInterface(IID_IHTMLDOMNode, ScriptDOMElement);
  fHead.appendChild(ScriptDOMElement);
end;

procedure TFrmImpress.AttachMeta(const Data: UTF8String);
var
  Match: IMatch;
  metaContent, metaHttpEquiv, metaName, metaScheme: UnicodeString;
  MetaElement: IHTMLElement;
  MetaElementReal: IHTMLMetaElement;
  MetaDOMElement: IHTMLDOMNode;
begin
  { Initialize }
  metaContent := EmptyStr;
  metaHttpEquiv := EmptyStr;
  metaName := EmptyStr;
  metaScheme := EmptyStr;

  { Further parse metadata to retrieve all the content.
    Get content attribute }
  Match := TRegex.Match(Data, '\bcontent=["''](?<content>.*?)["'']');
  if Match.Success then
    metaContent := UTF8ToUnicodeString(Match.Groups.ItemsByName['content'].Value);

  { Get http-equiv attribute }
  Match := TRegex.Match(Data, '\bhttp-equiv=["''](?<content>.*?)["'']');
  if Match.Success then
    metaHttpEquiv := UTF8ToUnicodeString(Match.Groups.ItemsByName['content'].Value);

  { Get name attribute }
  Match := TRegex.Match(Data, '\bname=["''](?<content>.*?)["'']');
  if Match.Success then
    metaName := UTF8ToUnicodeString(Match.Groups.ItemsByName['content'].Value);

  { Refreshing? }
  if metaHttpEquiv = 'refresh' then
  begin

    { Embedded IE doesn't handle redirections so do it manually }
    Match := TRegex.Match(Data, '\burl=(?<location>.*)');
    if Match.Success then
    begin
      metaContent := UTF8ToUnicodeString(Match.Groups.ItemsByName['location'].Value);
      fBrowser.Navigate(metaContent);
      Exit;
    end;
  end;

  { Put everything in header now }
  MetaElement := fDocument.createElement(sHTMLMeta);
  MetaElement.QueryInterface(IID_IHTMLMetaElement, MetaElementReal);
  with MetaElementReal do
  begin
    httpEquiv := metaHttpEquiv;
    content := metaContent;
    name := metaName;
  end;
  MetaElement.QueryInterface(IID_IHTMLDOMNode, MetaDOMElement);
  fHead.appendChild(MetaDOMElement);
end;

procedure TFrmImpress.AttachTitle(const Title: UnicodeString);
begin
  fDocument.title := StringReplace(Title, sHTMLLetterpressTitle,
    fCommandTitle, [rfReplaceAll]);
end;

// -----------------------------------------------------------------------------

procedure TFrmImpress.CommandOutputBufferReady(Sender: TObject;
  const Data: UTf8String);

  procedure DoFlush(const S: UnicodeString);
  begin
    try
      fOutput.insertAdjacentHTML(sHTMLBeforeEnd, S);
      if not fAfterContent then
        fOutput := GetElementById(fBrowser.Document, sHTMLLetterpressOutputId);
      if fOutput = nil then
        fOutput := fBody;
    finally
    end;
  end;

var
  P: Integer;
  Match: IMatch;
  OutputData: UnicodeString;
begin
  { Look for styles to embed }
  Match := TRegex.Match(Data, '<link(\s+?)href=["''](?<location>.*?css)["'']');
  while Match.Success do
  begin
    AttachStyle(UTF8ToUnicodeString(Match.Groups.ItemsByName['location'].Value));
    Match := Match.NextMatch;
  end;

  { Look for scripts to embed }
  Match := TRegex.Match(Data, '<script(\s+?)src=["''](?<location>.*?js)["'']');
  while Match.Success do
  begin
    AttachScript(UTF8ToUnicodeString(Match.Groups.ItemsByName['location'].Value));
    Match := Match.NextMatch;
  end;

  { Look for metadata to embed }
  Match := TRegex.Match(Data, '<meta\s.*?>');
  while Match.Success do
  begin
    AttachMeta(Match.Value);
    Match := Match.NextMatch;
  end;

  { Look for title to set }
  Match := TRegex.Match(Data, '<title>(?<title>.*?)</title>');
  while Match.Success do
  begin
    AttachTitle(UTF8ToUnicodeString(Match.Groups.ItemsByName['title'].Value));
    Match := Match.NextMatch;
  end;

  { Convert to Unicode }
  OutputData := UTF8ToUnicodeString(Data);

  { Look for Letterpress variables to substitute }
  OutputData := StringReplace(Outputdata, sHTMLLetterpressTitle,
    fCommandTitle, [rfReplaceAll]);

  { Look if outputting letterpress footer }
  P := Pos(sHTMLLetterpressFooterTag, OutputData);
  if P > 0 then
  begin
    DoFlush(Copy(OutputData, 1, Pred(P)));
    fAfterContent := True;
    fOutput := fBody;
    DoFlush(Copy(OutputData, P, MaxInt));
  end

  { Flush }
  else
    DoFlush(OutputData);

  { Reset scroll timer }
  with TimerScroll do
  begin
    Enabled := False;
    Enabled := True;
  end;
end;

procedure TFrmImpress.CommandInputRequested(Sender: TObject;
  const Data: PUTF8String);
begin
  if fCommandInputForm <> nil then
    Exit;
  fCommandInputForm := TFrmInput.Create(Self);
  with fCommandInputForm as TFrmInput do
  begin
    HTMLWindow := Self;
    InputThread := fCommandInputThread;
    InputData := Data;
    Show;
  end;
end;

procedure TFrmImpress.CommandInputCompleted(Sender: TObject);
begin
  { Close input form if there's still any }
  if fCommandInputForm <> nil then
    fCommandInputForm.Close;

  { Close stdin pipe }
  if not (Sender as TInputCommandThread).HandleClosed and
    (fChildStdWr <> INVALID_HANDLE_VALUE) then
  begin
    try
      CloseHandle(fChildStdWr);
    finally
    end;
    fChildStdWr := INVALID_HANDLE_VALUE;
  end;
end;

// -----------------------------------------------------------------------------
// Outputs exit code status information at the end of footer
procedure TFrmImpress.CommandProcessCompleted(Sender: TObject;
  const ExitCode: Cardinal);

  { Executes JavaScript code (function) on page }
  function ExecuteScript(const Script, Language: UnicodeString): Boolean;
  var
    Win: IHTMLWindow2;
    OleLanguage: Olevariant;
  begin
    Result := False;
    if fDocument <> nil then
    begin
      try
        Win := fDocument.parentWindow;
        if Win <> nil then
          try
            OleLanguage := Language;
            Win.ExecScript(Script, OleLanguage);
            Result := True;
          finally
            Win := nil;
          end;
      finally
      end;
    end;
  end;

var
  elemFooter: IHTMLElement;
begin
  elemFooter := GetElementById(fBrowser.Document, sHTMLLetterpressFooterId);
  if elemFooter = nil then
    elemFooter := fOutput;
  try
    ExecuteScript('letterpress.processStop()', 'JavaScript');
    elemFooter.insertAdjacentHTML(sHTMLBeforeEnd, 'Process exited with code: <strong>' + IntToStr(ExitCode) + '</strong>.');
    if elemFooter <> fOutput then
      elemFooter := nil;
  finally
  end;
end;

procedure TFrmImpress.CommandCompleted(Sender: TObject);
begin
  fRunning := False;

  { Close stdout pipe }
  if fChildStdRd <> INVALID_HANDLE_VALUE then
  begin
    try
      CloseHandle(fChildStdRd);
    finally
    end;
    fChildStdRd := INVALID_HANDLE_VALUE;
  end;

  { Close pipe monitors }
  if fChildStdMon1 <> INVALID_HANDLE_VALUE then
  begin
    DisconnectNamedPipe(fChildStdMon1);
    try
      CloseHandle(fChildStdMon1);
    finally
    end;
    fChildStdMon1 := INVALID_HANDLE_VALUE;
  end;
  if fChildStdMon2 <> INVALID_HANDLE_VALUE then
  begin
    DisconnectNamedPipe(fChildStdMon2);
    try
      CloseHandle(fChildStdMon2);
    finally
    end;
    fChildStdMon2 := INVALID_HANDLE_VALUE;
  end;

  { Close process' handle }
  if fChildProc <> INVALID_HANDLE_VALUE then
  begin
    try
      CloseHandle(fChildProc);
    finally
    end;
    fChildProc := INVALID_HANDLE_VALUE;
  end;
end;

procedure TFrmImpress.TerminateCommandProcess(Closing: Boolean = False);
begin
  if not fRunning then Exit;
  fRunning := False;
  if fCommandOutputThread <> nil then
  begin
    if Closing then
    begin
      fCommandOutputThread.OnOutputBufferReady := nil;
      fCommandOutputThread.OnOutputCompleted := nil;
      fCommandOutputThread.OnOutputProcessFinished := nil;
    end;
    fCommandOutputThread.Terminate;
  end;
  if fCommandInputThread <> nil then
  begin
    if Closing then
      fCommandInputThread.OnInputRequested := nil;
    fCommandInputThread.Canceled := True;
  end;
  TerminateProcess(fChildProc, 0);
  CloseHandle(fChildProc);
  fChildProc := INVALID_HANDLE_VALUE;
end;

// -----------------------------------------------------------------------------

procedure TFrmImpress.Build(const Command, Directory, CommandDir,
  Title: UnicodeString; const Input: UTF8String);
var
  I: Integer;
  bReadingFooter: Boolean;
  htmlFile: TStringList;
begin
  { Check }
  if fRunning then
    Exit;

  { Reset }
  fCommandInputForm := nil;
  fCommandTitle := Title;
  fInitializing := True;
  fWentAway := False;
  if FileExists(GetAppDir + sHTMLVoidFile) then
    fBrowser.Navigate(GetAppDir + sHTMLVoidFile)
  else
    fBrowser.Navigate('about:blank');

  { Read HTML header & footer parts }
  fHeader := 'LETTERPRESS_HTML_HEADER=';
  fFooter := 'LETTERPRESS_HTML_FOOTER=';
  bReadingFooter := False;
  htmlFile := TStringList.Create;
  try
    htmlFile.LoadFromFile(GetAppDir + sHTMLWindowFile, TEncoding.UTF8);
    for I := 0 to Pred(htmlFile.Count) do
    begin
      if bReadingFooter then
        fFooter := fFooter + htmlFile[I]
      else
        fHeader := fHeader + htmlFile[I];
      if CompareText(Trim(htmlFile[I]), sHTMLLetterpressOutputTag) = 0 then
        bReadingFooter := True;
    end;
  finally
    FreeAndNil(htmlFile);
  end;

  { Ensure page loaded }
  if fBrowser.ReadyState <> READYSTATE_UNINITIALIZED then
    while fBrowser.ReadyState = READYSTATE_COMPLETE do
      Application.ProcessMessages;

  { Initialize }
  if not LaunchPipedProcess(fChildStdRd, fChildStdWr, fChildStdMon1,
    fChildStdMon2, fChildProc, Command, Directory, CommandDir,
    fHeader + #26 + fFooter)
  then
    Exit;

  { Execute }
  fRunning := True;
  fSuspended := False;
  fAfterContent := False;

  { Create output reader }
  fCommandOutputThread := TOutputCommandThread.Create(True, False, True,
    fChildStdRd, fChildProc);
  with fCommandOutputThread do
  begin
    OnOutputBufferReady := CommandOutputBufferReady;
    OnOutputProcessFinished := CommandProcessCompleted;
    OnTerminate := CommandCompleted;
  end;
  fCommandInputThread := TInputCommandThread.Create(True, True, fChildStdWr,
    fChildStdMon1, fChildStdMon2, fChildProc, Input);
  with fCommandInputThread do
  begin
    OnInputRequested := CommandInputRequested;
    OnTerminate := CommandInputCompleted;
  end;
  fCommandOutputThread.Start;
  fCommandInputThread.Start;
end;

procedure TFrmImpress.RemoveInputForm;
begin
  fCommandInputForm := nil;
end;

end.
