(*

  Letterpress

  Copyright 2009-2010, Garnet

*)

unit UInput;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, UxTheme, StdCtrls,

  { Letterpress }
  URoutine,

  { SynEdit }
  SynUnicode, ExtCtrls;

type
  TFrmInput = class(TForm)
    memoInput: TMemo;
    btnOK: TButton;
    btnCancel: TButton;
    btnEOF: TButton;
    btnEOLEOF: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btnEOFClick(Sender: TObject);
    procedure btnEOLEOFClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    fData: PUTF8String;
    fInputThread: TInputCommandThread;
    fEditor: TObject;
    fHTMLWindow: TForm;
  public
    property InputData: PUTF8String write fData;
    property InputThread: TInputCommandThread read fInputThread write fInputThread;
    property Editor: TObject read fEditor write fEditor;
    property HTMLWindow: TForm read fHTMLWindow write fHTMLWindow;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  end;

var
  FrmInput: TFrmInput;

implementation

uses
  UImpress, UConst;

{$R *.dfm}

{ TFrmInput }

// -----------------------------------------------------------------------------

procedure TFrmInput.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
    WndParent := (Owner as TForm).Handle;
  end;
end;

// -----------------------------------------------------------------------------
// Allow to drag form by contents, allow to resize by gripper
procedure TFrmInput.WMNCHitTest(var Msg: TWMNCHitTest);
var
  GripRect: TRect;
begin
  inherited;
  if Msg.Result = htClient then
    Msg.Result := htCaption;

  { See if on gripper }
  GripRect := BoundsRect;
  with GripRect do
  begin
    Left := Right - GetSystemMetrics(SM_CXVSCROLL);
    Top := Bottom - GetSystemMetrics(SM_CYVSCROLL);
  end;
  if PtInRect(GripRect, Point(Msg.XPos, Msg.YPos)) then
    Msg.Result := HTBOTTOMRIGHT;
end;

// -----------------------------------------------------------------------------
// Paint size gripper
procedure TFrmInput.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
  Theme: HTHEME;
  GripRect: TRect;
begin
  { Initialize }
  Msg.Result := 1;
  inherited;

  { Get size grip rect }
  GripRect := ClientRect;
  with GripRect do
  begin
    Left := Right - GetSystemMetrics(SM_CXVSCROLL);
    Top := Bottom - GetSystemMetrics(SM_CYVSCROLL);

    { Fill it and everything to the lft with background color }
    Canvas.FillRect(Rect(0, Top, Right, Bottom));
  end;

  { Draw size grip }
  Theme := OpenThemeData(Application.Handle, 'STATUS');
  try
    DrawThemeBackground(Theme, Canvas.Handle, SP_GRIPPER, 0, GripRect, nil);
  finally
    CloseThemeData(Theme);
  end;
end;

{ Form }

// -----------------------------------------------------------------------------

procedure TFrmInput.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
(*
  if Assigned(fEditor) then
    (fEditor as TSynEdit).RemoveInputForm;
  if Assigned(fHTMLWindow) then
    (fHTMLWindow as TFrmImpress).RemoveInputForm;
  if ModalResult = mrNone then
  try
    fInputThread.Canceled := True;
  finally
  end;
*)
end;

procedure TFrmInput.FormCreate(Sender: TObject);
begin
  DesktopFont := True;
  fEditor := nil;
  fHTMLWindow := nil;
end;

{ Answers }

// -----------------------------------------------------------------------------

procedure TFrmInput.btnEOFClick(Sender: TObject);
begin
  try
    fData^ := UnicodeStringToUTF8(memoInput.Text) + #13#10#26;
    fInputThread.InputReady := True;
  finally
  end;
  ModalResult := mrOk;
  Close;
end;

procedure TFrmInput.btnEOLEOFClick(Sender: TObject);
begin
  try
    fData^ := UnicodeStringToUTF8(memoInput.Text) + #26;
    fInputThread.InputReady := True;
  finally
  end;
  ModalResult := mrOk;
  Close;
end;

procedure TFrmInput.btnOKClick(Sender: TObject);
begin
  try
    fData^ := UnicodeStringToUTF8(memoInput.Text) + #13#10;
    fInputThread.InputReady := True;
  finally
  end;
  ModalResult := mrOk;
  Close;
end;

procedure TFrmInput.btnCancelClick(Sender: TObject);
begin
  try
    fInputThread.Canceled := True;
  finally
  end;
  ModalResult := mrCancel;
  Close;
end;

end.
