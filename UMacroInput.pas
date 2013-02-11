unit UMacroInput;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, pngimage, ImgList;

type
  TFrmMacroInput = class(TForm)
    lblPrompt: TLabel;
    pnButtons: TPanel;
    memoResponse: TMemo;
    shpSeparator: TShape;
    btnCancel: TButton;
    btnOK: TButton;
    pnFooter: TPanel;
    lblFooter: TLabel;
    imgFooter: TImage;
    imagesAlerts: TImageList;
    shpShadow: TShape;
    shpLight: TShape;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure memoResponseChange(Sender: TObject);
  private
    fMacroHandler: TObject;
  public
    property MacroHandler: TObject read fMacroHandler write fMacroHandler;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
  end;

var
  FrmMacroInput: TFrmMacroInput;

implementation

uses
  UxTheme, SynMacroRecorder_TLB;

{$R *.dfm}

{ TFrmMacroInput }

// -----------------------------------------------------------------------------

procedure TFrmMacroInput.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if Assigned(Owner) then
  begin
    Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
    Params.WndParent := (Owner as TForm).Handle;
  end;
end;

// -----------------------------------------------------------------------------
// Allow to drag form by contents, allow to resize by gripper
procedure TFrmMacroInput.WMNCHitTest(var Msg: TWMNCHitTest);
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

{ Form }

// -----------------------------------------------------------------------------

procedure TFrmMacroInput.FormCreate(Sender: TObject);
begin
  DesktopFont := True;
  imagesAlerts.GetIcon(1, imgFooter.Picture.Icon);
end;

procedure TFrmMacroInput.FormResize(Sender: TObject);
begin
  memoResponse.Height := shpSeparator.Top - memoResponse.Top - 16;
  pnFooter.Top := Succ(pnButtons.BoundsRect.Bottom);
end;

procedure TFrmMacroInput.memoResponseChange(Sender: TObject);
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    S.LineBreak := #10;
    S.Text := (fMacroHandler as TSynMacroHandler).PromptUpdated(memoResponse.Text);
    if S.Count > 0 then
    begin
      if LowerCase(S[0]) = 'question' then
      begin
        S.Delete(0);
        imagesAlerts.GetIcon(0, imgFooter.Picture.Icon);
      end
      else if (LowerCase(S[0]) = 'information') or (LowerCase(S[0]) = 'info') then
      begin
        S.Delete(0);
        imagesAlerts.GetIcon(1, imgFooter.Picture.Icon);
      end
      else if LowerCase(S[2]) = 'warning' then
      begin
        S.Delete(0);
        imagesAlerts.GetIcon(2, imgFooter.Picture.Icon);
      end
      else if LowerCase(S[3]) = 'error' then
      begin
        S.Delete(0);
        imagesAlerts.GetIcon(3, imgFooter.Picture.Icon);
      end;
      lblFooter.Caption := S.Text;
      if Trim(lblFooter.Caption) = EmptyStr then
        pnFooter.Hide
      else
        pnFooter.Show;
    end
    else
      pnFooter.Hide;
  finally
    FreeAndNil(S);
  end;
  FormResize(Self);
end;

end.
