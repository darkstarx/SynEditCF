unit ScreenWin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  StdCtrls, PalUtils;

const
  crPickerCursor = 259;

type
  TScreenForm = class(TForm)
    procedure CreateParams(var Params:TCreateParams); override;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EndSelection(x, y: integer; ok: boolean);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  protected
   procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
  private
   FOnSelColorChange: TNotifyEvent;
   FOnKeyDown: TKeyEvent;
   bmp: TBitmap;
  public
    Button: TObject;
    FHintFormat: string;
    SelectedColor: TColor;
    property OnSelColorChange: TNotifyEvent read FOnSelColorChange write FOnSelColorChange;
    property OnScreenKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
  end;

var
  ScreenForm: TScreenForm;

implementation

uses
  mbDeskPickerButton;

{$R *.dfm}
{$R PickCursor.res}

function ColorToHex(Color: TColor): string;
begin
 Result := IntToHex(GetRValue(Color), 2) + IntToHex(GetGValue(Color), 2) + IntToHex(GetBValue(Color), 2);
end;

function GetDesktopColor(const X, Y: Integer): TColor;
var
 c: TCanvas;
begin
 c := TCanvas.Create;
 try
  c.Handle := GetWindowDC(GetDesktopWindow);
  Result := GetPixel(c.Handle, X, Y);
 finally
  c.Free;
 end;
end;

procedure TScreenForm.CreateParams(var Params:TCreateParams);
Begin
 inherited CreateParams(Params);
 Params.ExStyle := WS_EX_TOPMOST or WS_EX_TOOLWINDOW;
end;

procedure TScreenForm.FormShow(Sender: TObject);
begin
 OnShow := nil;
 Width := Screen.DesktopWidth;
 Height := Screen.DesktopHeight;
 Left := Screen.DesktopLeft;
 Top := Screen.DesktopTop;
end;

procedure TScreenForm.FormCreate(Sender: TObject);
const
 CAPTUREBLT = $40000000;
var
 hdcScreen: HDC;
 hdcCompatible: HDC;
 hbmScreen: HBITMAP;
begin
 { Initialize }
 Screen.Cursors[crPickerCursor] := LoadCursor(HInstance, 'CUR_PICKER');
 Cursor := crPickerCursor;
 SelectedColor := clNone;
 FHintFormat := 'RGB (%r, %g, %b)'#13'Hex: %h';

 { Create desktop snapshot }
 hdcScreen := CreateDC('DISPLAY', nil, nil, nil);
 hdcCompatible := CreateCompatibleDC(hdcScreen);

 { Create a compatible bitmap for hdcScreen }
 hbmScreen := CreateCompatibleBitmap(hdcScreen,
 GetDeviceCaps(hdcScreen, HORZRES),
 GetDeviceCaps(hdcScreen, VERTRES));

 { Select the bitmaps into the compatible DC }
 SelectObject(hdcCompatible, hbmScreen);
 bmp := TBitmap.Create;
 bmp.Handle := hbmScreen;
 BitBlt(hdcCompatible, 0, 0, bmp.Width, bmp.Height, hdcScreen, 0, 0, SRCCOPY or
  CAPTUREBLT);

 { Clean up }
 DeleteDC(hdcScreen);
 DeleteDC(hdcCompatible);
end;

procedure TScreenForm.FormDestroy(Sender: TObject);
begin
 FreeAndNil(bmp);
end;

procedure TScreenForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if (key = VK_ESCAPE) or (ssAlt in Shift) or (ssCtrl in Shift) then
  EndSelection(0, 0, false);
 if Assigned(FOnKeyDown) then FOnKeyDown(Self, Key, Shift);
end;

procedure TScreenForm.EndSelection(x, y: integer; ok: boolean);
begin
 if ok then
  SelectedColor := GetDesktopColor(x, y)
 else
  SelectedColor := clNone;
 close;
 (Button as TmbDeskPickerButton).Ended := True;
 if Assigned(FOnSelColorChange) then FOnSelColorChange(Self);
end;

procedure TScreenForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 EndSelection(x, y, true);
end;

procedure TScreenForm.FormPaint(Sender: TObject);
begin
  BitBlt(Canvas.Handle, 0, 0, Width, Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TScreenForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
 SelectedColor := GetDesktopColor(x, y);
 if Assigned(FOnSelColorChange) then FOnSelColorChange(Self);
end;

procedure TScreenForm.CMHintShow(var Message: TCMHintShow);
begin
 with TCMHintShow(Message) do
  if not ShowHint then
   Message.Result := 1
  else
   with HintInfo^ do
    begin
     Result := 0;
     ReshowTimeout := 1;
     HideTimeout := 5000;
     HintPos := Point(HintPos.X + 16, HintPos.y - 16);
     HintStr := FormatHint(FHintFormat, SelectedColor);
    end;
 inherited;
end;

end.
