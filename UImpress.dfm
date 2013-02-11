object FrmImpress: TFrmImpress
  Left = 100
  Top = 100
  Caption = 'Letterpress '#8211' Command'
  ClientHeight = 362
  ClientWidth = 464
  Color = clBtnFace
  Constraints.MinHeight = 240
  Constraints.MinWidth = 320
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ScreenSnap = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object TimerScroll: TTimer
    OnTimer = TimerScrollTimer
    Left = 8
    Top = 8
  end
end
