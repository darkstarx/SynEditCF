object FrmInput: TFrmInput
  Left = 537
  Top = 110
  BorderIcons = [biSystemMenu]
  Caption = 'Letterpress '#8722' Input Request'
  ClientHeight = 112
  ClientWidth = 409
  Color = clBtnFace
  Constraints.MaxHeight = 360
  Constraints.MinHeight = 150
  Constraints.MinWidth = 425
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    409
    112)
  PixelsPerInch = 96
  TextHeight = 13
  object memoInput: TMemo
    Left = 16
    Top = 16
    Width = 378
    Height = 54
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 0
    WantReturns = False
  end
  object btnCancel: TButton
    Left = 319
    Top = 81
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object btnOK: TButton
    Left = 238
    Top = 81
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnEOF: TButton
    Left = 16
    Top = 81
    Width = 75
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = 'Send EOF'
    TabOrder = 3
    OnClick = btnEOFClick
  end
  object btnEOLEOF: TButton
    Left = 97
    Top = 81
    Width = 96
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = 'Send EOL && EOF'
    TabOrder = 4
    OnClick = btnEOLEOFClick
  end
end
