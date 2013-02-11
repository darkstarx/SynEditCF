{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditMiscClasses.pas, released 2000-04-07.
The Original Code is based on the mwSupportClasses.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Hieke.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynEditMiscClasses.pas,v 1.35.2.9 2008/09/17 13:59:12 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditMiscClasses;

{$I SynEdit.inc}

interface

uses
  Consts, Windows, Messages, Graphics, Controls, Forms, StdCtrls, Menus,
  Registry, Dialogs, Math, Classes, SysUtils,

  { SynEdit }
  SynEditTypes, SynEditKeyConst, SynUnicode, SynEditCodeFolding;

const
  SEC_LineStatesBarWidth = 5;
  SEC_OutliningBarWidth = 12;

type
  TSynSelectedColor = class(TPersistent)
  private
    fBG: TColor;
    fFG: TColor;
    fOnChange: TNotifyEvent;
    procedure SetBG(Value: TColor);
    procedure SetFG(Value: TColor);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Background: TColor read fBG write SetBG default clHighLight;
    property Foreground: TColor read fFG write SetFG default clHighLightText;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  TSynGutter = class(TPersistent)
  private
    fOwner: TObject;
    fFont: TFont;
    fCharWidth: Integer;
    fDigitCount: Integer;
    fWidth: Integer;
    fColor: TColor;
    fBorderColor: TColor;
    fShowLineNumbers: Boolean;
    fShowLineStates: Boolean;
    fShowCodeFolding: Boolean;
    fLeadingZeros: Boolean;
    fZeroStart: Boolean;
    fLineNumberStart: Integer;
    fLeftOffset: Integer;
    fRightOffset: Integer;
    fOnChange: TNotifyEvent;
    fCursor: TCursor;
    fVisible: Boolean;
    fWordWrapGlyphVisible: Boolean;

    { Gutter methods }
    procedure SetColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetLeadingZeros(const Value: Boolean);
    procedure SetLeftOffset(Value: Integer);
    procedure SetRightOffset(Value: Integer);
    procedure SetShowLineNumbers(const Value: boolean);
    procedure SetZeroStart(const Value: boolean);
    procedure SetLineNumberStart(const Value: Integer);
    procedure SetFont(Value: TFont);
    procedure SetVisible(Value: Boolean);
    procedure SetWordWrapGlyphVisible(Value: Boolean);
    procedure SetShowLineStates(Value: Boolean);

    { Events }
    procedure OnFontChange(Sender: TObject);
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    function GetWidth: Integer;
    function FormatLineNumber(Line: Integer): String;
    procedure Assign(Source: TPersistent); override;
    procedure SetDigitCount(Value: Integer);
    procedure UpdateFont;
  published
    property Color: TColor read fColor write SetColor default clBtnFace;
    property BorderColor: TColor read fBorderColor write SetBorderColor default clWindow;
    property Cursor: TCursor read fCursor write fCursor default crDefault;
    property Font: TFont read fFont write SetFont;
    property DigitCount: Integer read fDigitCount write SetDigitCount;
    property Width: Integer read GetWidth;
    property LeadingZeros: boolean read fLeadingZeros write SetLeadingZeros
      default False;
    property LeftOffset: integer read fLeftOffset write SetLeftOffset
      default 16;
    property RightOffset: integer read fRightOffset write SetRightOffset
      default 2;
    property ShowLineNumbers: Boolean read fShowLineNumbers
      write SetShowLineNumbers default False;
    property ShowLineStates: Boolean read fShowLineStates write SetShowLineStates
      default False;
    property ShowCodeFolding: Boolean write fShowCodeFolding;
    property Visible: boolean read fVisible write SetVisible default True;
    property WordWrapGlyphVisible: Boolean read fWordWrapGlyphVisible write SetWordWrapGlyphVisible default True;
    property ZeroStart: boolean read fZeroStart write SetZeroStart
      default False;
    property LineNumberStart: Integer read fLineNumberStart write SetLineNumberStart default 1;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  TSynBookMarkOpt = class(TPersistent)
  private
    fBookmarkImages: TImageList;
    fDrawBookmarksFirst: boolean;
    fEnableKeys: Boolean;
    fGlyphsVisible: Boolean;
    fLeftMargin: Integer;
    fOwner: TComponent;
    fXoffset: integer;
    fOnChange: TNotifyEvent;
    procedure SetBookmarkImages(const Value: TImageList);
    procedure SetDrawBookmarksFirst(Value: boolean);
    procedure SetGlyphsVisible(Value: Boolean);
    procedure SetLeftMargin(Value: Integer);
    procedure SetXOffset(Value: integer);
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
  published
    property BookmarkImages: TImageList
      read fBookmarkImages write SetBookmarkImages;
    property DrawBookmarksFirst: boolean read fDrawBookmarksFirst
      write SetDrawBookmarksFirst default True;
    property EnableKeys: Boolean
      read fEnableKeys write fEnableKeys default True;
    property GlyphsVisible: Boolean
      read fGlyphsVisible write SetGlyphsVisible default True;
    property LeftMargin: Integer read fLeftMargin write SetLeftMargin default 2;
    property Xoffset: integer read fXoffset write SetXOffset default 12;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  { TSynMethodChain }

  ESynMethodChain = class(Exception);
  TSynExceptionEvent = procedure (Sender: TObject; E: Exception;
    var DoContinue: Boolean) of object;

  TSynMethodChain = class(TObject)
  private
    FNotifyProcs: TList;
    FExceptionHandler: TSynExceptionEvent;
  protected
    procedure DoFire(const AEvent: TMethod); virtual; abstract;
    function DoHandleException(E: Exception): Boolean; virtual;
    property ExceptionHandler: TSynExceptionEvent read FExceptionHandler
      write FExceptionHandler;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(AEvent: TMethod);
    procedure Remove(AEvent: TMethod);
    procedure Fire;
  end;

  { TSynNotifyEventChain }

  TSynNotifyEventChain = class(TSynMethodChain)
  private
    FSender: TObject;
  protected
    procedure DoFire(const AEvent: TMethod); override;
  public
    constructor CreateEx(ASender: TObject);
    procedure Add(AEvent: TNotifyEvent);
    procedure Remove(AEvent: TNotifyEvent);
    property ExceptionHandler;
    property Sender: TObject read FSender write FSender;
  end;

{ TSynHotKey }

const
  BorderWidth = 0;

type
  {$IFDEF SYN_CLX}
  TSynBorderStyle = bsNone..bsSingle;
  {$ELSE}
  TSynBorderStyle = TBorderStyle;
  {$ENDIF}

  THKModifier = (hkShift, hkCtrl, hkAlt);
  THKModifiers = set of THKModifier;
  THKInvalidKey = (hcNone, hcShift, hcCtrl, hcAlt, hcShiftCtrl,
    hcShiftAlt, hcCtrlAlt, hcShiftCtrlAlt);
  THKInvalidKeys = set of THKInvalidKey;

  TSynHotKey = class(TCustomControl)
  private
    FBorderStyle: TSynBorderStyle;
    FHotKey: TShortCut;
    FInvalidKeys: THKInvalidKeys;
    FModifiers: THKModifiers;
    FPressedOnlyModifiers: Boolean;
    procedure SetBorderStyle(const Value: TSynBorderStyle);
    procedure SetHotKey(const Value: TShortCut);
    procedure SetInvalidKeys(const Value: THKInvalidKeys);
    procedure SetModifiers(const Value: THKModifiers);
    {$IFNDEF SYN_CLX}
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
     procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    {$ENDIF}
  protected
    {$IFNDEF SYN_CLX}
    procedure CreateParams(var Params: TCreateParams); override;
    {$ENDIF}
    {$IFDEF SYN_CLX}
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;
    {$ENDIF}
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    {$IFDEF SYN_CLX}
    function WidgetFlags: Integer; override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BorderStyle: TSynBorderStyle read FBorderStyle write SetBorderStyle
      default bsSingle;
    property HotKey: TShortCut read FHotKey write SetHotKey default $0041; { Alt+A }
    property InvalidKeys: THKInvalidKeys read FInvalidKeys write SetInvalidKeys default [hcNone, hcShift];
    property Modifiers: THKModifiers read FModifiers write SetModifiers default [hkAlt];
  end;

  TSynEditSearchCustom = class(TComponent)
  protected
    function GetPattern: UnicodeString; virtual; abstract;
    procedure SetPattern(const Value: UnicodeString); virtual; abstract;
    function GetReplacePattern: UnicodeString; virtual; abstract;
    procedure SetReplacePattern(const Value: UnicodeString); virtual; abstract;
    function GetLength(Index: Integer): Integer; virtual; abstract;
    function GetResult(Index: Integer): Integer; virtual; abstract;
    function GetResultCount: Integer; virtual; abstract;
    procedure SetOptions(const Value: TSynSearchOptions); virtual; abstract;
  public
    function FindAll(const NewText: UnicodeString): Integer; virtual; abstract;
    function Replace(const aOccurrence, aReplacement: UnicodeString; Index: Integer): UnicodeString; virtual; abstract;
    property Pattern: UnicodeString read GetPattern write SetPattern;
    property ReplacePattern: UnicodeString read GetReplacePattern write SetReplacePattern;
    property ResultCount: Integer read GetResultCount;
    property Results[Index: Integer]: Integer read GetResult;
    property Lengths[Index: Integer]: Integer read GetLength;
    property Options: TSynSearchOptions write SetOptions;
  end;

implementation

uses
  SynEdit, SynEditMiscProcs;

type
  TRGBTripleArray = array[0..1] of TRGBTriple;
  PRGBTripleArray = ^TRGBTripleArray;

// -----------------------------------------------------------------------------

{ TSynSelectedColor }

// -----------------------------------------------------------------------------
constructor TSynSelectedColor.Create;
begin
  inherited Create;
  fBG := clHighLight;
  fFG := clHighLightText;
end;

procedure TSynSelectedColor.Assign(Source: TPersistent);
var
  Src: TSynSelectedColor;
begin
  if (Source <> nil) and (Source is TSynSelectedColor) then begin
    Src := TSynSelectedColor(Source);
    fBG := Src.fBG;
    fFG := Src.fFG;
    if Assigned(fOnChange) then fOnChange(Self);
  end else
    inherited Assign(Source);
end;

procedure TSynSelectedColor.SetBG(Value: TColor);
begin
  if (fBG <> Value) then begin
    fBG := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynSelectedColor.SetFG(Value: TColor);
begin
  if (fFG <> Value) then begin
    fFG := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

{ TSynGutter }

constructor TSynGutter.Create(AOwner: TObject);
begin
  inherited Create;

  fOwner := AOwner;

  fColor := clBtnFace;
  fVisible := False;
  fLeftOffset := 16;
  fRightOffset := 2;
  fBorderColor := clSilver;
  fShowLineNumbers := False;
  fShowLineStates := False;
  fShowCodeFolding := False;
  fLineNumberStart := 1;
  fZeroStart := False;
  fDigitCount := 2;

  fFont := TFont.Create;
  fFont.Name := 'Courier New';
  fFont.Size := 10;
  fFont.Style := [];
  fFont.OnChange := OnFontChange;

  SetFont(fFont); // Update character width and self width
end;

destructor TSynGutter.Destroy;
begin
  FreeAndNil(fFont);
  inherited Destroy;
end;

procedure TSynGutter.Assign(Source: TPersistent);
var
  Src: TSynGutter;
begin
  if Assigned(Source) and (Source is TSynGutter) then
  begin
    Src := TSynGutter(Source);
    fFont.Assign(src.Font);
    fColor := Src.fColor;
    fVisible := Src.fVisible;
    fShowLineNumbers := Src.fShowLineNumbers;
    fLeadingZeros := Src.fLeadingZeros;
    fZeroStart := Src.fZeroStart;
    fLeftOffset := Src.fLeftOffset;
    fRightOffset := Src.fRightOffset;
    fLineNumberStart := Src.fLineNumberStart;
    fBorderColor := Src.fBorderColor;
    if Assigned(fOnChange) then fOnChange(Self);
  end
  else
    inherited;
end;

procedure TSynGutter.SetDigitCount(Value: Integer);
begin
  if fDigitCount = Value then
    Exit;

  fDigitCount := MinMax(Value, 2, 12);
  fWidth := fDigitCount * fCharWidth;

  { Do not update SynEdit here }
end;

procedure TSynGutter.UpdateFont;
begin
  SetFont(fFont);
end;

function TSynGutter.FormatLineNumber(Line: Integer): String;
var
  I: Integer;
begin
  if fZeroStart then
    Dec(Line)
  else if fLineNumberStart > 1 then
    Inc(Line, fLineNumberStart - 1);
  Result := Format('%*d', [fDigitCount-1, Line]);
  if fLeadingZeros then
    for I := 1 to fDigitCount - 2 do
    begin
      if Result[I] <> ' ' then
        Break;
      Result[I] := '0';
    end;
end;

function TSynGutter.GetWidth: Integer;
begin
  Result := 0;
  if not fVisible then
    Exit;
  if fShowLineNumbers then
    Result := fLeftOffset + fWidth + fRightOffset + 2
  else
    Result := fLeftOffset + fRightOffset + 2;
  if fShowLineNumbers then
    Inc(Result, SEC_LineStatesBarWidth);
  //if fShowCodeFolding then
  //  Inc(Result, SEC_OutliningBarWidth);
end;

procedure TSynGutter.SetColor(const Value: TColor);
begin
  if fColor <> Value then
  begin
    fColor := Value;
    if Assigned(fOnChange) then
      fOnChange(Self);
  end;
end;

procedure TSynGutter.SetFont(Value: TFont);
var
  Temp: TCanvas;
begin
  { Assign new font }
  if fFont <> Value then
    fFont.Assign(Value);

  { Update char width }
  Temp := TCanvas.Create;
  try
    with Temp do
    begin
      Handle := CreateCompatibleDC(0);
      Font.Assign(fFont);
      fCharWidth := TextWidth('W');
    end;
  finally
    FreeAndNil(Temp);
  end;

  { Update gutter width }
  fWidth := fDigitCount*fCharWidth;

  { Inform editor about changes }
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

procedure TSynGutter.OnFontChange(Sender: TObject);
begin
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

procedure TSynGutter.SetLeadingZeros(const Value: boolean);
begin
  if fLeadingZeros <> Value then begin
    fLeadingZeros := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetLeftOffset(Value: integer);
begin
  Value := Max(0, Value);
  if fLeftOffset <> Value then begin
    fLeftOffset := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetRightOffset(Value: integer);
begin
  Value := Max(0, Value);
  if fRightOffset <> Value then begin
    fRightOffset := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetShowLineNumbers(const Value: boolean);
begin
  if fShowLineNumbers <> Value then begin
    fShowLineNumbers := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetShowLineStates(Value: Boolean);
begin
  if fShowLineStates <> Value then
  begin
    fShowLineStates := Value;
    if Assigned(fOnChange) then
      fOnChange(Self);
  end;
end;

procedure TSynGutter.SetVisible(Value: boolean);
begin
  if fVisible <> Value then
  begin
    fVisible := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetWordWrapGlyphVisible(Value: Boolean);
begin
  if fWordWrapGlyphVisible <> Value then
  begin
    fWordWrapGlyphVisible := Value;
    if Assigned(fOnChange) then
      fOnChange(Self);
  end;
end;

procedure TSynGutter.SetZeroStart(const Value: boolean);
begin
  if fZeroStart <> Value then begin
    fZeroStart := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetLineNumberStart(const Value: Integer);
begin
  if Value <> fLineNumberStart then
  begin
    fLineNumberStart := Value;
    if fLineNumberStart < 0 then
      fLineNumberStart := 0;
    if fLineNumberStart = 0 then
      fZeroStart := True
    else
      fZeroStart := False;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetBorderColor(const Value: TColor);
begin
  if fBorderColor <> Value then 
  begin
    fBorderColor := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

{ TSynBookMarkOpt }

constructor TSynBookMarkOpt.Create(AOwner: TComponent);
begin
  inherited Create;
  fDrawBookmarksFirst := TRUE;
  fEnableKeys := True;
  fGlyphsVisible := True;
  fLeftMargin := 2;
  fOwner := AOwner;
  fXOffset := 12;
end;

procedure TSynBookMarkOpt.Assign(Source: TPersistent);
var
  Src: TSynBookMarkOpt;
begin
  if (Source <> nil) and (Source is TSynBookMarkOpt) then begin
    Src := TSynBookMarkOpt(Source);
    fBookmarkImages := Src.fBookmarkImages;
    fDrawBookmarksFirst := Src.fDrawBookmarksFirst;
    fEnableKeys := Src.fEnableKeys;
    fGlyphsVisible := Src.fGlyphsVisible;
    fLeftMargin := Src.fLeftMargin;
    fXoffset := Src.fXoffset;
    if Assigned(fOnChange) then fOnChange(Self);
  end else
    inherited Assign(Source);
end;

procedure TSynBookMarkOpt.SetBookmarkImages(const Value: TImageList);
begin
  if fBookmarkImages <> Value then begin
    fBookmarkImages := Value;
    if Assigned(fBookmarkImages) then fBookmarkImages.FreeNotification(fOwner);
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynBookMarkOpt.SetDrawBookmarksFirst(Value: boolean);
begin
  if Value <> fDrawBookmarksFirst then begin
    fDrawBookmarksFirst := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynBookMarkOpt.SetGlyphsVisible(Value: Boolean);
begin
  if fGlyphsVisible <> Value then begin
    fGlyphsVisible := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynBookMarkOpt.SetLeftMargin(Value: Integer);
begin
  if fLeftMargin <> Value then begin
    fLeftMargin := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynBookMarkOpt.SetXOffset(Value: integer);
begin
  if fXOffset <> Value then begin
    fXOffset := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

{ TSynMethodChain }

procedure TSynMethodChain.Add(AEvent: TMethod);
begin
  if not Assigned(@AEvent) then
    raise ESynMethodChain.CreateFmt(
      '%s.Entry : the parameter `AEvent` must be specified.', [ClassName]);

  with FNotifyProcs, AEvent do
  begin
    Add(Code);
    Add(Data);
  end
end;

constructor TSynMethodChain.Create;
begin
  inherited;
  FNotifyProcs := TList.Create;
end;

destructor TSynMethodChain.Destroy;
begin
  FNotifyProcs.Free;
  inherited;
end;

function TSynMethodChain.DoHandleException(E: Exception): Boolean;
begin
  if not Assigned(FExceptionHandler) then
    raise E
  else
    try
      Result := True;
      FExceptionHandler(Self, E, Result);
    except
      raise ESynMethodChain.CreateFmt(
        '%s.DoHandleException : MUST NOT occur any kind of exception in '+
        'ExceptionHandler', [ClassName]);
    end;
end;

procedure TSynMethodChain.Fire;
var
  AMethod: TMethod;
  i: Integer;
begin
  i := 0;
  with FNotifyProcs, AMethod do
    while i < Count do
      try
        repeat
          Code := Items[i];
          Inc(i);
          Data := Items[i];
          Inc(i);

          DoFire(AMethod)
        until i >= Count;
      except
        on E: Exception do
          if not DoHandleException(E) then
            i := MaxInt;
      end;
end;

procedure TSynMethodChain.Remove(AEvent: TMethod);
var
  i: Integer;
begin
  if not Assigned(@AEvent) then
    raise ESynMethodChain.CreateFmt(
      '%s.Remove: the parameter `AEvent'' must be specified.', [ClassName]);

  with FNotifyProcs, AEvent do
  begin
    i := Count - 1;
    while i > 0 do
      if Items[i] <> Data then
        Dec(i, 2)
      else
      begin
        Dec(i);
        if Items[i] = Code then
        begin
          Delete(i);
          Delete(i);
        end;
        Dec(i);
      end;
  end;
end;

{ TSynNotifyEventChain }

procedure TSynNotifyEventChain.Add(AEvent: TNotifyEvent);
begin
  inherited Add(TMethod(AEvent));
end;

constructor TSynNotifyEventChain.CreateEx(ASender: TObject);
begin
  inherited Create;
  FSender := ASender;
end;

procedure TSynNotifyEventChain.DoFire(const AEvent: TMethod);
begin
  TNotifyEvent(AEvent)(FSender);
end;

procedure TSynNotifyEventChain.Remove(AEvent: TNotifyEvent);
begin
  inherited Remove(TMethod(AEvent));
end;

{ TSynHotKey }

function KeySameAsShiftState(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := (Key = SYNEDIT_SHIFT) and (ssShift in Shift) or
            (Key = SYNEDIT_CONTROL) and (ssCtrl in Shift) or
            (Key = SYNEDIT_MENU) and (ssAlt in Shift);
end;

function ModifiersToShiftState(Modifiers: THKModifiers): TShiftState;
begin
  Result := [];
  if hkShift in Modifiers then Include(Result, ssShift);
  if hkCtrl in Modifiers then Include(Result, ssCtrl);
  if hkAlt in Modifiers then Include(Result, ssAlt);
end;

function ShiftStateToTHKInvalidKey(Shift: TShiftState): THKInvalidKey;
begin
  Shift := Shift * [ssShift, ssAlt, ssCtrl];
  if Shift = [ssShift] then
    Result := hcShift
  else if Shift = [ssCtrl] then
    Result := hcCtrl
  else if Shift = [ssAlt] then
    Result := hcAlt
  else if Shift = [ssShift, ssCtrl] then
    Result := hcShiftCtrl
  else if Shift = [ssShift, ssAlt] then
    Result := hcShiftAlt
  else if Shift = [ssCtrl, ssAlt] then
    Result := hcCtrlAlt
  else if Shift = [ssShift, ssCtrl, ssAlt] then
    Result := hcShiftCtrlAlt
  else
    Result := hcNone;
end;

function ShortCutToTextEx(Key: Word; Shift: TShiftState): UnicodeString;
begin
  if ssCtrl in Shift then Result := SmkcCtrl;
  if ssShift in Shift then Result := Result + SmkcShift;
  if ssAlt in Shift then Result := Result + SmkcAlt;
    Result := Result + ShortCutToText(TShortCut(Key));
  if Result = '' then
    Result := srNone;
end;

constructor TSynHotKey.Create(AOwner: TComponent);
begin
  inherited;

  BorderStyle := bsSingle;
  ControlStyle := ControlStyle + [csNeedsBorderPaint];

  FInvalidKeys := [hcNone, hcShift];
  FModifiers := [hkAlt];
  SetHotKey($0041); { Alt+A }

  ParentColor := False;
  Color := clWindow;
  TabStop := True;
end;

procedure TSynHotKey.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TSynBorderStyle] of DWORD = (0, WS_BORDER);
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  inherited CreateParams(Params);
  with Params do
  begin
    WindowClass.Style := WindowClass.Style and not ClassStylesOff;
    Style := Style or BorderStyles[fBorderStyle] or WS_CLIPCHILDREN;

    if NewStyleControls and Ctl3D and (fBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TSynHotKey.DoExit;
begin
  inherited;
  if FPressedOnlyModifiers then
  begin
    Text := srNone;
    Invalidate;
  end;
end;

procedure TSynHotKey.KeyDown(var Key: Word; Shift: TShiftState);
var
  MaybeInvalidKey: THKInvalidKey;
  SavedKey: Word;
begin
  SavedKey := Key;
  FPressedOnlyModifiers := KeySameAsShiftState(Key, Shift);

  MaybeInvalidKey := ShiftStateToTHKInvalidKey(Shift);
  if MaybeInvalidKey in FInvalidKeys then
    Shift := ModifiersToShiftState(FModifiers);

  if not FPressedOnlyModifiers then
  begin
    FHotKey := ShortCut(Key, Shift)
  end
  else
  begin
    FHotKey := 0;
    Key := 0;
  end;

  if Text <> ShortCutToTextEx(Key, Shift) then
  begin
    Text := ShortCutToTextEx(Key, Shift);
    Invalidate;
    SetCaretPos(BorderWidth + 1 + TextWidth(Canvas, Text), BorderWidth + 1);
  end;

  Key := SavedKey;
end;

procedure TSynHotKey.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if FPressedOnlyModifiers then
  begin
    Text := srNone;
    Invalidate;
    SetCaretPos(BorderWidth + 1 + TextWidth(Canvas, Text), BorderWidth + 1);
  end;
end;

procedure TSynHotKey.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  SetFocus;
end;

procedure TSynHotKey.Paint;
var
  r: TRect;
begin
  r := ClientRect;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  InflateRect(r, -BorderWidth, -BorderWidth);
  Canvas.FillRect(r);
  TextRect(Canvas, r, BorderWidth + 1, BorderWidth + 1, Text);
end;

procedure TSynHotKey.SetBorderStyle(const Value: TSynBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TSynHotKey.SetHotKey(const Value: TShortCut);
var
  Key: Word;
  Shift: TShiftState;
  MaybeInvalidKey: THKInvalidKey;
begin
  ShortCutToKey(Value, Key, Shift);

  MaybeInvalidKey := ShiftStateToTHKInvalidKey(Shift);
  if MaybeInvalidKey in FInvalidKeys then
    Shift := ModifiersToShiftState(FModifiers);

  FHotKey := ShortCut(Key, Shift);
  Text := ShortCutToTextEx(Key, Shift);
  Invalidate;
  if not Visible then
    SetCaretPos(BorderWidth + 1 + TextWidth(Canvas, Text), BorderWidth + 1);
end;

procedure TSynHotKey.SetInvalidKeys(const Value: THKInvalidKeys);
begin
  FInvalidKeys := Value;
  SetHotKey(FHotKey);
end;

procedure TSynHotKey.SetModifiers(const Value: THKModifiers);
begin
  FModifiers := Value;
  SetHotKey(FHotKey);
end;

procedure TSynHotKey.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTTAB or DLGC_WANTARROWS;
end;

procedure TSynHotKey.WMKillFocus(var Msg: TWMKillFocus);
begin
  DestroyCaret;
end;

procedure TSynHotKey.WMSetFocus(var Msg: TWMSetFocus);
begin
  Canvas.Font := Font;
  CreateCaret(Handle, 0, 1, -Canvas.Font.Height + 2);
  SetCaretPos(BorderWidth + 1 + TextWidth(Canvas, Text), BorderWidth + 1);
  ShowCaret(Handle);
end;

end.
