(*
  Letterpress

  Classes handling info about headers and footers.

  Copyright 2000-2010, initial developers and Garnet
*)

{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPrintHeaderFooter.pas, released 2000-06-01.

The Initial Author of the Original Code is Morten J. Skovrup.
Portions written by Morten J. Skovrup are copyright 2000 Morten J. Skovrup.
Portions written by Michael Hieke are copyright 2000 Michael Hieke.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynEditPrintHeaderFooter.pas,v 1.10.2.7 2008/09/23 14:02:08 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://synedit.sourceforge.net
-------------------------------------------------------------------------------}

unit SynEditPrintHeaderFooter;

{$M+}
{$I SynEdit.inc}

interface

uses
  Windows, Graphics, Classes, SysUtils,

  { SynEdit }
  SynEditPrintTypes, SynUnicode;

type

  { An item in a header or footer. An item has a text,Font,LineNumber and
    Alignment (i.e. two items can be on the same line but have different
    fonts) }
  THeaderFooterItem = class
  private
    fText: UnicodeString;
    fFont: TFont;
    fLineNumber: Integer;
    fAlignment: TAlignment;

    { Used to store the original Index when the item was added - the index
      might change when the list is sorted }
    fIndex: Integer;

    function GetAsString: UnicodeString;
    procedure SetAsString(const Value: UnicodeString);
    procedure SetFont(const Value: TFont);
  public
    constructor Create;
    destructor Destroy; override;

    function GetText(NumPages, PageNum: Integer; Roman: Boolean;
      Title, ATime, ADate: UnicodeString): UnicodeString;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  public
    property Alignment: TAlignment read fAlignment write fAlignment;
    property AsString: UnicodeString read GetAsString write SetAsString;
    property Font: TFont read fFont write SetFont;
    property LineNumber: Integer read fLineNumber write fLineNumber;
    property Text: UnicodeString read fText write fText;
  end;

  THeaderFooterType = (hftHeader, hftFooter);

  { Used internally to calculate line height and font-base-line for header
    and footer }
  TLineInfo = class
  public
    LineHeight: Integer;
    MaxBaseDist: Integer;
  end;

  { The Header / Footer class }
  THeaderFooter = class(TPersistent)
  private
    fType: THeaderFooterType;
    fFrameTypes: TFrameTypes;
    fShadedColor: TColor;
    fLineColor: TColor;
    fItems: TList;
    fDefaultFont: TFont;
    fDate, fTime: UnicodeString;
    fNumPages: Integer;
    fTitle: UnicodeString;
    fMargins: TSynEditPrintMargins;
    fFrameHeight: Integer;
    fOldPen: TPen;
    fOldBrush: TBrush;
    fOldFont: TFont;
    fRomanNumbers: Boolean;
    fLineInfo: TList;
    fLineCount: Integer;
    fMirrorPosition: Boolean;

    procedure SetDefaultFont(const Value: TFont);
    procedure DrawFrame(ACanvas: TCanvas);
    procedure CalcHeight(ACanvas: TCanvas);
    procedure SaveFontPenBrush(ACanvas: TCanvas);
    procedure RestoreFontPenBrush(ACanvas: TCanvas);

    function GetAsString: UnicodeString;
    procedure SetAsString(const Value: UnicodeString);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Text: UnicodeString; Font: TFont; Alignment: TAlignment;
      LineNumber: Integer): Integer;
    procedure Delete(Index: Integer);
    procedure Clear;
    function Count: Integer;
    function Get(Index: Integer): THeaderFooterItem;
    procedure SetPixPrInch(Value: Integer);
    procedure InitPrint(ACanvas: TCanvas; NumPages: Integer; Title: UnicodeString;
      Margins: TSynEditPrintMargins);
    procedure Print(ACanvas: TCanvas; PageNum: Integer);
    procedure Assign(Source: TPersistent); override;
    procedure FixLines;
    property AsString: UnicodeString read GetAsString write SetAsString;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  published
    property FrameTypes: TFrameTypes read fFrameTypes write fFrameTypes
      default [ftLine];
    property ShadedColor: TColor read fShadedColor write fShadedColor
      default clSilver;
    property LineColor: TColor read fLineColor write fLineColor default clBlack;
    property DefaultFont: TFont read fDefaultFont write SetDefaultFont;
    property RomanNumbers: Boolean read fRomanNumbers write fRomanNumbers
      default False;
    property MirrorPosition: Boolean read fMirrorPosition write fMirrorPosition
      default False;
  end;

  { The header and footer - does nothing but set the value of fType in
    THeaderFooter }
  THeader = class(THeaderFooter)
  public
    constructor Create;
  end;

  TFooter = class(THeaderFooter)
  public
    constructor Create;
  end;

implementation

uses
  Math, SynEditMiscProcs;

// -----------------------------------------------------------------------------
// Helper routine for AsString processing
function GetFirstEl(var Value: UnicodeString; Delim: WideChar): UnicodeString;
var
  p: Integer;
begin
  p := Pos(Delim, Value);
  if p = 0 then
    p := Length(Value) + 1;
  Result := Copy(Value, 1, p - 1);
  Delete(Value, 1, p);
end;

{ THeaderFooterItem }

constructor THeaderFooterItem.Create;
begin
  inherited;
  fFont := TFont.Create;
end;

destructor THeaderFooterItem.Destroy;
begin
  inherited;
  FreeAndNil(fFont);
end;

// -----------------------------------------------------------------------------
// Returns string representation of THeaderFooterItem to alleviate storing
// items into external storage (registry, ini file)
function THeaderFooterItem.GetAsString: UnicodeString;
begin
  Result :=
    EncodeString(fText) + '/' +
    IntToStr(fFont.Charset) + '/' +
    IntToStr(fFont.Color) + '/' +
    IntToStr(fFont.Height) + '/' +
    EncodeString(fFont.Name) + '/' +
    IntToStr(Ord(fFont.Pitch)) + '/' +
    IntToStr(fFont.PixelsPerInch) + '/' +
    IntToStr(fFont.Size) + '/' +
    IntToStr(byte(fFont.Style)) + '/' +
    IntToStr(fLineNumber) + '/' +
    IntToStr(Ord(fAlignment));
end;

// -----------------------------------------------------------------------------
// This is basically copied from original SynEditPrint.pas. Returns the
// header / footer text with macros expanded
function THeaderFooterItem.GetText(NumPages, PageNum: Integer;
  Roman: Boolean; Title, ATime, ADate: UnicodeString): UnicodeString;
var
  Len, Start, Run: Integer;
  AStr: UnicodeString;

  procedure DoAppend(AText: UnicodeString);
  begin
    Result := Result + AText;
  end;

  procedure TryAppend(var First: Integer; After: Integer);
  begin
    if After > First then
    begin
      DoAppend(Copy(AStr, First, After - First));
      First := After;
    end;
  end;

  function TryExecuteMacro: Boolean;
  var
    Macro: UnicodeString;
  begin
    Result := True;
    Macro := WideUpperCase(Copy(fText, Start, Run - Start + 1));
    if Macro = '$PAGENUM$' then
    begin
      if Roman then
        DoAppend(IntToRoman(PageNum))
      else
        DoAppend(IntToStr(PageNum));
      Exit;
    end;
    if Macro = '$PAGECOUNT$' then
    begin
      if Roman then
        DoAppend(IntToRoman(NumPages))
      else
        DoAppend(IntToStr(NumPages));
      Exit;
    end;
    if Macro = '$TITLE$' then
    begin
      DoAppend(Title);
      Exit;
    end;
    if Macro = '$DATE$' then
    begin
      DoAppend(ADate);
      Exit;
    end;
    if Macro = '$TIME$' then
    begin
      DoAppend(ATime);
      Exit;
    end;
    if Macro = '$DATETIME$' then
    begin
      DoAppend(ADate + ' ' + ATime);
      Exit;
    end;
    if Macro = '$TIMEDATE$' then
    begin
      DoAppend(ATime + ' ' + ADate);
      Exit;
    end;
    Result := False;
  end;

begin
  Result := '';
  AStr := fText;
  if Trim(AStr) = '' then
    Exit;
  // parse the line
  Len := Length(AStr);
  if Len > 0 then
  begin
    // start with left-aligned text
    Start := 1;
    Run := 1;
    while Run <= Len do
    begin
      // test for embedded macro
      if AStr[Run] = '$' then
      begin
        TryAppend(Start, Run);
        Inc(Run);
        // search for next '$' which could mark the end of a macro
        while Run <= Len do begin
          if AStr[Run] = '$' then
          begin
            // if this is a macro execute it and skip the chars from output
            if TryExecuteMacro then
            begin
              Inc(Run); // also the '$'
              Start := Run;
              break;
            end
            else
            begin
              // this '$' might again be the start of a macro
              TryAppend(Start, Run);
              Inc(Run);
            end;
          end
          else
            Inc(Run);
        end;
      end
      else
        Inc(Run);
    end;
    TryAppend(Start, Run);
  end;
end;

procedure THeaderFooterItem.LoadFromStream(AStream: TStream);
var
  aCharset: TFontCharset;
  aColor: TColor;
  aHeight: Integer;
  aName: TFontName;
  aPitch: TFontPitch;
  aSize: Integer;
  aStyle: TFontStyles;
  Len, BufferSize: Integer;
  Buffer: Pointer;
begin
  with AStream do
  begin
    Read(Len, sizeof(Len));
    BufferSize := Len * sizeof(WideChar);
    GetMem(Buffer, BufferSize + sizeof(WideChar));
    try
      Read(Buffer^, BufferSize);
      PWideChar(Buffer)[BufferSize div sizeof(WideChar)] := #0;
      fText := PWideChar(Buffer);
    finally
      FreeMem(Buffer);
    end;
    Read(fLineNumber, sizeof(fLineNumber));
    // font
    Read(aCharset, sizeof(aCharset));
    Read(aColor, sizeof(aColor));
    Read(aHeight, sizeof(aHeight));
    Read(BufferSize, sizeof(BufferSize));
    GetMem(Buffer, BufferSize + 1);
    try
      Read(Buffer^, BufferSize);
      PAnsiChar(Buffer)[BufferSize div sizeof(AnsiChar)] := #0;
      aName := string(PAnsiChar(Buffer));
    finally
      FreeMem(Buffer);
    end;
    Read(aPitch, sizeof(aPitch));
    Read(aSize, sizeof(aSize));
    Read(aStyle, sizeof(aStyle));
    fFont.Charset := aCharset;
    fFont.Color := aColor;
    fFont.Height := aHeight;
    fFont.Name := aName;
    fFont.Pitch := aPitch;
    fFont.Size := aSize;
    fFont.Style := aStyle;
    Read(fAlignment, sizeof(fAlignment));
  end;
end;

procedure THeaderFooterItem.SaveToStream(AStream: TStream);
var
  aCharset: TFontCharset;
  aColor: TColor;
  aHeight: Integer;
  aName: TFontName;
  aPitch: TFontPitch;
  aSize: Integer;
  aStyle: TFontStyles;
  aLen: Integer;
begin
  with AStream do
  begin
    aLen := Length(fText);
    Write(aLen, sizeof(aLen));
    Write(PWideChar(fText)^, aLen * sizeof(WideChar));
    Write(fLineNumber, sizeof(fLineNumber));
    // font
    aCharset := fFont.Charset;
    aColor   := fFont.Color;
    aHeight  := fFont.Height;
    aName    := fFont.Name;
    aPitch   := fFont.Pitch;
    aSize    := fFont.Size;
    aStyle   := fFont.Style;
    Write(aCharset, SizeOf(aCharset));
    Write(aColor, SizeOf(aColor));
    Write(aHeight, SizeOf(aHeight));
    aLen := Length(aName);
    Write(aLen, SizeOf(aLen));
    Write(PAnsiChar(AnsiString(aName))^, aLen);
    Write(aPitch, SizeOf(aPitch));
    Write(aSize, SizeOf(aSize));
    Write(aStyle, SizeOf(aStyle));
    Write(fAlignment, SizeOf(fAlignment));
  end;
end;

procedure THeaderFooterItem.SetAsString(const Value: UnicodeString);
var
  s: UnicodeString;
  sty: TFontStyles;
begin
  s := Value;
  fText := DecodeString(GetFirstEl(s, '/'));
  fFont.Charset := StrToIntDef(GetFirstEl(s, '/'), 0);
  fFont.Color := StrToIntDef(GetFirstEl(s, '/'), 0);
  fFont.Height := StrToIntDef(GetFirstEl(s, '/'), 0);
  fFont.Name := DecodeString(GetFirstEl(s, '/'));
  fFont.Pitch := TFontPitch(StrToIntDef(GetFirstEl(s, '/'), 0));
  fFont.PixelsPerInch := StrToIntDef(GetFirstEl(s, '/'), 0);
  fFont.Size := StrToIntDef(GetFirstEl(s, '/'), 0);
  byte(sty) := StrToIntDef(GetFirstEl(s, '/'), 0);
  fFont.Style := sty;
  fLineNumber := StrToIntDef(GetFirstEl(s, '/'), 0);
  fAlignment := TAlignment(StrToIntDef(GetFirstEl(s, '/'), 0));
end;

procedure THeaderFooterItem.SetFont(const Value: TFont);
begin
  fFont.Assign(Value);
end;

{ THeaderFooter }

constructor THeaderFooter.Create;
begin
  inherited;
  fFrameTypes := [ftLine];
  fShadedColor := clSilver;
  fLineColor := clBlack;
  fItems := TList.Create;
  fDefaultFont := TFont.Create;
  fOldPen := TPen.Create;
  fOldBrush := TBrush.Create;
  fOldFont := TFont.Create;
  fRomanNumbers := False;
  fMirrorPosition := False;
  fLineInfo := TList.Create;
  with fDefaultFont do
  begin
    Name := 'Arial';
    Size := 10;
    Color := clBlack;
  end;
end;

destructor THeaderFooter.Destroy;
var
  i: Integer;
begin
  Clear;
  fItems.Free;
  fDefaultFont.Free;
  fOldPen.Free;
  fOldBrush.Free;
  fOldFont.Free;
  for i := 0 to fLineInfo.Count - 1 do
    TLineInfo(fLineInfo[i]).Free;
  fLineInfo.Free;
  inherited;
end;

function THeaderFooter.Add(Text: UnicodeString; Font: TFont;
  Alignment: TAlignment; LineNumber: Integer): Integer;
var
  AItem: THeaderFooterItem;
begin
  AItem := THeaderFooterItem.Create;
  if Font = nil then
    AItem.Font := fDefaultFont
  else
    AItem.Font := Font;
  AItem.Alignment := Alignment;
  AItem.LineNumber := LineNumber;
  AItem.fIndex := fItems.Add(AItem);
  AItem.Text := Text;
  Result := AItem.fIndex;
end;

procedure THeaderFooter.Delete(Index: Integer);
var
  i: Integer;
begin
  for i := 0 to fItems.Count - 1 do
  begin
    if THeaderFooterItem(fItems[i]).fIndex = Index then
    begin
      fItems.Delete(i);
      Break;
    end;
  end;
end;

procedure THeaderFooter.Clear;
var
  i: Integer;
begin
  for i := 0 to fItems.Count - 1 do
    THeaderFooterItem(fItems[i]).Free;
  fItems.Clear;
end;

procedure THeaderFooter.SetDefaultFont(const Value: TFont);
begin
  fDefaultFont.Assign(Value);
end;

{ Counts number of lines in header/footer and changes the line-number so they
  start with 1 (the user might add header/footer items starting at line 2) }
procedure THeaderFooter.FixLines;
var
  i, CurLine: Integer;
  LineInfo: TLineInfo;
begin
  for i := 0 to fLineInfo.Count - 1 do
    TLineInfo(fLineInfo[i]).Free;
  fLineInfo.Clear;
  CurLine := 0;
  fLineCount := 0;
  for i := 0 to fItems.Count - 1 do
  begin
    if THeaderFooterItem(fItems[i]).LineNumber <> CurLine then
    begin
      CurLine := THeaderFooterItem(fItems[i]).LineNumber;
      fLineCount := fLineCount + 1;
      LineInfo := TLineInfo.Create;
      fLineInfo.Add(LineInfo);
    end;
    THeaderFooterItem(fItems[i]).LineNumber := fLineCount;
  end;
end;

{ Calculates the hight of the header/footer, finds the line height for each line
  and calculates the font baseline where text is to be written }
procedure THeaderFooter.CalcHeight(ACanvas: TCanvas);
var
  i, CurLine: Integer;
  AItem: THeaderFooterItem;
  FOrgHeight: Integer;
  TextMetric: TTextMetric;
begin
  fFrameHeight := -1;
  if fItems.Count <= 0 then Exit;

  CurLine := 1;
  fFrameHeight := 0;
  FOrgHeight := fFrameHeight;
  for i := 0 to fItems.Count - 1 do
  begin
    AItem := THeaderFooterItem(fItems[i]);
    if AItem.LineNumber <> CurLine then
    begin
      CurLine := AItem.LineNumber;
      FOrgHeight := fFrameHeight;
    end;
    ACanvas.Font.Assign(AItem.Font);
    GetTextMetrics(ACanvas.Handle, TextMetric);
    with TLineInfo(fLineInfo[CurLine - 1]), TextMetric do
    begin
      LineHeight := Max(LineHeight, TextHeight(ACanvas, 'W'));
      MaxBaseDist := Max(MaxBaseDist, tmHeight - tmDescent);
    end;
    fFrameHeight := Max(fFrameHeight, FOrgHeight + TextHeight(ACanvas, 'W'));
  end;
  fFrameHeight := fFrameHeight + 2 * fMargins.PHFInternalMargin;
end;

// -----------------------------------------------------------------------------
// Used to sort header / footer items
function CompareItems(Item1, Item2: Pointer): Integer;
begin
  Result := THeaderFooterItem(Item1).LineNumber - THeaderFooterItem(Item2).LineNumber;
  if Result = 0 then
    Result := Integer(Item1) - Integer(Item2);
end;

procedure THeaderFooter.SetPixPrInch(Value: Integer);
var
  i, TmpSize: Integer;
  AFont: TFont;
begin
  for i := 0 to fItems.Count - 1 do
  begin
    AFont := THeaderFooterItem(fItems[i]).Font;
    TmpSize := AFont.Size;
    AFont.PixelsPerInch := Value;
    AFont.Size := TmpSize;
  end;
end;

procedure THeaderFooter.InitPrint(ACanvas: TCanvas; NumPages: Integer; Title: UnicodeString;
  Margins: TSynEditPrintMargins);
begin
  SaveFontPenBrush(ACanvas);
  fDate := DateToStr(Now);
  fTime := TimeToStr(Now);
  fNumPages := NumPages;
  fMargins := Margins;
  fTitle := Title;
  fItems.Sort(CompareItems);
  FixLines;
  CalcHeight(ACanvas);
  RestoreFontPenBrush(ACanvas);
end;

procedure THeaderFooter.SaveFontPenBrush(ACanvas: TCanvas);
begin
  fOldFont.Assign(ACanvas.Font);
  fOldPen.Assign(ACanvas.Pen);
  fOldBrush.Assign(ACanvas.Brush);
end;

procedure THeaderFooter.RestoreFontPenBrush(ACanvas: TCanvas);
begin
  ACanvas.Font.Assign(fOldFont);
  ACanvas.Pen.Assign(fOldPen);
  ACanvas.Brush.Assign(fOldBrush);
end;

// -----------------------------------------------------------------------------
// Draws frame around header / footer
procedure THeaderFooter.DrawFrame(ACanvas: TCanvas);
begin
  if (FrameTypes = []) then Exit;
  with ACanvas, fMargins do begin
    Pen.Color := LineColor;
    Brush.Color := ShadedColor;
    if ftShaded in FrameTypes then
      Brush.Style := bsSolid
    else
      Brush.Style := bsClear;
    if ftBox in FrameTypes then
      Pen.Style := psSolid
    else
      Pen.Style := psClear;
    if FrameTypes * [ftBox, ftShaded] <> [] then begin
      if fType = hftHeader then
        Rectangle(PLeft, PHeader - fFrameHeight, PRight, PHeader)
      else
        Rectangle(PLeft, PFooter, PRight, PFooter + fFrameHeight);
    end;
    if ftLine in FrameTypes then begin
      Pen.Style := psSolid;
      if fType = hftHeader then begin
        MoveTo(PLeft, PHeader);
        LineTo(PRight, PHeader);
      end
      else begin
        MoveTo(PLeft, PFooter);
        LineTo(PRight, PFooter);
      end
    end;
  end;
end;

procedure THeaderFooter.Print(ACanvas: TCanvas; PageNum: Integer);
var
  i, X, Y, CurLine: Integer;
  AStr: UnicodeString;
  AItem: THeaderFooterItem;
  OldAlign: UINT;
  TheAlignment: TAlignment;
begin
  if (fFrameHeight <= 0) then Exit; // No header/footer
  SaveFontPenBrush(ACanvas);
  DrawFrame(ACanvas);
  ACanvas.Brush.Style := bsClear;
  if fType = hftHeader then
    Y := fMargins.PHeader - fFrameHeight
  else
    Y := fMargins.PFooter;
  Y := Y + fMargins.PHFInternalMargin; // Add the specified internal margin

  CurLine := 1;
  for i := 0 to fItems.Count - 1 do
  begin
    AItem := THeaderFooterItem(fItems[i]);
    ACanvas.Font := AItem.Font;
    if AItem.LineNumber <> CurLine then
    begin
      Y := Y + TLineInfo(fLineInfo[CurLine - 1]).LineHeight;
      CurLine := AItem.LineNumber;
    end;
    AStr := AItem.GetText(fNumPages, PageNum, fRomanNumbers, fTitle, fTime, fDate);
    // Find the alignment of the header/footer item - check for MirrorPosition
    TheAlignment := AItem.Alignment;
    if MirrorPosition and ((PageNum mod 2) = 0) then
    begin
      case AItem.Alignment of
        taRightJustify: TheAlignment := taLeftJustify;
        taLeftJustify: TheAlignment := taRightJustify;
      end;
    end;
    // Find X-position of text
    with fMargins do begin
      X := PLeftHFTextIndent;
      case TheAlignment of
        taRightJustify: X := PRightHFTextIndent - TextWidth(ACanvas, AStr);
        taCenter: X := (PLeftHFTextIndent + PRightHFTextIndent - TextWidth(ACanvas, AStr)) div 2;
      end;
    end;
    { Aligning at base line - Fonts can have different size in headers and footers }
    OldAlign := SetTextAlign(ACanvas.Handle, TA_BASELINE);
    ExtTextOutW(ACanvas.Handle, X, Y + TLineInfo(fLineInfo[CurLine - 1]).MaxBaseDist,
      0, nil, PWideChar(AStr), Length(AStr), nil);
    SetTextAlign(ACanvas.Handle, OldAlign);
  end;
  RestoreFontPenBrush(ACanvas);
end;

procedure THeaderFooter.Assign(Source: TPersistent);
var
  Src: THeaderFooter;
  i: Integer;
begin
  if (Source <> nil) and (Source is THeaderFooter) then begin
    Src := THeaderFooter(Source);
    Clear;
    fType := Src.fType;
    fFrameTypes := Src.fFrameTypes;
    fShadedColor := Src.fShadedColor;
    fLineColor := Src.fLineColor;
    for i := 0 to Src.fItems.Count - 1 do begin
      with THeaderFooterItem(Src.fItems[i]) do
        Add(Text, Font, Alignment, LineNumber);
    end;
    fDefaultFont.Assign(Src.fDefaultFont);
    fRomanNumbers := Src.fRomanNumbers;
    fMirrorPosition := Src.fMirrorPosition;
  end else
    inherited Assign(Source);
end;

function THeaderFooter.Count: Integer;
begin
  Result := fItems.Count;
end;

function THeaderFooter.Get(Index: Integer): THeaderFooterItem;
begin
  Result := THeaderFooterItem(fItems[Index]);
end;

function THeaderFooter.GetAsString: UnicodeString;
var
  i: integer;
begin
  FixLines;
  Result := '';
  for i := 0 to fItems.Count - 1 do
  begin
    if Result <> '' then Result := Result + '/';
    Result := Result + EncodeString(THeaderFooterItem(fItems[i]).AsString);
  end;
end;

procedure THeaderFooter.SetAsString(const Value: UnicodeString);
var
  item: THeaderFooterItem;
  s: UnicodeString;
begin
  Clear;
  item := THeaderFooterItem.Create;
  try
    s := Value;
    while s <> '' do
    begin
      item.AsString := DecodeString(GetFirstEl(s, '/'));
      Add(item.Text, item.Font, item.Alignment, item.LineNumber);
    end; 
  finally
    item.Free;
  end;
end;

procedure THeaderFooter.LoadFromStream(AStream: TStream);
var
  Num, i: Integer;
  aCharset: TFontCharset;
  aColor: TColor;
  aHeight: Integer;
  aName: TFontName;
  aPitch: TFontPitch;
  aSize: Integer;
  aStyle: TFontStyles;
  bufSize: Integer;
  buffer: PAnsiChar;
begin
  with AStream do begin
    // read header/footer properties first
    Read(fFrameTypes, SizeOf(fFrameTypes));
    Read(fShadedColor, SizeOf(fShadedColor));
    Read(fLineColor, SizeOf(fLineColor));
    Read(fRomanNumbers, SizeOf(fRomanNumbers));
    Read(fMirrorPosition, SizeOf(fMirrorPosition));
    // font
    Read(aCharset, SizeOf(aCharset));
    Read(aColor, SizeOf(aColor));
    Read(aHeight, SizeOf(aHeight));
    Read(bufSize, SizeOf(bufSize));
    GetMem(buffer, bufSize+1);
    try
      Read(buffer^, bufSize);
      buffer[bufSize] := #0;
      aName := string(buffer);
    finally
      FreeMem(buffer);
    end;
    Read(aPitch, SizeOf(aPitch));
    Read(aSize, SizeOf(aSize));
    Read(aStyle, SizeOf(aStyle));
    fDefaultFont.Charset := aCharset;
    fDefaultFont.Color   := aColor;
    fDefaultFont.Height  := aHeight;
    fDefaultFont.Name    := aName;
    fDefaultFont.Pitch   := aPitch;
    fDefaultFont.Size    := aSize;
    fDefaultFont.Style   := aStyle;
    // now read in the items
    Read(Num, SizeOf(Num));
    while Num > 0 do
    begin
      // load headerfooter items from stream
      i := Add('', nil, taLeftJustify, 1);
      Get(i).LoadFromStream(AStream);
      Dec(Num);
    end;
  end;
end;

procedure THeaderFooter.SaveToStream(AStream: TStream);
var
  i, Num: integer;
  aCharset: TFontCharset;
  aColor: TColor;
  aHeight: Integer;
  aName: TFontName;
  aPitch: TFontPitch;
  aSize: Integer;
  aStyle: TFontStyles;
  aLen : integer;
begin
  with AStream do begin
    // write the header/footer properties first
    Write(fFrameTypes, SizeOf(fFrameTypes));
    Write(fShadedColor, SizeOf(fShadedColor));
    Write(fLineColor, SizeOf(fLineColor));
    Write(fRomanNumbers, SizeOf(fRomanNumbers));
    Write(fMirrorPosition, SizeOf(fMirrorPosition));
    // font
    aCharset := fDefaultFont.Charset;
    aColor   := fDefaultFont.Color;
    aHeight  := fDefaultFont.Height;
    aName    := fDefaultFont.Name;
    aPitch   := fDefaultFont.Pitch;
    aSize    := fDefaultFont.Size;
    aStyle   := fDefaultFont.Style;
    Write(aCharset, SizeOf(aCharset));
    Write(aColor, SizeOf(aColor));
    Write(aHeight, SizeOf(aHeight));
    aLen := Length(aName);
    Write(aLen, SizeOf(aLen));
    Write(PAnsiChar(AnsiString(aName))^, Length(aName));
    Write(aPitch, SizeOf(aPitch));
    Write(aSize, SizeOf(aSize));
    Write(aStyle, SizeOf(aStyle));

    // now write the items
    Num := Count;
    Write(Num, SizeOf(Num));
    for i := 0 to Num - 1 do
      Get(i).SaveToStream(AStream);
  end;
end;

{ THeader }

constructor THeader.Create;
begin
  inherited;
  fType := hftHeader;
end;

{ TFooter }

constructor TFooter.Create;
begin
  inherited;
  fType := hftFooter;
end;

end.
