(*
  Letterpress

  Copyright 2000-2010, initial developers and Garnet
*)

{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/mpl/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPrint.pas, released 2000-06-01.

The Initial Author of the Original Code is Morten J. Skovrup.
Portions written by Morten J. Skovrup are copyright 2000 Morten J. Skovrup.
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

$Id: SynEditPrint.pas,v 1.34.2.12 2008/09/23 14:02:08 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://synedit.sourceforge.net
-------------------------------------------------------------------------------}

unit SynEditPrint;

{$M+}
{$I SynEdit.inc}

interface

uses
  Types, Windows, Graphics, SysUtils, Classes,

  { SynEdit }
  SynEdit, SynEditHighlighter, SynEditTextBuffer, SynTextDrawer, SynEditTypes,
  SynEditPrintTypes, SynEditPrintHeaderFooter, SynEditPrinterInfo,
  SynEditWordWrap;

type
  TSynEditPrint = class(TComponent)
  private
    fEditor: TCustomSynEdit;
    fCopies: Integer;
    fFooter: TFooter;
    fHeader: THeader;
    fLines: TSynEditStringList;
    fMargins: TSynEditPrintMargins;
    fPageCount: Integer;
    fFont: TFont;
    fTitle: UnicodeString;
    fDocTitle: UnicodeString;
    fPrinterInfo: TSynEditPrinterInfo;
    fCanvas: TCanvas;
    fCharWidth: Integer;
    fMaxLeftChar: Integer;
    fOnPrintLine: TPrintLineEvent;
    fOnPrintStatus: TPrintStatusEvent;
    fYPos: Integer;
    fLineHeight: Integer;
    fExternal: Integer;
    fHighlight: Boolean;
    fColors: Boolean;
    fHighlighter: TSynCustomHighlighter;
    fOldFont: TFont;
    fSynOK: Boolean;
    fLineNumbers: Boolean;
    fLineNumber: Integer;
    fLineOffset: Integer;
    fAbort: Boolean;
    fPrinting: Boolean;
    fDefaultBG: TColor;
    fPageOffset: Integer;
    fMaxWidth: integer;
    fMaxCol: Integer;
    fPagesCounted: Boolean;
    fLineNumbersInMargin: Boolean;
    fTabWidth: integer;
    fFontColor: TColor;                                                         
    fSelectedOnly: Boolean;                                                     
    fSelAvail: Boolean;
    fSelMode: TSynSelectionMode;
    fBlockBegin: TBufferCoord;
    fBlockEnd: TBufferCoord;
    fETODist: PIntegerArray;
    fPreserveBackground: Boolean;

    fRows: PRowLengthArray;
    fRowCount: Integer;
    fPages: array of Integer;
    fLineRows: array of Integer;
    fEnd, fStart: Integer;

    procedure CalcPages;
    procedure WrapLines(const First, Last: Integer);
    procedure PrintPage(Num: Integer);
    procedure WriteLineNumber;
    procedure TextOut(var Value: Integer; bIndent: Boolean = False);
    procedure RestoreCurrentFont;
    procedure SaveCurrentFont;
    procedure SetPixelsPrInch;
    procedure InitPrint;

    function GetPageCount: Integer;
    procedure SetSynEdit(const Value: TCustomSynEdit);
    procedure SetFooter(const Value: TFooter);
    procedure SetHeader(const Value: THeader);
    procedure SetMargins(const Value: TSynEditPrintMargins);
    procedure SetCharWidth(const Value: Integer);
    procedure SetMaxLeftChar(const Value: Integer);
  protected
    property MaxLeftChar: Integer read fMaxLeftChar write SetMaxLeftChar;
    property CharWidth: Integer read fCharWidth write SetCharWidth;
    procedure PrintStatus(Status: TSynPrintStatus; PageNumber: integer;
      var Abort: boolean); virtual;
    procedure PrintLine(LineNumber, PageNumber: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdatePages(ACanvas: TCanvas);
    procedure PrintToCanvas(ACanvas: TCanvas; PageNumber: Integer);
    procedure Print;
    procedure PrintRange(StartPage, EndPage: Integer);

    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);

    property PageCount: Integer read GetPageCount;
    property PrinterInfo: TSynEditPrinterInfo read fPrinterInfo;
    property SynEdit: TCustomSynEdit write SetSynEdit;
  published
    property Copies: integer read fCopies write fCopies;
    property Header: THeader read fHeader write SetHeader;
    property Footer: TFooter read fFooter write SetFooter;
    property Margins: TSynEditPrintMargins read fMargins write SetMargins;
    property Lines: TSynEditStringList read fLines;
    property Font: TFont read fFont;
    property Title: UnicodeString read fTitle write fTitle;
    property DocTitle: UnicodeString read fDocTitle write fDocTitle;
    property Highlight: Boolean read fHighlight write fHighlight default True;
    property PreserveBackground: Boolean read fPreserveBackground
      write fPreserveBackground default True;
    property SelectedOnly: Boolean read FSelectedOnly write FSelectedOnly
      default False;
    property Colors: Boolean read fColors write fColors default False;
    property LineNumbers: Boolean read fLineNumbers write fLineNumbers
      default False;
    property LineOffset: Integer read fLineOffset write fLineOffset default 0;
    property PageOffset: Integer read fPageOffset write fPageOffset default 0;
    property OnPrintLine: TPrintLineEvent read fOnPrintLine write fOnPrintLine;
    property OnPrintStatus: TPrintStatusEvent read fOnPrintStatus
      write fOnPrintStatus;
    property Highlighter: TSynCustomHighlighter read fHighlighter;
    property LineNumbersInMargin: Boolean read fLineNumbersInMargin
      write fLineNumbersInMargin default False;
    property TabWidth: integer read fTabWidth write fTabWidth;
    property Color: TColor read fDefaultBG write fDefaultBG;
  end;

implementation

uses
  Math, Printers,

  { SynEdit }
  SynEditMiscProcs, SynUnicode;

{ TSynEditPrint }

constructor TSynEditPrint.Create(AOwner: TComponent);
begin
  inherited;
  fRowCount := 0;
  fCopies := 1;
  fFooter := TFooter.Create;
  fHeader := THeader.Create;
  fMargins := TSynEditPrintMargins.Create;
  fPrinterInfo := TSynEditPrinterInfo.Create;
  fFont := TFont.Create;
  fOldFont := TFont.Create;
  fMaxLeftChar := 1024;
  fHighlight := True;
  fColors := False;
  fLineNumbers := False;
  fLineOffset := 0;
  fPageOffset := 0;
  fLineNumbersInMargin := False;
  fTabWidth := 8;
  fDefaultBG := clWhite;
end;

destructor TSynEditPrint.Destroy;
begin
  FreeMem(fRows);
  FreeMem(fETODist);

  SetLength(fLineRows, 0);
  SetLength(fPages, 0);

  FreeAndNil(fFooter);
  FreeAndNil(fHeader);
  FreeAndNil(fMargins);
  FreeAndNil(fPrinterInfo);
  FreeAndNil(fFont);
  FreeAndNil(fOldFont);

  inherited;
end;

// -----------------------------------------------------------------------------
// Initialize Font.PixelsPerInch, Character widths, Margins, Total Page count,
// headers and footers
procedure TSynEditPrint.InitPrint;
var
  TmpSize: Integer;
  TmpTextMetrics: TTextMetric;
begin
  fFontColor := fFont.Color;
  fCanvas.Font.Assign(fFont);
  if not fPrinting then
  begin
    SetPixelsPrInch;
    TmpSize := fCanvas.Font.Size;
    fCanvas.Font.PixelsPerInch := fFont.PixelsPerInch;
    fCanvas.Font.Size := TmpSize;
  end;

  { Calculate TextMetrics with the (probably) most wider text styles so text is
    never clipped (although potentially wasting space) }
  fCanvas.Font.Style := [fsBold, fsItalic, fsUnderline, fsStrikeOut];
  GetTextMetrics(fCanvas.Handle, TmpTextMetrics);

  fCharWidth := TmpTextMetrics.tmAveCharWidth;
  fLineHeight := TmpTextMetrics.tmHeight + TmpTextMetrics.tmExternalLeading;

  fCanvas.Font.Style := fFont.Style;
  fMargins.InitPage(fCanvas, 1, fPrinterInfo, fLineNumbers, fLineNumbersInMargin,
    fLines.Count - 1 + fLineOffset);
  fExternal := ((fMargins.PBottom - fMargins.PTop) - fLineHeight *
    ((fMargins.PBottom - fMargins.PTop) div fLineHeight)) shr 1;

  CalcPages;

  fHeader.InitPrint(fCanvas, fPageCount + fPageOffset, fTitle, fMargins);
  fFooter.InitPrint(fCanvas, fPageCount + fPageOffset, fTitle, fMargins);

  fSynOK := fHighlight and Assigned(fHighlighter) and (fLines.Count > 0);
end;

// -----------------------------------------------------------------------------

procedure TSynEditPrint.SetPixelsPrInch;
var
  TmpSize: Integer;
begin
  fHeader.SetPixPrInch(fPrinterInfo.YPixPrInch);
  fFooter.SetPixPrInch(fPrinterInfo.YPixPrInch);

  TmpSize := fFont.Size;
  fFont.PixelsPerInch := fPrinterInfo.YPixPrInch;
  fFont.Size := TmpSize;
end;

// -----------------------------------------------------------------------------
// Wraps editor lines against page boundaries before calculating pages
procedure TSynEditPrint.WrapLines(const First, Last: Integer);
var
  I, J, K, Capacity: Integer;
  Wrapper: TSynWordWrapPlugin;
  TempRows: PRowLengthArray;

  procedure GrowRows(Size: integer);
  const
    vStepSize = 512;
  begin
    Assert(Size > 0);
    if Size > Capacity then
    begin
      Size := Size + vStepSize - (Size mod vStepSize);
      ReallocMem(fRows, Size * SizeOf(TRowLength));
      Capacity := Size;
      SetLength(fLineRows, Capacity);
    end;
  end;

begin
  { Initialize }
  fRowCount := 0;
  if fLines.Count = 0 then
    Exit;

  { Wrap with SynEdit word wrap }
  Wrapper := TSynWordWrapPlugin.Create(fEditor, True);
  try
    Capacity := 0;
    SetLength(fLineRows, Last - First + 1);
    GrowRows(Last - First + 1);
    for I := First to Last do
    begin
      J := 0;
      try
        Wrapper.DoWrapLine(J, TempRows, I - 1, True, fMaxCol);
        GrowRows(fRowCount + J);
        Move(TempRows[0], fRows[fRowCount], J * SizeOf(TRowLength));
        for K := fRowCount to fRowCount + J - 1 do
          fLineRows[K] := I;
        Inc(fRowCount, J);
      finally
        FreeMem(TempRows);
      end;
    end;
  finally
    FreeAndNil(Wrapper);
  end;
end;

// -----------------------------------------------------------------------------
// Calculates the total number of pages
procedure TSynEditPrint.CalcPages;
var
  I, YPos: Integer;
  AStr: UnicodeString;
begin
  { Find maximum text width }
  with fMargins do
    fMaxWidth := PRight - PLeft;

  { Find maximum text chars }
  AStr := EmptyStr;
  fMaxCol := {0}fMaxWidth div fCharWidth;
  {WriteLn(fMaxWidth div fCharWidth);
  while TextWidth(fCanvas, AStr) < fMaxWidth do
  begin
    AStr := AStr + 'W';
    fMaxCol := fMaxCol + 1;
  end;
  Dec(fMaxCol);}

  { Fix maximum text width }
  //AStr := UnicodeString(StringOfChar('W', fMaxCol));
  //fMaxWidth := TextWidth(fCanvas, AStr);

  { Prepare pages }
  fPageCount := 1;
  SetLength(fPages, 32);
  fPages[0] := 0;

  { Wrap lines using SynEdit word wrap }
  YPos := fMargins.PTop + fExternal;
  if fSelectedOnly then
    WrapLines(fBlockBegin.Line, fBlockEnd.Line)
  else
    WrapLines(1, fLines.Count);

  { Count pages }
  for I := 0 to Pred(fRowCount) do
  begin
    Inc(YPos, fLineHeight);
    if YPos > fMargins.PBottom - fExternal then
    begin
      Inc(fPageCount);
      YPos := fMargins.PTop + fLineHeight + fExternal;
      if Length(fPages) < fPageCount then
        SetLength(fPages, fPageCount * 2);
      fPages[Pred(fPageCount)] := I;
    end;
  end;
  SetLength(fPages, fPageCount);

  { Done }
  fPagesCounted := True;
end;

// -----------------------------------------------------------------------------
// Writes the line number. fMargins. PLeft is the position of the left margin
// (which is automatically incremented by the length of the linenumber text, if
// the linenumbers should not be placed in the margin)
procedure TSynEditPrint.WriteLineNumber;
var
  AStr: UnicodeString;
begin
  SaveCurrentFont;
  AStr := IntToStr(fLineNumber + fLineOffset) + ': ';
  with fCanvas do
  begin
    with Font do
    begin
      if fLineNumber mod 10 = 0 then
        Style := [fsBold]
      else
        Style := [];
      Color := clBlack;
    end;
    Brush.Style := bsClear;
    SynUnicode.TextOut(fCanvas, fMargins.PLeft - TextWidth(AStr), fYPos + fExternal, AStr);
    Brush.Style := bsSolid;
  end;
  RestoreCurrentFont;
end;

procedure TSynEditPrint.SaveCurrentFont;
begin
  fOldFont.Assign(fCanvas.Font);
end;

procedure TSynEditPrint.RestoreCurrentFont;
begin
  fCanvas.Font.Assign(fOldFont);
end;

// -----------------------------------------------------------------------------
// Does the actual printing
procedure TSynEditPrint.TextOut(var Value: Integer; bIndent: Boolean = False);
var
  Token: UnicodeString;
  I, StartPos, RealStartPos, TokenPos, TokenLen, OldTokenLen, PaintPos, nIndent: Integer;
  bNeedToFillIndent: Boolean;

  Attr: TSynHighlighterAttributes;
  FillColor: TColor;

  sLine: UnicodeString;
  StrA: PBArray;

  procedure InitETODist;
  var
    I: Integer;
  begin
    ReallocMem(fETODist, Length(Token) * SizeOf(Integer));
    for I := 0 to Length(Token) - 1 do
      fETODist[I] := StrA^[I + PaintPos] * fCharWidth;
  end;

  procedure ClippedTextOut(X, Y: Integer);
  begin
    fCanvas.Brush.Style := bsClear;
    try
      InitETODist;
      UniversalExtTextOut(fCanvas.Handle, X, Y, [], nil, PChar(Token),
        Length(Token), fETODist);
    finally
      fCanvas.Brush.Style := bsSolid;
    end;
  end;

  { Performs last fill to the right margin with correct background color }
  procedure FillToEol;
  begin
    if TokenPos <= fMaxCol then
    begin
      { Get color }
      FillColor := clNone;
      if fSynOK and fColors and fPreserveBackground then
      begin
        Attr := fHighlighter.WhitespaceAttribute;
        if Attr <> nil then
          FillColor := Attr.Background;
      end;
      if FillColor = clNone then
        if fColors then
          FillColor := fDefaultBG
        else
          FillColor := clWhite;

      { Fill }
      with fCanvas do
      begin
        Brush.Color := FillColor;
        FillRect(Rect(fMargins.PLeft + nIndent + TokenPos * fCharWidth,
          fYPos + fExternal, fMargins.PRight, fYPos + fExternal + fLineHeight));
      end;
    end;
  end;

begin
  { Get line of that row }
  sLine := fLines[fLineNumber - 1];
  StrA := fLines.Analyzis[fLineNumber - 1];
  nIndent := 0;

  { Set range for highlighter }
  if fLineNumber = 1 then
    fHighlighter.ResetRange
  else
    fHighlighter.SetRange(fLines.Ranges[fLineNumber - 2]);

  { And line }
  fHighlighter.SetLine(sLine, fLineNumber - 1);

  { Empty? }
  if Length(StrA^) = 0 then
  begin
    TokenPos := 0;
    FillToEol;
    Inc(fYPos, fLineHeight);
    Exit;
  end;

  { Find out start position according to previous rows }
  StartPos := 0;
  RealStartPos := 0;
  if Value > 0 then
  begin
    I := Value;
    while fLineRows[I] = fLineRows[Pred(I)] do
    begin
      Inc(StartPos, fRows[Pred(I)]);
      Dec(I);
    end;
  end;

  { Search for token start on this row if needed }
  TokenPos := 0;
  if StartPos > 0 then
  begin
    OldTokenLen := 0;
    with fHighlighter do
      while not GetEol do
      begin
        Inc(TokenPos, OldTokenLen);
        TokenLen := ExpandedPos(GetTokenPos, GetTokenPos + GetTokenLen, StrA);
        OldTokenLen := TokenLen;
        if TokenPos + TokenLen > StartPos then
          Break;
        Next;
      end;

    { Token wrapped from left? }
    if TokenPos < StartPos then
    begin
      with fHighlighter do
        TokenLen := ExpandedPos(GetTokenPos, GetTokenPos + GetTokenLen, StrA);
      RealStartPos := fHighlighter.GetTokenPos;
      while TokenPos < StartPos do
      begin
        Inc(TokenPos, StrA^[RealStartPos]);
        Dec(TokenLen, StrA^[RealStartPos]);
        Inc(RealStartPos);
      end;
      Inc(RealStartPos);
      Dec(RealStartPos, fHighlighter.GetTokenPos);
    end;
    TokenPos := 0;
  end
  else with fHighlighter do
    TokenLen := ExpandedPos(GetTokenPos, GetTokenPos + GetTokenLen, StrA);

  { Draw this row }
  bNeedToFillIndent := False;
  if bIndent then
  begin
    nIndent := GetLeadingExpandedLength(fEditor.Lines.fList^[Pred(fLineNumber)].fString, fEditor.TabWidth) * fCharWidth;
    bNeedToFillIndent := True;
  end;
  SaveCurrentFont;
  try
    { Loop exit check is done in the end }
    while True do
    begin
      { Get attributes }
      if fSynOK then
        Attr := fHighlighter.GetTokenAttribute
      else
        Attr := nil;
      if Assigned(Attr) then
      begin
        fCanvas.Font.Style := Attr.Style;
        if fColors then
        begin
          { Foreground }
          FillColor := Attr.Foreground;
          if FillColor = clNone then
            FillColor := fFont.Color;
          fCanvas.Font.Color := FillColor;

          { Background }
          FillColor := clNone;
          if fPreserveBackground then
          begin
            FillColor := Attr.Background;
            if FillColor = clNone then
            begin
              Attr := fHighlighter.WhitespaceAttribute;
              if Attr <> nil then
                FillColor := Attr.Background;
              if FillColor = clNone then
                FillColor := fDefaultBG;
            end;
          end
          else begin
            if (Attr <> fHighlighter.WhitespaceAttribute) and
              (Attr.Background <> fHighlighter.WhitespaceAttribute.Background)
            then
              FillColor := Attr.Background;
            if FillColor = clNone then
              FillColor := fDefaultBG;
          end;
          fCanvas.Brush.Color := FillColor;
        end

        { Defaults }
        else with fCanvas do
        begin
          Font.Color := {fFontColor}clBlack;
          Brush.Color := {fDefaultBG}clWhite;
        end;
      end

      { Defaults }
      else with fCanvas do
      begin
        if fColors then
        begin
          Font.Color := fFontColor;
          Brush.Color := fDefaultBG;
        end
        else begin
          Font.Color := clBlack;
          Brush.Color := clWhite;
        end;
      end;

      { Need to fill indent? }
      if bNeedToFillIndent then
      begin
        bNeedToFillIndent := False;
        fCanvas.FillRect(Rect(fMargins.PLeft, fYPos + fExternal,
          fMargins.PLeft + nIndent, fYPos + fExternal + fLineHeight));
      end;

      { Manage font }
      if eoFontForceRoman in fEditor.Options then
        fCanvas.Font.Style := fCanvas.Font.Style - [fsBold];
      if eoFontForceBold in fEditor.Options then
        fCanvas.Font.Style := fCanvas.Font.Style + [fsBold];

      { Fetch token }
      Token := fHighlighter.GetToken;
      PaintPos := fHighlighter.GetTokenPos;

      { Token overlaps right border? }
      if (TokenPos + TokenLen > fRows[Value]) or (TokenPos + TokenLen > fMaxCol) then
      begin
        I := Pred(fHighlighter.GetTokenPos + fHighlighter.GetTokenLen);
        while (TokenPos + TokenLen > fRows[Value]) or (TokenPos + TokenLen > fMaxCol) do
        begin
          Dec(TokenLen, StrA^[I]);
          Dec(I);
        end;
        Inc(I);
        Delete(Token, I - fHighlighter.GetTokenPos + 1, MAXINT);
      end;

      { Token overlaps left border? }
      if RealStartPos > 0 then
      begin
        Delete(Token, 1, RealStartPos - 1);
        Inc(PaintPos, RealStartPos - 1);
        RealStartPos := 0;
      end;

      { Draw background before text }
      fCanvas.FillRect(Rect(fMargins.PLeft + nIndent + TokenPos * fCharWidth, fYPos + fExternal,
        fMargins.PLeft + nIndent + (TokenPos + TokenLen) * fCharWidth, fYPos + fExternal + fLineHeight));

      { Draw }
      ClippedTextOut(fMargins.PLeft + nIndent + TokenPos * fCharWidth, fYPos + fExternal);

      { Proceed to next token }
      fHighlighter.Next;
      Inc(TokenPos, TokenLen);
      with fHighlighter do
        TokenLen := ExpandedPos(GetTokenPos, GetTokenPos + GetTokenLen, StrA);

      { Stop? }
      if fHighlighter.GetEol or (TokenPos >= fRows[Value]) then
      begin
        if fPreserveBackground then
          if TokenPos + TokenLen > fRows[Value] then
            TokenPos := fRows[Value]
          else
            TokenPos := TokenPos + TokenLen;
        Break;
      end;
    end;

    { Fill to EOL With correct background color }
    FillToEOL;

  finally
    RestoreCurrentFont;
    Inc(fYPos, fLineHeight);
  end;
end;

// -----------------------------------------------------------------------------
// Prints a page
procedure TSynEditPrint.PrintPage(Num: Integer);
var
  I, J, iEnd, LastLine: Integer;
  bCanIndent: Boolean;
begin
  PrintStatus(psNewPage, Num, fAbort);
  if not fAbort then
  begin
    fMargins.InitPage(fCanvas, Num, fPrinterInfo, fLineNumbers,
      fLineNumbersInMargin, fLines.Count - 1 + fLineOffset);
    fHeader.Print(fCanvas, Num + fPageOffset);
    if Length(fPages) > 0 then
    begin
      fYPos := fMargins.PTop;
      if Num = fPageCount then
        iEnd := fRowCount - 1
      else
        iEnd := fPages[Num] - 1;
      fStart := fPages[Num - 1];
      fEnd := iEnd;
      I := fStart;
      LastLine := -1;
      while I <= iEnd do
      begin
        fLineNumber := fLineRows[I];
        bCanIndent := False;
        if eoAlignedWrap in fEditor.Options then
        begin
          J := Pred(I);
          if J > -1 then
            if fLineRows[J] = fLineNumber then
              bCanIndent := True;
        end;
        if LastLine <> fLineNumber then
        begin
          if fLineNumbers then
            WriteLineNumber;
          PrintLine(fLineNumber, Num);
          LastLine := fLineNumber;
        end;
        TextOut(I, bCanIndent);
        Inc(I);
      end;
    end;
    fFooter.Print(fCanvas, Num + fPageOffset);
  end;
end;

// -----------------------------------------------------------------------------
// Update pages (called explicitly by preview component)
procedure TSynEditPrint.UpdatePages(ACanvas: TCanvas);
begin
  fCanvas := ACanvas;
  fPrinterInfo.UpdatePrinter;
  InitPrint;
end;

// -----------------------------------------------------------------------------
// Print to specified canvas. Used by preview component
procedure TSynEditPrint.PrintToCanvas(ACanvas: TCanvas; PageNumber: Integer);
begin
  fAbort := False;
  fPrinting := False;
  fCanvas := ACanvas;
  PrintPage(PageNumber);
end;

procedure TSynEditPrint.Print;
begin
  PrintRange(1, -1);
end;

// -----------------------------------------------------------------------------
// Prints the pages in the specified range
procedure TSynEditPrint.PrintRange(StartPage, EndPage: Integer);
var
  I, J: Integer;
begin
  { Check for selection }
  if fSelectedOnly and not fSelAvail then
    Exit;

  { Prepare }
  fPrinting := True;
  fAbort := False;
  if fDocTitle <> '' then
    Printer.Title := fDocTitle
  else
    Printer.Title := fTitle;

  { Start document }
  Printer.BeginDoc;
  PrintStatus(psBegin, StartPage, fAbort);
  UpdatePages(Printer.Canvas);

  { Print all copies }
  for J := 1 to Copies do
  begin
    I := StartPage;
    if EndPage < 0 then
      EndPage := fPageCount;
    while (I <= EndPage) and (not fAbort) do
    begin
      PrintPage(I);
      if ((I < EndPage) or (J<Copies)) and not fAbort then
        Printer.NewPage;
      Inc(I);
    end;
  end;

  { End document }
  if not fAbort then
    PrintStatus(psEnd, EndPage, fAbort);
  Printer.EndDoc;

  { Done }
  fPrinting := False;
end;

// -----------------------------------------------------------------------------
// Fires the OnPrintLine event
procedure TSynEditPrint.PrintLine(LineNumber, PageNumber: Integer);
begin
  if Assigned(fOnPrintLine) then
    fOnPrintLine(Self, LineNumber, PageNumber);
end;

// -----------------------------------------------------------------------------
// Fires the OnPrintStatus event
procedure TSynEditPrint.PrintStatus(Status: TSynPrintStatus;
  PageNumber: integer; var Abort: boolean);
begin
  Abort := False;

  if Assigned(fOnPrintStatus) then
    fOnPrintStatus(Self, Status, PageNumber, Abort);

  if Abort then
    if fPrinting then
      Printer.Abort;
end;

procedure TSynEditPrint.LoadFromStream(AStream: TStream);
var
  Len, BufferSize: Integer;
  Buffer: PWideChar;
begin
  fHeader.LoadFromStream(AStream);
  fFooter.LoadFromStream(AStream);
  fMargins.LoadFromStream(AStream);
  with AStream do
  begin
    Read(Len, sizeof(Len));
    BufferSize := Len * sizeof(WideChar);
    GetMem(Buffer, BufferSize + sizeof(WideChar));
    try
      Read(Buffer^, BufferSize);
      Buffer[BufferSize div sizeof(WideChar)] := #0;
      fTitle := Buffer;
    finally
      FreeMem(Buffer);
    end;
    Read(Len, sizeof(Len));
    BufferSize := Len * sizeof(WideChar);
    GetMem(Buffer, BufferSize + sizeof(WideChar));
    try
      Read(Buffer^, BufferSize);
      Buffer[BufferSize div sizeof(WideChar)] := #0;
      fDocTitle := Buffer;
    finally
      FreeMem(Buffer);
    end;
    Read(fHighlight, SizeOf(fHighlight));
    Read(fColors, SizeOf(fColors));
    Read(fLineNumbers, SizeOf(fLineNumbers));
    Read(fLineOffset, SizeOf(fLineOffset));
    Read(fPageOffset, SizeOf(fPageOffset));
  end;
end;

procedure TSynEditPrint.SaveToStream(AStream: TStream);
var
  aLen: Integer;
begin
  fHeader.SaveToStream(AStream);
  fFooter.SaveToStream(AStream);
  fMargins.SaveToStream(AStream);
  with AStream do
  begin
    aLen := Length(fTitle);
    Write(aLen, SizeOf(aLen));
    Write(PWideChar(fTitle)^, aLen * sizeof(WideChar));
    aLen := Length(fDocTitle);
    Write(aLen, SizeOf(aLen));
    Write(PWideChar(fDocTitle)^, aLen * sizeof(WideChar));
    Write(fHighlight, SizeOf(fHighlight));
    Write(fColors, SizeOf(fColors));
    Write(fLineNumbers, SizeOf(fLineNumbers));
    Write(fLineOffset, SizeOf(fLineOffset));
    Write(fPageOffset, SizeOf(fPageOffset));
  end;
end;

// -----------------------------------------------------------------------------
// Returns total page count. If pages hasn't been counted before,
// then a UpdatePages is called with a temporary canvas
function TSynEditPrint.GetPageCount: Integer;
var
  TmpCanvas: TCanvas;
  DC: HDC;
begin
  Result := 0;
  if fPagesCounted then
    Result := fPageCount
  else begin
    TmpCanvas := TCanvas.Create;
    try
      DC := GetDC(0);
      try
        if DC <> 0 then
        begin
          TmpCanvas.Handle := DC;
          UpdatePages(TmpCanvas);
          TmpCanvas.Handle := 0;
          Result := fPageCount;
          fPagesCounted := True;
        end;
      finally
        ReleaseDC(0, DC);
      end;
    finally
      TmpCanvas.Free;
    end;
  end;
end;

procedure TSynEditPrint.SetSynEdit(const Value: TCustomSynEdit);
begin
  if not Assigned(Value) then
    Exit;
  fEditor := Value;
  fHighLighter := Value.Highlighter;
  fTabWidth := Value.TabWidth;
  fLines := Value.Lines;
  fSelAvail := Value.SelAvail;
  fBlockBegin := Value.BlockBegin;
  fBlockEnd := Value.BlockEnd;
  fSelMode := Value.SelectionMode;
  fFont.Assign(Value.Font);
  fPagesCounted := False;
end;

procedure TSynEditPrint.SetFooter(const Value: TFooter);
begin
  fFooter.Assign(Value);
end;

procedure TSynEditPrint.SetHeader(const Value: THeader);
begin
  fHeader.Assign(Value);
end;

procedure TSynEditPrint.SetMargins(const Value: TSynEditPrintMargins);
begin
  fMargins.Assign(Value);
end;

procedure TSynEditPrint.SetCharWidth(const Value: Integer);
begin
  fCharWidth := Value;
end;

procedure TSynEditPrint.SetMaxLeftChar(const Value: Integer);
begin
  fMaxLeftChar := Value;
end;

end.
