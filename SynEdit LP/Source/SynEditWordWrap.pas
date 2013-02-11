(*

  Letterpress § Garnet

  Provides SynEditBufferPlugin with word wrap support.

  Copyright 2003-2010 initial developers and Garnet

*)

{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is SynEditWordWrap.pas by Flávio Etrusco, released 2003-12-11.
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

$Id: SynEditWordWrap.pas,v 1.8.2.6 2008/09/14 16:24:59 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://synedit.sourceforge.net
-------------------------------------------------------------------------------}

unit SynEditWordWrap;

interface

uses
  SysUtils, Classes,

  { SynEdit }
  SynEdit, SynEditTypes, SynUniHighlighter;

type
  TLineIndex = 0..MaxListSize;
  TRowIndex = 0..MaxListSize;
  TRowLength = Word;

  TRowIndexArray = array [TLineIndex] of TRowIndex;
  PRowIndexArray = ^TRowIndexArray;

  TRowLengthArray = array [TRowIndex] of TRowLength;
  PRowLengthArray = ^TRowLengthArray;

  { For clarity, I'll refer to buffer coordinates as 'Line' and
    "Char" and to display (wrapped) coordinates as 'Row' and 'Column'.

    fLineOffsets[n] is the index of the first row of the [n+1]th line.
    e.g. Starting row of first line (0) is 0. Starting row of second line (1)
    is fLineOffsets[0]. Clear? }
  TSynWordWrapPlugin = class(TInterfacedObject, ISynEditBufferPlugin)
  private
    fLineOffsets: PRowIndexArray;   // How many rows exist up to this line
    fRowLengths: PRowLengthArray;   // How many chars are stored in row
    fHiddenOffsets: PRowIndexArray; // How many rows are folded up to this line
    fLineCapacity: Integer;
    fRowCapacity: Integer;
    fLineCount: Integer;
    fEditor: TCustomSynEdit;
    fMinRowLength: TRowLength;
    fMaxRowLength: TRowLength;
    fSilent: Boolean;
    fExtractHidden: Boolean;        // Quick dirty hack for DisplayToBufferPos()

    procedure GrowLines(AMinSize: Integer);
    procedure MoveLines(AStart: TLineIndex; AMoveBy: Integer);
    procedure GrowRows(AMinSize: Integer);
    procedure MoveRows(AStart: TRowIndex; AMoveBy: Integer);
    procedure SetEmpty;
  protected
    function GetRealRowCount: Integer;
    function ReWrapLine(AIndex: TLineIndex): Integer;
    procedure WrapLines;
    procedure TrimArrays;
    procedure UpdateHiddenOffsets;
  public
    constructor Create(AOwner: TCustomSynEdit; Silent: Boolean = False);
    destructor Destroy; override;

    property LineCount: Integer read fLineCount;

    function BufferToDisplayPos(const APos: TBufferCoord): TDisplayCoord;
    function BufferToRealDisplayPos(const APos: TBufferCoord): TDisplayCoord;
    function DisplayToBufferPos(const aPos: TDisplayCoord): TBufferCoord;
    function RealDisplayToBufferPos(const aPos: TDisplayCoord): TBufferCoord;

    function LineToRow(const ALine: Integer): Integer;
    function LineToRealRow(const ALine: Integer): Integer;
    function RowToLine(const ARow: Integer): Integer;
    function RealRowToLine(const ARow: Integer): Integer;

    function GetRowLength(ARow, ALine: Integer): Integer;
    function GetRowCount(ALine: Integer): Integer; overload;
    function GetRowCount: Integer; overload;

    function LinesInserted(AIndex: Integer; ACount: Integer): Integer;
    function LinesDeleted(AIndex: integer; ACount: Integer): Integer;
    function LinesPutted(AIndex: integer; ACount: Integer): Integer;
    function LinesFolded(AFromLine, AToLine: Integer): Integer;
    function LinesUnFolded(AFromLine, AToLine: Integer): Integer;

    procedure DoWrapLine(var cRow: Integer; var RowLengths: PRowLengthArray;
      Index: TLineIndex; Rewrap: Boolean = False; Boundary: Integer = 0);

    procedure Reset;
    procedure DisplayChanged;
  end;

implementation

uses
  RTLConsts, Math,

  { SynEdit }
  SynUnicode, SynEditMiscProcs;

{ TSynWordWrapPlugin }

// -----------------------------------------------------------------------------

function TSynWordWrapPlugin.BufferToDisplayPos(const APos: TBufferCoord): TDisplayCoord;
begin
  { Get display coords }
  Result := BufferToRealDisplayPos(APos);

  { Extract folded rows from the result }
  if fLineCount >= APos.Line then
    Dec(Result.Row, fHiddenOffsets[APos.Line - 1]);
end;

function TSynWordWrapPlugin.BufferToRealDisplayPos(const APos: TBufferCoord): TDisplayCoord;
var
  vStartRow: integer; // First row of the line
  cRow: integer;      // Row counter
  vRowLen: integer;   // As it says
  nIndent: Integer;   // Amount of visual indent on a given line
begin
  { Checks }
  Assert(APos.Char > 0);
  Assert(APos.Line > 0);

  { Beyond EOF }
  if fLineCount < APos.Line then
  begin
    with Result do
    begin
      Column := APos.Char;
      Row := GetRowCount + APos.Line - fLineCount;
    end;
    Exit;
  end;

  { Initialize indent }
  nIndent := 0;

  { Find starting row and rows to extract from final result }
  if APos.Line = 1 then
    vStartRow := 0
  else
    vStartRow := fLineOffsets[APos.Line - 2];

  { Search for correct column and row pos }
  vRowLen := 0;
  for cRow := vStartRow to fLineOffsets[APos.Line - 1] - 1 do
  begin

    { Increase runner by current row length }
    Inc(vRowLen, fRowLengths[cRow]);

    { Find indent in aligned wrap mode }
    if cRow > vStartRow then
      if (eoAlignedWrap in fEditor.Options) and (nIndent = 0) then
        nIndent := GetLeadingExpandedLength(fEditor.Lines.fList^[APos.Line - 1].fString, fEditor.TabWidth);

    { Found }
    if APos.Char <= vRowLen then
    begin
      with Result do
      begin
        Column := APos.Char - vRowLen + fRowLengths[cRow] + nIndent;
        Row := cRow + 1;
      end;

      { Done }
      Exit;
    end;
  end;

  { Beyond EOL }
  with Result do
  begin
    Column := APos.Char - vRowLen + fRowLengths[fLineOffsets[APos.Line - 1] - 1] + nIndent;
    Row := fLineOffsets[APos.Line - 1];
  end;
end;

// -----------------------------------------------------------------------------

constructor TSynWordWrapPlugin.Create(AOwner: TCustomSynEdit;
  Silent: Boolean = False);
begin
  inherited Create;
  fEditor := AOwner;
  fSilent := Silent;
  fExtractHidden := False;
  if not fSilent then Reset;
end;

// -----------------------------------------------------------------------------

destructor TSynWordWrapPlugin.Destroy;
begin
  FreeMem(fLineOffsets);
  FreeMem(fHiddenOffsets);
  FreeMem(fRowLengths);
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TSynWordWrapPlugin.DisplayChanged;
begin
  if eoWrapAgainstMargin in fEditor.Options then
    if fEditor.RightEdge <> fMaxRowLength then
      Reset
    else
  else
    if (fEditor.CharsInWindow - 1) <> fMaxRowLength then
      Reset;
end;

// -----------------------------------------------------------------------------

function TSynWordWrapPlugin.DisplayToBufferPos(const aPos: TDisplayCoord): TBufferCoord;
begin
  fExtractHidden := True;
  try
    Result := RealDisplayToBufferPos(aPos);
  finally
    fExtractHidden := False;
  end;
end;

function TSynWordWrapPlugin.RealDisplayToBufferPos(const aPos: TDisplayCoord): TBufferCoord;
var
  cLine: Integer;   // Line runner
  cRow: Integer;    // Row runner
  cSkip: Integer;   // Folded rows to take into account
  nIndent: Integer; // Amount of visual indent on a found line
begin
  { Checks }
  Assert(aPos.Column > 0);
  Assert(aPos.Row > 0);

  { Beyond EOF }
  if aPos.Row > GetRealRowCount then
  begin
    with Result do
    begin
      Char := aPos.Column;
      Line := aPos.Row - GetRealRowCount + LineCount;
    end;
    Exit;
  end;

  { Initialize indent }
  nIndent := 0;

  { TODO: use a binary search or something smarter }
  for cLine := fLineCount - 2 downto 0 do
  begin
    { Should extract folded rows? }
    if fExtractHidden then
      cSkip := fHiddenOffsets[cLine + 1]
    else
      cSkip := 0;

    { See if line is found }
    if aPos.Row > fLineOffsets[cLine] - cSkip then
    begin

      { We've found a line }
      Result.Line := cLine + 2;

      { Apply indent if it's not a first row of this line }
      if eoAlignedWrap in fEditor.Options then
        if aPos.Row - (fLineOffsets[cLine] - cSkip) > 1 then
          nIndent := GetLeadingExpandedLength(fEditor.Lines.fList^[Result.Line - 1].fString, fEditor.TabWidth);

      { Last row of line? }
      if aPos.Row = fLineOffsets[cLine + 1] - cSkip then
        Result.Char := Min(aPos.Column - nIndent, fMaxRowLength + 1)
      else
        Result.Char := Min(aPos.Column - nIndent, fRowLengths[aPos.Row + cSkip - 1] + 1);

      for cRow := fLineOffsets[cLine] to aPos.Row + cSkip - 2 do
        Inc(Result.Char, fRowLengths[cRow]);

      { Done }
      Exit;
    end;
  end;

  { First line }
  Result.Line := 1;

  { Apply indent if it's not a first row of this line }
  if eoAlignedWrap in fEditor.Options then
    if aPos.Row > 1 then
      nIndent := GetLeadingExpandedLength(fEditor.Lines.fList^[Result.Line - 1].fString, fEditor.TabWidth);

  { Last row of line? }
  if aPos.Row = fLineOffsets[0] then
    Result.Char := Min(aPos.Column - nIndent, fMaxRowLength + 1)
  else
    Result.Char := Min(aPos.Column - nIndent, fRowLengths[aPos.Row - 1] + 1);

  { Fix }
  for cRow := 0 to aPos.Row - 2 do
    Inc(Result.Char, fRowLengths[cRow]);
end;

// -----------------------------------------------------------------------------
// § Garnet
function TSynWordWrapPlugin.LineToRow(const ALine: Integer): Integer;
begin
  { Get row }
  Result := LineToRealRow(ALine);

  { Extract folded rows }
  if fLineCount >= ALine then
    Dec(Result, fHiddenOffsets[ALine - 1]);
end;

function TSynWordWrapPlugin.LineToRealRow(const ALine: Integer): Integer;
begin
  { Chek }
  Assert(ALine > 0);

  { Beyond EOF? }
  if fLineCount < ALine then
  begin
    Result := GetRowCount + ALine - fLineCount;
    Exit;
  end;

  { First line? }
  if ALine = 1 then
    Result := 1
  else
    Result := fLineOffsets[ALine - 2] + 1;
end;

function TSynWordWrapPlugin.RowToLine(const ARow: Integer): Integer;
begin
  fExtractHidden := True;
  try
    Result := RealRowToLine(ARow);
  finally
    fExtractHidden := False;
  end;
end;

function TSynWordWrapPlugin.RealRowToLine(const ARow: Integer): Integer;
var
  cLine, cSkip: Integer;
begin
  { Chek }
  Assert(ARow > 0);

  { Beyond EOF? }
  if ARow > GetRowCount then
  begin
    Result := ARow - GetRowCount + LineCount;
    Exit;
  end;

  { TODO: use a binary search or something smarter }
  for cLine := LineCount - 2 downto 0 do
  begin
    if fExtractHidden then
      cSkip := fHiddenOffsets[cLine + 1]
    else
      cSkip := 0;
    if ARow > fLineOffsets[cLine] - cSkip then
    begin
      Result := cLine + 2;
      Exit;
    end;
  end;

  { First line }
  Result := 1;
end;

// -----------------------------------------------------------------------------
// § Garnet
function TSynWordWrapPlugin.GetRowLength(ARow, ALine: Integer): Integer;
begin
  if (ARow <= 0) or (ARow > GetRowCount) then
    TList.Error(SListIndexError, ARow);

  Result := fRowLengths[ARow + fHiddenOffsets[ALine - 1] - 1];
end;

// -----------------------------------------------------------------------------
// § Garnet
procedure TSynWordWrapPlugin.GrowLines(AMinSize: Integer);
const
  vStepSize = 256;
begin
  Assert(AMinSize > 0);
  if AMinSize > fLineCapacity then
  begin
    AMinSize := AMinSize + vStepSize - (AMinSize mod vStepSize);
    ReallocMem(fLineOffsets, AMinSize * SizeOf(TRowIndex));
    ReallocMem(fHiddenOffsets, AMinSize * SizeOf(TRowIndex));
    fLineCapacity := AMinSize;
  end;
end;

// -----------------------------------------------------------------------------

procedure TSynWordWrapPlugin.GrowRows(AMinSize: integer);
const
  vStepSize = 512;
begin
  Assert(AMinSize > 0);
  if AMinSize > fRowCapacity then
  begin
    AMinSize := AMinSize + vStepSize - (AMinSize mod vStepSize);
    ReallocMem(fRowLengths, AMinSize * SizeOf(TRowLength));
    fRowCapacity := AMinSize;
  end;
end;

// -----------------------------------------------------------------------------
// § Garnet
function TSynWordWrapPlugin.LinesDeleted(AIndex: Integer;
  ACount: Integer): integer;
var
  vStartRow: Integer;
  vEndRow: Integer;
  cLine: Integer;
begin
  { Checks }
  if fMaxRowLength = 0 then
  begin
    Result := 0;
    Exit;
  end;
  Assert(AIndex >= 0);
  Assert(ACount >= 1);
  Assert(AIndex + ACount <= LineCount);

  { Find starting row }
  if AIndex = 0 then
    vStartRow := 0
  else
    vStartRow := fLineOffsets[AIndex - 1];
  vEndRow := fLineOffsets[AIndex + ACount - 1];

  { How many rows were deleted }
  Result := vEndRow - vStartRow;

  { Resize fRowLengths }
  if vEndRow < GetRealRowCount then
    MoveRows(vEndRow, -Result);

  { Resize fLineOffsets }
  MoveLines(AIndex + ACount, -ACount);
  Dec(fLineCount, ACount);

  { Update offsets }
  for cLine := AIndex to fLineCount - 1 do
    Dec(fLineOffsets[cLine], Result);

  UpdateHiddenOffsets;
end;

// -----------------------------------------------------------------------------
// § Garnet
function TSynWordWrapPlugin.LinesInserted(AIndex: Integer; ACount: Integer): Integer;
var
  vPrevOffset: TRowIndex;
  cLine: integer;
begin
  { Initialize }
  Result := 0;

  { Check }
  if (fMaxRowLength = 0) or (ACount = 0) then
    Exit;

  { Need to do full wrap? }
  if ACount = fEditor.Lines.Count then
  begin
    WrapLines;
    Exit;
  end;

  { Resize fLineOffsets }
  GrowLines(LineCount + ACount);
  if AIndex < LineCount then // No need for MoveLines if inserting at LineCount
                             // (TSynEditStringList.Add)
  begin
    Inc(fLineCount, ACount); // fLineCount must be updated before calling
                             // MoveLines()
    MoveLines(AIndex, ACount);
  end
  else
    Inc(fLineCount, ACount);

  { Set offset to same as previous line (i.e. the line has 0 rows) }
  if AIndex = 0 then
    vPrevOffset := 0
  else
    vPrevOffset := fLineOffsets[AIndex - 1];
  for cLine := AIndex to AIndex + ACount - 1 do
    fLineOffsets[cLine] := vPrevOffset;

  { Rewrap }
  Result := 0;
  for cLine := AIndex to AIndex + ACount - 1 do
    Inc(Result, ReWrapLine(cLine));

  UpdateHiddenOffsets;
end;

// -----------------------------------------------------------------------------

function TSynWordWrapPlugin.LinesPutted(AIndex: Integer;
  ACount: Integer): Integer;
var
  cLine: Integer;
begin
  if fMaxRowLength = 0 then
  begin
    Result := 0;
    Exit;
  end;
  Assert(AIndex >= 0);
  Assert(ACount >= 1);
  Assert(AIndex + ACount <= LineCount);

  { Rewrap }
  Result := 0;
  for cLine := AIndex to AIndex + ACount - 1 do
    Inc(Result, ReWrapLine(cLine));

  UpdateHiddenOffsets; // § Garnet
end;

// -----------------------------------------------------------------------------

function TSynWordWrapPlugin.LinesFolded(AFromLine, AToLine: Integer): Integer;
begin
  Result := 0;
  UpdateHiddenOffsets;
end;

// -----------------------------------------------------------------------------

function TSynWordWrapPlugin.LinesUnfolded(AFromLine, AToLine: Integer): Integer;
begin
  Result := 0;
  UpdateHiddenOffsets;
end;

// -----------------------------------------------------------------------------
// § Garnet
procedure TSynWordWrapPlugin.MoveLines(AStart: TLineIndex; AMoveBy: integer);
var
  vMoveCount: Integer;
begin
  Assert(AMoveBy <> 0);
  Assert(AStart + AMoveBy >= 0);
  Assert(AStart + AMoveBy < LineCount);
  vMoveCount := LineCount - AStart;
  if AMoveBy > 0 then
    Dec(vMoveCount, AMoveBy);
  Move(fLineOffsets[AStart], fLineOffsets[AStart + AMoveBy],
    vMoveCount * SizeOf(TRowIndex));
  Move(fHiddenOffsets[AStart], fHiddenOffsets[AStart + AMoveBy],
    vMoveCount * SizeOf(TRowIndex));
end;

// -----------------------------------------------------------------------------

procedure TSynWordWrapPlugin.MoveRows(AStart: TRowIndex; AMoveBy: Integer);
var
  vMoveCount: Integer;
begin
  Assert(AMoveBy <> 0);
  Assert(AStart + AMoveBy >= 0);
  Assert(AStart + AMoveBy < GetRealRowCount);
  vMoveCount := GetRealRowCount - AStart;
  if AMoveBy > 0 then
    Dec(vMoveCount, AMoveBy);
  Move(fRowLengths[AStart], fRowLengths[AStart + AMoveBy],
    vMoveCount * SizeOf(TRowLength));
end;

// -----------------------------------------------------------------------------

procedure TSynWordWrapPlugin.Reset;
begin
  if (eoWrapAgainstMargin in fEditor.Options) and (fEditor.RightEdge > 0) then
  begin
    fMaxRowLength := fEditor.RightEdge;
    fMinRowLength := fEditor.RightEdge - (fEditor.RightEdge div 3);
  end
  else begin
    fMaxRowLength := (fEditor.CharsInWindow - 1);
    fMinRowLength := (fEditor.CharsInWindow - 1) - (fEditor.CharsInWindow div 3);
  end;
  fMaxRowLength := Max(fMaxRowLength, 48);
  fMinRowLength := Max(fMinRowLength, 48);
  WrapLines;
end;

// -----------------------------------------------------------------------------
// § Garnet
// This routine wraps only certain line and is used when loading file or some
// line changes. Returns RowCount delta (how many wrapped lines were added
// or removed by this change)
function TSynWordWrapPlugin.ReWrapLine(AIndex: TLineIndex): integer;
var
  vLineRowCount: Integer;
  vTempRowLengths: PRowLengthArray;

  vStartRow: Integer;   // First row of the line
  vOldNextRow: Integer; // First row of the next line, before the change

  cLine: Integer;
begin
  try
    { Find new rows on this line }
    vLineRowCount := 0;
    if fEditor.Lines.ExpandedStringLengths[AIndex] = 0 then
    begin
      vLineRowCount := 1;
      vTempRowLengths := AllocMem(vLineRowCount * SizeOf(TRowLength));
    end
    else
      DoWrapLine(vLineRowCount, vTempRowLengths, AIndex, True);

    { Then updates to the main arrays }
    if AIndex = 0 then
      vStartRow := 0
    else
      vStartRow := fLineOffsets[AIndex - 1];
    vOldNextRow := fLineOffsets[AIndex];

    Result := vLineRowCount - (vOldNextRow - vStartRow);
    if Result <> 0 then
    begin
      { MoveRows depends on RowCount, so we need some special processing... }
      if Result > 0 then
      begin
        { If growing, update offsets (and thus RowCount) before rowlengths }
        GrowRows(GetRealRowCount + Result);
        for cLine := AIndex to LineCount - 1 do
          Inc(fLineOffsets[cLine], Result);
        if vOldNextRow < GetRealRowCount - Result then
          MoveRows(vOldNextRow, Result);
      end
      else begin
        { If shrinking, update offsets after rowlengths }
        if vOldNextRow < GetRealRowCount then
          MoveRows(vOldNextRow, Result);
        for cLine := AIndex to LineCount - 1 do
          Inc(fLineOffsets[cLine], Result);
      end;
    end;

    { Put changes in main array }
    Move(vTempRowLengths[0], fRowLengths[vStartRow],
      vLineRowCount * SizeOf(TRowLength));
  finally
    FreeMem(vTempRowLengths);
  end;
end;

// -----------------------------------------------------------------------------
// § Garnet
// This routine is called when wrap boundary has been changed. Does a complete
// rewrap of whole editor buffer
procedure TSynWordWrapPlugin.WrapLines;
var
  cRow, cLine: Integer;
begin
  { Initialzie }
  if (fEditor.Lines.Count = 0) or (fMaxRowLength <= 0) then
  begin
    SetEmpty;
    Exit;
  end;

  { Preallocate enough space }
  GrowLines(fEditor.Lines.Count);
  GrowRows(fEditor.Lines.Count);

  { Get offset for each line }
  cRow := 0;
  for cLine := 0 to fEditor.Lines.Count - 1 do
  begin
    { Empty line? }
    if fEditor.Lines.ExpandedStringLengths[cLine] = 0 then
    begin
      fRowLengths[cRow] := 0;
      Inc(cRow);
    end

    { Get offset for this line }
    else
      DoWrapLine(cRow, fRowLengths, cLine);

    { Finally, update line offsets }
    fLineOffsets[cLine] := cRow;
  end;

  { Finish }
  fLineCount := fEditor.Lines.Count;
  UpdateHiddenOffsets;
end;

// -----------------------------------------------------------------------------
// This is a main routine which does actual wrapping
procedure TSynWordWrapPlugin.DoWrapLine(var cRow: Integer;
  var RowLengths: PRowLengthArray; Index: TLineIndex; Rewrap: Boolean = False;
  Boundary: Integer = 0);
const
  BreakChars = ['-', '\', '/'];
var
  vLine: UnicodeString; StrA: PBArray;
  vMaxNewRows, nTokenPos, nTokenLen, nOldTokenLen, nRealPos, nIndent, I: Integer;
  aLastTokenNature: TTokenNature;
  bFirst, bDoNotSkipNext: Boolean;

  { Returns and end for the row ensuring individual chars aren't broken }
  function DoNotCutWideGlyphsInParts: Integer;
  begin
    { Fallback }
    Result := nTokenPos;

    { fBreakWhitespace disabled? }
    if not fEditor.BreakWhitespace and ({fEditor.Highlighter.GetTokenNature}aLastTokenNature = tknnSpace) then
    begin
      Inc(Result, nTokenLen);
      Exit;
    end;

    { Fix }
    while Result < Boundary do
    begin
      Inc(Result, StrA^[nRealPos]);
      Inc(nRealPos);
    end;
    if Result - Boundary > 0 then
    begin
      Dec(nRealPos);
      Dec(Result, StrA^[nRealPos]);
    end;
  end;

begin
  { Initialize }
  if Boundary = 0 then
    Boundary := fMaxRowLength;
  if fSilent then
    fMinRowLength := Max(Boundary - (Boundary div 3), 48);
  if Boundary = High(TRowLength) then
    Exit;

  { Fetch line }
  vLine := fEditor.Lines.fList^[Index].fString;
  StrA := fEditor.Lines.Analyzis[Index];
  if Index = 0 then
    fEditor.Highlighter.ResetRange
  else
    fEditor.Highlighter.SetRange(fEditor.Lines.Ranges[Index - 1]);
  fEditor.Highlighter.SetLine(vLine, Index);

  { Get indent }
  bFirst := True;
  if eoAlignedWrap in fEditor.Options then
    nIndent := GetLeadingExpandedLength(fEditor.Lines.fList^[Index].fString,
      fEditor.TabWidth)
  else
    nIndent := 0;

  { Called from WrapLines()? }
  vMaxNewRows := ((fEditor.Lines.ExpandedStringLengths[Index] + nIndent - 1) div fMinRowLength) + 1;
  if not Rewrap then
    GrowRows(cRow + vMaxNewRows)

  { Called from RewrapLine() }
  else
    RowLengths := AllocMem(vMaxNewRows * SizeOf(TRowLength));

  { Prepare }
  nTokenPos := 0;
  nTokenLen := 0;
  nOldTokenLen := 0;
  nRealPos := 0;
  aLastTokenNature := tknnUnknown;
  bDoNotSkipNext := False;

  { Loop through tokenized line }
  while not fEditor.Highlighter.GetEol do
  begin

    { Do not leave word break chars alone on rows, wrap them with indent }
    if ((fEditor.Highlighter.GetTokenNature in [tknnIdent, tknnNumber, tknnSeparator, tknnNbsp]) and
      (aLastTokenNature in [tknnIdent, tknnNumber, tknnSeparator, tknnNbsp]) and
      (not (vLine[Succ(nRealPos)] in BreakChars))) then
    begin
      bDoNotSkipNext := True;
      while not fEditor.Highlighter.GetEol and
        (fEditor.Highlighter.GetTokenNature in [tknnIdent, tknnNumber, tknnSeparator, tknnNbsp]) do
      begin
        Inc(nTokenLen, ExpandedPos(fEditor.Highlighter.GetTokenPos,
          fEditor.Highlighter.GetTokenPos + fEditor.Highlighter.GetTokenLen, StrA));

        { Dash breaks in such conditions }
        if fEditor.Highlighter.GetToken[1] in BreakChars then begin fEditor.Highlighter.Next; aLastTokenNature := tknnUnknown; Break; end;
        fEditor.Highlighter.Next;
      end;
    end

    { Step on this token }
    else begin
      Inc(nTokenPos, nOldTokenLen);
      nTokenLen := ExpandedPos(fEditor.Highlighter.GetTokenPos,
        fEditor.Highlighter.GetTokenPos + fEditor.Highlighter.GetTokenLen, StrA);
      nRealPos := fEditor.Highlighter.GetTokenPos;
      aLastTokenNature := fEditor.Highlighter.GetTokenNature;
    end;

    nOldTokenLen := nTokenLen;
    if nTokenPos + nTokenLen > Boundary then
    begin
      if (nTokenPos = 0) or ({fEditor.Highlighter.GetTokenNature}aLastTokenNature in [tknnSpace{, tknnSeparator}]) then
      begin
        //if fEditor.Highlighter.GetTokenNature = tknnSpace then//-
        //  nTokenLen := nTokenPos + nTokenLen;//-

        nOldTokenLen := nTokenPos + nTokenLen;
        while nOldTokenLen > Boundary do
        begin
          RowLengths[cRow] := DoNotCutWideGlyphsInParts;
          Dec(nOldTokenLen, RowLengths[cRow]);
          Inc(cRow);
          if bFirst then
          begin
            Dec(Boundary, nIndent);
            bFirst := False;
          end;
        end;//+

        {while nTokenPos + nTokenLen > Boundary do
        begin
          RowLengths[cRow] := DoNotCutWideGlyphsInParts;
          Dec(nTokenLen, RowLengths[cRow]);
          Inc(cRow);
          if bFirst then
          begin
            Dec(Boundary, nIndent);
            bFirst := False;
          end;
        end;-}

        nTokenPos := 0;
      end
      else begin
        RowLengths[cRow] := nTokenPos;
        Inc(cRow);
        if bFirst then
        begin
          Dec(Boundary, nIndent);
          bFirst := False;
        end;
        nTokenPos := 0;
        while nTokenLen > Boundary do
        begin
          RowLengths[cRow] := DoNotCutWideGlyphsInParts;
          Dec(nTokenLen, RowLengths[cRow]);
          Inc(cRow);
        end;
        nOldTokenLen := nTokenLen;//+
        nTokenPos := 0;
      end;
      //nOldTokenLen := nTokenLen;//-
    end;
    if fEditor.Highlighter.GetEol then Break;
    if not bDoNotSkipNext then fEditor.Highlighter.Next;
    bDoNotSkipNext := False;
  end;

  { Done }
  RowLengths[cRow] := nTokenPos + nTokenLen;
  Inc(cRow);
end;

//------------------------------------------------------------------------------

function TSynWordWrapPlugin.GetRowCount(ALine: Integer): Integer;
begin
  if fLineCount > 0 then
    Result := fLineOffsets[ALine-1] - fLineOffsets[ALine-2]
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------
// § Garnet
function TSynWordWrapPlugin.GetRowCount: Integer;
begin
  if fLineCount > 0 then
    Result := fLineOffsets[fLineCount - 1] - fHiddenOffsets[fLineCount - 1]
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------
// § Garnet
function TSynWordWrapPlugin.GetRealRowCount: Integer;
begin
  if fLineCount > 0 then
    Result := fLineOffsets[fLineCount - 1]
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

procedure TSynWordWrapPlugin.SetEmpty;
begin
  fLineCount := 0;
  TrimArrays;
end;

// -----------------------------------------------------------------------------
// § Garnet
procedure TSynWordWrapPlugin.TrimArrays;
begin
  fLineCapacity := fLineCount;
  fRowCapacity := GetRealRowCount;
  ReallocMem(fLineOffsets, fLineCount * SizeOf(TRowIndex));
  ReallocMem(fHiddenOffsets, fLineCount * SizeOf(TRowIndex));
  ReallocMem(fRowLengths, GetRealRowCount*SizeOf(TRowLength));
end;

// -----------------------------------------------------------------------------
// § Garnet
// That's the best way I've found to update information about hidden rows.
// One can think that it's wiser to update fHiddenRows[] each time a range gets
// collapsed/expanded. But that's just an illusion. First, each collapse/expand
// will result in big update cycles up to the end of file. Also, you would have
// to take care about subranges: take in account their state. Then, you would
// need to update hidden offsets on rewrap and that is a total headache.
// That tiny procedure simply looks only at top-level collapsed ranges and
// updates hidden offsets in one pass
procedure TSynWordWrapPlugin.UpdateHiddenOffsets;
var
  I, J, vStartLine, vHidden: Integer;
begin
  { Initialize }
  vHidden := 0;
  vStartLine := 1;

  { Loop }
  for I := 0 to fEditor.AllFoldRanges.AllCount - 1 do
    with fEditor.AllFoldRanges[I] do
    begin
      { We need only top-level collapsed ranges }
      if ParentCollapsed then
        Continue;

      { Update hidden offsets up to found range start.
        We don't need to update lines in range }
      for J := vStartLine to FromLine do
        fHiddenOffsets[J - 1] := vHidden;
      for J := FromLine + 1 to ToLine - 1 do
        fHiddenOffsets[J - 1] := 0;

      { Find rows to skip }
      Inc(vHidden, fLineOffsets[ToLine - 2] - fLineOffsets[FromLine - 1]);

      { Step over }
      vStartLine := ToLine;
    end;

  { Last update }
  for I := vStartLine to fLineCount do
    fHiddenOffsets[I - 1] := vHidden;
end;

end.
