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
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
//todo: Use a single implementation of ReWrapLines that takes starting line and number of lines to rewrap
//todo: Tweak code to try finding better wrapping points. Some support by the highlighters will be needed, probably.
//todo: Document the code
//todo: The length of the last Row of a Line could be calculated from the Line length instead of being stored. This would be only useful when most of the lines aren't wrapped.

{$IFNDEF QSYNEDITWORDWRAP}
unit SynEditWordWrap;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QSynEditTypes,
  QSynEditTextBuffer,
  QSynEdit,
{$ELSE}
  SynEditTypes,
  SynEditTextBuffer,
  SynEdit,
{$ENDIF}
  SysUtils,
  Classes;

var
  // Accumulate/hide whitespace at EOL (at end of wrapped rows, actually)
  OldWhitespaceBehaviour: Boolean = False;

const
  MaxIndex = MaxInt div 16;

type
  TLineIndex = 0..MaxIndex;
  TRowIndex = 0..MaxIndex;
  TRowLength = word;

  TRowIndexArray = array [TLineIndex] of TRowIndex;
  PRowIndexArray = ^TRowIndexArray;

  TRowLengthArray = array [TRowIndex] of TRowLength;
  PRowLengthArray = ^TRowLengthArray;

  {$IFNDEF SYN_COMPILER_4_UP}
  TSysCharSet = set of Char;
  {$ENDIF}

  // For clarity, I'll refer to buffer coordinates as 'Line' and
  // 'Char' and to display (wrapped) coordinates as 'Row' and 'Column'.

  // fLineOffsets[n] is the index of the first row of the [n+1]th line.
  // e.g. Starting row of first line (0) is 0. Starting row of second line (1)
  // is fLineOffsets[0]. Clear?

  TSynWordWrapPlugin = class(TInterfacedObject, ISynEditBufferPlugin)
  private
    fLineOffsets: PRowIndexArray;   // How many rows exist up to this line
    fRowLengths: PRowLengthArray;   // How many chars are stored in row
    fHiddenOffsets: PRowIndexArray; // How many rows are folded up to this line
    fLineCapacity: integer;
    fRowCapacity: integer;
    fLineCount: integer;

    fEditor: TCustomSynEdit;
    fMinRowLength: TRowLength;
    fMaxRowLength: TRowLength;

    fExtractHidden: Boolean;        // Quick dirty hack for DisplayToBufferPos()

    procedure GrowLines(aMinSize: integer);
    procedure MoveLines(aStart: TLineIndex; aMoveBy: integer);
    procedure GrowRows(aMinSize: integer);
    procedure MoveRows(aStart: TRowIndex; aMoveBy: integer);
    procedure SetEmpty;
  protected
    procedure WrapLines;
    function ReWrapLine(aIndex: TLineIndex): integer;
    function GetRealRowCount: Integer;
    procedure TrimArrays;
    property LineOffsets: PRowIndexArray read fLineOffsets;
    property RowLengths: PRowLengthArray read fRowLengths;
    property Editor: TCustomSynEdit read fEditor;
    procedure UpdateHiddenOffsets;
  public
    constructor Create(aOwner: TCustomSynEdit);
    destructor Destroy; override;
    property LineCount: integer read fLineCount;
    { ISynEditBufferPlugin }
    function BufferToDisplayPos(const aPos: TBufferCoord): TDisplayCoord;
    function BufferToRealDisplayPos(const APos: TBufferCoord): TDisplayCoord;
    function DisplayToBufferPos(const aPos: TDisplayCoord): TBufferCoord;
    function RealDisplayToBufferPos(const aPos: TDisplayCoord): TBufferCoord;

    function LineToRow(const aLine: Integer): Integer;
    function LineToRealRow(const aLine: Integer): Integer;
    function RowToLine(const aRow: Integer): Integer;
    function RealRowToLine(const aRow: Integer): Integer;

    function GetRowLength(aRow, aLine: Integer): Integer;
    function GetRowCount(aLine: Integer): Integer; overload;
    function GetRowCount: Integer; overload;

    function LinesInserted(aIndex: integer; aCount: integer): integer;
    function LinesDeleted(aIndex: integer; aCount: integer): integer;
    function LinesPutted(aIndex: integer; aCount: integer): integer;
    function LinesFolded(aFromLine, aToLine: Integer): Integer;
    function LinesUnFolded(aFromLine, aToLine: Integer): Integer;
    procedure Reset;
    procedure DisplayChanged; 
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynUnicode,
{$ELSE}
  SynUnicode,
{$ENDIF}
{$IFDEF SYN_COMPILER_6_UP}
  RTLConsts,
{$ELSE}
  {$IFDEF SYN_CLX}
    QConsts,
  {$ELSE}
    Consts,
  {$ENDIF}
{$ENDIF}
  SynEditMiscProcs,
  Math;

{ TSynWordWrapPlugin }

function TSynWordWrapPlugin.BufferToDisplayPos(
  const aPos: TBufferCoord): TDisplayCoord;
begin
  { Get display coords }
  Result := BufferToRealDisplayPos(APos);

  { Extract folded rows from the result }
  if fLineCount >= APos.Line then
    Dec(Result.Row, fHiddenOffsets[APos.Line - 1]);
end;

function TSynWordWrapPlugin.BufferToRealDisplayPos(
  const APos: TBufferCoord): TDisplayCoord;
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
      if (eoAlignedWrap in FEditor.Options) and (nIndent = 0) then
        nIndent := GetLeadingExpandedLength(fEditor.Lines.List^[APos.Line - 1].fString, fEditor.TabWidth);

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

constructor TSynWordWrapPlugin.Create(aOwner: TCustomSynEdit);
begin
  inherited Create; // just to work as reminder in case I revert it to a TComponent...
  if aOwner = nil then
    raise Exception.Create( 'Owner of TSynWordWrapPlugin must be a TCustomSynEdit' );
  fEditor := aOwner;
  Reset;
end;

destructor TSynWordWrapPlugin.Destroy;
begin
  inherited;
  FreeMem(fLineOffsets);
  FreeMem(fRowLengths);
  FreeMem(fHiddenOffsets);
end;

procedure TSynWordWrapPlugin.DisplayChanged;
begin
  if Editor.CharsInWindow <> fMaxRowLength then
    Reset;
end;

function TSynWordWrapPlugin.DisplayToBufferPos(
  const aPos: TDisplayCoord): TBufferCoord;
begin
  fExtractHidden := True;
  try
    Result := RealDisplayToBufferPos(aPos);
  finally
    fExtractHidden := False;
  end;
end;

function TSynWordWrapPlugin.GetRowCount(aLine: Integer): Integer;
begin
  if fLineCount > 0 then
    Result := fLineOffsets[aLine-1] - fLineOffsets[aLine-2]
  else
    Result := 0;
end;

function TSynWordWrapPlugin.GetRealRowCount: Integer;
begin
  if fLineCount > 0 then
    Result := fLineOffsets[fLineCount - 1]
  else
    Result := 0;
end;

function TSynWordWrapPlugin.GetRowCount: Integer;
begin
  if fLineCount > 0 then
    Result := fLineOffsets[fLineCount - 1] - fHiddenOffsets[fLineCount - 1]
  else
    Result := 0;
end;

function TSynWordWrapPlugin.GetRowLength(ARow, ALine: Integer): Integer;
begin
  if (ARow <= 0) or (ARow > GetRowCount) then
    TList.Error(SListIndexError, ARow);

  Result := fRowLengths[ARow + fHiddenOffsets[ALine - 1] - 1];
end;

procedure TSynWordWrapPlugin.GrowLines(aMinSize: integer);
const
  vStepSize = 256;
begin
  Assert(aMinSize > 0);
  if aMinSize > fLineCapacity then
  begin
    aMinSize := aMinSize + vStepSize - (aMinSize mod vStepSize);
    ReallocMem(fLineOffsets, aMinSize * SizeOf(TRowIndex));
    ReallocMem(fHiddenOffsets, aMinSize * SizeOf(TRowIndex));
    fLineCapacity := aMinSize;
  end;
end;

procedure TSynWordWrapPlugin.GrowRows(aMinSize: integer);
const
  vStepSize = 512;
begin
  Assert(aMinSize > 0);
  if aMinSize > fRowCapacity then
  begin
    aMinSize := aMinSize + vStepSize - (aMinSize mod vStepSize);
    ReallocMem(fRowLengths, aMinSize * SizeOf(TRowLength));
    fRowCapacity := aMinSize;
  end;
end;

function TSynWordWrapPlugin.LinesDeleted(aIndex: integer; aCount: integer): integer;
var
  vStartRow: integer;
  vEndRow: integer;
  cLine: integer;
begin
  if fMaxRowLength = 0 then
  begin
    Result := 0;
    Exit;
  end;
  Assert(aIndex >= 0);
  Assert(aCount >= 1);
  Assert(aIndex + aCount <= LineCount);

  if aIndex = 0 then
    vStartRow := 0
  else
    vStartRow := fLineOffsets[aIndex - 1];
  vEndRow := fLineOffsets[aIndex + aCount - 1];
  Result := vEndRow - vStartRow;
  // resize fRowLengths
  if vEndRow < GetRealRowCount then
    MoveRows(vEndRow, -Result);
  // resize fLineOffsets
  MoveLines(aIndex + aCount, -aCount);
  Dec(fLineCount, aCount);
  // update offsets
  for cLine := aIndex to LineCount - 1 do
    Dec(fLineOffsets[cLine], Result);

  UpdateHiddenOffsets;
end;

function TSynWordWrapPlugin.LinesFolded(aFromLine, aToLine: Integer): Integer;
begin
  Result := 0;
  UpdateHiddenOffsets;
end;

function TSynWordWrapPlugin.LinesInserted(aIndex: integer; aCount: integer): integer;
var
  vPrevOffset: TRowIndex;
  cLine: integer;
begin
  if fMaxRowLength = 0 then
  begin
    Result := 0;
    Exit;
  end;
  Assert(aIndex >= 0);
  Assert(aCount >= 1);
  Assert(aIndex <= LineCount);
  // resize fLineOffsets
  GrowLines(LineCount + aCount);
  if aIndex < LineCount then // no need for MoveLines if inserting at LineCount (TSynEditStringList.Add)
  begin
    Inc(fLineCount, aCount); // fLineCount must be updated before calling MoveLines()
    MoveLines(aIndex, aCount);
  end
  else
    Inc(fLineCount, aCount); 
  // set offset to same as previous line (i.e. the line has 0 rows)
  if aIndex = 0 then
    vPrevOffset := 0
  else
    vPrevOffset := fLineOffsets[aIndex - 1];
  for cLine := aIndex to aIndex + aCount - 1 do
    fLineOffsets[cLine] := vPrevOffset;
  // Rewrap
  Result := 0;
  for cLine := aIndex to aIndex + aCount - 1 do
    Inc(Result, ReWrapLine(cLine));

  UpdateHiddenOffsets;
end;

function TSynWordWrapPlugin.LinesPutted(aIndex: integer; aCount: integer): integer;
var
  cLine: integer;
begin
  if fMaxRowLength = 0 then
  begin
    Result := 0;
    Exit;
  end;
  Assert(aIndex >= 0);
  Assert(aCount >= 1);
  Assert(aIndex + aCount <= LineCount);
  // Rewrap
  Result := 0;
  for cLine := aIndex to aIndex + aCount - 1 do
    Inc(Result, ReWrapLine(cLine));

  UpdateHiddenOffsets;
end;

function TSynWordWrapPlugin.LinesUnFolded(aFromLine, aToLine: Integer): Integer;
begin
  Result := 0;
  UpdateHiddenOffsets;
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

function TSynWordWrapPlugin.LineToRow(const aLine: Integer): Integer;
begin
  { Get row }
  Result := LineToRealRow(aLine);

  { Extract folded rows }
  if fLineCount >= aLine then
    Dec(Result, fHiddenOffsets[aLine - 1]);
end;

procedure TSynWordWrapPlugin.MoveLines(aStart: TLineIndex; aMoveBy: integer);
var
  vMoveCount: integer;
begin
  Assert(aMoveBy <> 0);
  Assert(aStart + aMoveBy >= 0);
  Assert(aStart + aMoveBy < LineCount);
  vMoveCount := LineCount - aStart;
  if aMoveBy > 0 then
    Dec(vMoveCount, aMoveBy);
  Move(fLineOffsets[aStart], fLineOffsets[aStart + aMoveBy],
    vMoveCount * SizeOf(TRowIndex));
  Move(fHiddenOffsets[aStart], fHiddenOffsets[aStart + aMoveBy],
    vMoveCount * SizeOf(TRowIndex));
end;

procedure TSynWordWrapPlugin.MoveRows(aStart: TRowIndex; aMoveBy: integer);
var
  vMoveCount: integer;
begin
  Assert(aMoveBy <> 0);
  Assert(aStart + aMoveBy >= 0);
  Assert(aStart + aMoveBy < GetRealRowCount);
  vMoveCount := GetRealRowCount - aStart;
  if aMoveBy > 0 then
    Dec(vMoveCount, aMoveBy);
  Move(fRowLengths[aStart], fRowLengths[aStart + aMoveBy],
    vMoveCount * SizeOf(TRowLength));
end;

function TSynWordWrapPlugin.RealDisplayToBufferPos(
  const aPos: TDisplayCoord): TBufferCoord;
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
          nIndent := GetLeadingExpandedLength(fEditor.Lines.List^[Result.Line - 1].fString, fEditor.TabWidth);

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
      nIndent := GetLeadingExpandedLength(fEditor.Lines.List^[Result.Line - 1].fString, fEditor.TabWidth);

  { Last row of line? }
  if aPos.Row = fLineOffsets[0] then
    Result.Char := Min(aPos.Column - nIndent, fMaxRowLength + 1)
  else
    Result.Char := Min(aPos.Column - nIndent, fRowLengths[aPos.Row - 1] + 1);

  { Fix }
  for cRow := 0 to aPos.Row - 2 do
    Inc(Result.Char, fRowLengths[cRow]);
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

procedure TSynWordWrapPlugin.Reset;
begin
  Assert(Editor.CharsInWindow >= 0);

  fMaxRowLength := Editor.CharsInWindow;
  fMinRowLength := Editor.CharsInWindow - (Editor.CharsInWindow div 3);

  if fMinRowLength <= 0 then
    fMinRowLength := 1;

  WrapLines;
end;

function TSynWordWrapPlugin.ReWrapLine(aIndex: TLineIndex): integer;
// Returns RowCount delta (how many wrapped lines were added or removed by this change).
var
  vMaxNewRows: Cardinal;
  vLine: UnicodeString;
  vLineRowCount: Integer; //numbers of rows parsed in this line
  vTempRowLengths: PRowLengthArray;
  vRowBegin: PWideChar;
  vLineEnd: PWideChar;
  vRowEnd: PWideChar;
  vRunner: PWideChar;
  vRowMinEnd: PWideChar;
  vLastVisibleChar: PWideChar;

  vStartRow: Integer; // first row of the line
  vOldNextRow: Integer; // first row of the next line, before the change
  cLine: Integer;

  p : PRowIndexArray;
begin
  // ****** First parse the new string using an auxiliar array *****
  vLine := TSynEditStringList(Editor.Lines).ExpandedStrings[aIndex];
  vLine := Editor.ExpandAtWideGlyphs(vLine);
  // Pre-allocate a buffer for rowlengths
  vMaxNewRows := ((Length(vLine) - 1) div fMinRowLength) + 1;
  vTempRowLengths := AllocMem(vMaxNewRows * SizeOf(TRowLength));
  try
    vLineRowCount := 0;
    vRowBegin := PWideChar(vLine);
    vRowEnd := vRowBegin + fMaxRowLength;
    vLineEnd := vRowBegin + Length(vLine);
    while vRowEnd < vLineEnd do
    begin
      if OldWhitespaceBehaviour and CharInSet(vRowEnd^, [#32, #9]) then
      begin
        repeat
          Inc(vRowEnd);
        until not CharInSet(vRowEnd^, [#32, #9]);
      end
      else
      begin
        vRowMinEnd := vRowBegin + fMinRowLength;
        vRunner := vRowEnd;
        while vRunner > vRowMinEnd do
        begin
          if Editor.IsWordBreakChar(vRunner^) then
          begin
            vRowEnd := vRunner;
            break;
          end;
          Dec(vRunner);
        end;
      end;
      // Check TRowLength overflow
      if OldWhitespaceBehaviour and (vRowEnd - vRowBegin > High(TRowLength)) then
      begin
        vRowEnd := vRowBegin + High(TRowLength);
        vRowMinEnd := vRowEnd - (High(TRowLength) mod Editor.TabWidth);
        while (vRowEnd^ = #9) and (vRowEnd > vRowMinEnd) do
          Dec(vRowEnd);
      end;

      // do not cut wide glyphs in half
      if vRowEnd > vRowBegin then
      begin
        vLastVisibleChar := vRowEnd - 1;
        while (vLastVisibleChar^ = FillerChar) and (vLastVisibleChar > vRowBegin) do
          dec(vLastVisibleChar);
        vRowEnd := vLastVisibleChar + 1;
      end;

      // Finally store the rowlength
      vTempRowLengths[vLineRowCount] := vRowEnd - vRowBegin;

      Inc(vLineRowCount);
      vRowBegin := vRowEnd;
      Inc(vRowEnd, fMaxRowLength);
    end; //endwhile vRowEnd < vLineEnd
    if (vLineEnd > vRowBegin) or (Length(vLine) = 0) then
    begin
      vTempRowLengths[vLineRowCount] := vLineEnd - vRowBegin;
      Inc(vLineRowCount);
    end;

    // ****** Then updates the main arrays ******
    if aIndex = 0 then
      vStartRow := 0
    else
      vStartRow := fLineOffsets[aIndex - 1];
    vOldNextRow := fLineOffsets[aIndex];
    Result := vLineRowCount - (vOldNextRow - vStartRow);
    if Result <> 0 then
    begin
      // MoveRows depends on RowCount, so we need some special processing...
      if Result > 0 then
      begin
        // ...if growing, update offsets (and thus RowCount) before rowlengths
        GrowRows(GetRealRowCount + Result);
        if Result = 1 then begin
          // EG: this makes Schlemiel run twice as fast, but doesn't solve
          // the algorithmic issue if someone can spend some time looking
          // at the big picture... there are huge speedups to be made by
          // eliminating this loop
          p:=fLineOffsets;
          for cLine := aIndex to LineCount - 1 do
             Inc(p[cLine])
        end else begin
          p:=fLineOffsets;
          for cLine := aIndex to LineCount - 1 do
            Inc(p[cLine], Result);
        end;
        if vOldNextRow < GetRealRowCount - Result then
          MoveRows(vOldNextRow, Result);
      end
      else
      begin
        // ...if shrinking, update offsets after rowlengths
        if vOldNextRow < GetRealRowCount then
          MoveRows(vOldNextRow, Result);
        for cLine := aIndex to LineCount - 1 do
          Inc(fLineOffsets[cLine], Result);
      end;
    end;
    Move(vTempRowLengths[0], fRowLengths[vStartRow], vLineRowCount * SizeOf(TRowLength));
  finally
    FreeMem(vTempRowLengths);
  end;
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

procedure TSynWordWrapPlugin.WrapLines;
var
  cRow: Integer;
  cLine: Integer;
  vLine: UnicodeString;
  vMaxNewRows: Integer;
  vRowBegin: PWideChar;
  vLineEnd: PWideChar;
  vRowEnd: PWideChar;
  vRunner: PWideChar;
  vRowMinEnd: PWideChar;
  vLastVisibleChar: PWideChar;
begin
  if (Editor.Lines.Count = 0) or (fMaxRowLength <= 0) then
  begin
    SetEmpty;
    Exit;
  end;

  GrowLines(Editor.Lines.Count);
  GrowRows(Editor.Lines.Count);

  cRow := 0;
  for cLine := 0 to Editor.Lines.Count - 1 do
  begin
    vLine := TSynEditStringList(Editor.Lines).ExpandedStrings[cLine];
    vLine := Editor.ExpandAtWideGlyphs(vLine);

    vMaxNewRows := ((Length(vLine) - 1) div fMinRowLength) + 1;
    GrowRows(cRow + vMaxNewRows);

    vRowBegin := PWideChar(vLine);
    vRowEnd := vRowBegin + fMaxRowLength;
    vLineEnd := vRowBegin + Length(vLine);
    while vRowEnd < vLineEnd do
    begin
      if OldWhitespaceBehaviour and CharInSet(vRowEnd^, [#32, #9]) then
      begin
        repeat
          Inc(vRowEnd);
        until not CharInSet(vRowEnd^, [#32, #9]);
      end
      else
      begin
        vRowMinEnd := vRowBegin + fMinRowLength;
        vRunner := vRowEnd;
        while vRunner > vRowMinEnd do
        begin
          if Editor.IsWordBreakChar(vRunner^) then
          begin
            vRowEnd := vRunner;
            break;
          end;
          Dec(vRunner);
        end;
      end;

      if OldWhitespaceBehaviour and (vRowEnd - vRowBegin > High(TRowLength)) then
      begin
        vRowEnd := vRowBegin + High(TRowLength);
        vRowMinEnd := vRowEnd - (High(TRowLength) mod Editor.TabWidth);
        while (vRowEnd^ = #9) and (vRowEnd > vRowMinEnd) do
          Dec(vRowEnd);
      end;

      // do not cut wide glyphs in half
      if vRowEnd > vRowBegin then
      begin
        vLastVisibleChar := vRowEnd - 1;
        while (vLastVisibleChar^ = FillerChar) and (vLastVisibleChar > vRowBegin) do
          dec(vLastVisibleChar);
        vRowEnd := vLastVisibleChar + 1;
      end;

      fRowLengths[cRow] := vRowEnd - vRowBegin;

      Inc(cRow);
      vRowBegin := vRowEnd;
      Inc(vRowEnd, fMaxRowLength);
    end;
    if (vLineEnd > vRowBegin) or (Length(vLine) = 0) then
    begin
      fRowLengths[cRow] := vLineEnd - vRowBegin;
      Inc(cRow);
    end;
    fLineOffsets[cLine] := cRow;
  end;
  fLineCount := Editor.Lines.Count;

  UpdateHiddenOffsets;
end;

procedure TSynWordWrapPlugin.SetEmpty;
begin
  fLineCount := 0;
  // free unsused memory
  TrimArrays;
end;

procedure TSynWordWrapPlugin.TrimArrays;
begin
  ReallocMem(fLineOffsets, LineCount * SizeOf(TRowIndex));
  ReallocMem(fHiddenOffsets, fLineCount * SizeOf(TRowIndex));
  fLineCapacity := LineCount;
  ReallocMem(fRowLengths, GetRealRowCount * SizeOf(TRowLength));
  fRowCapacity := GetRealRowCount;
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
