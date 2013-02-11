(*

  Letterpress

  Copyright 2010, Garnet

*)

{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditMiscProcs.pas, released 2000-04-07.
The Original Code is based on the mwSupportProcs.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Hieke.
Unicode translation by Maлl Hцrz.
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

$Id: SynEditMiscProcs.pas,v 1.35.2.7 2008/09/14 16:24:58 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://synedit.sourceforge.net
-------------------------------------------------------------------------------}

unit SynEditMiscProcs;

{$I SynEdit.inc}

interface

uses
  Windows, Graphics, Math, Classes,

  { SynEdit }
  SynEditTypes, SynEditHighlighter, SynUnicode;

type
  PIntArray = ^TIntArray;
  TIntArray = array[0..MaxListSize - 1] of Integer;

  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array of Integer;

  PBArray = ^TBArray;
  TBArray = array of Byte;

function ExpandedPos(const Start, Pos: Integer; const Analyzis: PBArray): Integer;
function RealPos(Start, Pos: Integer; const Analyzis: PBArray): Integer;
function CountUTF8Chars(const S: UTF8String; Border: Integer;
  Start: Integer = 1): Integer;
function CountUnicodeChars(const S: UTF8String; Border: Integer;
  Start: Integer = 1): Integer;

procedure SwapInt(var I1, I2: Integer); register;

function MinMax(x, Mi, Ma: Integer): Integer;
function MaxPoint(const P1, P2: TPoint): TPoint;
function MinPoint(const P1, P2: TPoint): TPoint;

function GetIntArray(Count: Cardinal; InitialValue: integer): PIntArray;

procedure InternalFillRect(dc: HDC; const rcPaint: TRect);

// -----------------------------------------------------------------------------

{ Converting tabs to spaces: To use the function several times it's better
  to use a function pointer that is set to the fastest conversion function }
type
  TConvertTabsProc = function(const Line: UnicodeString;
    TabWidth: Integer): UnicodeString;

function GetBestConvertTabsProc(TabWidth: Integer): TConvertTabsProc;
function ConvertTabs(const Line: UnicodeString; TabWidth: Integer): UnicodeString;

type
  TConvertTabsProcEx = function(const Line: UnicodeString; TabWidth: Integer;
    var HasTabs: Boolean): UnicodeString;

function GetBestConvertTabsProcEx(TabWidth: Integer): TConvertTabsProcEx;
function ConvertTabsEx(const Line: UnicodeString; TabWidth: Integer;
  var HasTabs: Boolean): UnicodeString;

// -----------------------------------------------------------------------------

{ Function to obtain expanded lenghts in various situations }
function GetLeadingWhite(const S: UnicodeString): UnicodeString;
function GetLeadingLength(const AStr: UnicodeString): Integer;
function GetLeadingExpandedLength(const AStr: UnicodeString; ATabWidth: Integer;
  ABorder: Integer = 0): Integer;
function GetExpandedLengthUpTo(const AStr: UnicodeString; ATabWidth: Integer;
  ABorder: Integer): Integer;
function GetExpandedLength(const S: UnicodeString; TabWidth: Integer = 1): Integer;
function GetWideExpandedLength(const AStr: UnicodeString; ATabWidth: Integer): Integer;

// -----------------------------------------------------------------------------

{ Functions to convert char index to caret pos in tabbed lines }
function CharIndex2CaretPos(Index, TabWidth: Integer;
  const Line: UnicodeString): Integer;
function CaretPos2CharIndex(Position, TabWidth: Integer; const Line: UnicodeString;
  var InsideTabChar: Boolean): Integer;

// -----------------------------------------------------------------------------

function StrScanForCharInCategory(const Line: UnicodeString; Start: Integer;
  IsOfCategory: TCategoryMethod): Integer;
function StrRScanForCharInCategory(const Line: UnicodeString; Start: Integer;
  IsOfCategory: TCategoryMethod): Integer;

function StrScanForCharInCategoryEx(const Line: UnicodeString; Start: Integer;
  AToken: Integer; IsOfCategory: TEXCategoryMethod): Integer;
function StrRScanForCharInCategoryEx(const Line: UnicodeString; Start: Integer;
  AToken: Integer; IsOfCategory: TEXCategoryMethod): Integer;

// -----------------------------------------------------------------------------

function GetEOL(Line: PWideChar): PWideChar;
function GetEOLBack(Line: PWideChar): PWideChar;

// -----------------------------------------------------------------------------

function EncodeString(s: UnicodeString): UnicodeString;
function DecodeString(s: UnicodeString): UnicodeString;

type
  THighlighterAttriProc = function (Highlighter: TSynCustomHighlighter;
    Attri: TSynHighlighterAttributes; UniqueAttriName: string;
    Params: array of Pointer): Boolean of object;

function EnumHighlighterAttris(Highlighter: TSynCustomHighlighter;
  SkipDuplicates: Boolean; HighlighterAttriProc: THighlighterAttriProc;
  Params: array of Pointer): Boolean;

// -----------------------------------------------------------------------------

function CalcFCS(const ABuf; ABufSize: Cardinal): Word;

function CharWidthTable(AChar: Char): SmallInt;

function GetTokenKind(ASynEdit: TObject; APoint: TBufferCoord): Integer;

// -----------------------------------------------------------------------------

{ Fast bit routines }
function  GetBitState(var V: Byte; B: Byte): Boolean; register;
procedure SetBitState(var V: Byte; B, S: Byte); register;
procedure TglBitState(var V: Byte; B: Byte); register;

// -----------------------------------------------------------------------------

{ Miscellanious }
function IntToRoman(Value: Integer): String;

function IsStringAllWhite(const S: UnicodeString): Boolean;
function IsLineAllWhite(ASynEdit: TObject; const ALine: Integer): Boolean;
function GetLineLeadingWhite(ASynEdit: TObject; const ALine: Integer): Integer;

implementation

uses
  SysUtils,

  { SynEdit }
  SynEdit, SynEditTextBuffer, SynTextDrawer;

// -----------------------------------------------------------------------------
// Get expanded pos (length) from analyzis
function ExpandedPos(const Start, Pos: Integer;
  const Analyzis: PBArray): Integer;
var
  I: Integer;
begin
  Result := 0;
  if Analyzis <> nil then
    for I := Start to Min(Pred(Pos), High(Analyzis^)) do
      Inc(Result, Analyzis^[I]);
end;

function RealPos(Start, Pos: Integer; const Analyzis: PBArray): Integer;
begin
  Result := 0;
  Start := Pred(Start);
  if Analyzis <> nil then
    while Pos > 0 do
    begin
      Dec(Pos, Analyzis^[Start]);
      Inc(Start); Inc(Result);
    end;
end;

// -----------------------------------------------------------------------------
// Counts real chars until Index in UTF8 string
function CountUTF8Chars(const S: UTF8String; Border: Integer;
  Start: Integer = 1): Integer;
var
  I: Integer;
begin
  Result := 0;
  if Length(S) = 0 then Exit;
  I := Start;
  while I <= Border do
  begin
    if (Ord(S[I]) and $C0) <> $80 then
      Inc(Result);
    Inc(I);
  end;
end;

function CountUnicodeChars(const S: UTF8String; Border: Integer;
  Start: Integer = 1): Integer;
begin
  Result := 0;
  while (Result < Length(S)) and (Border >= Start) do
  begin
    if (Ord(S[Result + 1]) and $C0) <> $80 then
      Dec(Border);
    Inc(Result);
  end;
end;

// -----------------------------------------------------------------------------
// EAX = address of I1, EDX = address of I2
procedure SwapInt(var I1, I2: Integer);
asm
  mov   ecx, [edx]  { ECX = I2 }
  xor   ecx, [eax]  { I2 XOR I1 }
  mov  [eax], ecx   { I1 = I2 XOR I2 }
  mov   ecx, [edx]  { ECX = I2 }
  xor   ecx, [eax]  { I2 XOR I1 }
  mov  [edx], ecx   { I2 = I2 XOR I1 (I2 = I1) }
  mov   ecx, [eax]  { ECX = I1 }
  xor   ecx, [edx]  { I1 XOR I2 }
  mov  [eax], ecx   { I1 = I1 xor I2 (I1 = I2) }
end;

function GetTokenKind(ASynEdit: TObject; APoint: TBufferCoord): Integer;
var
  SynEdit: TCustomSynEdit;
begin
  Result := 0;
  if APoint.Char = 0 then
    Exit;
  SynEdit := TCustomSynEdit(ASynEdit);
  if SynEdit.Highlighter = nil then
    Exit;
  Dec(APoint.Line);
  Dec(APoint.Char);
  with SynEdit, SynEdit.Highlighter do
  begin
    if APoint.Line = 0 then
      ResetRange
    else
      SetRange(TSynEditStringList(Lines).Ranges[APoint.Line - 1]);
    SetLine(Lines[APoint.Line], APoint.Line);
    while not GetEol and (APoint.Char >= GetTokenPos + GetTokenLen) do // § Garnet GetTokenLen
      Next;
    if GetEol then
      Exit;
    Result := GetTokenKind;
  end;
end;

function MinMax(X, Mi, Ma: Integer): Integer;
begin
  X := Min(X, Ma);
  Result := Max(X, Mi);
end;

function MaxPoint(const P1, P2: TPoint): TPoint;
begin
  if (P2.y > P1.y) or ((P2.y = P1.y) and (P2.x > P1.x)) then
    Result := P2
  else
    Result := P1;
end;

function MinPoint(const P1, P2: TPoint): TPoint;
begin
  if (P2.y < P1.y) or ((P2.y = P1.y) and (P2.x < P1.x)) then
    Result := P2
  else
    Result := P1;
end;

function GetIntArray(Count: Cardinal; InitialValue: Integer): PIntArray;
var
  p: PInteger;
begin
  Result := AllocMem(Count * SizeOf(Integer));
  if Assigned(Result) and (InitialValue <> 0) then
  begin
    p := PInteger(Result);
    while (Count > 0) do
    begin
      p^ := InitialValue;
      Inc(p);
      Dec(Count);
    end;
  end;
end;

procedure InternalFillRect(dc: HDC; const rcPaint: TRect);
begin
  ExtTextOut(dc, 0, 0, ETO_OPAQUE, @rcPaint, nil, 0, nil);
end;

// -----------------------------------------------------------------------------
// Please don't change this function; no stack frame and efficient register use
function GetHasTabs(pLine: PWideChar; var CharsBefore: Integer): Boolean;
begin
  CharsBefore := 0;
  if Assigned(pLine) then
  begin
    while pLine^ <> #0 do 
    begin
      if pLine^ = #9 then break;
      Inc(CharsBefore);
      Inc(pLine);
    end;
    Result := pLine^ = #9;
  end
  else
    Result := False;
end;

function ConvertTabs1Ex(const Line: UnicodeString; TabWidth: Integer;
  var HasTabs: Boolean): UnicodeString;
var
  pDest: PWideChar;
  nBeforeTab: Integer;
begin
  Result := Line;  // increment reference count only
  if GetHasTabs(pointer(Line), nBeforeTab) then
  begin
    HasTabs := True;
    pDest := @Result[nBeforeTab + 1]; // this will make a copy of Line
    // We have at least one tab in the string, and the tab width is 1.
    // pDest points to the first tab char. We overwrite all tabs with spaces.
    repeat
      if (pDest^ = #9) then pDest^ := ' ';
      Inc(pDest);
    until (pDest^ = #0);
  end
  else
    HasTabs := False;
end;

function ConvertTabs1(const Line: UnicodeString; TabWidth: Integer): UnicodeString;
var
  HasTabs: Boolean;
begin
  Result := ConvertTabs1Ex(Line, TabWidth, HasTabs);
end;

function ConvertTabs2nEx(const Line: UnicodeString; TabWidth: Integer;
  var HasTabs: Boolean): UnicodeString;
var
  i, DestLen, TabCount, TabMask: Integer;
  pSrc, pDest: PWideChar;
begin
  Result := Line;  // increment reference count only
  if GetHasTabs(pointer(Line), DestLen) then
  begin
    HasTabs := True;
    pSrc := @Line[1 + DestLen];
    // We have at least one tab in the string, and the tab width equals 2^n.
    // pSrc points to the first tab char in Line. We get the number of tabs
    // and the length of the expanded string now.
    TabCount := 0;
    TabMask := (TabWidth - 1) xor $7FFFFFFF;
    repeat
      if pSrc^ = #9 then
      begin
        DestLen := (DestLen + TabWidth) and TabMask;
        Inc(TabCount);
      end
      else
        Inc(DestLen);
      Inc(pSrc);
    until (pSrc^ = #0);
    // Set the length of the expanded string.
    SetLength(Result, DestLen);
    DestLen := 0;
    pSrc := PWideChar(Line);
    pDest := PWideChar(Result);
    // We use another TabMask here to get the difference to 2^n.
    TabMask := TabWidth - 1;
    repeat
      if pSrc^ = #9 then
      begin
        i := TabWidth - (DestLen and TabMask);
        Inc(DestLen, i);
        //This is used for both drawing and other stuff and is meant to be #9 and not #32
        repeat
          pDest^ := #9;
          Inc(pDest);
          Dec(i);
        until (i = 0);
        Dec(TabCount);
        if TabCount = 0 then
        begin
          repeat
            Inc(pSrc);
            pDest^ := pSrc^;
            Inc(pDest);
          until (pSrc^ = #0);
          exit;
        end;
      end
      else
      begin
        pDest^ := pSrc^;
        Inc(pDest);
        Inc(DestLen);
      end;
      Inc(pSrc);
    until (pSrc^ = #0);
  end
  else
    HasTabs := False;
end;

function ConvertTabs2n(const Line: UnicodeString; TabWidth: Integer): UnicodeString;
var
  HasTabs: Boolean;
begin
  Result := ConvertTabs2nEx(Line, TabWidth, HasTabs);
end;

function ConvertTabsEx(const Line: UnicodeString; TabWidth: Integer;
  var HasTabs: Boolean): UnicodeString;
var
  i, DestLen, TabCount: Integer;
  pSrc, pDest: PWideChar;
begin
  Result := Line;  // increment reference count only
  if GetHasTabs(pointer(Line), DestLen) then
  begin
    HasTabs := True;
    pSrc := @Line[1 + DestLen];
    // We have at least one tab in the string, and the tab width is greater
    // than 1. pSrc points to the first tab char in Line. We get the number
    // of tabs and the length of the expanded string now.
    TabCount := 0;
    repeat
      if pSrc^ = #9 then
      begin
        DestLen := DestLen + TabWidth - DestLen mod TabWidth;
        Inc(TabCount);
      end
      else
        Inc(DestLen);
      Inc(pSrc);
    until (pSrc^ = #0);
    // Set the length of the expanded string.
    SetLength(Result, DestLen);
    DestLen := 0;
    pSrc := PWideChar(Line);
    pDest := PWideChar(Result);
    repeat
      if pSrc^ = #9 then
      begin
        i := TabWidth - (DestLen mod TabWidth);
        Inc(DestLen, i);
        repeat
          pDest^ := #9;
          Inc(pDest);
          Dec(i);
        until (i = 0);
        Dec(TabCount);
        if TabCount = 0 then
        begin
          repeat
            Inc(pSrc);
            pDest^ := pSrc^;
            Inc(pDest);
          until (pSrc^ = #0);
          exit;
        end;
      end
      else
      begin
        pDest^ := pSrc^;
        Inc(pDest);
        Inc(DestLen);
      end;
      Inc(pSrc);
    until (pSrc^ = #0);
  end
  else
    HasTabs := False;
end;

function ConvertTabs(const Line: UnicodeString; TabWidth: Integer): UnicodeString;
var
  HasTabs: Boolean;
begin
  Result := ConvertTabsEx(Line, TabWidth, HasTabs);
end;

function IsPowerOfTwo(TabWidth: Integer): Boolean;
var
  nW: Integer;
begin
  nW := 2;
  repeat
    if (nW >= TabWidth) then
      Break;
    Inc(nW, nW);
  until
    (nW >= 32);
  Result := (nW = TabWidth);
end;

function GetBestConvertTabsProc(TabWidth: Integer): TConvertTabsProc;
begin
  if (TabWidth < 2) then Result := TConvertTabsProc(@ConvertTabs1)
    else if IsPowerOfTwo(TabWidth) then
      Result := TConvertTabsProc(@ConvertTabs2n)
    else
      Result := TConvertTabsProc(@ConvertTabs);
end;

function GetBestConvertTabsProcEx(TabWidth: Integer): TConvertTabsProcEx;
begin
  if (TabWidth < 2) then Result := ConvertTabs1Ex
    else if IsPowerOfTwo(TabWidth) then
      Result := ConvertTabs2nEx
    else
      Result := ConvertTabsEx;
end;

function GetLeadingWhite(const S: UnicodeString): UnicodeString;
var
  I: Integer;
begin
  Result := EmptyStr;

  I := 1;
  while (I <= Length(S)) and (S[I] < #33) do
  begin
    Result := Result + S[I];
    Inc(I);
  end;
end;

// -----------------------------------------------------------------------------

function GetLeadingLength(const AStr: UnicodeString): Integer;
var
  Runner: PChar;
begin
  Result := 0;
  Runner := PChar(AStr);
  while (Runner^ <> #0) and (Runner^ < #33) do
  begin
    Inc(Result);
    Inc(Runner);
  end;
end;

// -----------------------------------------------------------------------------
// § Garnet
function GetLeadingExpandedLength(const AStr: UnicodeString; ATabWidth: Integer;
  ABorder: Integer = 0): Integer;
var
  iRun: PChar;
  Len: Integer;
begin
  Result := 0;
  iRun := PChar(AStr);
  if ABorder > 0 then
    Len := Min(PInteger(iRun - 2)^, ABorder)
  else
    Len := PInteger(iRun - 2)^;
  while Len > 0 do
  begin
    if iRun^ = #9 then
      Inc(Result, ATabWidth - (Result mod ATabWidth))
    else if iRun^ = #32 then
      Inc(Result)
    else
      Exit;
    Inc(iRun);
    Dec(Len);
  end;
end;

function GetExpandedLengthUpTo(const AStr: UnicodeString; ATabWidth: Integer;
  ABorder: Integer): Integer;
var
  iRun: PChar;
  Len: Integer;
begin
  Result := 0;
  iRun := PChar(AStr);
  if ABorder > 0 then
    Len := Min(PInteger(iRun - 2)^, ABorder)
  else
    Len := PInteger(iRun - 2)^;
  while Len > 0 do
  begin
    if iRun^ = #9 then
      Inc(Result, ATabWidth - (Result mod ATabWidth))
    else if iRun^ = #32 then
      Inc(Result)
    else
      Inc(Result, CharWidthTable(iRun^));
    Inc(iRun);
    Dec(Len);
  end;
end;

// -----------------------------------------------------------------------------
// Computes visual length of a given string. Tab width defaults to ease calls
// for finding single token visual length
function GetExpandedLength(const S: UnicodeString;
  TabWidth: Integer = 1): Integer;
var
  Runner: PChar;
begin
  Result := 0;
  Runner := PWideChar(S);
  while Runner^ <> #0 do
  begin
    if Runner^ = #9 then
      Inc(Result, TabWidth - (Result mod TabWidth))
    else
      Inc(Result);
    Inc(Runner);
  end;
end;

function GetWideExpandedLength(const AStr: UnicodeString; ATabWidth: Integer): Integer;
var
  iRun: PChar;
begin
  Result := 0;
  iRun := PChar(AStr);
  while iRun^ <> #0 do
  begin
    if iRun^ = #9 then
      Inc(Result, ATabWidth - (Result mod ATabWidth))
    else
      Inc(Result, CharWidthTable(iRun^));
    Inc(iRun);
  end;
end;

function CharIndex2CaretPos(Index, TabWidth: Integer;
  const Line: UnicodeString): Integer;
var
  iChar: Integer;
  pNext: PWideChar;
begin
  if Index > 1 then
  begin
    if (TabWidth <= 1) or not GetHasTabs(pointer(Line), iChar) then
      Result := Index
    else
    begin
      if iChar + 1 >= Index then
        Result := Index
      else
      begin
        { iChar is number of chars before first #9 }
        Result := iChar;
        { Index is *not* zero-based }
        Inc(iChar);
        Dec(Index, iChar);
        pNext := @Line[iChar];
        while Index > 0 do
        begin
          case pNext^ of
            #0:
              begin
                Inc(Result, Index);
                break;
              end;
            #9:
              begin
                { Result is still zero-based }
                Inc(Result, TabWidth);
                Dec(Result, Result mod TabWidth);
              end;
            else
              Inc(Result);
          end;
          Dec(Index);
          Inc(pNext);
        end;

        { Done with zero-based computation }
        Inc(Result);
      end;
    end;
  end
  else
    Result := 1;
end;

function CaretPos2CharIndex(Position, TabWidth: Integer; const Line: UnicodeString;
  var InsideTabChar: Boolean): Integer;
var
  iPos: Integer;
  pNext: PWideChar;
begin
  InsideTabChar := False;
  if Position > 1 then
  begin
    if (TabWidth <= 1) or not GetHasTabs(pointer(Line), iPos) then
      Result := Position
    else
    begin
      if iPos + 1 >= Position then
        Result := Position
      else
      begin
        // iPos is number of chars before first #9
        Result := iPos + 1;
        pNext := @Line[Result];
        // for easier computation go zero-based (mod-operation)
        Dec(Position);
        while iPos < Position do
        begin
          case pNext^ of
            #0: break;
            #9: begin
                  Inc(iPos, TabWidth);
                  Dec(iPos, iPos mod TabWidth);
                  if iPos > Position then
                  begin
                    InsideTabChar := True;
                    break;
                  end;
                end;
            else
              Inc(iPos);
          end;
          Inc(Result);
          Inc(pNext);
        end;
      end;
    end;
  end
  else
    Result := Position;
end;

// -----------------------------------------------------------------------------
// Search for the first char of set AChars in Line, starting at index Start
function StrScanForCharInCategory(const Line: UnicodeString; Start: Integer;
  IsOfCategory: TCategoryMethod): Integer;
var
  p: PWideChar;
begin
  if (Start > 0) and (Start <= Length(Line)) then
  begin
    p := PWideChar(@Line[Start]);
    repeat
      if IsOfCategory(p^) then
      begin
        Result := Start;
        exit;
      end;
      Inc(p);
      Inc(Start);
    until p^ = #0;
  end;
  Result := 0;
end;

// -----------------------------------------------------------------------------
// The same, but searching backwards
function StrRScanForCharInCategory(const Line: UnicodeString; Start: Integer;
  IsOfCategory: TCategoryMethod): Integer;
var
  I: Integer;
begin
  Result := 0;
  if (Start > 0) and (Start <= Length(Line)) then
  begin
    for I := Start downto 1 do
      if IsOfCategory(Line[I]) then
      begin
        Result := I;
        Exit;
      end;
  end;
end;

// § Garnet
function StrScanForCharInCategoryEx(const Line: UnicodeString; Start: Integer;
  AToken: Integer; IsOfCategory: TEXCategoryMethod): Integer;
var
  p: PWideChar;
begin
  if (Start > 0) and (Start <= Length(Line)) then
  begin
    p := PWideChar(@Line[Start]);
    repeat
      if IsOfCategory(p^, AToken) then
      begin
        Result := Start;
        exit;
      end;
      Inc(p);
      Inc(Start);
    until p^ = #0;
  end;
  Result := 0;
end;

function StrRScanForCharInCategoryEx(const Line: UnicodeString; Start: Integer;
  AToken: Integer; IsOfCategory: TEXCategoryMethod): Integer;
var
  I: Integer;
begin
  Result := 0;
  if (Start > 0) and (Start <= Length(Line)) then
  begin
    for I := Start downto 1 do
      if IsOfCategory(Line[I], AToken) then
      begin
        Result := I;
        Exit;
      end;
  end;
end;
// § Garnet

function GetEOL(Line: PWideChar): PWideChar;
begin
  Result := Line;
  if Assigned(Result) then
    while (Result^ <> #0) and (Result^ <> #10) and (Result^ <> #13) do
      Inc(Result);
end;

function GetEOLBack(Line: PWideChar): PWideChar;
begin
  Result := Line;
  if Assigned(Result) then
    while Result^ <> #10 do
      Dec(Result);
end;

// -----------------------------------------------------------------------------
// Remove all '/' characters from string by changing them into '\.'.
// Change all '\' characters into '\\' to allow for unique decoding
{$IFOPT R+}
  {$DEFINE RestoreRangeChecking}
{$ELSE}
  {$UNDEF RestoreRangeChecking}
{$ENDIF}
{$R-}
function EncodeString(s: UnicodeString): UnicodeString;
var
  i, j: Integer;
begin
  SetLength(Result, 2 * Length(s)); // worst case
  j := 0;
  for i := 1 to Length(s) do
  begin
    Inc(j);
    if s[i] = '\' then
    begin
      Result[j] := '\';
      Result[j + 1] := '\';
      Inc(j);
    end
    else if s[i] = '/' then
    begin
      Result[j] := '\';
      Result[j + 1] := '.';
      Inc(j);
    end
    else
      Result[j] := s[i];
  end; //for
  SetLength(Result, j);
end;

// -----------------------------------------------------------------------------
// Decodes string, encoded with EncodeString()
function DecodeString(s: UnicodeString): UnicodeString;
var
  i, j: Integer;
begin
  SetLength(Result, Length(s)); // worst case
  j := 0;
  i := 1;
  while i <= Length(s) do
  begin
    Inc(j);
    if s[i] = '\' then
    begin
      Inc(i);
      if s[i] = '\' then
        Result[j] := '\'
      else
        Result[j] := '/';
    end
    else
      Result[j] := s[i];
    Inc(i);
  end;
  SetLength(Result,j);
end;
{$IFDEF RestoreRangeChecking}
  {$R+}
{$ENDIF}

// -----------------------------------------------------------------------------
// Enums all child highlighters and their attributes of a TSynMultiSyn through a
// callback function. This function also handles nested TSynMultiSyns
// including their MarkerAttri
function EnumHighlighterAttris(Highlighter: TSynCustomHighlighter;
  SkipDuplicates: Boolean; HighlighterAttriProc: THighlighterAttriProc;
  Params: array of Pointer): Boolean;

  function GetHighlighterIndex(Highlighter: TSynCustomHighlighter;
    HighlighterList: TList): Integer;
  var
    i: Integer;
  begin
    Result := 1;
    for i := 0 to HighlighterList.Count - 1 do
      if HighlighterList[i] = Highlighter then
        Exit
      else if Assigned(HighlighterList[i]) and
        (TObject(HighlighterList[i]).ClassType = Highlighter.ClassType)
      then
        inc(Result);
  end;

  function InternalEnumHighlighterAttris(Highlighter: TSynCustomHighlighter;
    SkipDuplicates: Boolean; HighlighterAttriProc: THighlighterAttriProc;
    Params: array of Pointer; HighlighterList: TList): Boolean;

    function DeleteTypePrefixAndSynSuffix(S: string): string;
    begin
      Result := S;
      if CharInSet(Result[1], ['T', 't']) then
        if Pos('tsyn', LowerCase(Result)) = 1 then
          Delete(Result, 1, 4)
        else
          Delete(Result, 1, 1);

      if Copy(LowerCase(Result), Length(Result) - 2, 3) = 'syn' then
        SetLength(Result, Length(Result) - 3);
    end;

  var
    i: Integer;
    UniqueAttriName: string;
  begin
    Result := True;

    if (HighlighterList.IndexOf(Highlighter) >= 0) then
    begin
      if SkipDuplicates then Exit;
    end
    else
      HighlighterList.Add(Highlighter);

    if Assigned(Highlighter) then
      for i := 0 to Highlighter.AttrCount - 1 do
      begin
        UniqueAttriName := DeleteTypePrefixAndSynSuffix(Highlighter.ClassName) +
          IntToStr(GetHighlighterIndex(Highlighter, HighlighterList)) + '.' +
          Highlighter.Attribute[i].Name;

        Result := HighlighterAttriProc(Highlighter, Highlighter.Attribute[i],
          UniqueAttriName, Params);
        if not Result then Exit
      end
  end;

var
  HighlighterList: TList;
begin
  if not Assigned(Highlighter) or not Assigned(HighlighterAttriProc) then
  begin
    Result := False;
    Exit;
  end;

  HighlighterList := TList.Create;
  try
    Result := InternalEnumHighlighterAttris(Highlighter, SkipDuplicates,
      HighlighterAttriProc, Params, HighlighterList)
  finally
    HighlighterList.Free
  end
end;

// -----------------------------------------------------------------------------
// Fast Frame Check Sequence (FCS) Implementation
// Translated from sample code given with RFC 1171 by Marko Njezic

const
  FCSTab: array[Byte] of Word = (
    $0000, $1189, $2312, $329b, $4624, $57ad, $6536, $74bf,
    $8c48, $9dc1, $af5a, $bed3, $ca6c, $dbe5, $e97e, $f8f7,
    $1081, $0108, $3393, $221a, $56a5, $472c, $75b7, $643e,
    $9cc9, $8d40, $bfdb, $ae52, $daed, $cb64, $f9ff, $e876,
    $2102, $308b, $0210, $1399, $6726, $76af, $4434, $55bd,
    $ad4a, $bcc3, $8e58, $9fd1, $eb6e, $fae7, $c87c, $d9f5,
    $3183, $200a, $1291, $0318, $77a7, $662e, $54b5, $453c,
    $bdcb, $ac42, $9ed9, $8f50, $fbef, $ea66, $d8fd, $c974,
    $4204, $538d, $6116, $709f, $0420, $15a9, $2732, $36bb,
    $ce4c, $dfc5, $ed5e, $fcd7, $8868, $99e1, $ab7a, $baf3,
    $5285, $430c, $7197, $601e, $14a1, $0528, $37b3, $263a,
    $decd, $cf44, $fddf, $ec56, $98e9, $8960, $bbfb, $aa72,
    $6306, $728f, $4014, $519d, $2522, $34ab, $0630, $17b9,
    $ef4e, $fec7, $cc5c, $ddd5, $a96a, $b8e3, $8a78, $9bf1,
    $7387, $620e, $5095, $411c, $35a3, $242a, $16b1, $0738,
    $ffcf, $ee46, $dcdd, $cd54, $b9eb, $a862, $9af9, $8b70,
    $8408, $9581, $a71a, $b693, $c22c, $d3a5, $e13e, $f0b7,
    $0840, $19c9, $2b52, $3adb, $4e64, $5fed, $6d76, $7cff,
    $9489, $8500, $b79b, $a612, $d2ad, $c324, $f1bf, $e036,
    $18c1, $0948, $3bd3, $2a5a, $5ee5, $4f6c, $7df7, $6c7e,
    $a50a, $b483, $8618, $9791, $e32e, $f2a7, $c03c, $d1b5,
    $2942, $38cb, $0a50, $1bd9, $6f66, $7eef, $4c74, $5dfd,
    $b58b, $a402, $9699, $8710, $f3af, $e226, $d0bd, $c134,
    $39c3, $284a, $1ad1, $0b58, $7fe7, $6e6e, $5cf5, $4d7c,
    $c60c, $d785, $e51e, $f497, $8028, $91a1, $a33a, $b2b3,
    $4a44, $5bcd, $6956, $78df, $0c60, $1de9, $2f72, $3efb,
    $d68d, $c704, $f59f, $e416, $90a9, $8120, $b3bb, $a232,
    $5ac5, $4b4c, $79d7, $685e, $1ce1, $0d68, $3ff3, $2e7a,
    $e70e, $f687, $c41c, $d595, $a12a, $b0a3, $8238, $93b1,
    $6b46, $7acf, $4854, $59dd, $2d62, $3ceb, $0e70, $1ff9,
    $f78f, $e606, $d49d, $c514, $b1ab, $a022, $92b9, $8330,
    $7bc7, $6a4e, $58d5, $495c, $3de3, $2c6a, $1ef1, $0f78
  );

// -----------------------------------------------------------------------------
// Calculates Frame Check Sequence (FCS) 16-bit Checksum
function CalcFCS(const ABuf; ABufSize: Cardinal): Word;
var
  CurFCS: Word;
  P: ^Byte;
begin
  CurFCS := $ffff;
  P := @ABuf;
  while ABufSize <> 0 do
  begin
    CurFCS := (CurFCS shr 8) xor FCSTab[(CurFCS xor P^) and $ff];
    Dec(ABufSize);
    Inc(P);
  end;
  Result := CurFCS;
end;

// -----------------------------------------------------------------------------
// Full-width ranges:
//
// U-16  U-8              U-16  U-8
// 1100  e1 84 80     ..  115F  e1 85 9f
// 2329  e2 8c a9     ..  232A  e2 8c aa
// 2E80  e2 ba 80     ..  303E  e3 80 be
// 3041  e3 81 81     ..  33FF  e3 8f bf
// 3400  e3 90 80     ..  4DB5  e4 b6 b5
// 4E00  e4 b8 80     ..  9FC3  e9 bf 83
// A000  ea 80 80     ..  A4C6  ea 93 86
// AC00  ea b0 80     ..  D7A3  ed 9e a3
// F900  ef a4 80     ..  FAD9  ef ab 99
// FE10  ef b8 90     ..  FE19  ef b8 99
// FE30  ef b8 b0     ..  FE6B  ef b9 ab
// FF01  ef bc 81     ..  FF60  ef bd a0
// FFE0  ef bf a0     ..  FFE6  ef bf a6
// 20000 f0 a0 80 80  ..  2FFFD f0 af bf bd
// 30000 f0 b0 80 80  ..  3FFFD f0 bf bf bd
//
// Actually, they are incomplete. One can easily find character out of this
// range which is actually full-width but doesn't fall in any of ranges above.
// Also, there are characters even wider than full-width, which take 3 or even 4
// regular spaces in text
function CharWidthTable(AChar: Char): SmallInt;
begin
  Result := 1;
  if (AChar >= #$1100) and (AChar <= #$115F) then
    Result := 2
  else if (AChar >= #$2329) and (AChar <= #$232A) then
    Result := 2
  else if (AChar >= #$2E80) and (AChar <= #$303E) then
    Result := 2
  else if (AChar >= #$3041) and (AChar <= #$33FF) then
    Result := 2
  else if (AChar >= #$3400) and (AChar <= #$4DB5) then
    Result := 2
  else if (AChar >= #$4E00) and (AChar <= #$9FC3) then
    Result := 2
  else if (AChar >= #$A000) and (AChar <= #$A4C6) then
    Result := 2
  else if (AChar >= #$AC00) and (AChar <= #$D7A3) then
    Result := 2
  else if (AChar >= #$F900) and (AChar <= #$FAD9) then
    Result := 2
  else if (AChar >= #$FE10) and (AChar <= #$FE19) then
    Result := 2
  else if (AChar >= #$FE30) and (AChar <= #$FE6B) then
    Result := 2
  else if (AChar >= #$FF01) and (AChar <= #$FF60) then
    Result := 2
  else if (AChar >= #$FFE0) and (AChar <= #$FFE6) then
    Result := 2;
end;

{ Fast bit routines }

// -----------------------------------------------------------------------------
// EAX = address of V, EDX = B
function GetBitState(var V: Byte; B: Byte): Boolean;
asm
  and  edx, 31    { Ensure EDX is correct (0..31) }
  bt   [eax], edx { Set carry to 1 if bit is set }
  setc al         { AX lower will now contain 0 or 1 }
  and  eax, 1     { Clean upper part of EAX }
end;

// -----------------------------------------------------------------------------
// EAX = address of V, EDX = B, ECX = S
procedure SetBitState(var V: Byte; B, S: Byte);
asm
  and  edx, 31    { Ensure EDX is correct (0..31) }
  and  ecx, 1     { Ensure ECX is correct (0..1) }
	jcxz @@disable  { Jump to disable bit routne if ECX contains 0 }
  bts  [eax], edx { Enable bit }
  jmp  @@finish   { Jump to result return }
@@disable:
  btr  [eax], edx { Disable bit }
  jmp  @@finish   { Jump to result return }
@@finish:
end;

// -----------------------------------------------------------------------------
// EAX = address of V, EDX = B
procedure TglBitState(var V: Byte; B: Byte);
asm
  and  edx, 31    { Ensure EDX is correct (0..31) }
  btc  [eax], edx { Complement (toggle) bit }
end;

// -----------------------------------------------------------------------------
// Integer to Roman - copied from SWAG
function IntToRoman(Value: Integer): String;
begin
  Result := '';

  while Value >= 1000 do
  begin
    Result := Result + 'M';
    Dec(Value, 1000);
  end;

  if Value >= 900 then
  begin
    Result := Result + 'CM';
    Dec(Value, 900);
  end;

  while Value >= 500 do
  begin
    Result := Result + 'D';
    Dec(Value, 500);
  end;

  if Value >= 400 then
  begin
    Result := Result + 'CD';
    Dec(Value, 400);
  end;

  while Value >= 100 do
  begin
    Result := Result + 'C';
    Dec(Value, 100);
  end;

  if Value >= 90 then
  begin
    Result := Result + 'XC';
    Dec(Value, 90);
  end;

  while Value >= 50 do
  begin
    Result := Result + 'L';
    Dec(Value, 50);
  end;

  if Value >= 40 then
  begin
    Result := Result + 'XL';
    Dec(Value, 40);
  end;

  while Value >= 10 do
  begin
    Result := Result + 'X';
    Dec(Value, 10);
  end;

  if Value >= 9 then
  begin
    Result := Result + 'IX';
    Dec(Value, 9);
  end;

  while Value >= 5 do
  begin
    Result := Result + 'V';
    Dec(Value, 5);
  end;

  if Value >= 4 then
  begin
    Result := Result + 'IV';
    Dec(Value, 4);
  end;

  while Value > 0 do
  begin
    Result := Result + 'I';
    Dec(Value);
  end;
end;

function IsStringAllWhite(const S: UnicodeString): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to Length(S) do
    if S[I] > #32 then
    begin
      Result := False;
      Break;
    end;
end;

function IsLineAllWhite(ASynEdit: TObject; const ALine: Integer): Boolean;
var
  I: Integer;
  S: UnicodeString;
begin
  Result := True;
  if (ALine < 1) or (ALine > (ASynEdit as TCustomSynEdit).Lines.Count) then
    Exit;
  S := (ASynEdit as TCustomSynEdit).Lines.fList^[Pred(ALine)].fString;
  for I := 1 to Length(S) do
    if S[I] > #32 then
    begin
      Result := False;
      Break;
    end;
end;

function GetLineLeadingWhite(ASynEdit: TObject; const ALine: Integer): Integer;
var
  I: Integer;
  S: UnicodeString;
begin
  Result := 0;
  if (ALine < 1) or (ALine > (ASynEdit as TCustomSynEdit).Lines.Count) then
    Exit;
  S := (ASynEdit as TCustomSynEdit).Lines.fList^[Pred(ALine)].fString;
  for I := 1 to Length(S) do
    if S[I] > #32 then
      Break
    else
      Inc(Result);
end;

end.
