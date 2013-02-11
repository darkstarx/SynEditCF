(*

  Letterpress

  Token, folding and indentation matching module.

  Copyright 2006-2010, Krystian Bigaj and Garnet

*)

{-------------------------------------------------------------------------------
SynWeb
Copyright (C) 2006, Krystian Bigaj

*** MPL
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is Krystian Bigaj.

Alternatively, the contents of this file may be used under the terms
of the GNU Lesser General Public license (the  "LGPL License"),
in which case the provisions of LGPL License are applicable instead of those
above. If you wish to allow use of your version of this file only
under the terms of the LGPL License and not to allow others to use
your version of this file under the MPL, indicate your decision by
deleting the provisions above and replace them with the notice and
other provisions required by the LGPL License. If you do not delete
the provisions above, a recipient may use your version of this file
under either the MPL or the LGPL License.

*** LGPL
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
***

You may retrieve the latest version of this file at the SynWeb home page,
located at http://sourceforge.net/projects/synweb

Contact: krystian.bigaj@gmail.com
Homepage: http://flatdev.ovh.org
-------------------------------------------------------------------------------}

unit SynTokenMatch;

interface

uses
  Windows,

  { SynEdit }
  SynEdit, SynEditTypes, SynEditHighlighter, SynEditCodeFolding;

{ Token matching }

{
  SynEditGetMatchingToken(Ex) returns:
  -2: Close and open token found;
  -1: Close token found;
   0: Kind not found;
  +1: Open token found;
  +2: Open and close token found.
}

// -----------------------------------------------------------------------------

function SynEditGetMatchingToken(ASynEdit: TCustomSynEdit; APoint: TBufferCoord;
  const ATokens: array of TSynTokenMatch; var AMatch: TSynTokenMatched;
  AComplete: Boolean = False; AFoldingOnly: Boolean = False): Integer;
function SynEditGetMatchingTokenEx(ASynEdit: TCustomSynEdit; APoint: TBufferCoord;
  const ATokens: array of TSynTokenMatch; var AMatch: TSynTokenMatched;
  AComplete: Boolean = False; AFoldingOnly: Boolean = False): Integer;

function SynEditGetMatchingPartKind(ASynEdit: TCustomSynEdit; const ALine: Integer;
  const ATokens: array of TSynTokenMatch; AFoldingOnly: Boolean = False): Integer;

{ Folding matching }

// -----------------------------------------------------------------------------

procedure SynEditGetOpenCloseFoldingRange(ASynEdit: TCustomSynEdit;
  ALine: Integer; const ARegions: TFoldRegions; var AOpen, AClose: Boolean;
  IndentationOnly: Boolean = False; NoIndentation: Boolean = False);

procedure SynEditCollpaseOpen(ASynEdit: TCustomSynEdit; ALine: Integer;
  const ARegions: TFoldRegions; var AFrom, ATo, AFromChar, AToChar, ARegion,
  AFromComplete, AToCompelte: Integer);
procedure SynEditCollpaseClose(ASynEdit: TCustomSynEdit; ALine: Integer;
  const ARegions: TFoldRegions; var AFrom, ATo, AFromChar, AToChar, ARegion,
  AFromComplete, AToComplete: Integer);

procedure SynEditCollpaseOpenFoldingRange(ASynEdit: TCustomSynEdit;
  var ALine, AChar: Integer; const ARegions: TFoldRegions);
procedure SynEditCollpaseCloseFoldingRange(ASynEdit: TCustomSynEdit;
  ALine: Integer; const ARegions: TFoldRegions);

procedure SynEditFindClosest(ASynEdit: TCustomSynEdit;
  ALine: Integer; const ARegions: TFoldRegions; var AFrom, ATo: Integer);

procedure SynEditToggleFoldingRange(ASynEdit: TCustomSynEdit;
  ALine: Integer; const ARegions: TFoldRegions; var AToggled: Boolean);

function OutliningCanCollapseOpen(ASynEdit: TCustomSynEdit; ALine: Integer): Boolean;
function OutliningCanCollapseClose(ASynEdit: TCustomSynEdit; ALine: Integer): Boolean;
function OutliningCanExpand(ASynEdit: TCustomSynEdit; ALine: Integer): Boolean;
function OutliningCollapse(ASynEdit: TCustomSynEdit; ALine: Integer): Boolean;
function OutliningExpand(ASynEdit: TCustomSynEdit; ALine: Integer): Boolean;
function OutliningToggle(ASynEdit: TCustomSynEdit; ALine: Integer): Boolean;
function OutliningScope(ASynEdit: TCustomSynEdit; ALine: Integer): UnicodeString;

{ Indentation matching }

// -----------------------------------------------------------------------------

function SynEditGetIndent(ASynEdit: TCustomSynEdit; APoint: TBufferCoord;
  var OnlyOnce: Boolean; Reindent: Boolean = False): Boolean;
function SynEditGetUnindent(ASynEdit: TCustomSynEdit; APoint: TBufferCoord;
  LookBehind: Boolean = False; Reindenting: Boolean = False): Boolean;

implementation

uses
  SysUtils, Math, RegularExpressions,

  { SynEdit }
  SynUnicode, SynEditMiscProcs,

  { SynUni }
  SynUniHighlighter, SynUniRules, SynUniClasses;

{ Token matching routines }

// -----------------------------------------------------------------------------

type
  TSynTokenBuf = record
    Pos: TBufferCoord;
    Token: UnicodeString;
  end;

var
  FMatchStack: array of TSynTokenBuf;
  FMatchOpenDup, FMatchCloseDup: array of Integer;

// -----------------------------------------------------------------------------

function SynEditGetMatchingToken(ASynEdit: TCustomSynEdit; APoint: TBufferCoord;
  const ATokens: array of TSynTokenMatch; var AMatch: TSynTokenMatched;
  AComplete: Boolean = False; AFoldingOnly: Boolean = False): Integer;
var
  TokenMatch: PSynTokenMatch;
  Token, BackreferenceToken: UnicodeString;
  TokenKind: Integer;
  TokenRange: Pointer;
  Level, DeltaLevel, I, J, FMatchStackID, OpenDupLen, CloseDupLen: Integer;
  bDummy1, bDummy2: Boolean;

  function CasedLowerCase(const S: UnicodeString; Range: Pointer): UnicodeString;
  begin
    if not (sroCaseSensitive in TSynRange(Range).Options) then
      Result := LowerCase(S)
    else
      Result := S;
  end;

  function IsOpenToken: Boolean;
  var
    X: Integer;
  begin
    for X := 0 to OpenDupLen - 1 do
      if Token = ATokens[FMatchOpenDup[X]].OpenToken then
      begin
        Result := True;
        Exit;
      end;
    Result := False
  end;

  function IsCloseToken: Boolean;
  var
    X: Integer;
  begin
    for X := 0 to CloseDupLen - 1 do
      if Token = ATokens[FMatchCloseDup[X]].CloseToken then
      begin
        Result := True;
        Exit;
      end;
    Result := False
  end;

  function IsBackreferencedToken: Boolean;
  begin
    Result := Token = BackreferenceToken;
  end;

  function CheckToken: Boolean;
  begin
    with ASynEdit.Highlighter do
    begin
      if TokenMatch^.Backreference then
      begin
        if (TokenMatch^.CloseRange = GetRange) and (GetTokenKind = TokenMatch^.CloseTokenKind) then
        begin
          Token := LowerCase(GetToken);
          if IsBackreferencedToken then
            Dec(Level);
        end
        else if (TokenMatch^.OpenRange = GetRange) and (GetTokenKind = TokenMatch^.OpenTokenKind) then
        begin
          Token := LowerCase(GetToken);
          if IsBackreferencedToken then
            Inc(Level);
        end;
      end
      else if GetTokenKind = TokenMatch^.OpenTokenKind then
      begin
        if (TokenMatch^.OpenRange <> nil) and (TokenMatch^.OpenRange <> GetRange) then
          Token := EmptyStr
        else
          Token := LowerCase(GetToken);
        if Token <> EmptyStr then
          if IsCloseToken and (TokenMatch^.OpenTokenKind = TokenMatch^.CloseTokenKind) then
            Dec(Level)
          else if IsOpenToken then
            Inc(Level);
      end
      else if GetTokenKind = TokenMatch^.CloseTokenKind then
      begin
        if (TokenMatch^.CloseRange <> nil) and (TokenMatch^.CloseRange <> GetRange) then
          Token := EmptyStr
        else
          Token := LowerCase(GetToken);
        if Token <> EmptyStr then
          if IsCloseToken then
            Dec(Level)
          else if IsOpenToken and (TokenMatch^.OpenTokenKind = TokenMatch^.CloseTokenKind) then
            Inc(Level);
      end;
      if Level = 0 then
      begin
        SynEditGetMatchingToken := 2;
        AMatch.CloseToken := GetToken;
        AMatch.CloseTokenPos.Line := APoint.Line + 1;
        AMatch.CloseTokenPos.Char := GetTokenPos + 1;
        if AComplete then
          Inc(AMatch.CloseTokenPos.Char, GetTokenLen);
        Result := True;
      end
      else begin
        Next;
        Result := False;
      end;
    end;
  end;

  procedure CheckTokenBack;
  begin
    with ASynEdit.Highlighter do
    begin
      if TokenMatch^.Backreference then
      begin
        if (TokenMatch^.CloseRange = GetRange) and (GetTokenKind = TokenMatch^.CloseTokenKind) then
        begin
          Token := LowerCase(GetToken);
          if IsBackreferencedToken then
          begin
            Dec(Level);
            if FMatchStackID >= 0 then
              Dec(FMatchStackID);
          end;
        end
        else if (TokenMatch^.OpenRange = GetRange) and (GetTokenKind = TokenMatch^.OpenTokenKind) then
        begin
          Token := LowerCase(GetToken);
          if IsBackreferencedToken then
          begin
            Inc(Level);
            Inc(FMatchStackID);
            if FMatchStackID >= Length(FMatchStack) then
              SetLength(FMatchStack, Length(FMatchStack) + 32);
            FMatchStack[FMatchStackID].Token := GetToken;
            FMatchStack[FMatchStackID].Pos.Line := APoint.Line + 1;
            FMatchStack[FMatchStackID].Pos.Char := GetTokenPos + 1;
          end;
        end;
      end
      else if GetTokenKind = TokenMatch^.OpenTokenKind then
      begin
        if (TokenMatch^.OpenRange <> nil) and (TokenMatch^.OpenRange <> GetRange) then
          Token := EmptyStr
        else
          Token := LowerCase(GetToken);
        if Token <> EmptyStr then
          if IsCloseToken and (TokenMatch^.OpenTokenKind = TokenMatch^.CloseTokenKind) then
          begin
            Dec(Level);
            if FMatchStackID >= 0 then
              Dec(FMatchStackID);
          end
          else if IsOpenToken then
          begin
            Inc(Level);
            Inc(FMatchStackID);
            if FMatchStackID >= Length(FMatchStack) then
              SetLength(FMatchStack, Length(FMatchStack) + 32);
            FMatchStack[FMatchStackID].Token := GetToken;
            FMatchStack[FMatchStackID].Pos.Line := APoint.Line + 1;
            FMatchStack[FMatchStackID].Pos.Char := GetTokenPos + 1;
          end;
      end
      else if GetTokenKind = TokenMatch^.CloseTokenKind then
      begin
        if (TokenMatch^.CloseRange <> nil) and (TokenMatch^.CloseRange <> GetRange) then
          Token := EmptyStr
        else
          Token := LowerCase(GetToken);
        if Token <> EmptyStr then
          if IsCloseToken then
          begin
            Dec(Level);
            if FMatchStackID >= 0 then
              Dec(FMatchStackID);
          end
          else if IsOpenToken and (TokenMatch^.OpenTokenKind = TokenMatch^.CloseTokenKind) then
          begin
            Inc(Level);
            Inc(FMatchStackID);
            if FMatchStackID >= Length(FMatchStack) then
              SetLength(FMatchStack, Length(FMatchStack) + 32);
            FMatchStack[FMatchStackID].Token := GetToken;
            FMatchStack[FMatchStackID].Pos.Line := APoint.Line + 1;
            FMatchStack[FMatchStackID].Pos.Char := GetTokenPos + 1;
          end;
      end;
      Next;
    end;
  end;

var
  Cache: PSynUniCacheItem;
begin
  Result := 0;
  if ASynEdit.Highlighter = nil then
    Exit;
  Dec(APoint.Line);
  Dec(APoint.Char);
  with ASynEdit, ASynEdit.Highlighter do
  begin
    if APoint.Line = 0 then
      ResetRange
    else
      SetRange(Lines.Ranges[APoint.Line - 1]);
    SetLine(Lines[APoint.Line], APoint.Line);
    while not GetEol and (APoint.Char >= GetTokenPos + GetTokenLen) do
      Next;
    if GetEol then
      Exit;

    { Outlining match? }
    Cache := (ASynEdit.Highlighter as TSynUniSyn).Cache[APoint.Line];
    if Cache <> nil then
    begin
    if not Cache^.AOutlined then
      SynEditGetOpenCloseFoldingRange(ASynEdit, Succ(APoint.Line), FoldRegions, bDummy1, bDummy2);
    for I := 0 to High(Cache^.AOutliningMap^) do
      if (Cache^.AOutliningMap^[I].ATokenIndex = Succ(GetTokenPos)) or
        (AFoldingOnly and (Cache^.AOutliningMap^[I].AIndex = Succ(GetTokenPos))) then
      begin
        Result := 0;
        with AMatch do
        begin
          TokenKind := GetTokenKind;
          TokenAttri := GetTokenAttribute;
        end;

        if Cache^.AOutliningMap^[I].AOpen then
        begin
          Result := 1;
          J := Succ(GetTokenPos);
          with AMatch do
          begin
            OpenToken := GetToken;
            OpenTokenPos.Line := Succ(APoint.Line);
            OpenTokenPos.Char := J;
          end;

          SynEditCollpaseOpen(ASynEdit, Succ(APoint.Line),
            ASynEdit.Highlighter.FoldRegions, Level, DeltaLevel, J, TokenKind, FMatchStackID,
            OpenDupLen, CloseDupLen);

        end
        else begin
          Result := -1;
          TokenKind := Succ(GetTokenPos);
          with AMatch do
          begin
            CloseToken := GetToken;
            CloseTokenPos.Line := Succ(APoint.Line);
            CloseTokenPos.Char := Succ(GetTokenPos);
          end;
          SynEditCollpaseClose(ASynEdit, Succ(APoint.Line),
            ASynEdit.Highlighter.FoldRegions, Level, DeltaLevel, J, TokenKind, FMatchStackID,
            OpenDupLen, CloseDupLen);
        end;

        if (Level > 0) and (DeltaLevel > 0) then
        begin
          Result := Result shl 1;
          if Result = -2 then
            DeltaLevel := Level
          else
            J := TokenKind;
          Dec(DeltaLevel);
          Dec(J);
          with AMatch do
          begin
            if DeltaLevel = 0 then
              ResetRange
            else
              SetRange(Lines.Ranges[DeltaLevel - 1]);
            SetLine(Lines.fList^[DeltaLevel].fString, DeltaLevel);
            while not GetEol and (J >= GetTokenPos + GetTokenLen) do
              Next;

            if GetEol then
            begin
              Result := Result shr 1;
              Exit;
            end;

            if Result = 2 then
            begin
              CloseToken := GetToken;
              CloseTokenPos.Line := Succ(DeltaLevel);
              if AComplete then
              begin
                OpenTokenPos.Char := OpenDupLen;
                CloseTokenPos.Char := CloseDupLen;
              end
              else
                CloseTokenPos.Char := Succ(GetTokenPos);
            end
            else begin
              OpenToken := GetToken;
              OpenTokenPos.Line := Succ(DeltaLevel);
              if AComplete then
              begin
                OpenTokenPos.Char := OpenDupLen;
                CloseTokenPos.Char := CloseDupLen;
              end
              else
                OpenTokenPos.Char := Succ(GetTokenPos);
            end;
          end;
        end;
        Exit;
      end;
    end;

    if AFoldingOnly then
      Exit;

    TokenKind := GetTokenKind;
    TokenRange := GetRange;
    I := 0;
    J := Length(ATokens);
    while I < J do
    begin
      if (TokenKind = ATokens[I].OpenTokenKind) or (TokenKind = ATokens[I].CloseTokenKind) then
      begin
        if sroCaseSensitive in TSynRange(TokenRange).Options then
          Token := GetToken
        else
          Token := LowerCase(GetToken);
        if ATokens[I].Backreference then
        begin
          BackreferenceToken := Token;
          if (TokenRange = ATokens[I].OpenRange) then
          begin
            Result := 1;
            AMatch.OpenToken := GetToken;
            AMatch.OpenTokenPos.Line := APoint.Line + 1;
            AMatch.OpenTokenPos.Char := GetTokenPos + 1;
            Break;
          end
          else if (TokenRange = ATokens[I].CloseRange) then
          begin
            Result := -1;
            AMatch.CloseToken := GetToken;
            AMatch.CloseTokenPos.Line := APoint.Line + 1;
            AMatch.CloseTokenPos.Char := GetTokenPos + 1;
            if AComplete then
              Inc(AMatch.CloseTokenPos.Char, GetTokenLen);
            Break;
          end;
        end
        else begin
          if (Token = ATokens[I].OpenToken) and (TokenKind = ATokens[I].OpenTokenKind) then
          begin
            Result := 1;
            AMatch.OpenToken := GetToken;
            AMatch.OpenTokenPos.Line := APoint.Line + 1;
            AMatch.OpenTokenPos.Char := GetTokenPos + 1;
            Break;
          end
          else if (Token = ATokens[I].CloseToken) and (TokenKind = ATokens[I].CloseTokenKind) then
          begin
            Result := -1;
            AMatch.CloseToken := GetToken;
            AMatch.CloseTokenPos.Line := APoint.Line + 1;
            AMatch.CloseTokenPos.Char := GetTokenPos + 1;
            if AComplete then
              Inc(AMatch.CloseTokenPos.Char, GetTokenLen);
            Break;
          end;
        end;
      end;
      Inc(I);
    end;
    if Result = 0 then
      Exit;
    TokenMatch := @ATokens[I];
    AMatch.TokenKind := TokenKind;
    AMatch.TokenAttri := GetTokenAttribute;
    if not TokenMatch^.Backreference then
    begin
      if J > Length(FMatchOpenDup) then
      begin
        SetLength(FMatchOpenDup, J);
        SetLength(FMatchCloseDup, J);
      end;
      OpenDupLen := 0;
      CloseDupLen := 0;
      for I:=0 to J-1 do
        if (TokenKind = ATokens[I].OpenTokenKind) or (TokenKind = ATokens[I].CloseTokenKind) then
        begin
          if (TokenMatch^.OpenToken = ATokens[I].OpenToken) and (TokenMatch^.OpenTokenKind = ATokens[I].OpenTokenKind) then
          begin
            FMatchCloseDup[CloseDupLen] := I;
            Inc(CloseDupLen);
          end;
          if (TokenMatch^.CloseToken = ATokens[I].CloseToken) and (TokenMatch^.CloseTokenKind = ATokens[I].CloseTokenKind) then
          begin
            FMatchOpenDup[OpenDupLen] := I;
            Inc(OpenDupLen);
          end;
        end;
    end;
    if Result = 1 then
    begin
      Level := 1;
      Next;
      while True do
      begin
        while not GetEol do
          if CheckToken then
            Exit;

        Inc(APoint.Line);

        if APoint.Line >= ASynEdit.Lines.Count then
          Break;

        SetLine(Lines[APoint.Line], APoint.Line);
      end;

    end else
    begin
      if Length(FMatchStack) < 32 then
        SetLength(FMatchStack, 32);
      FMatchStackID := -1;
      Level := -1;
      if APoint.Line = 0 then
        ResetRange
      else
        SetRange(Lines.Ranges[APoint.Line - 1]);
      SetLine(Lines[APoint.Line], APoint.Line);
      while not GetEol and (GetTokenPos < AMatch.CloseTokenPos.Char -1) do
        CheckTokenBack;
      if FMatchStackID > -1 then
      begin
        Result := -2;
        AMatch.OpenToken := FMatchStack[FMatchStackID].Token;
        AMatch.OpenTokenPos := FMatchStack[FMatchStackID].Pos;
      end
      else
        while APoint.Line > 0 do
        begin
          DeltaLevel := -Level - 1;
          Dec(APoint.Line);
          if APoint.Line = 0 then
            ResetRange
          else
            SetRange(Lines.Ranges[APoint.Line - 1]);
          SetLine(Lines[APoint.Line], APoint.Line);
          FMatchStackID := -1;
          while not GetEol do
          begin
            CheckTokenBack;
            if Result = 0 then Break;
          end;
          if (DeltaLevel <= FMatchStackID) then
          begin
            Result := -2;
            AMatch.OpenToken := FMatchStack[FMatchStackID - DeltaLevel].Token;
            AMatch.OpenTokenPos := FMatchStack[FMatchStackID - DeltaLevel].Pos;
            Exit;
          end
          else if Result = 0 then Exit;
        end;
    end;
  end;
end;

function SynEditGetMatchingTokenEx(ASynEdit: TCustomSynEdit; APoint: TBufferCoord;
  const ATokens: array of TSynTokenMatch; var AMatch: TSynTokenMatched;
  AComplete: Boolean = False; AFoldingOnly: Boolean = False): Integer;
begin
  Result := SynEditGetMatchingToken(ASynEdit, APoint, ATokens, AMatch, AComplete,
    AFoldingOnly);
  if (Result = 0) and (APoint.Char > 1) then
  begin
    Dec(APoint.Char);
    Result := SynEditGetMatchingToken(ASynEdit, APoint, ATokens, AMatch,
      AComplete, AFoldingOnly);
  end;
end;

function SynEditGetMatchingPartKind(ASynEdit: TCustomSynEdit; const ALine: Integer;
  const ATokens: array of TSynTokenMatch; AFoldingOnly: Boolean = False): Integer;
var
  I, J: Integer;
  Cache: PSynUniCacheItem;
  bDummyOpen, bDummyClose: Boolean;
  Token: UnicodeString;
  TokenKind: Integer;
  TokenRange: Pointer;
begin
  Result := 0;
  if ASynEdit.Highlighter = nil then
    Exit;
  with ASynEdit, ASynEdit.Highlighter do
  begin

    { Outlining match? }
    Cache := (ASynEdit.Highlighter as TSynUniSyn).Cache[ALine];
    if not Cache^.AOutlined then
      SynEditGetOpenCloseFoldingRange(ASynEdit, ALine, FoldRegions, bDummyOpen, bDummyClose);
    for I := 0 to High(Cache^.AOutliningMap^) do
    begin
      if (Cache^.AOutliningMap^[I].ATokenIndex = Succ(GetTokenPos))
        or (Cache^.AOutliningMap^[I].AIndex = Succ(GetTokenPos)) then
      begin
        if Cache^.AOutliningMap^[I].AOpen then
          Result := 1
        else
          Result := -1;
        Exit;
      end;
    end;

    if AFoldingOnly then
      Exit;

    TokenKind := GetTokenKind;
    TokenRange := GetRange;
    I := 0;
    J := Length(ATokens);
    while I < J do
    begin
      if (TokenKind = ATokens[I].OpenTokenKind) or (TokenKind = ATokens[I].CloseTokenKind) then
      begin
        if sroCaseSensitive in TSynRange(TokenRange).Options then
          Token := GetToken
        else
          Token := LowerCase(GetToken);
        if ATokens[I].Backreference then
        begin
          if (TokenRange = ATokens[I].OpenRange) then
          begin
            Result := 1;
            Break;
          end
          else if (TokenRange = ATokens[I].CloseRange) then
          begin
            Result := -1;
            Break;
          end;
        end
        else begin
          if (Token = ATokens[I].OpenToken) and (TokenKind = ATokens[I].OpenTokenKind) then
          begin
            Result := 1;
            Break;
          end
          else if (Token = ATokens[I].CloseToken) and (TokenKind = ATokens[I].CloseTokenKind) then
          begin
            Result := -1;
            Break;
          end;
        end;
      end;
      Inc(I);
    end;
  end;
end;

{ Helper folding routines }

// -----------------------------------------------------------------------------

type
  TSynFoldingMatch = record
    Pos: TBufferCoord;   // Position of match in line
    Length: Integer;     // Length of line
    TokenIndex: Integer;
    Region: Integer;
    BrefIndex: Integer;
  end;

  TSynFoldingMatches = array of TSynFoldingMatch;

  TBrefItem = record
    Region: Integer;
    Close: UTF8String;
    BrefIndex: Integer;
  end;

  TBrefItems = array of TBrefItem;

const
  RegExOpts = [roIgnoreCase, roSingleline];

// -----------------------------------------------------------------------------
// Add found match to open or close preparation stack
procedure AddMatch(ALine, ACharPos, ARegion: Integer;
  var Arr: TSynFoldingMatches; ABrefIndex: Integer = 0);
var
  P: Integer;
begin
  P := Length(Arr);
  SetLength(Arr, Length(Arr) + 1);
  Arr[P].Pos := BufferCoord(ACharPos, ALine);
  Arr[P].Region := ARegion;
  Arr[P].BrefIndex := ABrefIndex;
end;


// -----------------------------------------------------------------------------
// Sort matches as they appear on a line
procedure SortMatches(const Arr: TSynFoldingMatches;
  Ascending: Boolean = True);

  { Compare routine }
  function Compare(Index1, Index2: Integer): Integer;
  begin
    if Arr[Index1].Pos.Char < Arr[Index2].Pos.Char then
      Result := -1
    else if Arr[Index1].Pos.Char > Arr[Index2].Pos.Char then
      Result := 1
    else
      Result := 0;
    if not Ascending then
      Result := -Result;
  end;

  { Exchange routine }
  procedure Exchange(Index1, Index2: Integer);
  var
    Temp: TSynFoldingMatch;
  begin
    Temp := Arr[Index1];
    Arr[Index1] := Arr[Index2];
    Arr[Index2] := Temp;
  end;

  { Quick sort }
  procedure QuickSort(L, R: Integer);
  var
    I, J, P: Integer;
  begin
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        while Compare(I, P) < 0 do Inc(I);
        while Compare(J, P) > 0 do Dec(J);
        if I <= J then
        begin
          if I <> J then
            Exchange(I, J);
          if P = I then
            P := J
          else if P = J then
            P := I;
          Inc(I);
          Dec(J);
        end;
      until
        I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until
      I >= R;
  end;

begin
  if Length(Arr) = 0 then
    Exit;
  QuickSort(0, High(Arr));
end;

{ Checks if there are indentation rules to apply }
function CheckIndentationRules(const ARegions: TFoldRegions): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Pred(ARegions.Count) do
    if ARegions[I].Kind = frikIndentation then
    begin
      Result := True;
      Break;
    end;
end;

{ Check a line for a certain indentation rule match }
function CheckIndentationRegion(ASynEdit: TCustomSynEdit;
  const ARegions: TFoldRegions; ARegion, ALine: Integer;
  var Leading: Integer): Boolean;
var
  S: UTF8String;
  Match: IMatch;
begin
  { Initialize }
  Result := True;

  { Get leading length }
  Leading := Succ(GetLineLeadingWhite(ASynEdit, ALine));

  { Fetch string }
  S := UnicodeStringToUTF8(ASynEdit.Lines.fList[Pred(ALine)].fString);

  { Check whitespace pattern }
  if ARegions[ARegion].Open <> EmptyAnsiStr then
  begin
    Match := TRegex.Match(S, ARegions[ARegion].Open, RegExOpts);
    if not Match.Success or (Match.Index > 1) then
      Result := False;
  end;

  { Check token kind }
  if Result then
    if (ARegions[ARegion].OpenTokenKind > -1) and
      (GetTokenKind(ASynEdit, BufferCoord(Leading, ALine)) <> ARegions[ARegion].OpenTokenKind)
    then
      Result := False;

  { Check range }
  if Result then
    if ARegions[ARegion].OpenRng <> nil then
    begin
      GetTokenKind(ASynEdit, BufferCoord(1, ALine));
      if ASynEdit.Highlighter.GetRange <> ARegions[ARegion].OpenRng then
        Result := False;
    end;
end;

{ Main folding routines start }

// -----------------------------------------------------------------------------

const
  sToken: UTF8String = 'token';

// -----------------------------------------------------------------------------
// Find out if there is unmatched open or close part on line
procedure SynEditGetOpenCloseFoldingRange(ASynEdit: TCustomSynEdit;
  ALine: Integer; const ARegions: TFoldRegions; var AOpen, AClose: Boolean;
  IndentationOnly: Boolean = False; NoIndentation: Boolean = False);

  { Add backreference close item for later search }
  procedure AddBrefItem(ARegion: Integer; const AClose: UnicodeString;
    var BrefItems: TBrefItems; var AUnique: Integer; Increment: Boolean = True);
  begin
    SetLength(BrefItems, Succ(Length(BrefItems)));
    with BrefItems[High(BrefItems)] do
    begin
      Region := ARegion;
      Close := AClose;
      BrefIndex := AUnique;
    end;
    if Increment then
      Inc(AUnique);
  end;

  { Marks backreference item as checked }
  procedure AddBrefCheckItem(APos: Integer; var ABrefChecks: TIntegerArray);
  begin
    SetLength(ABrefChecks, Succ(Length(ABrefChecks)));
    ABrefChecks[High(ABrefChecks)] := APos;
  end;

var
  { Highlighter cache }
  Cache: PSynUniCacheItem;

  { A map of regions which 100% aren't in line }
  FailedOpenRegions, FailedCloseRegions: array of Boolean;

  { Line. All regex operations are done on UTF8 strings.
    We must convert our Unicode string to it's UTF8 equivalent,
    do match and convert UTF8 index matches to UTF16 coordinates }
  S: UTF8String;

  { Put a valid match in outlining map }
  procedure AddOutliningMatch(const ARgn: TFoldRegionItem;
    const APos, ALen: Integer; const AOpn: Boolean; const ATkn: Integer);
  begin
    SetLength(Cache^.AOutliningMap^, Succ(Length(Cache^.AOutliningMap^)));
    with Cache^.AOutliningMap^[High(Cache^.AOutliningMap^)] do
    begin
      AIndex := APos;
      ALength := ALen;
      ATokenIndex := ATkn;
      AOpen := AOpn;
      ARegion := ARgn;
    end;
  end;

  { Obtain next search bounds for a given region.
    P - last index in map;
    B - last begin pos, will contain new one;
    E - same for end pos;
    Returns False if no further block to search found }
  (*function GetSearchBounds(ARgn: TFoldRegionItem; const AOpn: Boolean;
    var P, B, E: Integer): Boolean;
  var
    Tkn: TSynHighlighterAttributes;
    Range, LineRange: TSynRange;
    Map: PSynRangeMatchArray;
  begin
    { Initialize }
    Result := False;
    Map := Cache^.AMap;
    if ALine = 1 then
      LineRange := (ASynEdit.Highlighter as TSynUniSyn).Rules
    else
      LineRange := ASynEdit.Lines.Ranges[ALine - 2];
    if LineRange.Linked then
      LineRange := LineRange.LinkSource;

    { Get token and it's rule }
    if AOpn then
      if ARgn.OpenTokenKind = -1 then
        Tkn := nil
      else
        Tkn := TSynHighlighterAttributes(ARgn.OpenTokenKind)
    else
      if ARgn.CloseTokenKind = -1 then
        Tkn := nil
      else
        Tkn := TSynHighlighterAttributes(ARgn.CloseTokenKind);
    if Tkn = nil then
    begin
      Result := False;
      Exit;
    end;

    if Tkn.Rule is TSynRange then
      Range := Tkn.Rule as TSynRange
    else
      Range := (Tkn.Rule as TSynAbstractRule).Parent;

    { Get start }
    if (P = 0) and (LineRange = Range) then
      B := 1
    else begin
      while (P < Length(Map^)) and ((Map^[P].ARange <> Range) and (Map^[P].ARange.LinkSource <> Range)) do
        Inc(P);
      if P >= Length(Map^) then Exit;
      if Map^[P].AOpen then
      begin
        B := Map^[P].AIndex;
        if not (stoInside in Map^[P].AToken.Options) then Inc(B, Map^[P].ALength);
      end
      else begin
        if P = 0 then
          B := 1
        else begin
          B := Map^[Pred(P)].AIndex;
          if (stoInside in Map^[Pred(P)].AToken.Options) then Inc(B, Map^[Pred(P)].ALength);
        end;
      end;
      Inc(P);
    end;

    { Get end }
    if P >= Length(Map^) then
      E := Length(S)
    else begin
      E := Map^[P].AIndex - 1;
      if (((Map^[P].ARange = Range) or (Map^[P].ARange.LinkSource = Range)) and (stoInside in Map^[P].AToken.Options)) or not (stoInside in Map^[P].AToken.Options) then
        Inc(E, Map^[P].ALength);
    end;
    Inc(P);

    { Done }
    Result := True;
  end;*)

  procedure AnalyzeFailedRegions;
  var
    I: Integer;
    Tkn: TSynHighlighterAttributes;
  begin
    { Initialzie token arrays }
    SetLength(FailedOpenRegions, ARegions.Count);
    for I := 0 to Pred(ARegions.Count) do
      FailedOpenRegions[I] := True;
    SetLength(FailedCloseRegions, ARegions.Count);
    for I := 0 to Pred(ARegions.Count) do
      FailedCloseRegions[I] := True;

    { Prepare line }
    with ASynEdit.Highlighter do
    begin
      if ALine = 1 then
        ResetRange
      else
        SetRange(ASynEdit.Lines.Ranges[ALine - 2]);
      SetLine(ASynEdit.Lines.fList^[Pred(ALine)].fString, Pred(ALine));

      { Walk through line }
      while not GetEol do
      begin
        for I := 0 to Pred(ARegions.Count) do
        begin
          if GetTokenKind = ARegions[I].OpenTokenKind then
            FailedOpenRegions[I] := False;
          if GetTokenKind = ARegions[I].CloseTokenKind then
            FailedCloseRegions[I] := False;
        end;
        Next;
      end;
    end;
  end;

var
  { Counters }
  I, J, K, C, D, P, B, E: Integer;
  bDummy: Boolean;

  { Support variables for backreference matching }
  BrefItems: TBrefItems;
  BrefUnique: Integer;
  BrefChecks: TIntegerArray;
  BrefChecked: Boolean;

  { Found open and close matches }
  Match: IMatch;

  { Matches on line being analyzed }
  LineOpenMatches, LineCloseMatches: TSynFoldingMatches;

  { Duplications }
  CloseDupTable, OpenDupTable: array of array of Integer;
  CloseDupSeal, OpenDupSeal: array of Boolean;
begin
  { Initialize }
  AOpen := False;
  AClose := False;

  { Empty? }
  if (ALine < 1) or (ALine > ASynEdit.Lines.Count) then
    Exit;

  { Try to read from cache if there were no changes so far on line }
  Cache := (ASynEdit.Highlighter as TSynUniSyn).Cache[Pred(ALine)];
  if Cache = nil then
    Exit;

  { Read from cache }
  if Cache^.AOutlined then
  begin
    if not IndentationOnly then
    begin
      AOpen := Cache^.ACollapseOpen;
      AClose := Cache^.ACollapseClose;
      if not NoIndentation then
      begin
        if not AOpen then
          AOpen := Cache^.AIndentationOpen;
        if not AClose then
          AClose := Cache^.AIndentationClose;
      end;
      Exit;
    end
    else begin
      AOpen := Cache^.ACollapseOpen;
      AClose := Cache^.ACollapseClose;
    end;
  end
  else begin
    if IndentationOnly then
      Exit
    else
      SetLength(Cache^.AOutliningMap^, 0);
  end;

  { First we will try to match ordinary fold region items and then,
    if first were no success, we will try to match indentation regions.
    Check if can do indentation check }
  if IndentationOnly and Cache^.AOutlined and
    (Cache^.ACollapseOpen or Cache^.ACollapseClose)
  then
    Exit;

  { Fetch string }
  S := UnicodeStringToUTF8(ASynEdit.Lines.fList[Pred(ALine)].fString);

  { Prepare arrays }
  BrefUnique := 1;
  SetLength(BrefItems, 0);
  SetLength(BrefChecks, 0);

  { Close duplicates }
  SetLength(CloseDupTable, ARegions.Count);
  SetLength(CloseDupSeal, ARegions.Count);
  for I := 0 to Pred(ARegions.Count) do
  begin
    if (ARegions[I].Kind = frikIndentation) or IndentationOnly then
      Continue;
    SetLength(CloseDupTable[I], 0);
    for J := 0 to Pred(ARegions.Count) do
    begin
      if I = J then
        Continue;
      if (ARegions[I].CloseTokenKind = ARegions[J].CloseTokenKind) and
        (ARegions[I].Close = ARegions[J].Close)
      then begin
        SetLength(CloseDupTable[I], Length(CloseDupTable[I]) + 1);
        CloseDupTable[I][High(CloseDupTable[I])] := J;
      end;
    end;
  end;

  { Open duplicates }
  SetLength(OpenDupTable, ARegions.Count);
  SetLength(OpenDupSeal, ARegions.Count);
  for I := 0 to Pred(ARegions.Count) do
  begin
    if (ARegions[I].Kind = frikIndentation) or IndentationOnly then
      Continue;
    SetLength(OpenDupTable[I], 0);
    for J := 0 to Pred(ARegions.Count) do
    begin
      if I = J then
        Continue;
      if (ARegions[I].OpenTokenKind = ARegions[J].OpenTokenKind) and
        (ARegions[I].Open = ARegions[J].Open)
      then begin
        SetLength(OpenDupTable[I], Length(OpenDupTable[I]) + 1);
        OpenDupTable[I][High(OpenDupTable[I])] := J;
      end;
    end;
  end;

  { get failed states }
  AnalyzeFailedRegions;

  { Find all open matches on that line }
  SetLength(LineOpenMatches, 0);
  for J := 0 to Pred(ARegions.Count) do
  begin

    { Skip indentation regions here }
    if (ARegions[J].Kind = frikIndentation) or IndentationOnly then
      Continue;

    { Seal duplicate regions }
    for K := 0 to High(OpenDupTable[J]) do
      OpenDupSeal[OpenDupTable[J][K]] := True;

    { See if region being analyzed is a duplicate and has been sealed }
    if OpenDupSeal[J] then
      Continue;

    { See if failed }
    if FailedOpenRegions[J] then
      Continue;

    { Otherwise, can match }
    P := 0; B := 1; E := 1;
    {while GetSearchBounds(ARegions[J], True, P, B, E) do
    begin}
      Match := TRegex.Match(S, ARegions[J].Open, RegExOpts{, B, E});
      while Match.Success do
      begin

        { Get real position in string }
        C := CountUTF8Chars(S, Match.Index);

        { Check token kind }
        if ARegions[J].OpenTokenKind = GetTokenKind(ASynEdit, BufferCoord(C, ALine)) then
        begin

          { Add to outlining map }
          K := CountUTF8Chars(S, Match.Groups.ItemsByName[sToken].Index, Match.Index);
          D := CountUTF8Chars(S, Match.Index + Match.Length, Succ(Match.Index));
          AddOutliningMatch(ARegions[J], C, D, True, (Pred(C) + K) * Ord(K > 0));

          { Backreferenced regions require special treatment to match their closing paris }
          if ARegions[J].CloseBackreference then
          begin

            AddMatch(D, C, J, LineOpenMatches, BrefUnique);

            { Fill special items for closing part match }
            AddBrefItem(J, Match.Result(ARegions[J].Close), BrefItems, BrefUnique, False);
            for K := 0 to High(OpenDupTable[J]) do
              if Aregions[OpenDupTable[J][K]].CloseBackreference then
                AddBrefItem(OpenDupTable[J][K], Match.Result(ARegions[OpenDupTable[J][K]].Close),
                  BrefItems, BrefUnique, False);
            Inc(BrefUnique);
          end

          { Simply add }
          else
            AddMatch(D, C, J, LineOpenMatches);
        end;

        { Proceed to the next match }
        Match := Match.NextMatch;
      end;
    //end;
  end;
  SortMatches(LineOpenMatches);

  { Find all close matches on that line }
  SetLength(LineCloseMatches, 0);

  { First, match all non-backreferenced regions }
  if not IndentationOnly then
  for J := 0 to ARegions.Count - 1 do
    if not ARegions[J].CloseBackreference and (ARegions[J].Kind = frikNormal) then
    begin

      { Seal duplicate regions }
      for K := 0 to High(CloseDupTable[J]) do
        CloseDupSeal[CloseDupTable[J][K]] := True;

      { See if region being analyzed is a duplicate and has been sealed }
      if CloseDupSeal[J] then
        Continue;

      { See if failed }
      if FailedCloseRegions[J] then
        Continue;

      { Otherwise, can match }
      P := 0; B := 1; E := 1;
      {while GetSearchBounds(ARegions[J], False, P, B, E) do
      begin}
        Match := TRegex.Match(S, ARegions[J].Close, RegExOpts{, B, E});
        while Match.Success do
        begin

          { Get real position in string }
          C := CountUTF8Chars(S, Match.Index);

          { Check token kind }
          if ARegions[J].CloseTokenKind = GetTokenKind(ASynEdit, BufferCoord(C, ALine)) then
          begin

            { Add to outlining map }
            K := CountUTF8Chars(S, Match.Groups.ItemsByName[sToken].Index, Match.Index);
            D := CountUTF8Chars(S, Match.Index + Match.Length, Succ(Match.Index));
            AddOutliningMatch(ARegions[J], C, D, False, (Pred(C) + K) * Ord(K > 0));

            { Simply add in stack }
            AddMatch(D, C, J, LineCloseMatches);
          end;

          { Proceed to the next match }
          Match := Match.NextMatch;
        end;
      //end;
    end;

  { Now, search for backreference replacements added from matching opens.
    We match only once here since the amount off collected replacements is
    equal to amount of detected open parts }
  for J := High(BrefItems) downto 0 do
  begin
    if FailedCloseRegions[BrefItems[J].Region] then
      Continue;

    { Match }
    P := 0; B := 1; E := 1;
    {while GetSearchBounds(ARegions[BrefItems[J].Region], False, P, B, E) do
    begin}
      Match := TRegex.Match(S, BrefItems[J].Close, RegExOpts{, B, E});
      while Match.Success do
      begin

        { Get real position in string }
        C := CountUTF8Chars(S, Match.Index);

        { Check token kind }
        if ARegions[BrefItems[J].Region].CloseTokenKind = GetTokenKind(ASynEdit, BufferCoord(C, ALine)) then
        begin

          { Check if it already aws picked up by other bref check }
          K := 0;
          for I := 0 to High(BrefChecks) do
            if BrefChecks[I] = C then
            begin
              K := 1;
              Break;
            end;

          { Can put? }
          if K = 0 then
          begin

            { Put in close parts stack }
            AddMatch(Match.Length{ Garnet: not counted! (NEED FIX) }, C, BrefItems[J].Region, LineCloseMatches,
              BrefItems[J].BrefIndex);

            { Mark this part as checked }
            AddBrefCheckItem(C, BrefChecks);

            { Disable match }
            Match := TRegex.Match(EmptyAnsiStr, #32);
          end
          else
            Match := Match.NextMatch;
        end
        else
          Match := Match.NextMatch;
      end;
    //end;
  end;

  { Complete outlining map and do additional checks for backreferences items }
  BrefChecked := False;
  if not IndentationOnly then
  for J := 0 to Pred(ARegions.Count) do
    if ARegions[J].CloseBackreference and (ARegions[J].Kind = frikNormal) then
    begin

      { Seal duplicate regions }
      for K := 0 to High(CloseDupTable[J]) do
        CloseDupSeal[CloseDupTable[J][K]] := True;

      { See if region being analyzed is a duplicate and has been sealed }
      if CloseDupSeal[J] then
        Continue;

      if FailedCloseRegions[J] then
        Continue;

      { Otherwise, can match }
      P := 0; B := 1; E := 1;
      //while GetSearchBounds(ARegions[J], False, P, B, E) do
      //begin
        Match := TRegex.Match(S, ARegions[J].CloseBref, RegExOpts{, B, E});
        while Match.Success do
        begin

          { Get real position in string }
          C := CountUTF8Chars(S, Match.Index);

          { Check token kind }
          if ARegions[J].CloseTokenKind = GetTokenKind(ASynEdit, BufferCoord(C, ALine)) then
          begin

            { Add to outlining map }
            K := CountUTF8Chars(S, Match.Groups.ItemsByName[sToken].Index, Match.Index);
            AddOutliningMatch(ARegions[J], C, CountUTF8Chars(S,
              Match.Index + Match.Length, Succ(Match.Index)),
              False, (Pred(C) + K) * Ord(K > 0));

            { Is still not checked }
            if not BrefChecked then
            begin
              BrefChecked := True;
              for K := 0 to High(BrefChecks) do
                if C = BrefChecks[K] then
                begin
                  BrefChecked := False;
                  Break;
                end;
            end;
          end;

          { Proceed to the next match }
          Match := Match.NextMatch;
        end;
      //end;
    end;

  { Sort and add }
  SortMatches(LineCloseMatches, False);

  { Look what we got }
  AOpen := Length(LineOpenMatches) > Length(LineCloseMatches);
  AClose := Length(LineCloseMatches) > Length(LineOpenMatches);

  { Give result }
  if not AClose then
    AClose := BrefChecked;

  { Even more checks }
  if not AOpen and not AClose then
    if (Length(LineOpenMatches) > 0) and (Length(LineCloseMatches) > 0) then
      if (Length(LineOpenMatches) = Length(LineCloseMatches)) then
      begin
        if (LineOpenMatches[High(LineOpenMatches)].Pos.Char > LineCloseMatches[High(LineCloseMatches)].Pos.Char) then
          AOpen := True;
        if (LineCloseMatches[0].Pos.Char < LineOpenMatches[0].Pos.Char) then
          AClose := True;
      end;

  { Try to match indentation regions now if no matches so far from ordinary }
  BrefChecked := False;
  bDummy := False;
  if not NoIndentation and CheckIndentationRules(ARegions) and not AOpen and not AClose then
  begin

    { See if any of indentation regions match whitespace on this line }
    for J := 0 to Pred(ARegions.Count) do
      if ARegions[J].Kind = frikIndentation then
        if CheckIndentationRegion(ASynEdit, ARegions, J, ALine, K) then
        begin

          { In continious mode the line can be defined as open if
            amount of whitespace pn previous line is not equal and amount
            of whitespace on next is equal. We also will need to check next
            line for scope }
          if ARegions[J].CloseBackreference and not IsLineAllWhite(ASynEdit, ALine) then
          begin

            { Continious regions must span across at lesat three lines }
            BrefChecked := ((ALine = 1) or not CheckIndentationRegion(ASynEdit, ARegions, J, ALine - 1, C)) and
              (ALine < ASynEdit.Lines.Count - 1) and
              CheckIndentationRegion(ASynEdit, ARegions, J, ALine + 1, C) and
              CheckIndentationRegion(ASynEdit, ARegions, J, ALine + 2, C);

            { Continious regions must span across at lesat three lines }
            bDummy := ((ALine = ASynEdit.Lines.Count) or not CheckIndentationRegion(ASynEdit, ARegions, J, ALine + 1, C)) and
              (ALine > 2) and
              CheckIndentationRegion(ASynEdit, ARegions, J, ALine - 1, C) and
              CheckIndentationRegion(ASynEdit, ARegions, J, ALine - 2, C);
          end

          { This line can be defined as open if amount of whitespace on it is
            less than on next. And line can be defined as close if amount of
            whitespace on it is less than on previous }
          else begin
            if not IsLineAllWhite(ASynEdit, ALine) then
            begin
              for I := Succ(ALine) to ASynEdit.Lines.Count do
                if not IsLineAllWhite(ASynEdit, I) then
                begin
                  if K < Succ(GetLineLeadingWhite(ASynEdit, I)) then
                    BrefChecked := True;
                  Break;
                end;

              if not ((K = 1) and (IsLineAllWhite(ASynEdit, Pred(ALine)))) then
                for I := Pred(ALine) downto 1 do
                  if not IsLineAllWhite(ASynEdit, I) then
                  begin
                    if K < Succ(GetLineLeadingWhite(ASynEdit, I)) then
                      bDummy := True;
                    Break;
                  end;
            end;

            { Additional checks for close }
            if not bDummy and IsLineAllWhite(ASynEdit, ALine) and (ALine > 1) then
              if ((ALine = ASynEdit.Lines.Count) or not IsLineAllWhite(ASynEdit, Succ(ALine))) and
                ((ALine = ASynEdit.Lines.Count) or (GetLineLeadingWhite(ASynEdit, Succ(ALine)) = 0))
              then
                for I := Pred(ALine) downto 1 do
                  if not IsLineAllWhite(ASynEdit, I) then
                  begin
                    if GetLineLeadingWhite(ASynEdit, I) > 0 then
                      bDummy := True;
                    Break;
                  end;
          end;

          { Found? }
          if BrefChecked or bDummy then
            Break;
        end;
  end;

  { Cache }
  with Cache^ do
    if IndentationOnly then
    begin
      if (AIndentationOpen <> BrefChecked) or (AIndentationClose <> bDummy) then
        ASynEdit.InvalidateGutterLine(ALine);
      AIndentationOpen := BrefChecked;
      AIndentationClose := bDummy;
    end
    else begin
      ACollapseOpen := AOpen;
      ACollapseClose := AClose;
      AIndentationOpen := BrefChecked;
      AIndentationClose := bDummy;
      AOutlined := True;
    end;

  { Final result with indentation took into account }
  if not AOpen then
    AOpen := BrefChecked;
  if not AClose then
    AClose := bDummy;

  { Outline indentation regions on previous and next lines }
  if not NoIndentation and not IndentationOnly and CheckIndentationRules(ARegions) then
  begin

    { On previous line }
    for I := Pred(ALine) downto 1 do
    begin
      SynEditGetOpenCloseFoldingRange(ASynEdit, I, ARegions, BrefChecked, bDummy, True);
      if not IsLineAllWhite(ASynEdit, I) then
        Break;
    end;

    { On next line }
    for I := Succ(ALine) to ASynEdit.Lines.Count do
    begin
      SynEditGetOpenCloseFoldingRange(ASynEdit, I, ARegions, BrefChecked, bDummy, True);
      if not IsLineAllWhite(ASynEdit, I) then
        Break;
    end;
  end;

  { Free }
  for I := 0 to High(OpenDupTable) do
    SetLength(OpenDupTable[I], 0);
  SetLength(OpenDupTable, 0);
  for I := 0 to High(CloseDupTable) do
    SetLength(CloseDupTable[I], 0);
  SetLength(CloseDupTable, 0);
  SetLength(OpenDupSeal, 0);
  SetLength(CloseDupSeal, 0);
  SetLength(CloseDupTable, 0);
  SetLength(LineOpenMatches, 0);
  SetLength(LineCloseMatches, 0);
  SetLength(BrefItems, 0);
  SetLength(BrefChecks, 0);
end;

// -----------------------------------------------------------------------------

procedure SynEditCollpaseOpen(ASynEdit: TCustomSynEdit; ALine: Integer;
  const ARegions: TFoldRegions; var AFrom, ATo, AFromChar, AToChar,
  ARegion, AFromComplete, AToCompelte: Integer);
var
  { Line }
  S: UTF8String;

  { Counters }
  I, J, K, C, D, Matched, MatchedPos, Runner, RunnerEnd: Integer;
  Open, Close: array of UTF8String;

  { Found open and close matches }
  Match: IMatch;

  { Matches on line being analyzed }
  LineOpenMatches, LineCloseMatches: TSynFoldingMatches;

  { Duplications }
  OpenDupTable, CloseDupTable: array of Integer;
  OpenSingleDupes, CloseSingleDupes: TSynFoldingMatches;
  OpenSingleDupesChecked, CloseSingleDupesChecked: Boolean;
  CloseDupes: Integer;

  { Cache }
  Cache: PSynUniCacheItem;

  function DoCheckExit: Boolean;
  begin
    Result := False;
    if Matched > CloseDupes then
    begin
      AFrom := ALine;
      ATo := I;
      Result := True;
      MatchedPos := 1;
    end;
  end;

  procedure OpenSingleDupesCheck(ExplicitBorder: Integer = 0);
  var
    K: Integer;
  begin
    { Check }
    if not OpenSingleDupesChecked then
      Exit;

    { Get match bounds }
    if ExplicitBorder > 0 then
      K := ExplicitBorder
    else begin
      if (Length(LineOpenMatches) > 0) and (Length(LineCloseMatches) > 0) then
        K := Min(LineOpenMatches[0].Pos.Char, LineCloseMatches[0].Pos.Char)
      else if (Length(LineOpenMatches) > 0) then
        K := LineOpenMatches[0].Pos.Char
      else if (Length(LineCloseMatches) > 0) then
        K := LineCloseMatches[0].Pos.Char
      else
        K := Length(S);
    end;

    { Match }
    Match := TRegex.Match(S, ARegions[ARegion].Open, RegExOpts, 1, K);
    if Match.Success then
    begin

      { Analyze result set }
      while Match.Success do
      begin

        { Get real position in string }
        C := CountUTF8Chars(S, Match.Index);

        { Check token kind }
        if ARegions[ARegion].OpenTokenKind = GetTokenKind(ASynEdit, BufferCoord(C, I)) then
        begin

          { See if it's not a ARegion (which we are searching) open match }
          Runner := 0;
          for K := 0 to High(LineOpenMatches) do
            if LineOpenMatches[K].Pos.Char = C then
            begin
              Runner := 1;
              Break;
            end;

          { If not, we found a open part of some other item which makes
            our open single duplicate obsolete }
          if Runner = 0 then
          begin

            { If we are one the same line where it has been found, then
              delete it from stack as it's there and will cause false
              duplicate increase if not removed }
            if Length(LineCloseMatches) > 0 then
              SetLength(LineCloseMatches, Pred(Length(LineCloseMatches)));

            { Reset }
            OpenSingleDupesChecked := False;
            Break;
          end;
        end;
        Match := Match.NextMatch;
      end;
    end;
  end;

begin
  { Initialize }
  AFrom := 0;
  ATo := 0;

  { Empty? }
  if ASynEdit.Lines.Count = 0 then
    Exit;

  { Get a line }
  S := UnicodeStringToUTF8(ASynEdit.Lines.fList[Pred(ALine)].fString);

  { Try to read from cache if there were no changes so far on line }
  Cache := (ASynEdit.Highlighter as TSynUniSyn).Cache[Pred(ALine)];
  if not Cache^.AOutlined then
    SynEditGetOpenCloseFoldingRange(ASynEdit, ALine, ARegions, OpenSingleDupesChecked,
      CloseSingleDupesChecked);

  { Indentation rules have priority }
  if CheckIndentationRules(ARegions) and Cache^.AIndentationOpen and not Cache^.ACollapseOpen then
  begin

    { See if any of indentation regions match whitespace on this line }
    Matched := -1;
    for J := 0 to Pred(ARegions.Count) do
      if ARegions[J].Kind = frikIndentation then
        if CheckIndentationRegion(ASynEdit, ARegions, J, ALine, K) then
        begin
          Matched := J;
          Break;
        end;

    { Look for closing part }
    if Matched > -1 then
    begin

      { Iterate through lines }
      I := ALine + 2;
      while I <= ASynEdit.Lines.Count do
      begin
        if ARegions[Matched].CloseBackreference then
        begin
          if not CheckIndentationRegion(ASynEdit, ARegions, Matched, I, C) then
          begin
            Dec(I);
            Break;
          end;
        end
        else begin
          if CheckIndentationRegion(ASynEdit, ARegions, Matched, I, C) then
            if IsLineAllWhite(ASynEdit, I) then
            begin
              if I < ASynEdit.Lines.Count then
                if not IsLineAllWhite(ASynEdit, Succ(I)) and
                  CheckIndentationRegion(ASynEdit, ARegions, Matched, Succ(I), C)
                then
                  if C <= K then
                    Break;
            end
            else if C <= K then
              Break;
        end;
        Inc(I);
      end;
      I := Min(I, ASynEdit.Lines.Count);

      { Done }
      AFrom := ALine;
      ATo := I;
      AFromChar := K;
      AFromComplete := K;
      AToChar := C;
      AToCompelte := C;
      ARegion := Matched;
      Exit;
    end;
  end;

  { For open collpase we choose first open match if there's less close parts
    than open or last open match if they are in equal amounts }
  for I := 0 to High(Cache^.AOutliningMap^) do
    with Cache^.AOutliningMap^[I] do
      if AOpen then
        AddMatch(ALength, AIndex, ARegion.Index, LineOpenMatches)
      else
        AddMatch(ALength, AIndex, ARegion.Index, LineCloseMatches);

  { No matches? }
  if Length(LineOpenMatches) = 0 then
    Exit;

  { Find open region }
  SortMatches(LineOpenMatches);
  if AFromChar > 0 then
  begin
    for I := High(LineOpenMatches) downto 0 do
      if AFromChar >= LineOpenMatches[I].Pos.Char then
      begin
        ARegion := LineOpenMatches[I].Region;
        MatchedPos := LineOpenMatches[I].Pos.Char;
        AFromComplete := MatchedPos;
        D := LineOpenMatches[I].Pos.Line;
        Break;
      end;
  end
  else begin
    if (Length(LineOpenMatches) = Length(LineCloseMatches)) and
      (LineOpenMatches[High(LineOpenMatches)].Pos.Char > LineCloseMatches[High(LineCloseMatches)].Pos.Char) then
    begin
      ARegion := LineOpenMatches[High(LineOpenMatches)].Region;
      MatchedPos := LineOpenMatches[High(LineOpenMatches)].Pos.Char;
      D := LineOpenMatches[High(LineOpenMatches)].Pos.Line;
    end
    else begin
      ARegion := LineOpenMatches[0].Region;
      MatchedPos := LineOpenMatches[0].Pos.Char;
      D := LineOpenMatches[0].Pos.Line;
    end;
    AFromChar := MatchedPos;
    AFromComplete := MatchedPos;
  end;

  { Find out backreferenced parts }
  SetLength(Open, ARegions.Count);
  SetLength(Close, ARegions.Count);
  if ARegions[ARegion].CloseBackreference then
  begin
    Runner := CountUnicodeChars(S, MatchedPos);
    Match := TRegex.Match(S, ARegions[ARegion].Open, RegExOpts,
      Runner, Length(S));
    if not Match.Success then
      Exit;
    Open[ARegion] := Match.Result(ARegions[ARegion].OpenBref);
    Close[ARegion] := Match.Result(ARegions[ARegion].Close);
    Inc(Runner);
  end;

  { Close duplicates for that region }
  SetLength(CloseDupTable, 0);
  SetLength(CloseSingleDupes, 0);
  for I := 0 to Pred(ARegions.Count) do
  begin
    if I = ARegion then
      Continue;
    if (ARegions[I].CloseTokenKind = ARegions[ARegion].CloseTokenKind) and
      (ARegions[I].Close = ARegions[ARegion].Close)
    then begin
      SetLength(CloseDupTable, Length(CloseDupTable) + 1);
      CloseDupTable[High(CloseDupTable)] := I;
      if ARegions[I].CloseBackreference then
        if ARegions[ARegion].CloseBackreference then
          Open[I] := Match.Result(ARegions[I].OpenBref)
        else
          Open[I] := ARegions[I].Open
      else if ARegions[ARegion].CloseBackreference then
      begin
        SetLength(CloseSingleDupes, Succ(Length(CloseSingleDupes)));
        CloseSingleDupes[High(CloseSingleDupes)].Region := I;
      end;
    end;
  end;

  { Open duplicates fot that region }
  SetLength(OpenDupTable, 0);
  SetLength(OpenSingleDupes, 0);
  for I := 0 to Pred(ARegions.Count) do
  begin
    if I = ARegion then
      Continue;
    if (ARegions[I].OpenTokenKind = ARegions[ARegion].OpenTokenKind) and
      (ARegions[I].Open = ARegions[ARegion].Open)
    then begin
      SetLength(OpenDupTable, Length(OpenDupTable) + 1);
      OpenDupTable[High(OpenDupTable)] := I;
      if ARegions[I].CloseBackreference then
        if ARegions[ARegion].CloseBackreference then
          Close[I] := Match.Result(ARegions[I].Close)
        else
          Close[I] := ARegions[I].CloseBref
      else if ARegions[ARegion].CloseBackreference then
      begin
        SetLength(OpenSingleDupes, Succ(Length(OpenSingleDupes)));
        OpenSingleDupes[High(OpenSingleDupes)].Region := I;
      end;
    end;
  end;

  { Search for corresponding close part until BOF }
  Matched := 1;
  CloseDupes := 0;
  OpenSingleDupesChecked := False;
  for I := ALine to ASynEdit.Lines.Count do
  begin
    { Reset arrays }
    SetLength(LineOpenMatches, 0);
    SetLength(LineCloseMatches, 0);

    { Get this line in UTF8 }
    S := UnicodeStringToUTF8(ASynEdit.Lines.fList[Pred(I)].fString);

    { Add self }
    if ALine = I then
      AddMatch(D, MatchedPos, ARegion, LineOpenMatches);

    { Obtain number of open matches on that line }
    if not ARegions[ARegion].CloseBackreference then
      Match := TRegex.Match(S, ARegions[ARegion].Open, RegExOpts)
    else
      Match := TRegex.Match(S, Open[ARegion], RegExOpts);

    { Add and remove self }
    while Match.Success do
    begin
      C := CountUTF8Chars(S, Match.Index);
      if ARegions[ARegion].OpenTokenKind = GetTokenKind(ASynEdit, BufferCoord(C, I)) then
        if (I = ALine) and (C <= MatchedPos) then
          { Skip }
        else
          AddMatch(Match.Length, C, ARegion, LineOpenMatches);
      Match := Match.NextMatch;
    end;

    { Obtain number of close matches on that line }
    if not ARegions[ARegion].CloseBackreference then
      Match := TRegex.Match(S, ARegions[ARegion].Close, RegExOpts)
    else
      Match := TRegex.Match(S, Close[ARegion], RegExOpts);
    while Match.Success do
    begin

      { Get real position in string }
      C := CountUTF8Chars(S, Match.Index);

      { Check token kind }
      if ARegions[ARegion].CloseTokenKind = GetTokenKind(ASynEdit, BufferCoord(C, I)) then
        if (I = ALine) and (C < MatchedPos) then
          { Skip }
        else begin
          AddMatch(Match.Length, C, ARegion, LineCloseMatches);
          K := CountUnicodeChars(S, Match.Groups.ItemsByName[sToken].Index, Match.Index);
          if K = 1 then begin K := 0; Inc(C); end;
          LineCloseMatches[High(LineCloseMatches)].TokenIndex := Pred(C) + K;
        end;

      { Proceed to the next match }
      Match := Match.NextMatch;
    end;

    { Obtain number of open duplicates on that line }
    for J := 0 to High(CloseDupTable) do
    begin

      { Sealed? }
      if CloseDupTable[J] = -1 then Continue;

      { Choose action }
      if not ARegions[CloseDupTable[J]].CloseBackreference then
        if ARegions[ARegion].CloseBackreference then
          Continue
        else
          Match := TRegex.Match(S, ARegions[CloseDupTable[J]].Open, RegExOpts)
      else
        Match := TRegex.Match(S, Open[CloseDupTable[J]], RegExOpts);

      { Get matches }
      while Match.Success do
      begin

        { Get real position in string }
        C := CountUTF8Chars(S, Match.Index);

        { Check token kind }
        if ARegions[CloseDupTable[J]].OpenTokenKind = GetTokenKind(ASynEdit, BufferCoord(C, I)) then
          if (I = ALine) and (C <= MatchedPos) then
            { Skip }
          else
            AddMatch(Match.Length, C, CloseDupTable[J], LineOpenMatches);

        { Proceed to the next match }
        if not ARegions[CloseDupTable[J]].CloseBackreference and
          ARegions[ARegion].CloseBackreference then
        begin
          Match := TRegex.Match(EmptyAnsiStr, #32);
          CloseDupTable[J] := -1;
        end
        else
          Match := Match.NextMatch;
      end;
    end;

    { Obtain number of close duplicates on that line }
    for J := 0 to High(OpenDupTable) do
    begin

      { Sealed? }
      if OpenDupTable[J] = -1 then Continue;

      { Choose action }
      if not ARegions[OpenDupTable[J]].CloseBackreference then
        if ARegions[ARegion].CloseBackreference then
          Continue
        else
          Match := TRegex.Match(S, ARegions[OpenDupTable[J]].Close, RegExOpts)
      else
        Match := TRegex.Match(S, Close[OpenDupTable[J]], RegExOpts);

      { Get matches }
      while Match.Success do
      begin

        { Get real position in string }
        C := CountUTF8Chars(S, Match.Index);

        { Check token kind }
        if ARegions[OpenDupTable[J]].CloseTokenKind = GetTokenKind(ASynEdit, BufferCoord(C, I)) then
          if (I = ALine) and (C <= MatchedPos) then
            { Skip }
          else begin
            AddMatch(Match.Length, C, OpenDupTable[J], LineCloseMatches);
            K := CountUnicodeChars(S, Match.Groups.ItemsByName[sToken].Index, Match.Index);
            if K = 1 then begin K := 0; Inc(C); end;
            LineCloseMatches[High(LineCloseMatches)].TokenIndex := Pred(C) + K;
          end;

        { Seal if needed and proceed to the next match }
        if not ARegions[OpenDupTable[J]].CloseBackreference and
          ARegions[ARegion].CloseBackreference then
        begin
          Match := TRegex.Match(EmptyAnsiStr, #32);
          OpenDupTable[J] := -1;
        end
        else
          Match := Match.NextMatch;
      end;
    end;

    { Manage open single duplicates on that line }
    OpenSingleDupesCheck;
    if Length(OpenSingleDupes) > 0 then
    begin
      { Sort ascending }
      SortMatches(LineOpenMatches);
      SortMatches(LineCloseMatches);

      { Get }
      J := High(LineOpenMatches);

      { Iterate down to first }
      while J >= 0 do
      begin

        { Pretend there's no possible close counterpart }
        C := 0;

        { Needed to get correct close counterpart }
        if J = High(LineOpenMatches) then
          RunnerEnd := MaxInt
        else
          RunnerEnd := LineOpenMatches[Succ(J)].Pos.Char;

        { Try to find close counterpart }
        K := 0;
        while K <= High(LineCloseMatches) do
        begin
          if (LineCloseMatches[K].Pos.Char > LineOpenMatches[J].Pos.Char) and
            (ARegions[LineCloseMatches[K].Region].CloseBackreference) then
          begin
            if LineCloseMatches[K].Pos.Char < RunnerEnd then
              C := 1;
            Break;
          end;
          Inc(K);
        end;

        { Choose correct match borders }
        Runner := CountUnicodeChars(S, LineOpenMatches[J].Pos.Char);
        if C > 0 then
          if J < High(LineOpenMatches) then
            RunnerEnd := Min(CountUnicodeChars(S, LineOpenMatches[Succ(J)].Pos.Char),
              CountUnicodeChars(S, LineCloseMatches[K].Pos.Char))
          else
            RunnerEnd := CountUnicodeChars(S, LineCloseMatches[K].Pos.Char)
        else
          if J < High(LineOpenMatches) then
            RunnerEnd := CountUnicodeChars(S, LineOpenMatches[Succ(J)].Pos.Char)
          else
            RunnerEnd := Length(S);

        { See any of close single duplicates match }
        for K := 0 to High(OpenSingleDupes) do
        begin

          { Do match }
          Match := TRegex.Match(S, ARegions[OpenSingleDupes[K].Region].Close, RegExOpts, Runner, RunnerEnd);
          if Match.Success then
          begin

            { Get real position in string }
            C := CountUTF8Chars(S, Match.Index);

            { Check token kind and put in stack }
            if ARegions[OpenSingleDupes[K].Region].CloseTokenKind = GetTokenKind(ASynEdit, BufferCoord(C, I)) then
            begin
              AddMatch(Match.Length, C, OpenSingleDupes[K].Region, LineCloseMatches);
              Runner := CountUnicodeChars(S, Match.Groups.ItemsByName[sToken].Index, Match.Index);
              if Runner = 1 then begin Runner := 0; Inc(C); end;
              LineCloseMatches[High(LineCloseMatches)].TokenIndex := Pred(C) + Runner;
              Break;
            end;
          end;
        end;

        { Procceed to previus open match }
        Dec(J);
      end;

      { We still (sigh) aren't done with open single dupes. Need to look if
        there are any of them before any open and close parts }
      if OpenSingleDupesChecked then
      begin
        Runner := 1;
        if (Length(LineOpenMatches) = 0) and (Length(LineCloseMatches) = 0) then
          RunnerEnd := Length(S)
        else if (Length(LineOpenMatches) > 0) and (Length(LineCloseMatches) > 0) then
          RunnerEnd := Min(CountUnicodeChars(S, LineOpenMatches[0].Pos.Char),
            CountUnicodeChars(S, LineCloseMatches[0].Pos.Char))
        else if Length(LineOpenMatches) > 0 then
          RunnerEnd := CountUnicodeChars(S, LineOpenMatches[0].Pos.Char)
        else if Length(LineCloseMatches) > 0 then
          RunnerEnd := CountUnicodeChars(S, LineCloseMatches[0].Pos.Char)
        else
          RunnerEnd := Length(S);

        { See if any of open single duplicates match }
        for K := 0 to High(OpenSingleDupes) do
        begin

          { Do match }
          Match := TRegex.Match(S, ARegions[OpenSingleDupes[K].Region].Close, RegExOpts, Runner, RunnerEnd);
          if Match.Success then
          begin

            { Get real position in string }
            C := CountUTF8Chars(S, Match.Index);

            { Check token kind and put in stack }
            if ARegions[OpenSingleDupes[K].Region].CloseTokenKind = GetTokenKind(ASynEdit, BufferCoord(C, I)) then
            begin
              AddMatch(Match.Length, C, OpenSingleDupes[K].Region, LineCloseMatches);
              Runner := CountUnicodeChars(S, Match.Groups.ItemsByName[sToken].Index, Match.Index);
              if Runner = 1 then begin Runner := 0; Inc(C); end;
              LineCloseMatches[High(LineCloseMatches)].TokenIndex := Pred(C) + Runner;
              OpenSingleDupesChecked := False;
              Break;
            end;
          end;
        end;
      end;

      { TODO: recheck it more deeply. Looks like troublesome moment here with
        ALine }
      if not OpenSingleDupesChecked then
      begin
        if (I = ALine) and (Length(LineOpenMatches) = 1) and (Length(LineCloseMatches) = 0) then
          OpenSingleDupesChecked := True
        else if (I <> ALine) and (Length(LineOpenMatches) > 0) then
        begin
          OpenSingleDupesChecked := True;
          if Length(LineCloseMatches) > 0 then
            if LineCloseMatches[High(LineCloseMatches)].Pos.Char > LineOpenMatches[High(LineOpenMatches)].Pos.Char then
              OpenSingleDupesChecked := False;
        end;

      end;
    end;
    OpenSingleDupesCheck;

    { Anything found? }
    if (Length(LineOpenMatches) = 0) and (Length(LineCloseMatches) = 0) then
      Continue;

    { Sort stacks as they appear in text }
    SortMatches(LineOpenMatches);
    SortMatches(LineCloseMatches);

    { Analyze filled stacks }
    J := 0; // Open runner
    K := 0; // Close runner
    MatchedPos := 0; // Reuse to know we are finished
    if Length(LineOpenMatches) = 0 then
      Runner := LineCloseMatches[0].Pos.Char
    else if Length(LineCloseMatches) = 0 then
      Runner := LineOpenMatches[0].Pos.Char
    else
      Runner := Min(LineOpenMatches[0].Pos.Char, LineCloseMatches[0].Pos.Char);
    if Length(LineOpenMatches) = 0 then
      RunnerEnd := LineCloseMatches[High(LineCloseMatches)].Pos.Char
    else if Length(LineCloseMatches) = 0 then
      RunnerEnd := LineOpenMatches[High(LineOpenMatches)].Pos.Char
    else
      RunnerEnd := Max(LineOpenMatches[High(LineOpenMatches)].Pos.Char, LineCloseMatches[High(LineCloseMatches)].Pos.Char);

    while Runner <= RunnerEnd do
    begin
      if (Length(LineOpenMatches) > J) and (Runner = LineOpenMatches[J].Pos.Char) then
      begin
        Inc(CloseDupes);

        { Check, if we are done }
        if DoCheckExit then
          Break;

        { To next open }
        Inc(J);
      end
      else if (Length(LineCloseMatches) > K) and (Runner = LineCloseMatches[K].Pos.Char) then
      begin
        Inc(Matched);

        { Check, if we are done }
        if DoCheckExit then
        begin
          AToChar := LineCloseMatches[K].TokenIndex;
          D := CountUnicodeChars(S, LineCloseMatches[K].Pos.Char);
          AToCompelte := LineCloseMatches[K].Pos.Char + CountUTF8Chars(S, LineCloseMatches[K].Pos.Line + D, D);
          Break;
        end;

        { To next close }
        Inc(K);
      end;
      Inc(Runner);
    end;

    { Done! }
    if MatchedPos = 1 then
      Break;
  end;

  { Free }
  SetLength(OpenDupTable, 0);
  SetLength(CloseDupTable, 0);
  SetLength(LineOpenMatches, 0);
  SetLength(LineCloseMatches, 0);
end;

// -----------------------------------------------------------------------------

procedure SynEditCollpaseClose(ASynEdit: TCustomSynEdit; ALine: Integer;
  const ARegions: TFoldRegions; var AFrom, ATo, AFromChar, AToChar, ARegion,
  AFromComplete, AToComplete: Integer);
var
  { Line }
  S: UTF8String;

  { Counters }
  I, J, K, C, Matched, MatchedPos, Runner, RunnerEnd: Integer;
  Open, Close: array of UTF8String;

  { Found open and close matches }
  Match: IMatch;

  { Matches on line being analyzed }
  LineCloseMatches, LineOpenMatches: TSynFoldingMatches;

  { Duplications }
  OpenDupTable, CloseDupTable: array of Integer;
  OpenSingleDupes, CloseSingleDupes: TSynFoldingMatches;
  OpenSingleDupesChecked, CloseSingleDupesChecked: Boolean;
  CloseDupes: Integer;

  { Cache }
  Cache: PSynUniCacheItem;

  procedure OpenSingleDupesCheck(DecrementCloseDupes: Boolean = True);
  var
    K: Integer;
  begin
    if OpenSingleDupesChecked then
    begin

      { Get position from where to match }
      if (Length(LineCloseMatches) > 0) and DecrementCloseDupes then
      begin
        RunnerEnd := High(LineCloseMatches);
        if not ARegions[LineCloseMatches[RunnerEnd].Region].CloseBackreference then
          Dec(RunnerEnd);
        if RunnerEnd >= 0 then
          Runner := Succ(CountUnicodeChars(S, LineCloseMatches[RunnerEnd].Pos.Char))
        else
          Runner := 1;
      end
      else
        Runner := 1;

      { Match }
      Match := TRegex.Match(S, ARegions[ARegion].Open, RegExOpts, Runner, Length(s));
      if Match.Success then
      begin

        { Analyze result set }
        while Match.Success do
        begin

          { Get real position in string }
          C := CountUTF8Chars(S, Match.Index);

          { Check token kind }
          if ARegions[ARegion].OpenTokenKind = GetTokenKind(ASynEdit, BufferCoord(C, I)) then
          begin

            { See if it's not a ARegion (which we are searching) open match }
            Runner := 0;
            for K := 0 to High(LineOpenMatches) do
              if LineOpenMatches[K].Pos.Char = C then
              begin
                Runner := 1;
                Break;
              end;

            { If not, we found a open part of some other item which makes
              our open single duplicate obsolete }
            if Runner = 0 then
            begin

              { Omit duplicate created by our open dingle dupe }
              if DecrementCloseDupes then
                Dec(CloseDupes)

              { If we are one the same line where it has been found, then
                delete it from stack as it's there and will cause false
                duplicate increase if not removed }
              else
                SetLength(LineCloseMatches, Pred(Length(LineCloseMatches)));

              { Reset }
              OpenSingleDupesChecked := False;
              Break;
            end;
          end;
          Match := Match.NextMatch;
        end;
      end

      { if there are any close parts on lines other than where open single dupe
        was matches, then it becomes obsolete }
      else if (Length(LineCloseMatches) > 0) and DecrementCloseDupes then
        OpenSingleDupesChecked := False;
    end;
  end;

  function DoCheckExit: Boolean;
  begin
    if Matched > CloseDupes then
    begin
      AFrom := I;
      ATo := ALine;
      Result := True;
      MatchedPos := 1;
    end
    else
      Result := False;
  end;

begin
  { Initialize }
  AFrom := 0;
  ATo := 0;

  { Empty? }
  if ASynEdit.Lines.Count = 0 then Exit;

  { Get line }
  S := UnicodeStringToUTF8(ASynEdit.Lines.fList[Pred(ALine)].fString);

  { Try to read from cache if there were no changes so far on line }
  Cache := (ASynEdit.Highlighter as TSynUniSyn).Cache[Pred(ALine)];
  if not Cache^.AOutlined then
    SynEditGetOpenCloseFoldingRange(ASynEdit, ALine, ARegions, OpenSingleDupesChecked,
      CloseSingleDupesChecked);

  { Indentation rules have priority }
  if CheckIndentationRules(ARegions) and Cache^.AIndentationClose and not Cache^.ACollapseClose then
  begin

    { See if any of indentation regions match whitespace on this line }
    Matched := -1;
    for J := 0 to Pred(ARegions.Count) do
      if ARegions[J].Kind = frikIndentation then
        if CheckIndentationRegion(ASynEdit, ARegions, J, ALine, K) then
        begin
          Matched := J;
          Break;
        end;

    { Look for closing part }
    if Matched > -1 then
    begin

      { Fix indent level }
      if IsLineAllWhite(ASynEdit, ALine) then
        K := 1;

      { Iterate through lines }
      I := ALine - 2;
      while I > 0 do
      begin
        if ARegions[Matched].CloseBackreference then
        begin
          if not CheckIndentationRegion(ASynEdit, ARegions, Matched, I, C) then
          begin
            Inc(I);
            Break;
          end;
        end
        else begin
          if not IsLineAllWhite(ASynEdit, I) and CheckIndentationRegion(ASynEdit, ARegions, Matched, I, C) then
            if C <= K then
              Break;
        end;
        Dec(I);
      end;

      { Done }
      AFrom := I;
      ATo := ALine;
      AFromChar := C;
      AFromComplete := C;
      AToChar := K;
      AToComplete := K;
      Exit;
    end;
  end;

  { Fill parsed matches }
  for I := 0 to High(Cache^.AOutliningMap^) do
    if not Cache^.AOutliningMap^[I].AOpen then
      with Cache^.AOutliningMap^[I] do
        AddMatch(ALength, AIndex, ARegion.Index, LineCloseMatches);

  { No matches? }
  if Length(LineCloseMatches) = 0 then
    Exit;

  { Find close region }
  SortMatches(LineCloseMatches);
  if AToChar > 0 then
  begin
    for I := High(LineCloseMatches) downto 0 do
      if AToChar >= LineCloseMatches[I].Pos.Char then
      begin
        ARegion := LineCloseMatches[I].Region;
        MatchedPos := LineCloseMatches[I].Pos.Char;
        AToComplete := LineCloseMatches[I].Pos.Char + LineCloseMatches[I].Pos.Line;
        Break;
      end;
  end
  else begin
    ARegion := LineCloseMatches[High(LineCloseMatches)].Region;
    MatchedPos := LineCloseMatches[High(LineCloseMatches)].Pos.Char;
    AToChar := LineCloseMatches[High(LineCloseMatches)].Pos.Char;
    AToComplete := LineCloseMatches[High(LineCloseMatches)].Pos.Char +
      LineCloseMatches[High(LineCloseMatches)].Pos.Line;
  end;
  Matched := 0;

  { Find out backreferenced parts }
  SetLength(Open, ARegions.Count);
  SetLength(Close, ARegions.Count);
  if ARegions[ARegion].CloseBackreference then
  begin
    Runner := CountUnicodeChars(S, MatchedPos);
    Match := TRegex.Match(S, ARegions[ARegion].CloseBref, RegExOpts,
      Runner, Length(S));
    if not Match.Success then
      Exit;
    Open[ARegion] := Match.Result(ARegions[ARegion].OpenBref);
    Close[ARegion] := Match.Result(ARegions[ARegion].Close);
  end;

  { Close duplicates for that region. Backreferenced items
    can't have duplicates }
  SetLength(CloseDupTable, 0);
  SetLength(CloseSingleDupes, 0);
  for I := 0 to Pred(ARegions.Count) do
  begin
    if I = ARegion then
      Continue;
    if (ARegions[I].CloseTokenKind = ARegions[ARegion].CloseTokenKind) and
      (ARegions[I].Close = ARegions[ARegion].Close)
    then begin
      SetLength(CloseDupTable, Succ(Length(CloseDupTable)));
      CloseDupTable[High(CloseDupTable)] := I;
      if ARegions[I].CloseBackreference then
        if ARegions[ARegion].CloseBackreference then
          Open[I] := Match.Result(ARegions[I].OpenBref)
        else
          Open[I] := ARegions[I].Open
      else if ARegions[ARegion].CloseBackreference then
      begin
        SetLength(CloseSingleDupes, Succ(Length(CloseSingleDupes)));
        with CloseSingleDupes[High(CloseSingleDupes)] do
        begin
          Region := I;
          BrefIndex := 0;
        end;
      end;
    end;
  end;

  { Open duplicates fot that region. Backreferenced items
    can't have duplicates }
  SetLength(OpenDupTable, 0);
  SetLength(OpenSingleDupes, 0);
  for I := 0 to Pred(ARegions.Count) do
  begin
    if I = ARegion then
      Continue;
    if (ARegions[I].OpenTokenKind = ARegions[ARegion].OpenTokenKind) and
      (ARegions[I].Open = ARegions[ARegion].Open)
    then begin
      SetLength(OpenDupTable, Succ(Length(OpenDupTable)));
      OpenDupTable[High(OpenDupTable)] := I;
      if ARegions[I].CloseBackreference then
        if ARegions[ARegion].CloseBackreference then
          Close[I] := Match.Result(ARegions[I].Close)
        else
          Close[I] := ARegions[I].CloseBref
      else if ARegions[ARegion].CloseBackreference then
      begin
        SetLength(OpenSingleDupes, Succ(Length(OpenSingleDupes)));
        with OpenSingleDupes[High(OpenSingleDupes)] do
        begin
          Region := I;
          BrefIndex := 0;
        end;
      end;
    end;
  end;

  { Search for corresponding open part until EOF }
  CloseDupes := 0;
  OpenSingleDupesChecked := False;
  for I := ALine downto 1 do
  begin

    { Reset arrays }
    SetLength(LineOpenMatches, 0);
    SetLength(LineCloseMatches, 0);

    { Get that line in UTF8 }
    S := UnicodeStringToUTF8(ASynEdit.Lines.fList[Pred(I)].fString);

    { Obtain number of close matches on that line }
    if not ARegions[ARegion].CloseBackreference then
      Match := TRegex.Match(S, ARegions[ARegion].Close, RegExOpts)
    else
      Match := TRegex.Match(S, Close[ARegion], RegExOpts);

    { Add them and remove self }
    while Match.Success do
    begin
      C := CountUTF8Chars(S, Match.Index);
      if ARegions[ARegion].CloseTokenKind = GetTokenKind(ASynEdit, BufferCoord(C, I)) then
        if (ALine = I) and (C >= MatchedPos) then
          { Skip }
        else
          AddMatch(Match.Length, C, ARegion, LineCloseMatches);
      Match := Match.NextMatch;
    end;

    { Obtain number of open matches on that line }
    if not ARegions[ARegion].CloseBackreference then
      Match := TRegex.Match(S, ARegions[ARegion].Open, RegExOpts)
    else
      Match := TRegex.Match(S, Open[ARegion], RegExOpts);
    while Match.Success do
    begin
      C := CountUTF8Chars(S, Match.Index);
      if ARegions[ARegion].OpenTokenKind = GetTokenKind(ASynEdit, BufferCoord(C, I)) then
        if (I = ALine) and (C > MatchedPos) then
          { Skip }
        else begin
          AddMatch(I, C, ARegion, LineOpenMatches);
          K := CountUnicodeChars(S, Match.Groups.ItemsByName[sToken].Index, Match.Index);
          if K = 1 then begin K := 0; Inc(C); end;
          LineOpenMatches[High(LineOpenMatches)].TokenIndex := Pred(C) + K;
        end;
      Match := Match.NextMatch;
    end;

    { Obtain number of close duplicates on that line }
    for J := 0 to High(OpenDupTable) do
    begin

      { Sealed? }
      if OpenDupTable[J] = -1 then Continue;

      { Choose an action }
      if not ARegions[OpenDupTable[J]].CloseBackreference then
        if ARegions[ARegion].CloseBackreference then
          Continue
        else
          Match := TRegex.Match(S, ARegions[OpenDupTable[J]].Close, RegExOpts)
      else
        Match := TRegex.Match(S, Close[OpenDupTable[J]], RegExOpts);

      { Get matches }
      while Match.Success do
      begin

        { Get real position in string }
        C := CountUTF8Chars(S, Match.Index);

        { Check token kind }
        if ARegions[OpenDupTable[J]].CloseTokenKind = GetTokenKind(ASynEdit, BufferCoord(C, I)) then
          if (I = ALine) and (C > MatchedPos) then
            { Skip }
          else
            AddMatch(Match.Length, C, OpenDupTable[J], LineCloseMatches);

        { Seal if needed and proceed to the next match }
        if not ARegions[OpenDupTable[J]].CloseBackreference and
          ARegions[ARegion].CloseBackreference then
        begin
          Match := TRegex.Match(EmptyAnsiStr, #32);
          OpenDupTable[J] := -1;
        end
        else
          Match := Match.NextMatch;
      end;
    end;

    { Obtain number of open duplicates on that line }
    for J := 0 to High(CloseDupTable) do
    begin

      { Sealed? }
      if CloseDupTable[J] = -1 then Continue;

      { Choose an action }
      if not ARegions[CloseDupTable[J]].CloseBackreference then
        if ARegions[ARegion].CloseBackreference then
          Continue
        else
          Match := TRegex.Match(S, ARegions[CloseDupTable[J]].Open, RegExOpts)
      else
        Match := TRegex.Match(S, Open[CloseDupTable[J]], RegExOpts);

      { Get matches }
      while Match.Success do
      begin

        { Get real position in string }
        C := CountUTF8Chars(S, Match.Index);

        { Check token kind }
        if ARegions[CloseDupTable[J]].OpenTokenKind = GetTokenKind(ASynEdit, BufferCoord(C, I)) then
          if (I = ALine) and (C > MatchedPos) then
            { Skip }
          else begin
            AddMatch(Match.Length, C, CloseDupTable[J], LineOpenMatches);
            K := CountUnicodeChars(S, Match.Groups.ItemsByName[sToken].Index, Match.Index);
            if K = 1 then begin K := 0; Inc(C); end;
            LineOpenMatches[High(LineOpenMatches)].TokenIndex := Pred(C) + K;
          end;

        { Proceed to the next match }
        if not ARegions[CloseDupTable[J]].CloseBackreference and
          ARegions[ARegion].CloseBackreference then
        begin
          Match := TRegex.Match(EmptyAnsiStr, #32);
          CloseDupTable[J] := -1;
        end
        else
          Match := Match.NextMatch;
      end;
    end;

    { Manage open single duplicates on this line }
    OpenSingleDupesCheck;
    if not OpenSingleDupesChecked and (Length(OpenSingleDupes) > 0) then
    begin
      { Sort ascending }
      SortMatches(LineOpenMatches);
      SortMatches(LineCloseMatches);

      { Get  }
      J := High(LineOpenMatches);

      { Iterate down to first }
      while J >= 0 do
      begin

        { Pretend there's no possible close counterpart }
        C := 0;

        { Needed to get correct close counterpart }
        if J = High(LineOpenMatches) then
          RunnerEnd := MaxInt
        else
          RunnerEnd := LineOpenMatches[Succ(J)].Pos.Char;

        { Try to find close counterpart }
        K := 0;
        while K <= High(LineCloseMatches) do
        begin
          if (LineCloseMatches[K].Pos.Char > LineOpenMatches[J].Pos.Char) and (ARegions[LineCloseMatches[K].Region].CloseBackreference) then
          begin
            if LineCloseMatches[K].Pos.Char < RunnerEnd then
              C := 1;
            Break;
          end;
          Inc(K);
        end;

        { Choose correct match borders }
        Runner := CountUnicodeChars(S, LineOpenMatches[J].Pos.Char);
        if C > 0 then
          if J < High(LineOpenMatches) then
            RunnerEnd := Min(CountUnicodeChars(S, LineOpenMatches[Succ(J)].Pos.Char),
              CountUnicodeChars(S, LineCloseMatches[K].Pos.Char))
          else
            RunnerEnd := CountUnicodeChars(S, LineCloseMatches[K].Pos.Char)
        else
          if J < High(LineOpenMatches) then
            RunnerEnd := CountUnicodeChars(S, LineOpenMatches[Succ(J)].Pos.Char)
          else
            if I = ALine then
              RunnerEnd := CountUnicodeChars(S, MatchedPos)
            else
              RunnerEnd := Length(S);

        { See any of close single duplicates match }
        for K := 0 to High(OpenSingleDupes) do
        begin

          { Do match }
          Match := TRegex.Match(S, ARegions[OpenSingleDupes[K].Region].Close, RegExOpts, Runner, RunnerEnd);
          if Match.Success then
          begin

            { Get real position in string }
            C := CountUTF8Chars(S, Match.Index);

            { Check token kind and put in stack }
            if ARegions[OpenSingleDupes[K].Region].CloseTokenKind = GetTokenKind(ASynEdit, BufferCoord(C, I)) then
            begin
              AddMatch(Match.Length, C, OpenSingleDupes[K].Region, LineCloseMatches);
              Break;
            end;
          end;
        end;

        { Procceed to previus open match }
        Dec(J);
      end;

      { We still (sigh) aren't done with open single dupes. Need to look if
        there are any of them before any open and close parts }
      Runner := 1;
      if (Length(LineOpenMatches) = 0) and (Length(LineCloseMatches) = 0) then
        RunnerEnd := Length(S)
      else if (Length(LineOpenMatches) > 0) and (Length(LineCloseMatches) > 0) then
        RunnerEnd := Min(CountUnicodeChars(S, LineOpenMatches[0].Pos.Char),
          CountUnicodeChars(S, LineCloseMatches[0].Pos.Char))
      else if Length(LineOpenMatches) > 0 then
        RunnerEnd := CountUnicodeChars(S, LineOpenMatches[0].Pos.Char)
      else if Length(LineCloseMatches) > 0 then
        RunnerEnd := CountUnicodeChars(S, LineCloseMatches[0].Pos.Char);

      { See if any of open single duplicates match }
      for K := 0 to High(OpenSingleDupes) do
      begin

        { Do match }
        Match := TRegex.Match(S, ARegions[OpenSingleDupes[K].Region].Close, RegExOpts, Runner, RunnerEnd);
        if Match.Success then
        begin

          { Get real position in string }
          C := CountUTF8Chars(S, Match.Index);

          { Check token kind and put in stack }
          if ARegions[OpenSingleDupes[K].Region].CloseTokenKind = GetTokenKind(ASynEdit, BufferCoord(C, I)) then
          begin
            AddMatch(Match.Length, C, OpenSingleDupes[K].Region, LineCloseMatches);
            OpenSingleDupesChecked := True;
            Break;
          end;
        end;
      end;
    end;
    OpenSingleDupesCheck(False);

    { Anything found? }
    if (Length(LineOpenMatches) = 0) and (Length(LineCloseMatches) = 0) then
      Continue;

    { Sort stacks as they appear in text }
    SortMatches(LineOpenMatches, False);
    SortMatches(LineCloseMatches, False);

    { Analyze filled stacks }
    J := 0; // Open runner
    K := 0; // Close runner
    MatchedPos := 0; // Reuse to know if we are finished
    if Length(LineOpenMatches) = 0 then
      Runner := LineCloseMatches[High(LineCloseMatches)].Pos.Char
    else if Length(LineCloseMatches) = 0 then
      Runner := LineOpenMatches[High(LineOpenMatches)].Pos.Char
    else
      Runner := Min(LineOpenMatches[High(LineOpenMatches)].Pos.Char, LineCloseMatches[High(LineCloseMatches)].Pos.Char);
    if Length(LineOpenMatches) = 0 then
      RunnerEnd := LineCloseMatches[0].Pos.Char
    else if Length(LineCloseMatches) = 0 then
      RunnerEnd := LineOpenMatches[0].Pos.Char
    else
      RunnerEnd := Max(LineOpenMatches[0].Pos.Char, LineCloseMatches[0].Pos.Char);

    while Runner <= RunnerEnd do
    begin
      if (Length(LineOpenMatches) > J) and (RunnerEnd = LineOpenMatches[J].Pos.Char) then
      begin
        Inc(Matched);

        { Check, if we are done }
        if DoCheckExit then
        begin
          AFromChar := LineOpenMatches[J].TokenIndex;
          AFromComplete := LineOpenMatches[J].Pos.Char;
          Break;
        end;

        { To next open }
        Inc(J);
      end
      else if (Length(LineCloseMatches) > K) and (RunnerEnd = LineCloseMatches[K].Pos.Char) then
      begin
        Inc(CloseDupes);

        { Check, if we are done }
        if DoCheckExit then
          Break;

        { To next close }
        Inc(K);
      end;
      Dec(RunnerEnd);
    end;

    { Done }
    if MatchedPos = 1 then
      Break;
  end;

  { Free }
  SetLength(OpenDupTable, 0);
  SetLength(CloseDupTable, 0);
  SetLength(LineCloseMatches, 0);
  SetLength(LineOpenMatches, 0);
end;

// -----------------------------------------------------------------------------
// Collapses found range from open part to correct close part
procedure SynEditCollpaseOpenFoldingRange(ASynEdit: TCustomSynEdit;
  var ALine, AChar: Integer; const ARegions: TFoldRegions);
var
  jFrom, jTo, jRegion, nDummy1, nDummy2, nDummy3: Integer;
  NewFoldRange: TSynEditFoldRange;
begin
  { Try to find it }
  AChar := 0;
  nDummy1 := 0;
  SynEditCollpaseOpen(ASynEdit, ALine, ARegions, jFrom, jTo, AChar, nDummy1, jRegion,
    nDummy2, nDummy3);
  if (jFrom = 0) or (jTo = 0) then
    Exit;

  { Create new collapsed range }
  NewFoldRange := TSynEditFoldRange.Create(ASynEdit.AllFoldRanges);
  with NewFoldRange do
  begin
    FromLine := jFrom;
    ToLine := jTo;
    FoldRegion := ARegions[jRegion];
  end;
  with ASynEdit.AllFoldRanges do
  begin
    AddFold(NewFoldRange);
    UpdateFoldRanges;
  end;
  ASynEdit.UpdateWordWrapHiddenOffsets;
end;

// -----------------------------------------------------------------------------

procedure SynEditCollpaseCloseFoldingRange(ASynEdit: TCustomSynEdit;
  ALine: Integer; const ARegions: TFoldRegions);
var
  jFrom, jTo, jRegion, Dummy, nDummy2, nDummy3, nDummy4: Integer;
  NewFoldRange: TSynEditFoldRange;
begin
  { Try to find it }
  Dummy := 0;
  nDummy2 := 0;
  SynEditCollpaseClose(ASynEdit, ALine, ARegions, jFrom, jTo, Dummy, nDummy2, jRegion,
    nDummy3, nDummy4);
  if (jFrom = 0) or (jTo = 0) then
    Exit;

  { Create new collapsed range }
  NewFoldRange := TSynEditFoldRange.Create(ASynEdit.AllFoldRanges);
  with NewFoldRange do
  begin
    FromLine := jFrom;
    ToLine := jTo;
    ParentCollapsed := False;
    FoldRegion := ARegions[jRegion];
  end;
  with ASynEdit.AllFoldRanges do
  begin
    AddFold(NewFoldRange);
    UpdateFoldRanges;
  end;
  ASynEdit.UpdateWordWrapHiddenOffsets;
end;

// -----------------------------------------------------------------------------

procedure SynEditFindClosest(ASynEdit: TCustomSynEdit; ALine: Integer;
  const ARegions: TFoldRegions; var AFrom, ATo: Integer);
var
  jFrom, jTo, jLine, jStart, nDummy, nDummy2, nDummy3, nDummy4: Integer;
  jOpen, jClose: Boolean;
  jOpenLine, jCloseLine: Integer;
  jOpenLevel, jCloseLevel: Integer;
begin
  { Check }
  if ASynEdit.Lines.Count = 0 then
    Exit;

  { Preapre }
  AFrom := 0;
  ATo := 0;
  jStart := 0;
  nDummy := 0;

  { Initialize }
  jFrom := 0;
  jTo := 0;
  jLine := ALine;
  jOpenLine := ALine;
  jCloseLine := ALine;
  jOpenLevel := 1;
  jCloseLevel := 1;

  { Search on current line }
  SynEditGetOpenCloseFoldingRange(ASynEdit, jLine, ARegions, jOpen, jClose);
  if jOpen then Dec(jOpenLevel)
  else if jClose then Dec(jCloseLevel);

  { Search until we finally find possible part }
  while (jOpenLine > 0) and (jCloseLine < ASynEdit.Lines.Count) do
  begin

    { Search top for open part }
    if jOpenLevel > 0 then
      while jOpenLine >= 1 do
      begin
        Dec(jOpenLine);
        if jOpenLine = 0 then
          Break;
        SynEditGetOpenCloseFoldingRange(ASynEdit, jOpenLine, ARegions, jOpen, jClose);
        { !!! Check if jCloseLevel must or mustn't be increased }
        if jClose and not jOpen then
        begin
          Inc(jOpenLevel);
          Break;
        end;
        if jOpen then
        begin
          Dec(jOpenLevel);
          Break;
        end;
      end;

    { Avoid looking for final close if open was just found }
    if jOpenLevel = 0 then jCloseLevel := 0;

    { Search bottom for close part }
    if jCloseLevel > 0 then
      while jLine <= ASynEdit.Lines.Count do
      begin
        Inc(jCloseLine);
        if jCloseLine > ASynEdit.Lines.Count then
          Break;
        SynEditGetOpenCloseFoldingRange(ASynEdit, jCloseLine, ARegions, jOpen, jClose);
        if jOpen then
        begin
          Inc(jCloseLevel);
          Break;
        end;
        if jClose then
        begin
          Dec(jCloseLevel);
          Break;
        end;
      end;

    { If any valid part found, search for counterpart }
    if jOpenLevel = 0 then
      SynEditCollpaseOpen(ASynEdit, jOpenLine, ARegions, jFrom, jTo, jStart, nDummy,
        nDummy2, nDummy3, nDummy4)
    else if jCloseLevel = 0 then
      SynEditCollpaseClose(ASynEdit, jCloseLine, ARegions, jFrom, jTo, jStart, nDummy,
        nDummy2, nDummy3, nDummy4);

    { Done? }
    if (jOpenLevel = 0) or (jCloseLevel = 0) then
      Break;
  end;

  { If found, exit with success }
  if (jFrom > 0) and (jTo > 0) then
  begin
    AFrom := jFrom;
    ATo := jTo;
  end;
end;

// -----------------------------------------------------------------------------

procedure SynEditToggleFoldingRange(ASynEdit: TCustomSynEdit; ALine: Integer;
  const ARegions: TFoldRegions; var AToggled: Boolean);
var
  GotOpen, GotClose: Boolean;
  FoldRange: TSynEditFoldRange;

  procedure DoExpand;
  begin
    AToggled := True;
    with ASynEdit.AllFoldRanges do
    begin
      Delete(FoldRange);
      UpdateFoldRanges;
    end;
    ASynEdit.UpdateWordWrapHiddenOffsets;
  end;

  procedure DoCollapse;
  var
    Dummy: Integer;
  begin
    { Find possible parts }
    SynEditGetOpenCloseFoldingRange(ASynEdit, ALine, ARegions, GotOpen, GotClose);
    AToggled := GotOpen or GotClose;

    { Open part always has priority over close }
    if GotOpen then
      SynEditCollpaseOpenFoldingRange(ASynEdit, ALine, Dummy, ARegions)
    else if GotClose then
      SynEditCollpaseCloseFoldingRange(ASynEdit, ALine, ARegions);
  end;

begin
  AToggled := False;
  FoldRange := ASynEdit.FoldRangeForLine(ALine);
  if not Assigned(FoldRange) then
    FoldRange := ASynEdit.FoldRangeForLineTo(ALine);
  if Assigned(FoldRange) then
    DoExpand
  else
    DoCollapse;
end;

{ Shorthand outlining functions }

function OutliningCanCollapseOpen(ASynEdit: TCustomSynEdit; ALine: Integer): Boolean;
var
  Cache: PSynUniCacheItem;
  Dummy1, Dummy2: Boolean;
begin
  { Get cache and ouline line if needed }
  Cache := (ASynEdit.Highlighter as TSynUniSyn).Cache[Pred(ALine)];
  if not Cache^.AOutlined then
    SynEditGetOpenCloseFoldingRange(ASynEdit, ALine, ASynEdit.Highlighter.FoldRegions,
      Dummy1, Dummy2);

  { Return }
  Result := Cache^.ACollapseOpen or Cache^.AIndentationOpen;
end;

function OutliningCanCollapseClose(ASynEdit: TCustomSynEdit; ALine: Integer): Boolean;
var
  Cache: PSynUniCacheItem;
  Dummy1, Dummy2: Boolean;
begin
  { Get cache and ouline line if needed }
  Cache := (ASynEdit.Highlighter as TSynUniSyn).Cache[Pred(ALine)];
  if not Cache^.AOutlined then
    SynEditGetOpenCloseFoldingRange(ASynEdit, ALine, ASynEdit.Highlighter.FoldRegions,
      Dummy1, Dummy2);

  { Return }
  Result := Cache^.ACollapseClose or Cache^.AIndentationClose;
end;

function OutliningCanExpand(ASynEdit: TCustomSynEdit; ALine: Integer): Boolean;
var
  FoldRange: TSynEditFoldRange;
begin
  FoldRange := ASynEdit.FoldRangeForLine(ALine);
  if FoldRange = nil then
    FoldRange := ASynEdit.FoldRangeForLineTo(ALine);
  Result := FoldRange <> nil;
end;

function OutliningCollapse(ASynEdit: TCustomSynEdit; ALine: Integer): Boolean;
var
  Dummy: Integer;
begin
  SynEditCollpaseOpenFoldingRange(ASynEdit, ALine, Dummy, ASynEdit.Highlighter.FoldRegions);
  if Dummy = 0 then
    SynEditCollpaseCloseFoldingRange(ASynEdit, ALine, ASynEdit.Highlighter.FoldRegions);
  Result := Dummy > 0;
end;

function OutliningExpand(ASynEdit: TCustomSynEdit; ALine: Integer): Boolean;
var
  FoldRange: TSynEditFoldRange;
begin
  FoldRange := ASynEdit.FoldRangeForLine(ALine);
  if not Assigned(FoldRange) then
    FoldRange := ASynEdit.FoldRangeForLineTo(ALine);
  Result := Assigned(FoldRange);
  if Result then
  begin
    with ASynEdit.AllFoldRanges do
    begin
      Delete(FoldRange);
      UpdateFoldRanges;
    end;
    ASynEdit.UpdateWordWrapHiddenOffsets;
  end;
end;

function OutliningToggle(ASynEdit: TCustomSynEdit; ALine: Integer): Boolean;
begin
  SynEditToggleFoldingRange(ASynEdit, ALine, ASynEdit.Highlighter.FoldRegions, Result);
end;

function OutliningScope(ASynEdit: TCustomSynEdit; ALine: Integer): UnicodeString;
var
  ARegions: TFoldRegions;
  ARange: TSynEditFoldRange;
  LineOpenMatches, LineCloseMatches: TSynFoldingMatches;
  Cache: PSynUniCacheItem;
  Dummy1, Dummy2: Boolean;
  Matched, I, J: Integer;
begin
  { Initialize }
  Result := EmptyStr;

  { Get fold regions }
  ARegions := ASynEdit.Highlighter.FoldRegions;

  { Get cache and ouline line if needed }
  Cache := (ASynEdit.Highlighter as TSynUniSyn).Cache[Pred(ALine)];
  if not Cache^.AOutlined then
    SynEditGetOpenCloseFoldingRange(ASynEdit, ALine, ARegions, Dummy1, Dummy2);

  { Look for open collapsed range }
  ARange := ASynEdit.FoldRangeForLine(ALine);
  if ARange <> nil then
  begin
    if ARange.FoldRegion.OpenTokenKind <> -1 then
      Result := TSynHighlighterAttributes(ARange.FoldRegion.OpenTokenKind).Name;
    Exit;
  end;

  { Look for close collapsed range }
  ARange := ASynEdit.FoldRangeForLineTo(ALine);
  if ARange <> nil then
  begin
    if ARange.FoldRegion.CloseTokenKind <> -1 then
      Result := TSynHighlighterAttributes(ARange.FoldRegion.CloseTokenKind).Name;
    Exit;
  end;

  { Check indentation regions }
  if CheckIndentationRules(ARegions) then
  begin

    { Check open indentation regions }
    if (Cache^.AIndentationOpen and not Cache^.ACollapseOpen) or
      (Cache^.AIndentationClose and not Cache^.ACollapseClose) then
    begin

      { Get regions that matches }
      Matched := -1;
      for I := 0 to Pred(ARegions.Count) do
        if ARegions[I].Kind = frikIndentation then
          if CheckIndentationRegion(ASynEdit, ARegions, I, ALine, J) then
          begin
            Matched := I;
            Break;
          end;

      { Found }
      if Matched > -1 then
      begin
        if ARegions[Matched].OpenTokenKind <> -1 then
          Result := TSynHighlighterAttributes(ARegions[Matched].OpenTokenKind).Name;
        if ARegions[Matched].OpenRng = nil then
          Result := TSynRange(ARegions[Matched].OpenRng).Name;
        Exit;
      end;
    end;
  end;

  { For open collpase we choose first open match if there's less close parts
    than open or last open match if they are in equal amounts }
  SetLength(LineOpenMatches, 0);
  SetLength(LineCloseMatches, 0);
  for I := 0 to High(Cache^.AOutliningMap^) do
    with Cache^.AOutliningMap^[I] do
      if AOpen then
        AddMatch(ALength, AIndex, ARegion.Index, LineOpenMatches)
      else
        AddMatch(ALength, AIndex, ARegion.Index, LineCloseMatches);

  { Find open region }
  if Cache^.ACollapseOpen and (Length(LineOpenMatches) > 0) then
  begin
    SortMatches(LineOpenMatches);
    if (Length(LineOpenMatches) = Length(LineCloseMatches)) and
      (LineOpenMatches[High(LineOpenMatches)].Pos.Char >
        LineCloseMatches[High(LineCloseMatches)].Pos.Char)
    then
      Result := UTF8ToUnicodeString(TSynHighlighterAttributes(ARegions[LineOpenMatches[High(LineOpenMatches)].Region].OpenTokenKind).Name)
    else
      Result := UTF8ToUnicodeString(TSynHighlighterAttributes(ARegions[LineOpenMatches[0].Region].OpenTokenKind).Name);
    Exit;
  end;

  { Find close region }
  if Cache^.ACollapseClose and (Length(LineCloseMatches) > 0) then
  begin
    SortMatches(LineCloseMatches);
    Result := UTF8ToUnicodeString(TSynHighlighterAttributes(ARegions[LineOpenMatches[High(LineOpenMatches)].Region].OpenTokenKind).Name);
    Exit;
  end;
end;

{ Indentation matching routines }

// -----------------------------------------------------------------------------

function SynEditGetIndent(ASynEdit: TCustomSynEdit; APoint: TBufferCoord;
  var OnlyOnce: Boolean; Reindent: Boolean = False): Boolean;
var
  I, J, Boundary: Integer;
  Open, Close, OpenNext, CloseNext, bOnlyOnce, bFound: Boolean;
  CacheItem: PSynUniCacheItem;
  Match: IMatch;
  S: UTF8String;
  Tkn: Integer;
begin
  { Initialize }
  Result := False;

  { Check }
  if AsynEdit.Lines.Count = 0 then Exit;
  if IsLineAllWhite(ASynEdit, APoint.Line) then Exit;

  { Find indent }
  with ASynEdit, (ASynEdit.Highlighter as TSynUniSyn) do
  begin

    { Get outlining data }
    SynEditGetOpenCloseFoldingRange(ASynEdit, APoint.Line, FoldRegions,
      Open, Close, False, True);
    CacheItem := Cache[Pred(APoint.Line)];

    { See if caret is just after open token }
    if Reindent then
    begin
      if CacheItem^.ACollapseOpen then
      begin
        Result := True;
        Exit;
      end;
    end
    else
      for I := 0 to High(CacheItem^.AOutliningMap^) do
        if CacheItem^.AOutliningMap^[I].AOpen and
          (CacheItem^.AOutliningMap^[I].AIndex + CacheItem^.AOutliningMap^[I].ALength = APoint.Char) then
        begin
          Result := True;
          Exit;
        end;

    { Try to apply indentation rules }
    if Length(IndentationRules) = 0 then
      Exit;
    bFound := False;
    bOnlyOnce := False;
    S := UnicodeStringToUTF8(ASynEdit.Lines.fList^[Pred(APoint.Line)].fString);
    for I := 0 to High(IndentationRules) do
    begin

      { Skip unindent rules }
      if not (sioIndent in IndentationRules[I].Options) then
        Continue;

      { Skip indent only once rules if conditions aren't met }
      if (sioOnlyOnce in IndentationRules[I].Options) then
      begin
        Open := False;

        { Conditions for reindent routine }
        if Reindent then
        begin

          { Can apply conditions? }
          if APoint.Line < Lines.Count then
          begin

            { Do not indent if there's outlining block ahead }
            SynEditGetOpenCloseFoldingRange(ASynEdit, Succ(APoint.Line), FoldRegions,
              OpenNext, CloseNext, False, True);
            Open := not OpenNext and not CloseNext;
          end
          else
            Open := True;
        end

        { Do not indent if previous line was already once-indented }
        else begin

          if APoint.Line > 1 then
          begin
            SynEditGetIndent(ASynEdit, BufferCoord(ASynEdit.Lines.AccessStringLength(APoint.Line - 2) + 1,
              Pred(APoint.Line)), Open, True);
            Open := not Open;
          end
          else
            Open := True;
        end;
        (*if Force and OnlyOnce then
          Open := True
        else if Force and not OnlyOnce then
          Open := True
        else
          Open := False;

        if not Open then
        begin
          Boundary := GetLeadingExpandedLength(ASynEdit.Lines.fList^[Pred(APoint.Line)].fString, ASynEdit.TabWidth);
          for J := Pred(APoint.Line) downto 1 do
          begin

            { Skip blank lines and lines consistiong only from whitespace }
            if IsLineAllWhite(ASynEdit, J) then
              Continue;

            { Indentation on prior line should be same or more }
            if GetLeadingExpandedLength(ASynEdit.Lines.fList^[Pred(J)].fString, ASynEdit.TabWidth) >= Boundary then
            begin
              Open := True;
              Break;
            end
            else
              Break;
          end;
        end;*)
        if not Open then
          Continue;
      end;

      { Try match }
      if Reindent then
        Boundary := Length(S)
      else
        Boundary := Min(CountUnicodeChars(S, APoint.Char), Length(S));
      Match := TRegex.Match(S, IndentationRules[I].Pattern, RegExOpts, 1, Boundary);
      while Match.Success do
      begin
        Boundary := CountUTF8Chars(S, Match.Index);

        Tkn := SynEditMiscProcs.GetTokenKind(ASynEdit, BufferCoord(Boundary, APoint.Line));
        if IndentationRules[I].TokenKind > -1 then
          if Tkn <> IndentationRules[I].TokenKind then
          begin
            Match := Match.NextMatch;
            Continue;
          end;

        if IndentationRules[I].Range <> nil then
          if GetRange <> IndentationRules[I].Range then
          begin
            Match := Match.NextMatch;
            Continue;
          end;

        Boundary := Boundary + CountUTF8Chars(S, Match.Index + Match.Length, Succ(Match.Index));
        if Boundary = APoint.Char then
        begin
          Result := True;

          if (sioOnlyOnce in IndentationRules[I].Options) then
            bOnlyOnce := True
          else begin
            bFound := True;
            Break;
          end;
        end;
        Match := Match.NextMatch;
      end;

      if bFound then
        Break;
    end;

    if not bFound and bOnlyOnce then
      OnlyOnce := bOnlyOnce
    else
      OnlyOnce := False;
  end;
end;

function SynEditGetUnindent(ASynEdit: TCustomSynEdit; APoint: TBufferCoord;
  LookBehind: Boolean = False; Reindenting: Boolean = False): Boolean;
var
  I, J, Boundary, Tkn: Integer;
  Open, Close, OpenPrev, ClosePrev: Boolean;
  CacheItem: PSynUniCacheItem;
  Match: IMatch;
  S: UTF8String;
begin
  { Initialize }
  Result := False;

  if AsynEdit.Lines.Count = 0 then
    Exit;

  { Can't unindent if already in the beginning of the line }
  if GetLeadingLength(ASynEdit.Lines.fList^[Pred(APoint.Line)].fString) = 0 then
    Exit;

  { Can't unindent if there are some other chars before }
  if not Reindenting then
    if not ASynEdit.IsAllWhiteUpToCaret(ASynEdit.Lines.fList^[Pred(APoint.Line)].fString, APoint.Char) then
      Exit;

  { Can't unindent if already unindented }
  if not Reindenting then
  begin
    Open := False;
    Boundary := GetLeadingExpandedLength(ASynEdit.Lines.fList^[Pred(APoint.Line)].fString, ASynEdit.TabWidth);
    for I := Pred(APoint.Line) downto 1 do
    begin

      { Skip blank lines and lines consistiong only from whitespace }
      if IsLineAllWhite(ASynEdit, I) then
        Continue;

      { Indentation on prior line should be less }
      J := GetLeadingExpandedLength(ASynEdit.Lines.fList^[Pred(I)].fString, ASynEdit.TabWidth);
      if J <= Boundary then
      begin
        if J = Boundary then
        begin
          SynEditGetOpenCloseFoldingRange(ASynEdit, I, ASynEdit.Highlighter.FoldRegions, OpenPrev, ClosePrev, False, True);
          Open := not OpenPrev;
        end
        else
          Open := True;
        Break;
      end
      else
        Break;
    end;
    if not Open then
      Exit;
  end;

  with ASynEdit, (ASynEdit.Highlighter as TSynUniSyn) do
  begin

    { Get outlining data }
    CacheItem := Cache[Pred(APoint.Line)];
    if not CacheItem^.AOutlined then
      SynEditGetOpenCloseFoldingRange(ASynEdit, APoint.Line, FoldRegions, Open, Close, False, True);

    { See if caret is just behind close token }
    if Reindenting then
    begin
      if CacheItem^.ACollapseClose then
      begin
        Result := True;
        Exit;
      end;
    end
    else
    for I := 0 to High(CacheItem^.AOutliningMap^) do
      if not CacheItem^.AOutliningMap^[I].AOpen then
      begin
        if not LookBehind then
          Boundary := CacheItem^.AOutliningMap^[I].AIndex
        else
          Boundary := CacheItem^.AOutliningMap^[I].AIndex + CacheItem^.AOutliningMap^[I].ALength;
        if Boundary = APoint.Char then
        begin
          Result := True;
          Exit;
        end;
      end;

    { Try to apply indentation rules }
    if Length(IndentationRules) = 0 then
      Exit;
    S := UnicodeStringToUTF8(ASynEdit.Lines.fList^[Pred(APoint.Line)].fString);
    for I := 0 to High(IndentationRules) do
    begin

      { Skip unindent rules }
      if sioIndent in IndentationRules[I].Options then
        Continue;

      { Try match }
      Boundary := Length(S);
      Match := TRegex.Match(S, IndentationRules[I].Pattern, RegExOpts, 1, Boundary);
      while Match.Success do
      begin
        Boundary := CountUTF8Chars(S, Match.Index);

        Tkn := SynEditMiscProcs.GetTokenKind(ASynEdit, BufferCoord(Boundary, APoint.Line));
        if IndentationRules[I].TokenKind > -1 then
          if Tkn <> IndentationRules[I].TokenKind then
          begin
            Match := Match.NextMatch;
            Continue;
          end;

        if IndentationRules[I].Range <> nil then
          if GetRange <> IndentationRules[I].Range then
          begin
            Match := Match.NextMatch;
            Continue;
          end;

        Boundary := Boundary + CountUTF8Chars(S, Match.Index + Match.Length, Succ(Match.Index));
        if Boundary = APoint.Char then
        begin
          Result := True;
          Exit;
        end;
        Match := Match.NextMatch;
      end;
    end;

  end;
end;

initialization
  { Do nothing }

finalization
  SetLength(FMatchStack, 0);
  SetLength(FMatchOpenDup, 0);
  SetLength(FMatchCloseDup, 0);

end.
