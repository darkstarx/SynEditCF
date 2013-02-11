{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditTypes.pas, released 2000-04-07.
The Original Code is based on parts of mwCustomEdit.pas by Martin Waldenburg,
part of the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
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

$Id: SynEditTypes.pas,v 1.13.2.1 2004/08/31 12:55:18 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditTypes;

{$I SynEdit.inc}

interface

uses
  SysUtils, Classes, Character, Graphics, RegularExpressions;

type
  ESynError = class(Exception);

  TLineStates = array of Byte;

  TSynSearchOption = (ssoMatchCase, ssoWholeWord, ssoBackwards,
    ssoEntireScope, ssoSelectedOnly, ssoReplace, ssoReplaceAll, ssoPrompt);
  TSynSearchOptions = set of TSynSearchOption;

  TCategoryMethod = function (AChar: Char): Boolean of object;

  TExCategoryMethod = function (AChar: Char; AToken: Integer): Boolean of object;

  TKeyPressWEvent = procedure (Sender: TObject; var Key: Char) of object;

  PSynSelectionMode = ^TSynSelectionMode;
  TSynSelectionMode = (smNormal, smLine, smColumn);

  PBufferCoord = ^TBufferCoord;
  TBufferCoord = record
    Char: Integer;
    Line: Integer;
  end;

  TDisplayCoord = record
    Column: integer;
    Row: integer;
  end;

  TTokenNature = (tknnIdent, tknnNumber, tknnSymbol, tknnSpace, tknnNbsp, tknnSeparator, tknnUnknown);

  { Visual appearance of a specific token }
  TSynHighlighterAttributes = class(TPersistent)
  private
    fName: UTF8String;
    fFriendlyName: UTF8String;
    fBackground: TColor;
    fForeground: TColor;
    fStyle: TFontStyles;

    fHash: Cardinal;

    fTag: Integer;
    fTagString: UTF8String;
    fCloseRange: Boolean;

    fRule: TObject;

    function GetStyleFromInt: Integer;
    procedure SetStyleFromInt(Value: Integer);
  public
    constructor Create(AName: UnicodeString; AFriendlyName: UnicodeString);

    procedure Assign(Source: TPersistent); override;
    procedure AssignColorAndStyle(Source: TSynHighlighterAttributes);
  public
    property Name: UTF8String read fName write fName;
    property FriendlyName: UTF8String read fFriendlyName write fFriendlyName;
    property IntegerStyle: Integer read GetStyleFromInt write SetStyleFromInt;

    property Hash: Cardinal read fHash write fHash;

    property Tag: Integer read fTag write fTag;
    property TagString: UTF8String read fTagString write fTagString;
    property CloseRange: Boolean read fCloseRange write fCloseRange;

    property Rule: TObject read fRule write fRule;
  published
    property Background: TColor read fBackground write fBackground;
    property Foreground: TColor read fForeground write fForeground;
    property Style: TFontStyles read fStyle write fStyle;
  end;

  TSynTokenMatched = record
    OpenToken: UnicodeString;
    CloseToken: UnicodeString;
    OpenTokenPos: TBufferCoord;
    CloseTokenPos: TBufferCoord;
    TokenKind: Integer;
    TokenAttri: TSynHighlighterAttributes;
  end;

  function IsWhitespaceChar(AChar: Char): Boolean;
  function IsWordBreakChar(AChar: Char): Boolean;
  function IsWordBreakCharNotSpace(AChar: Char): Boolean;
  function IsDigitChar(AChar: Char): Boolean;
  function DisplayCoord(AColumn, ARow: Integer): TDisplayCoord;
  function BufferCoord(AChar, ALine: Integer): TBufferCoord;
  function IsWordBreakCharPrint(const S: String; const Index: Integer): Boolean; overload;
  function IsWordBreakCharPrint(AChar: Char): Boolean; overload;
  procedure SwapCarets(var C1, C2: TBufferCoord);
  function CaretsEqual(const C1, C2: TBufferCoord): Boolean;
  function MinCaret(const C1, C2: TBufferCoord): TBufferCoord;
  function MaxCaret(const C1, C2: TBufferCoord): TBufferCoord;
  function CaretInRange(const C, B, E: TBufferCoord; Inclusive: Boolean): Boolean;
  function CaretBefore(const C1, C2: TBufferCoord): Boolean;
  function CaretAfter(const C1, C2: TBufferCoord): Boolean;
  function StringToRegexOptions(const S: UnicodeString): TRegexOptions;

implementation

uses
  SynEditMiscProcs;

{ TSynHighlighterAttributes }

procedure TSynHighlighterAttributes.Assign(Source: TPersistent);
begin
  if Source is TSynHighlighterAttributes then
  begin
    fName := TSynHighlighterAttributes(Source).fName;
    AssignColorAndStyle(TSynHighlighterAttributes(Source));
  end
  else
    inherited Assign(Source);
end;

procedure TSynHighlighterAttributes.AssignColorAndStyle(Source: TSynHighlighterAttributes);
begin
  if fBackground <> Source.fBackground then
    fBackground := Source.fBackground;
  if fForeground <> Source.fForeground then
    fForeground := Source.fForeground;
  if fStyle <> Source.fStyle then
    fStyle := Source.fStyle;
end;

constructor TSynHighlighterAttributes.Create(AName: UnicodeString;
  AFriendlyName: UnicodeString);
begin
  inherited Create;
  Background := clNone;
  Foreground := clNone;
  fName := AName;
  fHash := 0;
  fFriendlyName := AFriendlyName;
  fTag := 0;
  fTagString := EmptyStr;
end;

function TSynHighlighterAttributes.GetStyleFromInt: Integer;
begin
  if fsBold in Style then Result := 1 else Result := 0;
  if fsItalic in Style then Result := Result + 2;
  if fsUnderline in Style then Result:= Result + 4;
  if fsStrikeout in Style then Result:= Result + 8;
end;

procedure TSynHighlighterAttributes.SetStyleFromInt(Value: Integer);
begin
  if Value and $1 = 0 then  Style:= [] else Style := [fsBold];
  if Value and $2 <> 0 then Style:= Style + [fsItalic];
  if Value and $4 <> 0 then Style:= Style + [fsUnderline];
  if Value and $8 <> 0 then Style:= Style + [fsStrikeout];
end;

// -----------------------------------------------------------------------------

function IsWhitespaceChar(AChar: Char): Boolean;
begin
  Result := (AChar < #33) or TCharacter.IsWhiteSpace(AChar);
end;

// -----------------------------------------------------------------------------

function IsWordBreakCharPrint(const S: String; const Index: Integer): Boolean;
begin
  Result := (TCharacter.IsPunctuation(S[Index]) or TCharacter.IsSeparator(S, Index)) and (S[Index] <> '_');
end;

// -----------------------------------------------------------------------------

function IsWordBreakCharPrint(AChar: Char): Boolean;
begin
  Result := (TCharacter.IsPunctuation(AChar) or TCharacter.IsSeparator(AChar)) and (AChar <> '_');
end;

// -----------------------------------------------------------------------------
// Universal word break test function to use inside and outside SynEdit instance
function IsWordBreakChar(AChar: Char): Boolean;
begin
  { 1st most likely case }
  if (AChar < #33) or TCharacter.IsWhiteSpace(AChar) then
    Result := True

  { 2nd most likely case: "_" }
  else if AChar = #95 then
    Result := False
  else
    Result := not TCharacter.IsLetterOrDigit(AChar);
end;

// -----------------------------------------------------------------------------
// Same, but doesn't treat whitespace as break
function IsWordBreakCharNotSpace(AChar: Char): Boolean;
begin
  { 1st most likely case }
  if (AChar < #33) or TCharacter.IsWhiteSpace(AChar) then
    Result := False

  { 2nd most likely case }
  else if AChar = #95 then
    Result := False
  else
    Result := not TCharacter.IsLetterOrDigit(AChar);
end;

// -----------------------------------------------------------------------------

function IsDigitChar(AChar: Char): Boolean;
begin
  Result := TCharacter.IsDigit(AChar) or TCharacter.IsNumber(AChar);
end;

// -----------------------------------------------------------------------------

function DisplayCoord(AColumn, ARow: Integer): TDisplayCoord;
begin
  with Result do
  begin
    Column := AColumn;
    Row := ARow;
  end;
end;

function BufferCoord(AChar, ALine: Integer): TBufferCoord;
begin
  with Result do
  begin
    Char := AChar;
    Line := ALine;
  end;
end;

procedure SwapCarets(var C1, C2: TBufferCoord);
begin
  SwapInt(C1.Char, C2.Char);
  SwapInt(C1.Line, C2.Line);
end;

function CaretsEqual(const C1, C2: TBufferCoord): Boolean;
begin
  Result := (C1.Line = C2.Line) and (C1.Char = C2.Char);
end;

function MinCaret(const C1, C2: TBufferCoord): TBufferCoord;
begin
  if (C1.Line > C2.Line) or ((C1.Line = C2.Line) and (C1.Char > C2.Char)) then
    Result := C2
  else
    Result := C1;
end;

function MaxCaret(const C1, C2: TBufferCoord): TBufferCoord;
begin
  if (C1.Line > C2.Line) or ((C1.Line = C2.Line) and (C1.Char > C2.Char)) then
    Result := C1
  else
    Result := C2;
end;

function CaretInRange(const C, B, E: TBufferCoord; Inclusive: Boolean): Boolean;
begin
  Result := ((C.Line > B.Line) or ((C.Line = B.Line) and
    (C.Char > B.Char - Ord(Inclusive)))) and ((C.Line < E.Line) or
    ((C.Line = E.Line) and (C.Char < E.Char + Ord(Inclusive))));
end;

function CaretBefore(const C1, C2: TBufferCoord): Boolean;
begin
  Result := (C1.Line < C2.Line) or ((C1.Line = C2.Line) and
    (C1.Char < C2.Char));
end;

function CaretAfter(const C1, C2: TBufferCoord): Boolean;
begin
  Result := (C1.Line > C2.Line) or ((C1.Line = C2.Line) and
    (C1.Char > C2.Char));
end;

function StringToRegexOptions(const S: UnicodeString): TRegexOptions;
begin
  Result := [roSingleline];
  if Pos('i', S) > 0 then
    Include(Result, roIgnoreCase);
  if Pos('m', S) > 0 then
    Include(Result, roMultiline);
  if Pos('s', S) > 0 then
    Exclude(Result, roSingleline);
  if Pos('x ', S) > 0 then
    Include(Result, roIgnorePatternWhitespace);
  if Pos('S', S) > 0 then
    Include(Result, roStudy);
end;

end.
