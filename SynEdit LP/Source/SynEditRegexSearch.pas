(*

  Letterpress

  Copyright Garnet, 2009-2010

*)

{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditRegexSearch.pas, released 2002-07-26.

Original Code by Eduardo Mauro, Gerald Nunn and Flávio Etrusco.
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

$Id: SynEditRegexSearch.pas,v 1.5.2.2 2008/09/14 16:24:59 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net
-------------------------------------------------------------------------------}

unit SynEditRegexSearch;

{$I SynEdit.inc}

interface

uses
  Classes, RegularExpressions,

  { SynEdit }
  SynEditTypes, SynEditMiscClasses, SynUnicode;

type
  TSynEditMatch = record
    Index, Length: Integer;
    Replacement: UnicodeString;
  end;

  TSynEditMatches = array of TSynEditMatch;

  TSynEditRegexSearch = class(TSynEditSearchCustom)
  private
    fExpression, fReplacement: UnicodeString;
    fCaseInsensitive: Boolean;
    fMatches: TSynEditMatches;
  protected
    function GetPattern: UnicodeString; override;
    procedure SetPattern(const Value: UnicodeString); override;
    function GetReplacePattern: UnicodeString; override;
    procedure SetReplacePattern(const Value: UnicodeString); override;
    procedure SetOptions(const Value: TSynSearchOptions); override;
    function GetLength(Index: Integer): Integer; override;
    function GetResult(Index: Integer): Integer; override;
    function GetResultCount: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindAll(const NewText: UnicodeString): Integer; override;
    function Replace(const aOccurrence, aReplacement: UnicodeString; Index: Integer): UnicodeString; override;
  end;

implementation

uses
  Consts, SynEditMiscProcs;

{ TSynEditRegexSearch }

// -----------------------------------------------------------------------------
constructor TSynEditRegexSearch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetLength(fMatches, 0);
end;

destructor TSynEditRegexSearch.Destroy;
begin
  SetLength(fMatches, 0);
  inherited;
end;

function TSynEditRegexSearch.FindAll(const NewText: UnicodeString): Integer;
var
  Options: TRegexOptions;
  S, Expression, ReplacementS: UTF8String;
  Match: IMatch;
begin
  SetLength(fMatches, 0);
  S := UnicodeStringToUTF8(NewText);
  Expression := UnicodeStringToUTF8(fExpression);
  ReplacementS := UnicodeStringToUTF8(fReplacement);

  if fCaseInsensitive then
    Options := [roStudy, roIgnoreCase, roIgnorePatternWhitespace, roSkipEmptyMatches, roSingleline]
  else
    Options := [roStudy, roIgnorePatternWhitespace, roSkipEmptyMatches, roSingleline];

  Match := TRegex.Match(S, Expression, Options);
  while Match.Success do
  begin
    SetLength(fMatches, Succ(Length(fMatches)));
    with fMatches[High(fMatches)] do
    begin
      Index := CountUTF8Chars(S, Match.Index);
      Length := CountUTF8Chars(S, Match.Index + Match.Length - 1, Match.Index);
      Replacement := UTF8ToUnicodeString(Match.Result(ReplacementS));
    end;
    Match := Match.NextMatch;
  end;

  Result := Length(fMatches);
end;

function TSynEditRegexSearch.Replace(const aOccurrence, aReplacement: UnicodeString; Index: Integer): UnicodeString;
begin
  if (Index >= 0) and (Index < Length(fMatches)) then
    Result := fMatches[Index].Replacement
  else
    Result := aOccurrence;
end;

function TSynEditRegexSearch.GetLength(Index: Integer): Integer;
begin
  Result := fMatches[Index].Length;
end;

function TSynEditRegexSearch.GetPattern: UnicodeString;
begin
  Result := fExpression;
end;

function TSynEditRegexSearch.GetResult(Index: Integer): Integer;
begin
  Result := fMatches[Index].Index;
end;

function TSynEditRegexSearch.GetResultCount: Integer;
begin
  Result := Length(fMatches);
end;

procedure TSynEditRegexSearch.SetOptions(const Value: TSynSearchOptions);
begin
  fCaseInsensitive := not (ssoMatchCase in Value);
end;

procedure TSynEditRegexSearch.SetPattern(const Value: UnicodeString);
begin
  fExpression := Value;
end;

function TSynEditRegexSearch.GetReplacePattern: UnicodeString;
begin
  Result := fReplacement;
end;

procedure TSynEditRegexSearch.SetReplacePattern(const Value: UnicodeString);
begin
  fReplacement := Value;
end;

end.

