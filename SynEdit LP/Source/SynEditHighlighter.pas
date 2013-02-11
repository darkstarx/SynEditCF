{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditHighlighter.pas, released 2000-04-07.

The Original Code is based on mwHighlighter.pas by Martin Waldenburg, part of
the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
Unicode translation by Maлl Hцrz.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

$Id: SynEditHighlighter.pas,v 1.36.2.18 2008/09/14 16:24:58 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net
-------------------------------------------------------------------------------}

unit SynEditHighlighter;

{$I SynEdit.inc}

interface

uses
  Graphics, Windows, Registry, IniFiles, SysUtils, Classes, HTMLColors,

  { SynEdit }
  SynEditTypes, SynEditMiscClasses, SynUnicode, SynEditCodeFolding;

type

  { Highlighter paired elements match }
  PSynTokenMatch = ^TSynTokenMatch;
  TSynTokenMatch = record
    OpenRange: Pointer;
    OpenToken: UnicodeString;
    OpenTokenKind: Integer;
    CloseRange: Pointer;
    CloseToken: UnicodeString;
    CloseTokenKind: Integer;
    Backreference: Boolean;
  end;

  { An array of matching rules }
  TSynMatchTokenArray = array of TSynTokenMatch;

  { Indentation rule option }
  TSynIndentationOption = (sioIndent, sioOnlyOnce);

  { A set of indentation options }
  TSynIndentationOptions = set of TSynIndentationOption;

  { Indentation rule }
  PSynIndentationRule = ^TSynIndentationRule;
  TSynIndentationRule = record
    Pattern: UTF8String;
    TokenKind: Integer;
    Range: Pointer;
    Options: TSynIndentationOptions;
  end;

  { An array of indentation rules }
  TSynIndentationRuleArray = array of TSynIndentationRule;

  { Theme file element }
  PSynThemeColor = ^TSynThemeColor;
  TSynThemeColor = record
    vNames: UTF8String;
    vHashes: TArray<Cardinal>;
    vParents: TArray<TArray<UTF8String>>;
    vBG, vFG: TColor;
    vStyle: Integer;
  end;

  { Array of theme elements }
  TSynThemeColors = array of TSynThemeColor;

  { Theme file reader }
  TSynHighlighterTheme = class
  private
    fFileName: UnicodeString;
    fUUID, fName, fDeveloper, fVersion: UTF8String;
    fColors: TSynThemeColors;

    function GetCount: Integer;
    function GetElement(Index: Integer): PSynThemeColor;

    procedure FreeColors;
  public
    constructor Create(const FileName: UnicodeString; Hash: Boolean = True);
    destructor Destroy; override;

    function AddElement: PSynThemeColor;
    procedure RemoveElement(Index: Integer);

    procedure StyleAttribute(Attr: TSynHighlighterAttributes);
    procedure SaveStylesheet(const FileName: UnicodeString);

    property Element[Index: Integer]: PSynThemeColor read GetElement;
    property FileName: UnicodeString read fFileName write fFileName;
    property UUID: UTF8String read fUUID write fUUID;
    property Name: UTF8String read fName write fName;
    property Developer: UTF8String read fDeveloper write fDeveloper;
    property Version: UTF8String read fVersion write fVersion;
    property Count: Integer read GetCount;
  end;

  procedure FillHashes(const S: UTF8String; var Hashes: TArray<Cardinal>;
    var Parents: TArray<TArray<UTF8String>>);

const
  SYN_ATTR_COMMENT    = 0;
  SYN_ATTR_IDENTIFIER = 1;
  SYN_ATTR_KEYWORD    = 2;
  SYN_ATTR_STRING     = 3;
  SYN_ATTR_WHITESPACE = 4;
  SYN_ATTR_SYMBOL     = 5;
  SYN_ATTR_NUMBER     = 6;

type
  TSynCustomHighlighter = class(TComponent)
  private
    fAttrChangeHooks: TSynNotifyEventChain;
    fUpdateCount: Integer;
    fEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetFriendlyLanguageName: String;
    function GetLanguageName: String;
  protected
    Run: Integer;
    fThemeFileName,
    fLanguageName: String;
    fFriendlyLanguageName: String;
    fAttributes: TStringList;
    fFoldRegions: TFoldRegions;
    fCasedLine: PChar;
    fCasedLineStr: UnicodeString;
    fCaseSensitive: Boolean;
    fDefaultFilter: String;
    fLine: PChar;
    fLineLen: Integer;
    fLineStr: UnicodeString;
    fLineNumber: Integer;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    fUpdateChange: Boolean;
    fTokenNature: TTokenNature;
    procedure Loaded; override;
    procedure AddAttribute(Attri: TSynHighlighterAttributes);
    procedure SetAttributesOnChange(AEvent: TNotifyEvent);
    procedure DefHighlightChange(Sender: TObject);
    procedure DefineProperties(Filer: TFiler); override;
    procedure FreeHighlighterAttributes;
    function GetAttribCount: Integer; virtual;
    function GetAttribute(Index: Integer): TSynHighlighterAttributes; virtual;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      virtual; abstract;
    function GetDefaultFilter: string; virtual;
    function GetSampleSource: UnicodeString; virtual;
    procedure DoSetLine(const Value: UnicodeString; LineNumber: Integer;
      Force: Boolean = False); virtual;
    function IsCurrentToken(const Token: UnicodeString): Boolean; virtual;
    function IsLineEnd(Run: Integer): Boolean; virtual;
    procedure SetDefaultFilter(Value: string); virtual;
  public
    MatchTokens: TSynMatchTokenArray;
    IndentationRules: TSynIndentationRuleArray;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetEol: Boolean; virtual; abstract;
    function GetKeyWords(TokenKind: Integer): UnicodeString; virtual;
    function GetRange: Pointer; virtual;
    function GetToken: UnicodeString; virtual;
    function GetTokenAttribute: TSynHighlighterAttributes; virtual; abstract;
    function GetTokenKind: Integer; virtual; abstract;
    function GetTokenPos: Integer; virtual;
    function GetTokenLen: Integer; virtual;
    function GetTokenNature: TTokenNature; virtual;
    function IsKeyword(const AKeyword: UnicodeString): Boolean; virtual;
    procedure Next; virtual;
    procedure NextToEol;
    procedure SetLine(const Value: UnicodeString; LineNumber: Integer;
      Force: Boolean = False); virtual;
    procedure SetRange(Value: Pointer); virtual;
    procedure ResetRange; virtual;

    function AddMatchToken: Integer;
    procedure DeleteMatchToken(Index: Integer);

    function AddIndentationRule: Integer;
    procedure DeleteIndentationRule(Index: Integer);

    procedure LoadFromFile(ThemeFile: TSynHighlighterTheme); overload; virtual;

    procedure LinesInserted(AIndex: Integer; ACount: Integer); virtual;
    procedure LinesDeleted(AIndex: integer; ACount: Integer); virtual;
    procedure LinesPutted(AIndex: integer; ACount: Integer); virtual;

    procedure HookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
    procedure UnhookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
    function IsIdentChar(AChar: WideChar): Boolean; virtual;
    function IsIdentCharEx(AChar: WideChar; AToken: Integer): Boolean; virtual;
    function IsWhiteChar(AChar: Char): Boolean; virtual;
    function IsWordBreakChar(AChar: Char): Boolean; virtual;
    property FriendlyLanguageName: String read GetFriendlyLanguageName write fFriendlyLanguageName;
    property LanguageName: String read GetLanguageName;
    property ThemeFileName: String read fThemeFileName;
  public
    property AttrCount: Integer read GetAttribCount;
    property Attribute[Index: Integer]: TSynHighlighterAttributes
      read GetAttribute;
    property SampleSource: UnicodeString read GetSampleSource;
    property CommentAttribute: TSynHighlighterAttributes
      index SYN_ATTR_COMMENT read GetDefaultAttribute;
    property IdentifierAttribute: TSynHighlighterAttributes
      index SYN_ATTR_IDENTIFIER read GetDefaultAttribute;
    property KeywordAttribute: TSynHighlighterAttributes
      index SYN_ATTR_KEYWORD read GetDefaultAttribute;
    property StringAttribute: TSynHighlighterAttributes
      index SYN_ATTR_STRING read GetDefaultAttribute;
    property SymbolAttribute: TSynHighlighterAttributes
      index SYN_ATTR_SYMBOL read GetDefaultAttribute;
    property WhitespaceAttribute: TSynHighlighterAttributes
      index SYN_ATTR_WHITESPACE read GetDefaultAttribute;
    property FoldRegions: TFoldRegions read fFoldRegions;
  published
    property DefaultFilter: string read GetDefaultFilter write SetDefaultFilter;
    property Enabled: Boolean read fEnabled write SetEnabled default True;
  end;

implementation

uses
  WideStrUtils, SynEditStrConst, SynUniClasses, SynUniRules;

procedure FillHashes(const S: UTF8String; var Hashes: TArray<Cardinal>;
  var Parents: TArray<TArray<UTF8String>>);
var
  I, J, K: Integer;
begin
  I := 1;
  J := I;
  SetLength(Hashes, 0);
  SetLength(Parents, 0);
  while I <= Length(S) do
  begin
    SetLength(Parents, Succ(Length(Parents)));
    SetLength(Parents[Pred(Length(Parents))], 0);
    while J <= Length(S) do
    begin
      if S[J] = #$2C then // ','
        Break;
      if S[J] = #32 then
      begin
        SetLength(Parents[Pred(Length(Parents))], Succ(Length(Parents[Pred(Length(Parents))])));
        Parents[Pred(Length(Parents))][Pred(Length(Parents[Pred(Length(Parents))]))] := Copy(S, I, J - I);
        while (J < Length(S)) and (S[J] = #32) do Inc(J);
        I := J;
      end;
      Inc(J);
    end;
    if I = J then
    begin
      SetLength(Parents, Pred(Length(Parents)));
      Break;
    end;
    SetLength(Hashes, Succ(Length(Hashes)));
    Hashes[Pred(Length(Hashes))] := MurmurHash2(@S[I], J - I, HashSeed);
    I := J + 2;
    J := I;
  end;
end;

{ TSynHighlighterTheme }

const
  sColorsTag: UTF8String = 'colors {';
  sUUIDParam: UTF8String = 'uuid = ''';
  sNameParam: UTF8String = 'name = ''';
  sDeveloperParam: UTF8String = 'developer = ''';
  sVersionParam: UTF8String = 'version = ''';
  sBgParam: UTF8String = 'background = ''';
  sFgParam: UTF8String = 'foreground = ''';
  sStyleParam: UTF8String = 'style = ''';
  sTail: UTF8String = ''';';
  sEnd: UTF8String = '}';

constructor TSynHighlighterTheme.Create(const FileName: UnicodeString;
  Hash: Boolean = True);
var
  F: TextFile;
  S: UTF8String;
  P: PAnsiChar;
  Valid: Boolean;

  {$INCLUDE UFormatInput.inc}

  procedure ReadInfo;
  begin
    RdLn;
    fUUID := GetValue(sUUIDParam); RdLn;
    fName := GetValue(sNameParam); RdLn;
    fDeveloper := GetValue(sDeveloperParam); RdLn;
    fVersion := GetValue(sVersionParam); RdLn;
  end;

  procedure ReadElements;

    function ReadColor(const S: UTF8String): TColor;
    begin
      { Initialize }
      Result := clNone;

      { Try to read as HTML / CSS color }
      if (Length(S) > 0) and (S[1] = '#') then
        try
          Result := HexToTColor(S);
          Exit;
        except
          Result := clNone;
        end;

      { Try to read as a Delphi color }
      try
        Result := StringToColor(S);
      except
        Result := clNone;
      end;
    end;

  begin
    while not IsTagRead(sEnd) do
    begin
      SetLength(fColors, Succ(Length(fColors)));
      with fColors[High(fColors)] do
      begin
        vNames := GetTag; RdLn;
        if Hash then
          FillHashes(vNames, vHashes, vParents);
        vBG := ReadColor(GetValue(sBgParam)); RdLn;
        vFG := ReadColor(GetValue(sFgParam)); RdLn;
        vStyle := StrToInt(GetValue(sStyleParam)); RdLn;
      end;
      Valid := IsTagRead(sEnd); if not Valid then Exit;
      RdLn;
    end;
  end;

  procedure ReadColors;
  begin
    Valid := IsTag(sColorsTag); if not Valid then Exit;
    ReadInfo; if not Valid then Exit;
    ReadElements; if not Valid then Exit;
    Valid := IsTagRead(sEnd);
  end;

begin
  { Initialize }
  SetLength(fColors, 0);

  { Check }
  if not FileExists(FileName) then Valid := False else Valid := True;

  { Read colors }
  if Valid then
  begin
    AssignFile(F, FileName);
    try
      Reset(F);
      ReadColors;
      fFileName := FileName;
    finally
      CloseFile(F);
    end;
  end;

  { Reset }
  if not Valid then
  begin
    fFileName := EmptyStr;
    fUUID := EmptyStr;
    fName := EmptyStr;
    fDeveloper := EmptyStr;
    fVersion := EmptyStr;
    FreeColors;

    { Always add text element }
    SetLength(fColors, 1);
    with fColors[0] do
    begin
      vNames := 'text, source';
      if Hash then
        FillHashes(vNames, vHashes, vParents);
      vBG := clNone;
      vFG := clNone;
      vStyle := 0;
    end;
  end;
end;

destructor TSynHighlighterTheme.Destroy;
begin
  FreeColors;
  inherited;
end;

function TSynHighlighterTheme.GetCount: Integer;
begin
  Result := Length(fColors);
end;

function TSynHighlighterTheme.GetElement(Index: Integer): PSynThemeColor;
begin
  Result := nil;
  if (Index < 0) or (Index >= Length(fColors)) then Exit;
  Result := PSynThemeColor(@fColors[Index]);
end;

function TSynHighlighterTheme.AddElement: PSynThemeColor;
begin
  SetLength(fColors, Succ(Length(fColors)));
  Result := PSynThemeColor(@fColors[High(fColors)]);
end;

procedure TSynHighlighterTheme.RemoveElement(Index: Integer);
var
  I, J: Integer;
begin
  if Length(fColors) = 0 then Exit;
  if (Index < 0) or (Index >= Length(fColors)) then Exit;
  for I := Index to Pred(High(fColors)) do
  begin
    fColors[I].vNames := fColors[Succ(I)].vNames;
    fColors[I].vBG := fColors[Succ(I)].vBG;
    fColors[I].vFG := fColors[Succ(I)].vFG;
    fColors[I].vStyle := fColors[Succ(I)].vStyle;
    SetLength(fColors[I].vHashes, Length(fColors[Succ(I)].vHashes));
    for J := 0 to Pred(Length(fColors[I].vHashes)) do
      fColors[I].vHashes[J] := fColors[Succ(I)].vHashes[J];
  end;
  SetLength(fColors, High(fColors));
end;

procedure TSynHighlighterTheme.FreeColors;
var
  I: Integer;
begin
  for I := 0 to High(fColors) do
    SetLength(fColors[I].vHashes, 0);
  SetLength(fColors, 0);
end;

procedure TSynHighlighterTheme.StyleAttribute(Attr: TSynHighlighterAttributes);
var
  I, J, N, K, C, Deep: Integer;
  S: UTF8String;
  Hashes: array of TArray<Cardinal>;
  Positions: TArray<Word>;
  Match: array [0..1] of Byte;
  bCan: Boolean;
  Rule: TSynRule;

  function ParentMatch: Boolean;
  var
    D: Integer;
    S: UTF8String;
  begin
    Result := False;
    S := Rule.Style;
    D := 1;
    while D <= Length(S) do
    begin
      if CompareMem(@fColors[I].vParents[K][C][1], @S[D],
        Length(fColors[I].vParents[K][C])) then
      begin
        Result := True;
        Break;
      end;
      while (D <= Length(S)) and (S[D] <> #$2C) do
        Inc(D);
      Inc(D, 2);
    end;
  end;

begin
  { Check for empty stylesheet and errors }
  if Length(fColors) = 0 then Exit;

  { Find how much diffirent scope variants we have and their positions }
  try
    S := Attr.FriendlyName;
  except
    S := EmptyAnsiStr;
  end;
  if S = EmptyAnsiStr then Exit;
  J := 0;
  SetLength(Positions, 1);
  Positions[0] := 1;
  for I := 1 to Length(S) do
    if S[I] = ',' then
    begin
      SetLength(Positions, Length(Positions) + 2);
      Positions[Length(Positions) - 2] := Pred(I);
      Positions[Length(Positions) - 1] := I + 2;
      Inc(J);
    end;
  SetLength(Positions, Succ(Length(Positions)));
  Positions[Pred(Length(Positions))] := Length(S);
  SetLength(Hashes, Succ(J));

  { Expand each style set }
  for I := 0 to High(Hashes) do
  begin

    { Find how much sub-styles given style has }
    SetLength(Hashes[I], 0);
    N := Positions[I shl 1];
    for J := N to Positions[I shl 1 + 1] do
      if S[J] = '.' then
      begin
        SetLength(Hashes[I], Succ(Length(Hashes[I])));
        Hashes[I][Pred(Length(Hashes[I]))] := MurmurHash2(@S[N], J - N, HashSeed);
      end;
    SetLength(Hashes[I], Succ(Length(Hashes[I])));
    Hashes[I][Pred(Length(Hashes[I]))] := MurmurHash2(@S[N],
      Positions[I shl 1 + 1] - N + 1, HashSeed);
  end;

  { Now look for the deepest match }
  Deep := -1;
  for I := 0 to High(fColors) do
    for J := 0 to High(Hashes) do
      for N := High(Hashes[J]) downto 0 do
        for K := 0 to High(fColors[I].vHashes) do
          if (fColors[I].vHashes[K] = Hashes[J][N]) and (Deep < N) then
          begin

            { Check parent scope if present }
            if (Length(fColors[I].vParents[K]) > 0) and (Attr.Rule <> nil) then
            begin
              bCan := True;
              Rule := (Attr.Rule as TSynRule).Parent;
              for C := High(fColors[I].vParents[K]) downto 0 do
              begin
                if Rule = nil then
                begin
                  bCan := False;
                  Break;
                end;
                if not ParentMatch then
                begin
                  bCan := False;
                  Break;
                end;
                Rule := Rule.Parent;
              end;
              if not bCan then Continue;
            end;
            Deep := N;
            Match[0] := I;
            Match[1] := K;
          end;

  { Output result }
  if Deep = -1 then Match[0] := 0;
  with Attr, fColors[Match[0]] do
  begin
    Background := vBG;
    Foreground := vFG;
    IntegerStyle := vStyle;
  end;
end;

procedure TSynHighlighterTheme.SaveStylesheet(const FileName: UnicodeString);
var
  F: TextFile;
  I: Integer;

  {$INCLUDE UFormatOutput.inc}

  procedure WriteInfo;
  begin
    WrLn(sUUIDParam + fUUID + sTail);
    WrLn(sNameParam + fName + sTail);
    WrLn(sDeveloperParam + fDeveloper + sTail);
    WrLn(sVersionParam + fVersion + sTail);
  end;

  procedure WriteColors;
  var
    J: Integer;

    function ConvertColorToString(const C: TColor): UTF8String;
    var
      S: UnicodeString;
    begin
      S := ColorToString(C);
      if S[1] = '$' then
        S := '#' + LowerCase(HTMLColors.ColorToHex(C));
      Result := UnicodeStringToUTF8(S);
    end;

  begin
    for J := 0 to High(fColors) do
      with fColors[J] do
      begin
        WrLn(vNames + ' {');
        Inc(I);
          WrLn(sBgParam + ConvertColorToString(vBG) + sTail);
          WrLn(sFgParam + ConvertColorToString(vFG) + sTail);
          WrLn(sStyleParam + IntToStr(vStyle) + sTail);
        Dec(I);
        WrLn(sEnd);
      end;
  end;

begin
  I := 0;
  AssignFile(F, FileName);
  try
    Rewrite(F);
    WrLn(sColorsTag);
    Inc(I);
      WriteInfo;
      WriteColors;
    Dec(I);
    WrLn(sEnd);
  finally
    CloseFile(F);
  end;
end;

{ TSynCustomHighlighter }

// -----------------------------------------------------------------------------

constructor TSynCustomHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fThemeFileName := '';
  fLanguageName := '';
  fFriendlyLanguageName := '';
  fAttributes := TStringList.Create;
  fAttributes.Duplicates := dupIgnore;
  fAttributes.Sorted := True;
  fAttrChangeHooks := TSynNotifyEventChain.CreateEx(Self);
  fDefaultFilter := '';
  fEnabled := True;
  fFoldRegions := TFoldRegions.Create(TFoldRegionItem);
  fTokenNature := tknnUnknown;
end;

destructor TSynCustomHighlighter.Destroy;
begin
  inherited Destroy;
  FreeHighlighterAttributes;
  fAttributes.Free;
  fAttrChangeHooks.Free;
  fFoldRegions.Clear;
  fFoldRegions.Free;
  SetLength(MatchTokens, 0);
  SetLength(IndentationRules, 0);
end;

procedure TSynCustomHighlighter.BeginUpdate;
begin
  Inc(fUpdateCount);
end;

procedure TSynCustomHighlighter.EndUpdate;
begin
  if fUpdateCount > 0 then
  begin
    Dec(fUpdateCount);
    if (fUpdateCount = 0) and fUpdateChange then
    begin
      fUpdateChange := False;
      DefHighlightChange(nil);
    end;
  end;
end;

procedure TSynCustomHighlighter.FreeHighlighterAttributes;
var
  i: Integer;
begin
  if fAttributes <> nil then
  begin
    for i := fAttributes.Count - 1 downto 0 do
      TSynHighlighterAttributes(fAttributes.Objects[i]).Free;
    fAttributes.Clear;
  end;
end;

procedure TSynCustomHighlighter.Assign(Source: TPersistent);
var
  Src: TSynCustomHighlighter;
  i, j: Integer;
  AttriName: string;
  SrcAttri: TSynHighlighterAttributes;
begin
  if (Source <> nil) and (Source is TSynCustomHighlighter) then
  begin
    Src := TSynCustomHighlighter(Source);
    for i := 0 to AttrCount - 1 do
    begin
      // assign first attribute with the same name
      AttriName := Attribute[i].Name;
      for j := 0 to Src.AttrCount - 1 do
      begin
        SrcAttri := Src.Attribute[j];
        if AttriName = SrcAttri.Name then
        begin
          Attribute[i].Assign(SrcAttri);
          break;
        end;
      end;
    end;
    DefaultFilter := Src.DefaultFilter;
    Enabled := Src.Enabled;
  end
  else
    inherited Assign(Source);
end;

function TSynCustomHighlighter.AddMatchToken: Integer;
begin
  Result := Length(MatchTokens);
  SetLength(MatchTokens, Succ(Result));
  with MatchTokens[Result] do
  begin
    OpenRange := nil;
    OpenToken := EmptyStr;
    OpenTokenKind := -1;
    CloseRange := nil;
    CloseToken := EmptyStr;
    CloseTokenKind := -1;
    Backreference := False;
  end;
end;

procedure TSynCustomHighlighter.DeleteMatchToken(Index: Integer);
begin
  if (Index < 0) or (Index > High(MatchTokens)) then
    Exit;
  for Index := Succ(Index) to High(MatchTokens) do
    MatchTokens[Pred(Index)] := MatchTokens[Index];
  SetLength(MatchTokens, Pred(Length(MatchTokens)));
end;

function TSynCustomHighlighter.AddIndentationRule: Integer;
begin
  Result := Length(IndentationRules);
  SetLength(IndentationRules, Succ(Result));
  with IndentationRules[Result] do
  begin
    Pattern := '';
    TokenKind := -1;
    Range := nil;
    Options := [];
  end;
end;

procedure TSynCustomHighlighter.DeleteIndentationRule(Index: Integer);
begin
  if (Index < 0) or (Index > High(IndentationRules)) then
    Exit;
  for Index := Succ(Index) to High(IndentationRules) do
    IndentationRules[Pred(Index)] := IndentationRules[Index];
  SetLength(IndentationRules, Pred(Length(IndentationRules)));
end;

procedure TSynCustomHighlighter.LoadFromFile(ThemeFile: TSynHighlighterTheme);
var
  I: Integer;
begin
  fThemeFileName := ThemeFile.FileName;
  for I := 0 to Pred(AttrCount) do
    ThemeFile.StyleAttribute(Attribute[I]);
end;

procedure TSynCustomHighlighter.LinesInserted(AIndex: Integer; ACount: Integer);
begin
end;

procedure TSynCustomHighlighter.LinesDeleted(AIndex: integer; ACount: Integer);
begin
end;

procedure TSynCustomHighlighter.LinesPutted(AIndex: integer; ACount: Integer);
begin
end;

procedure TSynCustomHighlighter.AddAttribute(Attri: TSynHighlighterAttributes);
begin
  fAttributes.AddObject(Attri.Name, Attri);
end;

procedure TSynCustomHighlighter.SetAttributesOnChange(AEvent: TNotifyEvent);
begin
  { Do nothing }
end;

procedure TSynCustomHighlighter.DefHighlightChange(Sender: TObject);
begin
  if fUpdateCount > 0 then
    fUpdateChange := True
  else if not(csLoading in ComponentState) then
  begin
    fAttrChangeHooks.Sender := Sender;
    fAttrChangeHooks.Fire;
  end;
end;

procedure TSynCustomHighlighter.DefineProperties(Filer: TFiler);
begin
  inherited;
{$IFNDEF UNICODE}
  UnicodeDefineProperties(Filer, Self);
{$ENDIF}
end;

function TSynCustomHighlighter.GetAttribCount: Integer;
begin
  Result := fAttributes.Count;
end;

function TSynCustomHighlighter.GetAttribute(Index: Integer):
  TSynHighlighterAttributes;
begin
  Result := nil;
  if (Index >= 0) and (Index < fAttributes.Count) then
    Result := TSynHighlighterAttributes(fAttributes.Objects[Index]);
end;

function TSynCustomHighlighter.GetDefaultFilter: string;
begin
  Result := fDefaultFilter;
end;

function TSynCustomHighlighter.GetFriendlyLanguageName: String;
begin
  Result := fFriendlyLanguageName;
end;

function TSynCustomHighlighter.GetLanguageName: String;
begin
  Result := fLanguageName;
end;

function TSynCustomHighlighter.GetRange: Pointer;
begin
  Result := nil;
end;

function TSynCustomHighlighter.GetToken: UnicodeString;
var
  Len: Integer;
begin
  Len := Run - fTokenPos;
  SetLength(Result, Len);
  if Len > 0 then
    WStrLCopy(@Result[1], fCasedLine + fTokenPos, Len);
end;

function TSynCustomHighlighter.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynCustomHighlighter.GetTokenLen: Integer;
begin
  Result := Run - fTokenPos;
end;

function TSynCustomHighlighter.GetTokenNature: TTokenNature;
begin
  Result := fTokenNature;
end;

function TSynCustomHighlighter.GetKeyWords(TokenKind: Integer): UnicodeString;
begin
  Result := '';
end;

function TSynCustomHighlighter.GetSampleSource: UnicodeString;
begin
  Result := '';
end;

procedure TSynCustomHighlighter.HookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
begin
  fAttrChangeHooks.Add(ANotifyEvent);
end;

function TSynCustomHighlighter.IsCurrentToken(const Token: UnicodeString): Boolean;
var
  I: Integer;
  Temp: PChar;
begin
  Temp := fToIdent;
  if Length(Token) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do
    begin
      if Temp^ <> Token[i] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end
  else
    Result := False;
end;

function TSynCustomHighlighter.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := not IsWordBreakChar(AChar);
end;

// § Garnet
// -----------------------------------------------------------------------------
// Check additional chars for specific token.
// Used *only* after parsing and painting routines for retrieving word at
// cursor
function TSynCustomHighlighter.IsIdentCharEx(AChar: WideChar;
  AToken: Integer): Boolean;
begin
  Result := IsIdentChar(AChar); // Obviously standard highlighter has no
                                // additional chars
end;

function TSynCustomHighlighter.IsKeyword(const AKeyword: UnicodeString): Boolean;
begin
  Result := False;
end;

function TSynCustomHighlighter.IsLineEnd(Run: Integer): Boolean;
begin
  Result := (Run >= fLineLen) or (fLine[Run] = #10) or (fLine[Run] = #13);
end;

function TSynCustomHighlighter.IsWhiteChar(AChar: Char): Boolean;
begin
  Result := SynEditTypes.IsWhitespaceChar(AChar);
end;

function TSynCustomHighlighter.IsWordBreakChar(AChar: Char): Boolean;
begin
  Result := SynEditTypes.IsWordBreakChar(AChar);
end;

procedure TSynCustomHighlighter.Next;
begin
  { Do nothing }
end;

procedure TSynCustomHighlighter.NextToEol;
begin
  while not GetEol do Next;
end;

procedure TSynCustomHighlighter.ResetRange;
begin
  { Do nothing }
end;

procedure TSynCustomHighlighter.SetLine(const Value: UnicodeString;
  LineNumber: Integer; Force: Boolean = False);
begin
  DoSetLine(Value, LineNumber, Force);
  Next;
end;

procedure TSynCustomHighlighter.DoSetLine(const Value: UnicodeString;
  LineNumber: Integer; Force: Boolean = False);
begin
  { Old comment by someone:
    UnicodeStrings are not reference counted, hence we need to copy.
    § Garnet: UnicodeStrings ARE reference counted. WideStrings aren't.
    See any Embarcadero document on Delphi & Unicode }
  if fCaseSensitive then
  begin
    fLine := PChar(Value);
    fLineLen := Length(Value);
    fCasedLineStr := '';
    fCasedLine := PChar(Value);
  end
  else begin
    fLineStr := WideLowerCase(Value);
    fLine := PChar(fLineStr);
    fLineLen := Length(fLineStr);
    fCasedLine := PChar(Value);
  end;
  Run := 0;
  fLineNumber := LineNumber;
end;

procedure TSynCustomHighlighter.SetRange(Value: Pointer);
begin
end;

procedure TSynCustomHighlighter.SetDefaultFilter(Value: string);
begin
  fDefaultFilter := Value;
end;

procedure TSynCustomHighlighter.UnhookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
begin
  fAttrChangeHooks.Remove(ANotifyEvent);
end;

procedure TSynCustomHighlighter.SetEnabled(const Value: Boolean);
begin
  if fEnabled <> Value then
  begin
    fEnabled := Value;
    DefHighlightChange(nil);
  end;
end;

procedure TSynCustomHighlighter.Loaded;
begin
  inherited;
  DefHighlightChange(nil);
end;

end.
