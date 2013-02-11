(*

  Letterpress, Lantern

  Implements all SynUni rules (ranges, keywords, sets, special rules and links).

  Copyright 2006-2010, initial developers and Garnet

*)

unit SynUniRules;

interface

uses
  Classes, SysUtils,

  { SynEdit }
  SynEditTypes, SynEditMiscProcs, SynEditHighlighter,

  { SynUni }
  SynUniClasses;

type
  { For parent reference declaration }
  TSynRange = class;

  { Basic rule definition }
  TSynAbstractRule = class abstract
  private
    fRange: TSynRange; // Parent range of this rule
  public
    property Parent: TSynRange read fRange write fRange;
  end;

  { Basic rule definition as an object with it's unique name, link to style
    in color scheme, parent (range) and highlight attributes }
  TSynRule = class(TSynAbstractRule)
  private
    fName: UTF8String;    // Name of a rule for unique access
    fLinkName: UTF8String; // Original name before linking
    fStyle: UTF8String;   // Name of style to apply to attributes from color scheme
    fLinkStyle: UTF8String; // Original scope before linking
    fAttributes: TSynHighlighterAttributes;

    procedure SetName(const AName: UTF8String); virtual;
    procedure SetStyle(const AStyle: UTF8String); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    property Name: UTF8String read fName write SetName;
    property LinkName: UTF8String read fLinkName write fLinkName;
    property Style: UTF8String read fStyle write SetStyle;
    property LinkStyle: UTF8String read fLinkStyle write fLinkStyle;
    property Attributes: TSynHighlighterAttributes read fAttributes;
  end;

  { Extends rule to be a keyword processor }
  TSynKeywords = class(TSynRule)
  private
    fCount: Integer;
    fKeys: array of UnicodeString;
    fHashes: array of Cardinal;
    fOptions: TSynKeywordsOptions;
    fUpdating: Boolean;

    function GetKey(Index: Integer): UnicodeString;
    procedure DoHash;
  public
    constructor Create;
    destructor Destroy; override;

    function Match(const AToken: Pointer; const ALength: Integer;
      const AHash: Cardinal): Boolean;
    procedure AddKey(const AKey: UnicodeString);
    procedure BeginUpdate;
    procedure EndUpdate;

    property Keys[Index: Integer]: String read GetKey;
    property Count: Integer read fCount;
    property Options: TSynKeywordsOptions read fOptions write fOptions;
  end;

  { Extends rule to be a symbol, non-ident char processor }
  TSynSet = class(TSynRule)
  private
    fSymbols: UnicodeString;
    fOptions: TSynSetOptions;
  public
    constructor Create;

    function Match(const AChar: Char): Boolean;

    property Symbols: UnicodeString read fSymbols write fSymbols;
    property Options: TSynSetOptions read fOptions write fOptions;
  end;

  { Extends rule to be a simple regex pattern matcher }
  TSynSpecialRule = class(TSynRule)
  private
    fPattern, fReplacePattern: UTF8String;
    fOptions: TSynSpecialRuleOptions;
    fPriority: Integer;
    fGroupAttributes: TSynHighlighterAttributesArray;

    function GetGroupAttributeCount: Integer;
    function GetGroupAttribute(Index: Integer): TSynHighlighterAttributes;
  public
    constructor Create;
    destructor Destroy; override;

    function AddGroupAttribute(const AName, AStyle: String): TSynHighlighterAttributes;
    procedure DeleteGroupAttribute(Index: Integer);

    property Pattern: UTF8String read fPattern write fPattern;
    property ReplacePattern: UTF8String read fReplacePattern write fReplacePattern;
    property Options: TSynSpecialRuleOptions read fOptions write fOptions;
    property Priority: Integer read fPriority write fPriority;
    property GroupAttributeCount: Integer read GetGroupAttributeCount;
    property GroupAttributes[Index: Integer]: TSynHighlighterAttributes
      read GetGroupAttribute;
  end;

  { A simple link to any of rule above. A link can not be linked to another
    link. You can't link a rule which parent is the same as link's parent.
    You can't links main rules range }
  TSynRuleLink = class(TSynAbstractRule)
  private
    fLink: UTF8String;
    fLinkedRule: TSynRule;
    fOptions: TSynLinkOptions;
  public
    LinkOpenTokens, LinkCloseTokens: TIntegerArray;

    constructor Create;

    property Link: UTF8String read fLink write fLink;
    property Rule: TSynRule read fLinkedRule write fLinkedRule;
    property Options: TSynLinkOptions read fOptions write fOptions;
  end;

  { Extends rule to be a complete range with ability to hold subrules }
  TSynRange = class(TSynRule)
  private
    { General proeprties }
    fOptions: TSynRangeOptions; // Affects range bahaviour in string and map

    { Linking properties }
    fLinked: Boolean;           // Reference everything to linked range
    fLinksRules: Boolean;       // References everything but tokens to linked r.
    fLinkSource: TSynRange;     // From which range is linked

    { Range has additional global attributes }
    fSymbolAttributes: TSynHighlighterAttributes; // Additional symbol highlight
    fNumberAttributes: TSynHighlighterAttributes; // Additional number highlight

    { Delimiter properties }
    fAddDelimiters: UnicodeString;    // Chars to treat as delimiters
    fRemoveDelimiters: UnicodeString; // Chars to treat as ident

    { Range tokens }
    fOpenTokens: TSynTokens;    // Array of tokens which open this range
    fCloseTokens: TSynTokens;   // Array of tokens which close this range

    { Lists with rules }
    fRanges,                    // List of range rules
    fKeywords,                  // List of keyword list rules
    fSets,                      // List of set rules
    fSpecialRules,              // List of special pattern rules
    fLinks: TList;              // List of links

    { Things required for linking }
    fOriginalAttributes,
    fOriginalSymbolAttributes,
    fOriginalNumberAttributes: TSynHighlighterAttributes;
  private
    { Additional attributes }
    procedure SetName(const AName: UTF8String); override;
    procedure SetStyle(const AStyle: UTF8String); override;
    procedure SetLinked(const Value: Boolean);

    { Tokens }
    function GetOpenTokenCount: Integer;
    function GetOpenToken(AIndex: Integer): TSynToken;
    function GetCloseTokenCount: Integer;
    function GetCloseToken(AIndex: Integer): TSynToken;

    { Rules }
    function GetRangeCount: Integer;
    function GetRange(AIndex: Integer): TSynRange;
    function GetKeyWordsCount: Integer;
    function GetKeyWords(AIndex: Integer): TSynKeywords;
    function GetSetCount: Integer;
    function GetSet(AIndex: Integer): TSynSet;
    function GetSpecialRuleCount: Integer;
    function GetSpecialRule(AIndex: Integer): TSynSpecialRule;
    function GetLinkCount: Integer;
    function GetLink(AIndex: Integer): TSynRuleLink;
  public
    constructor Create(FromLink: Boolean = False; FromLinkRules: Boolean = False);
    destructor Destroy; override;

    { Tokens }
    function AddOpenToken(const AToken: UTF8String; AIns: Boolean;
      APriority: Integer = 0): TSynToken;
    function AddCloseToken(const AToken: UTF8String; AIns, AGlob: Boolean;
      APriority: Integer = 0): TSynToken;
    procedure RemoveToken(Index: Integer; Open: Boolean);

    { Rules }
    procedure AddRule(ARule: TSynAbstractRule);
    procedure RemoveRule(ARule: TSynAbstractRule);
    function GetRuleByName(const RuleName: UTF8String;
      Recursive: Boolean = True; ForLink: Boolean = False): TSynRule;

    { Linking }
    procedure LinkRange(ARange: TSynRange; LinkOpenTokens: TIntegerArray;
      LinkCloseTokens: TIntegerArray; LinkOptions: TSynLinkOptions = []);

    { Properties }
    property Options: TSynRangeOptions read fOptions write fOptions;

    { Linking properties }
    property Linked: Boolean read fLinked write SetLinked;
    property LinksRules: Boolean read fLinksRules write fLinksRules;
    property LinkSource: TSynRange read fLinkSource;

    { Attributes }
    property SymbolAttributes: TSynHighlighterAttributes read fSymbolAttributes;
    property NumberAttributes: TSynHighlighterAttributes read fNumberAttributes;

    { Delimiters }
    property AddDelimiters: UnicodeString read fAddDelimiters
      write fAddDelimiters;
    property RemoveDelimiters: UnicodeString read fRemoveDelimiters
      write fRemoveDelimiters;

    { Tokens }
    property OpenTokenCount: Integer read GetOpenTokenCount;
    property OpenTokens[Index: Integer]: TSynToken read GetOpenToken;
    property CloseTokenCount: Integer read GetCloseTokenCount;
    property CloseTokens[Index: Integer]: TSynToken read GetCloseToken;

    { Rules }
    property RangeCount: Integer read GetRangeCount;
    property Ranges[Index: Integer]: TSynRange read GetRange;
    property KeyWordsCount: Integer read GetKeyWordsCount;
    property KeyWords[Index: Integer]: TSynKeywords read GetKeyWords;
    property SetCount: Integer read GetSetCount;
    property Sets[Index: Integer]: TSynSet read GetSet;
    property SpecialRuleCount: Integer read GetSpecialRuleCount;
    property SpecialRules[Index: Integer]: TSynSpecialRule read GetSpecialRule;
    property LinkCount: Integer read GetLinkCount;
    property Links[Index: Integer]: TSynRuleLink read GetLink;
  end;

  function AddStyleSuffix(const S, P: UTF8String; Prepend: Boolean = False): UTF8String;

implementation

{ Fixes style param when embedding }
function AddStyleSuffix(const S, P: UTF8String; Prepend: Boolean = False): UTF8String;
const
  sSeparator: UTF8String = ', ';
  sSourceDot = 'source.';
  sTextDot = 'text.';
  sSource = 'source';
  sText = 'text';
var
  I, J, K: Integer;
  sElem: UTF8String;
  sPDot: UTF8String;
begin
  { Add embedded analog to every style element name }
  I := 1; J := I; K := 0;
  SetLength(Result, (Length(S) + 10) * 3);

  { Read style elements }
  while I <= Length(S) do
  begin

    { Get next element name }
    while J <= Length(S) do
    begin
      if S[J] = ',' then
        Break;
      Inc(J);
    end;
    if I = J then Break;
    if Prepend then
    begin
      sPDot := P + '.';
      sElem := Copy(S, I, J - I);
      if Pos(sSourceDot, sElem) = 1 then
      begin
        sElem := Copy(sElem, Succ(Length(sSourceDot)), MaxInt);
        sElem := sPDot + sElem;
      end
      else if Pos(sTextDot, sElem) = 1 then
      begin
        sElem := Copy(sElem, Succ(Length(sTextDot)), MaxInt);
        sElem := sPDot + sElem;
      end
      else if Pos(sPDot, sElem) = 1 then
        //sElem := Copy(sElem, Succ(Length(sTextDot)), MaxInt)
      else if sElem = sSource then
        sElem := P
      else if sElem = sText then
        sElem := P
      else if sElem <> P then
        sElem := sPDot + sElem;
    end
    else
      sElem := Copy(S, I, J - I) + '.' + sEmbedded;

    { Put into resulting style string }
    Move(sElem[1], Result[Succ(K)], Length(sElem)); Inc(K, Length(sElem));
    Move(sSeparator[1], Result[Succ(K)], Length(sSeparator)); Inc(K, Length(sSeparator));

    { Proceed next }
    I :=  J + 2;
    J := I;
  end;

  { Finalize string }
  if K > 0 then
    SetLength(Result, K - Length(sSeparator))
  else
    SetLength(Result, 0);
end;

{ TSynRule }

// -----------------------------------------------------------------------------

constructor TSynRule.Create;
begin
  inherited Create;
  fName := '';
  fStyle := '';
  fAttributes := TSynHighlighterAttributes.Create('', '');
  fAttributes.Rule := Self;
end;

destructor TSynRule.Destroy;
begin
  FreeAndNil(fAttributes);
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TSynRule.SetName(const AName: UTF8String);
begin
  fName := AName;
  fAttributes.Name := AName;
end;

procedure TSynRule.SetStyle(const AStyle: UTF8String);
begin
  fStyle := AStyle;
  fAttributes.FriendlyName := AStyle;
end;

{ TSynRange }

// -----------------------------------------------------------------------------

constructor TSynRange.Create(FromLink: Boolean = False;
  FromLinkRules: Boolean = False);
begin
  inherited Create;

  { Default properties }
  fOptions := [];

  { Delimiters }
  fAddDelimiters := EmptyStr;
  fRemoveDelimiters := EmptyStr;

  { Tokens }
  SetLength(fOpenTokens, 0);
  SetLength(fCloseTokens, 0);

  { Linked ranges don't have self attributes and rules. And they don't have
    self tokens unless they created with FromLinkRules parameter set to True }
  fLinked := FromLink;
  fLinksRules := FromLinkRules;

  { Additional default attributes }
  if not fLinked then
  begin
    fOriginalAttributes := nil;
    fOriginalSymbolAttributes := nil;
    fOriginalNumberAttributes := nil;
    fNumberAttributes := TSynHighlighterAttributes.Create(fName + '.digit', fAttributes.FriendlyName);
    fNumberAttributes.Rule := Self;
    fSymbolAttributes := TSynHighlighterAttributes.Create(fName + '.symbol', fAttributes.FriendlyName);
    fSymbolAttributes.Rule := Self;
  end

  { Save fAttributes reference for parent class }
  else begin
    fOriginalAttributes := fAttributes;
    fOriginalSymbolAttributes := fSymbolAttributes;
    fOriginalNumberAttributes := fNumberAttributes;
  end;

  { Lists }
  fRanges := TList.Create;
  fKeyWords := TList.Create;
  fSets := TList.Create;
  fSpecialRules := TList.Create;
  fLinks := TList.Create;
end;

destructor TSynRange.Destroy;
var
  I: Integer;
begin
  if not fLinked then
  begin
    { Attributes }
    FreeAndNil(fNumberAttributes);
    FreeAndNil(fSymbolAttributes);
  end
  else begin
    { Restore reference }
    fAttributes := fOriginalAttributes;
    if Assigned(fOriginalSymbolAttributes) then FreeAndNil(fOriginalSymbolAttributes);
    if Assigned(fOriginalNumberAttributes) then FreeAndNil(fOriginalNumberAttributes);
  end;

  { Tokens }
  if not fLinked or (fLinked and fLinksRules) then
  begin
    for I := 0 to High(fOpenTokens) do
      FreeAndNil(fOpenTokens[I]);
    for I := 0 to High(fCloseTokens) do
      FreeAndNil(fCloseTokens[I]);
  end;

  { Rules }
  FreeList(fRanges);
  FreeList(fKeyWords);
  FreeList(fSets);
  FreeList(fSpecialRules);
  FreeList(fLinks);

  { Length of tokens even if linked }
  SetLength(fOpenTokens, 0);
  SetLength(fCloseTokens, 0);

  inherited;
end;

// -----------------------------------------------------------------------------

procedure TSynRange.SetName(const AName: UTF8String);
begin
  inherited;
  if not fLinked then
  begin
    fNumberAttributes.Name := AName + '.digit';
    fSymbolAttributes.Name := AName + '.symbol';
  end;
end;

procedure TSynRange.SetStyle(const AStyle: UTF8String);
begin
  inherited;
  if not fLinked then
  begin
    fNumberAttributes.FriendlyName := AStyle;
    fSymbolAttributes.FriendlyName := AStyle;
  end;
end;

procedure TSynRange.SetLinked(const Value: Boolean);

// Once Value set to true it can't be reverted back.
// Do not free these. It can be set after attributes are filled in the list
// and finalization can cause uncatchable AV

begin
  if Value and not fLinked then
  begin
    fLinked := True;
    fOriginalAttributes := fAttributes;
    fOriginalSymbolAttributes := fSymbolAttributes;
    fOriginalNumberAttributes := fNumberAttributes;
  end;
end;

// -----------------------------------------------------------------------------
// Tokens
function TSynRange.GetOpenTokenCount: Integer;
begin
  Result := Length(fOpenTokens);
end;

function TSynRange.GetOpenToken(AIndex: Integer): TSynToken;
begin
  Result := fOpenTokens[AIndex];
end;

function TSynRange.GetCloseTokenCount: Integer;
begin
  Result := Length(fCloseTokens);
end;

function TSynRange.GetCloseToken(AIndex: Integer): TSynToken;
begin
  Result := fCloseTokens[AIndex];
end;

// -----------------------------------------------------------------------------
// Rules
function TSynRange.GetRangeCount: Integer;
begin
  Result := fRanges.Count;
end;

function TSynRange.GetRange(AIndex: Integer): TSynRange;
begin
  Result := TSynRange(fRanges[AIndex]);
end;

function TSynRange.GetKeyWordsCount: Integer;
begin
  Result := fKeyWords.Count;
end;

function TSynRange.GetKeyWords(AIndex: Integer): TSynKeywords;
begin
  Result := TSynKeywords(fKeyWords[AIndex]);
end;

function TSynRange.GetSetCount: Integer;
begin
  Result := fSets.Count;
end;

function TSynRange.GetSet(AIndex: Integer): TSynSet;
begin
  Result := TSynSet(fSets[AIndex]);
end;

function TSynRange.GetSpecialRuleCount: Integer;
begin
  Result := fSpecialRules.Count;
end;

function TSynRange.GetSpecialRule(AIndex: Integer): TSynSpecialRule;
begin
  Result := TSynSpecialRule(fSpecialRules[AIndex]);
end;

function TSynRange.GetLinkCount: Integer;
begin
  Result := fLinks.Count;
end;

function TSynRange.GetLink(AIndex: Integer): TSynRuleLink;
begin
  Result := TSynRuleLink(fLinks[AIndex]);
end;

// -----------------------------------------------------------------------------

function TSynRange.AddOpenToken(const AToken: UTF8String;
  AIns: Boolean; APriority: Integer = 0): TSynToken;
begin
  Result := TSynToken.Create(Self);
  SetLength(fOpenTokens, Succ(Length(fOpenTokens)));
  fOpenTokens[High(fOpenTokens)] := Result;
  with Result do
  begin
    Pattern := AToken;
    if AIns then Options := Options + [stoInside];
    Priority := APriority;
  end;
end;

function TSynRange.AddCloseToken(const AToken: UTF8String;
  AIns, AGlob: Boolean; APriority: Integer = 0): TSynToken;
begin
  Result := TSynToken.Create(Self);
  SetLength(fCloseTokens, Succ(Length(fCloseTokens)));
  fCloseTokens[High(fCloseTokens)] := Result;
  with Result do
  begin
    Pattern := AToken;
    if AIns then Options := Options + [stoInside];
    if AGlob then Options := Options + [stoGlobal];
    Priority := APriority;
  end;
end;

procedure TSynRange.RemoveToken(Index: Integer; Open: Boolean);
var
  Arr: PSynTokens;
begin
  if Open then
    Arr := @fOpenTokens
  else
    Arr := @fCloseTokens;
  FreeAndNil(Arr^[Index]);
  for Index := Succ(Index) to High(Arr^) do
    Arr^[Pred(Index)] := Arr^[Index];
  SetLength(Arr^, High(Arr^));
end;

// -----------------------------------------------------------------------------

procedure TSynRange.AddRule(ARule: TSynAbstractRule);
var
  List: TList;
begin
  if ARule is TSynRange then
    List := fRanges
  else if ARule is TSynKeywords then
    List := fKeyWords
  else if ARule is TSynSet then
    List := fSets
  else if ARule is TSynSpecialRule then
    List := fSpecialRules
  else if ARule is TSynRuleLink then
    List := fLinks
  else
    Exit;
  List.Add(ARule);
  (ARule as TSynAbstractRule).Parent := Self;
end;

procedure TSynRange.RemoveRule(ARule: TSynAbstractRule);
var
  List: TList;
begin
  if ARule is TSynRange then
    List := fRanges
  else if ARule is TSynKeywords then
    List := fKeyWords
  else if ARule is TSynSet then
    List := fSets
  else if ARule is TSynSpecialRule then
    List := fSpecialRules
  else if ARule is TSynRuleLink then
    List := fLinks
  else
    Exit;
  List.Remove(ARule);
  ARule.Free;
end;

// -----------------------------------------------------------------------------
// Returns a rule by it's name
function TSynRange.GetRuleByName(const RuleName: UTF8String;
  Recursive: Boolean = True; ForLink: Boolean = False): TSynRule;
var
  I: Integer;
begin
  { Initialize }
  Result := nil;

  { Check self }
  if (not ForLink and (Name = RuleName)) or
    (ForLink and (LinkName = RuleName)) then
  begin
    Result := Self;
    Exit;
  end;

  { Check rules }
  for I := 0 to Pred(KeyWordsCount) do
    if (not ForLink and (KeyWords[I].Name = RuleName)) or
      (ForLink and (KeyWords[I].LinkName = RuleName)) then
    begin
      Result := KeyWords[I];
      Exit;
    end;
  for I := 0 to Pred(SetCount) do
    if (not ForLink and (Sets[I].Name = RuleName)) or
      (ForLink and (Sets[I].LinkName = RuleName)) then
    begin
      Result := Sets[I];
      Exit;
    end;
  for I := 0 to Pred(SpecialRuleCount) do
    if (not ForLink and (SpecialRules[I].Name = RuleName)) or
      (ForLink and (SpecialRules[I].LinkName = RuleName)) then
    begin
      Result := SpecialRules[I];
      Exit;
    end;
  for I := 0 to Pred(RangeCount) do
  begin
    if (not ForLink and (Ranges[I].Name = RuleName)) or
      (ForLink and (Ranges[I].LinkName = RuleName)) then
    begin
      Result := Ranges[I];
      Exit;
    end;
  end;

  { Not found, check subranges now }
  if Recursive then
    for I := 0 to Pred(RangeCount) do
    begin
      Result := Ranges[I].GetRuleByName(RuleName, True, ForLink);
      if Result <> nil then
        Exit;
    end;
end;

// -----------------------------------------------------------------------------

procedure TSynRange.LinkRange(ARange: TSynRange;
  LinkOpenTokens: TIntegerArray; LinkCloseTokens: TIntegerArray;
  LinkOptions: TSynLinkOptions = []);

  procedure CopyRangeRules(Source, Dest: TSynRange);
  var
    I, J, K: Integer;
    NewSpecRule: TSynSpecialRule;
    NewKeywords: TSynKeywords;
    NewSet: TSynSet;
    NewLink: TSynRuleLink;
    NewRange: TSynRange;
  begin
    for I := 0 to Pred(Source.SpecialRuleCount) do
    begin
      NewSpecRule := TSynSpecialRule.Create;
      with NewSpecRule do
      begin
        Name := Self.Name + '.' + Source.SpecialRules[I].LinkName;
        Style := AddStyleSuffix(Source.SpecialRules[I].Style, Self.Name);
        Pattern := Source.SpecialRules[I].Pattern;
        ReplacePattern := Source.SpecialRules[I].ReplacePattern;
        Priority := Source.SpecialRules[I].Priority;
        Options := Source.SpecialRules[I].Options;
        for J := 0 to Pred(Source.SpecialRules[I].GroupAttributeCount) do
          AddGroupAttribute(
            Self.Name + '.' + Source.SpecialRules[I].GroupAttributes[J].Name,
            AddStyleSuffix(Source.SpecialRules[I].GroupAttributes[J].FriendlyName,
              Self.Name));
      end;
      Dest.AddRule(NewSpecRule);
    end;
    for I := 0 to Pred(Source.KeyWordsCount) do
    begin
      NewKeywords := TSynKeywords.Create;
      with NewKeywords do
      begin
        Name := Self.Name + '.' + Source.KeyWords[I].Name;
        Style := AddStyleSuffix(Source.KeyWords[I].Style, Self.Name);
        Options := Source.KeyWords[I].Options;
        BeginUpdate;
          for J := 0 to Pred(Source.KeyWords[I].Count) do
            AddKey(Source.KeyWords[I].GetKey(J));
        EndUpdate;
      end;
      Dest.AddRule(NewKeywords);
    end;
    for I := 0 to Pred(Source.SetCount) do
    begin
      NewSet := TSynSet.Create;
      with NewSet do
      begin
        Name := Self.Name + '.' + Source.Sets[I].Name;
        Style := AddStyleSuffix(Source.Sets[I].Style, Self.Name);
        Options := Source.Sets[I].Options;
        Symbols := Source.Sets[I].Symbols;
      end;
      Dest.AddRule(NewSet);
    end;
    for I := 0 to Pred(Source.LinkCount) do
    begin
      NewLink := TSynRuleLink.Create;
      with NewLink do
      begin
        Options := Source.Links[I].Options;
        SetLength(LinkOpenTokens, Length(Source.Links[I].LinkOpenTokens));
        for J := 0 to High(LinkOpenTokens) do
          LinkOpenTokens[J] := Source.Links[I].LinkOpenTokens[J];
        SetLength(LinkCloseTokens, Length(Source.Links[I].LinkCloseTokens));
        for J := 0 to High(LinkCloseTokens) do
          LinkCloseTokens[J] := Source.Links[I].LinkCloseTokens[J];
        if Source.Links[I].Rule <> nil then
          Rule := Source.Links[I].Rule
        else
          Link := Source.Links[I].Link;
      end;
      Dest.AddRule(NewLink);
    end;
    for I := 0 to Pred(Source.RangeCount) do
    begin
      NewRange := TSynRange.Create;
      with NewRange do
      begin
        Name := Self.Name + '.' + Source.Ranges[I].Name;
        Style := AddStyleSuffix(Source.Ranges[I].Style, Self.Name);
        NumberAttributes.FriendlyName := AddStyleSuffix(Source.Ranges[I].NumberAttributes.FriendlyName,
          Self.Name);
        SymbolAttributes.FriendlyName := AddStyleSuffix(Source.Ranges[I].SymbolAttributes.FriendlyName,
          Self.Name);
        Options := Source.Ranges[I].Options;
        AddDelimiters := Source.Ranges[I].AddDelimiters;
        RemoveDelimiters := Source.Ranges[I].RemoveDelimiters;
        for J := 0 to Pred(Source.Ranges[I].OpenTokenCount) do
        begin
          AddOpenToken(Source.Ranges[I].OpenTokens[J].Pattern, False);
          OpenTokens[Pred(OpenTokenCount)].Priority := Source.Ranges[I].OpenTokens[J].Priority;
          OpenTokens[Pred(OpenTokenCount)].Options := Source.Ranges[I].OpenTokens[J].Options;
          for K := 0 to Pred(Source.Ranges[I].OpenTokens[J].GroupAttributeCount) do
            OpenTokens[Pred(OpenTokenCount)].AddGroupAttribute(
              Self.Name + '.' + Source.Ranges[I].OpenTokens[J].GroupAttributes[K].Name,
              AddStyleSuffix(Source.Ranges[I].OpenTokens[J].GroupAttributes[K].FriendlyName,
                Self.Name));
        end;
        for J := 0 to Pred(Source.Ranges[I].CloseTokenCount) do
        begin
          AddCloseToken(Source.Ranges[I].CloseTokens[J].Pattern, False, False);
          CloseTokens[Pred(CloseTokenCount)].Priority := Source.Ranges[I].CloseTokens[J].Priority;
          CloseTokens[Pred(CloseTokenCount)].Options := Source.Ranges[I].CloseTokens[J].Options;
          for K := 0 to Pred(Source.Ranges[I].CloseTokens[J].GroupAttributeCount) do
            CloseTokens[Pred(CloseTokenCount)].AddGroupAttribute(
              Self.Name + '.' + Source.Ranges[I].CloseTokens[J].GroupAttributes[K].Name,
              AddStyleSuffix(Source.Ranges[I].CloseTokens[J].GroupAttributes[K].FriendlyName,
                Self.Name));
        end;
      end;
      Dest.AddRule(NewRange);
      CopyRangeRules(Source.Ranges[I], NewRange);
    end;
  end;

var
  I, J: Integer;
  bFound: Boolean;
  Link: TSynRuleLink;
begin
  { Only ranges created as links can continue here }
  if not fLinked and not (sloChildrenOnly in LinkOptions) then
    Exit;

  if not (sloChildrenOnly in LinkOptions) then
  begin

    { Source }
    if ARange.Linked then
      fLinkSource := ARange.LinkSource
    else
      fLinkSource := ARange;

    { Properties }
    fOptions := ARange.Options;
    if not fLinksRules then
    begin
      if ARange.Linked then
        Name := ARange.Name
      else
        Name := ARange.Name + '_link';
      LinkName := Name; // Somehow this is important.
                        // Puts into infinite loop otherwise
    end;

    { Attributes }
    if sloAsEmbedded in LinkOptions then
    begin
      fAttributes := TSynHighlighterAttributes.Create(Self.Name + '.' + ARange.LinkName,
        AddStyleSuffix(ARange.Attributes.FriendlyName, Self.Name));
      fAttributes.Rule := Self;
      fNumberAttributes := TSynHighlighterAttributes.Create(Self.Name + '.' + ARange.LinkName + '.digit',
        AddStyleSuffix(ARange.NumberAttributes.FriendlyName, Self.Name));
      fNumberAttributes.Rule := Self;
      fSymbolAttributes := TSynHighlighterAttributes.Create(Self.Name + '.' + ARange.LinkName + '.symbol',
        AddStyleSuffix(ARange.SymbolAttributes.FriendlyName, Self.Name));
      fSymbolAttributes.Rule := Self;
    end
    else begin
      fAttributes := ARange.Attributes;
      fNumberAttributes := ARange.NumberAttributes;
      fSymbolAttributes := ARange.SymbolAttributes;
    end;

    { Open tokens }
    if not fLinksRules then
      for I := 0 to Pred(ARange.OpenTokenCount) do
      begin

        { Check if it's possible to link given token }
        if (LinkOpenTokens = nil) or (Length(LinkOpenTokens) = 0) then
          bFound := True
        else begin
          bFound := False;
          for J := 0 to High(LinkOpenTokens) do
            if LinkOpenTokens[J] = I then
            begin
              bFound := True;
              Break;
            end;
        end;
        if bFound then
        begin
          if sloAsEmbedded in LinkOptions then
          begin
            AddOpenToken(ARange.OpenTokens[I].Pattern, False);
            fOpenTokens[High(fOpenTokens)].Priority := ARange.OpenTokens[I].Priority;
            fOpenTokens[High(fOpenTokens)].Options := ARange.OpenTokens[I].Options;
            for J := 0 to Pred(ARange.OpenTokens[I].GroupAttributeCount) do
              fOpenTokens[High(fOpenTokens)].AddGroupAttribute(
                Self.Name + '.' + ARange.OpenTokens[I].GroupAttributes[J].Name,
                AddStyleSuffix(ARange.OpenTokens[I].GroupAttributes[J].FriendlyName,
                  Self.Name));
          end
          else begin
            SetLength(fOpenTokens, Succ(Length(fOpenTokens)));
            fOpenTokens[High(fOpenTokens)] := ARange.OpenTokens[I];
          end;
        end;
      end;

    { Close tokens }
    if not fLinksRules then
      for I := 0 to Pred(ARange.CloseTokenCount) do
      begin

        { Check if it's possible to link given token }
        if (LinkCloseTokens = nil) or (Length(LinkCloseTokens) = 0) then
          bFound := True
        else begin
          bFound := False;
          for J := 0 to High(LinkCloseTokens) do
            if LinkCloseTokens[J] = I then
            begin
              bFound := True;
              Break;
            end;
        end;
        if bFound then
        begin
          if sloAsEmbedded in LinkOptions then
          begin
            AddCloseToken(ARange.CloseTokens[I].Pattern, False, False);
            fCloseTokens[High(fCloseTokens)].Priority := ARange.CloseTokens[I].Priority;
            fCloseTokens[High(fCloseTokens)].Options := ARange.CloseTokens[I].Options;
            for J := 0 to Pred(ARange.CloseTokens[I].GroupAttributeCount) do
              fCloseTokens[High(fCloseTokens)].AddGroupAttribute(
                Self.Name + '.' + ARange.CloseTokens[I].GroupAttributes[J].Name,
                AddStyleSuffix(ARange.CloseTokens[I].GroupAttributes[J].FriendlyName,
                  Self.Name));
          end
          else begin
            SetLength(fCloseTokens, Succ(Length(fCloseTokens)));
            fCloseTokens[High(fCloseTokens)] := ARange.CloseTokens[I];
          end;
        end;
      end;

    { Delimiters }
    fAddDelimiters := ARange.AddDelimiters;
    fRemoveDelimiters := ARange.RemoveDelimiters;
  end;

  { Rules }
  if sloAsEmbedded in LinkOptions then
  begin

    { Copy everything into new instances }
    CopyRangeRules(ARange, Self);
  end
  else begin

    { Create links for everyhting }
    for I := 0 to Pred(ARange.RangeCount) do
    begin
      Link := TSynRuleLink.Create;
      Link.Link := ARange.Ranges[I].LinkName;
      AddRule(Link);
    end;
    for I := 0 to Pred(ARange.KeyWordsCount) do
    begin
      Link := TSynRuleLink.Create;
      Link.Rule := ARange.KeyWords[I];
      AddRule(Link);
    end;
    for I := 0 to Pred(ARange.SetCount) do
    begin
      Link := TSynRuleLink.Create;
      Link.Rule := ARange.Sets[I];
      AddRule(Link);
    end;
    for I := 0 to Pred(ARange.SpecialRuleCount) do
    begin
      Link := TSynRuleLink.Create;
      Link.Rule := ARange.SpecialRules[I];
      AddRule(Link);
    end;
    for I := 0 to Pred(ARange.LinkCount) do
    begin
      Link := TSynRuleLink.Create;
      Link.Options := ARange.Links[I].Options;
      SetLength(Link.LinkOpenTokens, Length(ARange.Links[I].LinkOpenTokens));
      for J := 0 to High(Link.LinkOpenTokens) do
        Link.LinkOpenTokens[J] := ARange.Links[I].LinkOpenTokens[J];
      SetLength(Link.LinkCloseTokens, Length(ARange.Links[I].LinkCloseTokens));
      for J := 0 to High(Link.LinkCloseTokens) do
        Link.LinkCloseTokens[J] := ARange.Links[I].LinkCloseTokens[J];
      if ARange.Links[I].Rule <> nil then
        Link.Rule := ARange.Links[I].Rule
      else
        Link.Link := ARange.Links[I].Link;
      AddRule(Link);
    end;
  end;
end;

{ TSynKeywords }

// -----------------------------------------------------------------------------

constructor TSynKeywords.Create;
begin
  inherited;
  fCount := 0;
  SetLength(fKeys, 0);
  SetLength(fHashes, 0);
  fOptions := [];
  fUpdating := False;
end;

destructor TSynKeywords.Destroy;
begin
  SetLength(fHashes, 0);
  SetLength(fKeys, 0);
  inherited;
end;

// -----------------------------------------------------------------------------

function TSynKeywords.GetKey(Index: Integer): UnicodeString;
begin
  if (Index < 0) or (Index > fCount - 1) then
    Result := ''
  else
    Result := fKeys[Index];
end;

// -----------------------------------------------------------------------------
// Generate hashes using selected hash function
procedure TSynKeywords.DoHash;
var
  I: Integer;
begin
  SetLength(fHashes, fCount);
  for I := 0 to Pred(fCount) do
    fHashes[I] := MurmurHash2(Pointer(fKeys[I]),
      Length(fKeys[I]) * SizeOf(Char), HashSeed or Cardinal(fCount));
end;

// -----------------------------------------------------------------------------

function TSynKeywords.Match(const AToken: Pointer;
  const ALength: Integer; const AHash: Cardinal): Boolean;

  { Fix for one-char (two byte) words }
  function SearchKey: Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to High(fKeys) do
    begin
      if Length(fKeys[I]) <> 1 then Break;
      if CompareMem(Pointer(fKeys[I]), AToken, ALength) then
      begin
        Result := True;
        Break;
      end
    end;
  end;

  function SearchHash: Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to High(fHashes) do
      if fHashes[I] = AHash then
      begin
        Result := True;
        Break;
      end;
  end;

begin
  if ALength > SizeOf(Char) then
    Result := SearchHash
  else
    Result := SearchKey;
end;

// -----------------------------------------------------------------------------

procedure TSynKeywords.AddKey(const AKey: UnicodeString);
begin
  if fUpdating then
  begin
    Inc(fCount);
    if Length(fKeys) < fCount then
      SetLength(fKeys, fCount * 2);
    fKeys[Pred(fCount)] := AKey;
  end;
end;

// -----------------------------------------------------------------------------

procedure TSynKeywords.BeginUpdate;
begin
  if not fUpdating then
  begin
    fUpdating := True;
    fCount := 0;
    SetLength(fKeys, 32);
  end;
end;

procedure TSynKeywords.EndUpdate;
begin
  if fUpdating then
  begin
    fUpdating := False;
    SetLength(fKeys, fCount);
    DoHash;
  end;
end;

{ TSynSet }

// -----------------------------------------------------------------------------

constructor TSynSet.Create;
begin
  inherited;
  fSymbols := '';
end;

// -----------------------------------------------------------------------------
// Tests if a given character intersects with this set
function TSynSet.Match(const AChar: Char): Boolean;
begin
  Result := Pos(AChar, fSymbols) > 0;
end;

{ TSynSpecialRule }

// -----------------------------------------------------------------------------

constructor TSynSpecialRule.Create;
begin
  inherited;
  fPattern := '';
  fOptions := [];
  fPriority := 0;
  SetLength(fGroupAttributes, 0);
end;

destructor TSynSpecialRule.Destroy;
begin
  ClearAttributeArray(fGroupAttributes);
  inherited;
end;

// -----------------------------------------------------------------------------

function TSynSpecialRule.GetGroupAttributeCount: Integer;
begin
  Result := Length(fGroupAttributes);
end;

function TSynSpecialRule.GetGroupAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  Result := fGroupAttributes[Index];
end;

// -----------------------------------------------------------------------------

function TSynSpecialRule.AddGroupAttribute(const AName, AStyle: String): TSynHighlighterAttributes;
begin
  Result := TSynHighlighterAttributes.Create(AName, AStyle);
  Result.Rule := Self;
  SetLength(fGroupAttributes, Length(fGroupAttributes) + 1);
  fGroupAttributes[High(fGroupAttributes)] := Result;
end;

procedure TSynSpecialRule.DeleteGroupAttribute(Index: Integer);
begin
  FreeAndNil(fGroupAttributes[Index]);
  for Index := Index + 1 to High(fGroupAttributes) do
    fGroupAttributes[Pred(Index)] := fGroupAttributes[Index];
  SetLength(fGroupAttributes, Pred(Length(fGroupAttributes)));
end;

{ TSynRuleLink }

// -----------------------------------------------------------------------------
// Link gets assigned after complete grammar parse
constructor TSynRuleLink.Create;
begin
  inherited Create;
  fLink := EmptyAnsiStr;
  fLinkedRule := nil;
  fOptions := [];
  SetLength(LinkOpenTokens, 0);
  SetLength(LinkCloseTokens, 0);
end;

end.
