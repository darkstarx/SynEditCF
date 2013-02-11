(*

  Letterpress, Lantern

  SynEdit Universal Highlighter 3.0 RC2.

  Copyright 2006-2010 initial developers and Garnet

*)

unit SynUniHighlighter;

interface

uses
  Windows, Classes, SysUtils, StrUtils, IniFiles, RegularExpressions, Math,

  { SynEdit }
  SynEditHighlighter, SynEditTypes, SynEditMiscProcs, SynEditCodeFolding,
  SynTokenMatch, SynUnicode,

  { SynUni }
  SynUniRules, SynUniClasses;

type
  { Match indicating folding range beginning / ending }
  TSynOutliningMatch = record
    AIndex, ALength: Integer; // Position and length of whole matched token \
    ATokenIndex: Integer;
    AOpen: Boolean;           // Is it open or close token
    ARegion: TFoldRegionItem; // Region caused match
  end;

  PSynOutliningMatchArray = ^TSynOutliningMatchArray;
  TSynOutliningMatchArray = array of TSynOutliningMatch;

  { Match indicating regex pattern match with subgroups }
  TSynRuleMatch = record
    AIndex, ALength: Integer; // Position and length of whole match or portion
    AGroupIndex: Integer;     // Index of regex pattern group (0: whole match)
    ARule: TObject;           // Rule caused match. Range token or special rule
  end;

  PSynRuleMatchArray = ^TSynRuleMatchArray;
  TSynRuleMatchArray = array of TSynRuleMatch;

  { Match indicating range beginning / ending }
  TSynRangeMatch = record
    AIndex, ALength: Integer; // Position and length of open / close token
    ARange: TSynRange;        // Range that opens / closes on / after this token
    AToken: TSynToken;        // Open / close token caused match
    AOpen: Boolean;           // Is it an open or close token
    AGroupMatches: TSynRuleMatchArray; // Used only during map creation
    ACapture: UTF8String;     // Result of applied pattern
  end;

  PSynRangeMatchArray = ^TSynRangeMatchArray;
  TSynRangeMatchArray = array of TSynRangeMatch;

  { Captures }
  TSynCapture = record
    ACapture: UTF8String;
    AToken: TSynToken;
  end;

  PSynCaptureArray = ^TSynCaptureArray;
  TSynCaptureArray = array of TSynCapture;

  { Maps cache for all SynEdit lines }
  PSynUniCacheItem = ^TSynUniCacheItem;
  TSynUniCacheItem = packed record
    ACached,                      // True if a line of this index was parsed
    AOutlined,                    // True if outlining parser parsed line
    ACollapseOpen,                // There's an unclosed open folding token
    ACollapseClose,               // There's an unsealed close folding token
    AIndentationOpen,             // There's opening indentation folding token
    AIndentationClose: Boolean;   // There's closing indentation folding token
    AMap: PSynRangeMatchArray;    // Cached range map
    ASpecMap: PSynRuleMatchArray; // Cached pattern map
    AOutliningMap: PSynOutliningMatchArray; // Cached outlining map
    ACaptureMap: PSynCaptureArray; // Cached capture map
  end;

  { Fast hashed links' lookup table }
  TSynUniLinkLookup = record
    AHash: Cardinal;
    ARule: TObject;
  end;

  TSynUniLinkLookupArray = array of TSynUniLinkLookup;

  { Event for reading external grammars }
  TSynUniExternalGrammarEvent = procedure (Sender: TObject;
    var FileName: UnicodeString) of object;

  { Options for loading external grammar }
  TSynUniLoadOption = (suloAsEmbedded, suloEmbedIntoParentScope,
    suloExternalGrammars);

  TSynUniLoadOptions = set of TSynUniLoadOption;

  { SynEdit Universal Highlighter }
  TSynUniSyn = class(TSynCustomHighlighter)
  private
    fFileName: String;            // Loaded .grammar file name
    fMap: PSynRangeMatchArray;    // Map of ranges on current set line
    fSpecMap: PSynRuleMatchArray; // Map of matched special rules for that line
    fSilenceMap: TSynHighlighterAttributesArray; // Needed for sroOpenOnBol
    fCaptureMap: PSynCaptureArray; // Needed for HEREDOC-like ranges

    fTriggerPos: Integer;         // Where range (next) triggers
    fTriggerRange,                // Range to trigger

    fRules,                       // Root rules node
    fCurrRangePrev,               // Current range inside which we are
    fCurrRange: TSynRange;        // Current active range
    fCurrToken: TSynRule;         // Current token on fTokenPos
    fCurrAttr: TSynHighlighterAttributes; // Current token attributes
    fCurrStates: TSynUniStates;   // Current highlighter state

    fOnExternalGrammar: TSynUniExternalGrammarEvent;

    { Cache }
    fCache: TList;
    fLineDone: Boolean;

    { Optimizations }
    fRuleLookup, fAttrLookup: TSynUniLinkLookupArray;
    fRuleLookupPos, fAttrLookupPos: Integer;

    procedure GrowCache(Index, Count: Integer);
    function GetCacheItem(Index: Integer): PSynUniCacheItem;

    procedure FreeResources;
    procedure CreateRangeAndRuleMaps(Force: Boolean; Start: Integer = 1);
    procedure ActivateAttributes(Range: TSynRange);
    procedure ActivateColors(Range: TSynRange);
    procedure ActivateLinks(Range, OriginalRange: TSynRange;
      const Lookup: TSynUniLinkLookupArray; LookupStart: Integer = 0);

    function RangeSealed(Range: TSynRange): Boolean;
    procedure SealRange(Range: TSynRange; Unseal: Boolean = False);

    procedure RangeProc;
    procedure NullProc;
    procedure NbspProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure IdentProc;
    procedure NumberProc;
    procedure KeywordProc;
    procedure SpecialProc;
    procedure SymbolProc;
    procedure SetProc;
    procedure UnknownProc;
  protected
    function GetDefaultAttribute(AIndex: Integer): TSynHighlighterAttributes;
      override;
    function GetSampleSource: UnicodeString; override;

    function GetRuleFromLookup(const Name: UTf8String;
      LookupStart: Integer): TSynRule;
    function GetTokenFromLookup(const Name: UTf8String;
      LookupStart: Integer): Integer;

    procedure DoSetLine(const Value: UnicodeString; LineNumber: Integer;
      Force: Boolean = False); override;
  public
    Info: TSynGrammar;
    Environment: TStringList;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure Activate(Links: Boolean = True);

    function GetTokenKindByRuleName(const Name: UTF8String): Integer;
    function GetRuleNameByTokenKind(TokenKind: Integer): UTF8String;
    function HasInParents(Rule: TSynRule; Parent: TSynRange): Boolean;

    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetPrevRange: Pointer;
    function GetRule: TSynRule;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;

    procedure Next; override;
    procedure SetRange(AValue: Pointer); override;
    procedure ResetRange; override;
    procedure LoadFromFile(ThemeFile: TSynHighlighterTheme); overload; override;

    function IsWordBreakChar(AChar: Char): Boolean; override;

    procedure LinesInserted(AIndex: Integer; ACount: Integer); override;
    procedure LinesDeleted(AIndex: Integer; ACount: Integer); override;
    procedure LinesPutted(AIndex: Integer; ACount: Integer); override;

    procedure LoadGrammar(const AFileName: String; EnableLinks: Boolean = True;
      ExternalParent: TSynRange = nil; TargetName: UTF8String = '';
      Options: TSynUniLoadOptions = [suloExternalGrammars];
      PrependScope: UTF8String = '');
    procedure SaveGrammar(const AFileName: String);

    property FileName: String read fFileName write fFileName;
    property Rules: TSynRange read fRules;
    property OnExternalGrammar: TSynUniExternalGrammarEvent
      write fOnExternalGrammar;
    property Cache[Index: Integer]: PSynUniCacheItem read GetCacheItem;
  end;

implementation

{ TSynUniSyn }

// -----------------------------------------------------------------------------
// Always set fCaseSensitive of base highlighter class to True to avoid
// SynEdit copying string in memory (fCasedLine)
constructor TSynUniSyn.Create(AOwner: TComponent);
begin
  inherited;
  fCaseSensitive := True;
  fOnExternalGrammar := nil;
  New(fCaptureMap);
  Environment := TStringList.Create;
  Environment.NameValueSeparator := #9;
  Clear;
end;

destructor TSynUniSyn.Destroy;
begin
  FreeAndNil(Environment);
  SetLength(fCaptureMap^, 0);
  Dispose(fCaptureMap);
  FreeResources;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TSynUniSyn.GrowCache(Index, Count: Integer);
var
  I: Integer;
  NewCacheItem: PSynUniCacheItem;
begin
  { Add? }
  if Count > 0 then
  begin
    I := Count;
    while I > 0 do
    begin
      New(NewCacheItem);
      New(NewCacheItem^.AMap);
      New(NewCacheItem^.ASpecMap);
      New(NewCacheItem^.AOutliningMap);
      New(NewCacheItem^.ACaptureMap);
      NewCacheItem^.ACached := False;
      NewCacheItem^.AOutlined := False;
      if Index = -1 then
        fCache.Add(NewCacheItem)
      else
        fCache.Insert(Index, NewCacheItem);
      Dec(I);
    end;
  end

  { Remove? }
  else if Count < 0 then
  begin
    I := Count;
    while I < 0 do
    begin
      SetLength(PSynUniCacheItem(fCache[Index])^.AMap^, 0);
      SetLength(PSynUniCacheItem(fCache[Index])^.ASpecMap^, 0);
      SetLength(PSynUniCacheItem(fCache[Index])^.AOutliningMap^, 0);
      Dispose(PSynUniCacheItem(fCache[Index])^.AMap);
      Dispose(PSynUniCacheItem(fCache[Index])^.ASpecMap);
      Dispose(PSynUniCacheItem(fCache[Index])^.AOutliningMap);
      Dispose(PSynUniCacheItem(fCache[Index])^.ACaptureMap);
      Dispose(PSynUniCacheItem(fCache[Index]));
      fCache.Delete(Index);
      Inc(I);
    end;
  end

  { Reset }
  else begin
    PSynUniCacheItem(fCache[Index])^.ACached := False;
    PSynUniCacheItem(fCache[Index])^.AOutlined := False;
  end;
end;

function TSynUniSyn.GetCacheItem(Index: Integer): PSynUniCacheItem;
begin
  if (Index < 0) or (Index >= fCache.Count) then
    Result := nil
  else
    Result := PSynUniCacheItem(fCache[Index]);
end;

// -----------------------------------------------------------------------------

procedure TSynUniSyn.Clear;
begin

  { Properties }
  fFileName := sUndefined + sExt;
  fLanguageName := sUndefined;
  fFriendlyLanguageName := sUndefined;
  fDefaultFilter := EmptyStr;
  with Info do
  begin
    UID := EmptyAnsiStr;
    Developer := EmptyAnsiStr;
    Contacts := EmptyAnsiStr;
    Version := EmptyAnsiStr;
    Sample := EmptyAnsiStr;
  end;
  Environment.Clear;

  { Destroy }
  if Assigned(fRules) then
    FreeResources;

  { Rules }
  fRules := TSynRange.Create;
  with fRules do
  begin
    Name := sTextElem;
    Style := sTextElem;
    Options := Options + [sroSpellCheck];
  end;
  ResetRange;

  { Cache }
  fCache := TList.Create;
  fRuleLookupPos := 0;
  fAttrLookupPos := 0;
  SetLength(fRuleLookup, 0);
  SetLength(fAttrLookup, 0);

  { Finish }
  Activate;
end;

// -----------------------------------------------------------------------------

procedure TSynUniSyn.Activate(Links: Boolean = True);
begin
  ActivateAttributes(fRules);
  if Links then
    ActivateLinks(fRules, fRules, nil);
end;

// -----------------------------------------------------------------------------

function TSynUniSyn.GetDefaultAttribute(AIndex: Integer): TSynHighlighterAttributes;

  function LookForRange(const Name: String): TSynHighlighterAttributes;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to fCurrRange.RangeCount - 1 do
      if Pos(Name, LowerCase(fCurrRange.Ranges[I].Name)) > 0 then
      begin
        Result := fCurrRange.Ranges[I].Attributes;
        Break;
      end;
  end;

  function LookForList(const Name: String): TSynHighlighterAttributes;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to fCurrRange.KeyWordsCount - 1 do
      if Pos(Name, LowerCase(fCurrRange.KeyWords[I].Name)) > 0 then
      begin
        Result := fCurrRange.KeyWords[I].Attributes;
        Break;
      end;
  end;

  function LookForColor(Range: TSynRange): TSynHighlighterAttributes;
  begin
    Result := Range.Attributes;
    while (Result.Background = clNone) and (Range <> fRules) do
    begin
      Range := Range.Parent;
      Result := Range.Attributes;
    end;
  end;

begin
  case AIndex of
    SYN_ATTR_IDENTIFIER: Result := fCurrRange.Attributes;
    SYN_ATTR_SYMBOL: Result := fCurrRange.SymbolAttributes;
    SYN_ATTR_NUMBER: Result := fCurrRange.NumberAttributes;
    SYN_ATTR_KEYWORD: Result := LookForList(sKeywordElem);
    SYN_ATTR_STRING: Result := LookForRange(sStringElem);
    SYN_ATTR_COMMENT: Result := LookForList(sCommentElem);
    SYN_ATTR_WHITESPACE: Result := LookForColor(fCurrRange);
    else Result := nil;
  end;
end;

// -----------------------------------------------------------------------------

function TSynUniSyn.GetSampleSource: UnicodeString;
begin
  Result := UTF8ToUnicodeString(Info.Sample);
end;

// -----------------------------------------------------------------------------

function TSynUniSyn.GetRuleFromLookup(const Name: UTf8String;
  LookupStart: Integer): TSynRule;
var
  I: Integer;
  Hash: Cardinal;
begin
  Result := nil;
  Hash := MurmurHash2(@Name[1], Length(Name), HashSeed);
  for I := LookupStart to Pred(fRuleLookupPos) do
    if fRuleLookup[I].AHash = Hash then
    begin
      Result := fRuleLookup[I].ARule as TSynRule;
      Break;
    end;
end;

function TSynUniSyn.GetTokenFromLookup(const Name: UTf8String;
  LookupStart: Integer): Integer;
var
  I: Integer;
  Hash: Cardinal;
begin
  Result := -1;
  Hash := MurmurHash2(@Name[1], Length(Name), HashSeed);
  for I := LookupStart to Pred(fAttrLookupPos) do
    if fAttrLookup[I].AHash = Hash then
    begin
      Result := Integer(fAttrLookup[I].ARule);
      Break;
    end;
end;

// -----------------------------------------------------------------------------
// During SetLine() a map of ranges and rules is built
procedure TSynUniSyn.DoSetLine(const Value: UnicodeString; LineNumber: Integer;
  Force: Boolean = False);
begin
  inherited;

  { Ensure enough cache }
  while fCache.Count < LineNumber + 1 do
    GrowCache(-1, 1);

  { Reset }
  fLineDone := False;
  fTriggerRange := nil;
  fCurrRangePrev := fCurrRange;
  fCurrStates := [];

  { Manage silence }
  SetLength(fSilenceMap, 0);
  if (fCurrRange <> fRules) or (LineNumber > 0) then
    SealRange(fCurrRange);

  { Build maps for the line }
  CreateRangeAndRuleMaps(Force);
end;

// -----------------------------------------------------------------------------
// This is a main routine which handles correct memory unallocation
procedure TSynUniSyn.FreeResources;
var
  I: Integer;
begin
  { Must be in this order }
  fAttributes.Clear;
  FreeAndNil(fRules);

  { Regions, matches }
  fFoldRegions.Clear;
  SetLength(MatchTokens, 0);
  SetLength(IndentationRules, 0);
  SetLength(fSilenceMap, 0);

  { Cache }
  for I := 0 to Pred(fCache.Count) do
  begin
    SetLength(PSynUniCacheItem(fCache[I])^.AMap^, 0);
    SetLength(PSynUniCacheItem(fCache[I])^.ASpecMap^, 0);
    SetLength(PSynUniCacheItem(fCache[I])^.AOutliningMap^, 0);
    SetLength(PSynUniCacheItem(fCache[I])^.ACaptureMap^, 0);
    Dispose(PSynUniCacheItem(fCache[I])^.AMap);
    Dispose(PSynUniCacheItem(fCache[I])^.ASpecMap);
    Dispose(PSynUniCacheItem(fCache[I])^.AOutliningMap);
    Dispose(PSynUniCacheItem(fCache[I])^.ACaptureMap);
    Dispose(PSynUniCacheItem(fCache[I]));
  end;
  fCache.Clear;
  FreeAndNil(fCache);
end;

// -----------------------------------------------------------------------------
// Creates map of ranges on set line
procedure TSynUniSyn.CreateRangeAndRuleMaps(Force: Boolean; Start: Integer = 1);

  { Decide options accroding to range settings }
  function GetRegexOptions(Range: TSynRange): TRegexOptions;
  begin
    Result := [roIgnoreCase];
    if sroCaseSensitive in Range.Options then
      Exclude(Result, roIgnoreCase);
  end;

  { Decide options according to range and special rule settins }
  function GetRegexOptionsEx(Range: TSynRange;
    Special: TSynSpecialRule): TRegexOptions;
  begin
    Result := GetRegexOptions(Range);
    if ssroCaseSensitive in Special.Options then
      Exclude(Result, roIgnoreCase)
    else
      Include(Result, roIgnoreCase);
  end;

  { Free temporary match array }
  procedure DoClean(var Arr: TSynRangeMatchArray);
  var
    I: Integer;
  begin
    for I := 0 to High(Arr) do
      SetLength(Arr[I].AGroupMatches, 0);
    SetLength(Arr, 0);
  end;

  { Put found match into range map }
  procedure AddRangeMatch(var Arr: TSynRangeMatchArray; Index, Length: Integer;
    Range: TSynRange; Token: TSynToken; Open: Boolean; const Capture: UTF8String);
  begin
    SetLength(Arr, System.Length(Arr) + 1);
    with Arr[High(Arr)] do
    begin
      AIndex := Index;
      ALength := Length;
      ARange := Range;
      AToken := Token;
      AOpen := Open;
      ACapture := Capture;
    end;
  end;

  { Put matched special rule into rule map }
  procedure AddSpecRuleMatch(var Arr: TSynRuleMatchArray; Index, Length,
    GroupIndex: Integer; Rule: TObject);
  begin
    if (Index < 1) or (Length = 0) then
      Exit;
    SetLength(Arr, System.Length(Arr) + 1);
    with Arr[High(Arr)] do
    begin
      AIndex := Index;
      ALength := Length;
      AGroupIndex := GroupIndex;
      ARule := Rule;
    end;
  end;

var
  SwapMap: array of array [0..1] of Integer;

  { Add swap item }
  procedure AddSwapItem(Index1, Index2: Integer);
  begin
    if Index1 = Index2 then
      Exit;
    SetLength(SwapMap, Length(SwapMap) + 1);
    SwapMap[High(SwapMap)][0] := Index1;
    SwapMap[High(SwapMap)][1] := Index2;
  end;

  procedure SortRangeMap(var Arr: TSynRangeMatchArray);

    function Compare(Index1, Index2: Integer): Integer;
    begin
      { Intitialize }
      Result := 0;

      { First is a simple. Index positions compare }
      if Arr[Index1].AIndex < Arr[Index2].AIndex then
        Result := -1
      else if Arr[Index1].AIndex > Arr[Index2].AIndex then
        Result := 1

      { Second one is priority compare }
      else if Arr[Index1].AToken.Priority < Arr[Index2].AToken.Priority then
        Result := -1
      else if Arr[Index1].AToken.Priority > Arr[Index2].AToken.Priority then
        Result := 1

      { Third one is more tricky }
      else if Arr[Index1].AIndex = Arr[Index2].AIndex then

        { There can be a case when there are two adjanced matches with equal
          positions but the first one closes range and second opens next range.
          In such cases the one that closes a range is placed before second }
        if (Index1 < Index2) and (not Arr[Index1].AOpen) then
          Result := -1
        else if (Index1 > Index2) and (not Arr[Index2].AOpen) then
          Result := 1;
    end;

    procedure Exchange(Index1, Index2: Integer);
    var
      Temp: TSynRangeMatch;
    begin
      Temp := Arr[Index1];
      Arr[Index1] := Arr[Index2];
      Arr[Index2] := Temp;
    end;

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
    QuickSort(0, High(Arr));
  end;

  procedure SortRuleMap(var Arr: TSynRuleMatchArray);

    function Compare(Index1, Index2: Integer): Integer;
    begin
      { Intitialize }
      Result := 0;

      { First is simple. Index positions compare }
      if Arr[Index1].AIndex < Arr[Index2].AIndex then
        Result := -1
      else if Arr[Index1].AIndex > Arr[Index2].AIndex then
        Result := 1

      { Second one is priority compare }
      else if TSynSpecialRule(Arr[Index1].ARule).Priority < TSynSpecialRule(Arr[Index2].ARule).Priority then
        Result := -1
      else if TSynSpecialRule(Arr[Index1].ARule).Priority > TSynSpecialRule(Arr[Index2].ARule).Priority then
        Result := 1

      { Third one is more tricky }
      else if Arr[Index1].AIndex = Arr[Index2].AIndex then

        { Compare by group index. Don't know if this needed anymore }
        if Arr[Index1].ALength = Arr[Index2].ALength then
          if Arr[Index1].AGroupIndex < Arr[Index2].AGroupIndex then
            Result := -1
          else if Arr[Index1].AGroupIndex > Arr[Index2].AGroupIndex then
            Result := 1

        { Compare by length }
        else
          if Arr[Index1].ALength < Arr[Index2].ALength then
            Result := 1
          else if Arr[Index1].ALength > Arr[Index2].ALength then
            Result := -1;
    end;

    procedure Exchange(Index1, Index2: Integer);
    var
      Temp: TSynRuleMatch;
    begin
      Temp := Arr[Index1];
      Arr[Index1] := Arr[Index2];
      Arr[Index2] := Temp;
    end;

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
    QuickSort(0, High(Arr));
  end;

var
  S: UTF8String;

  { Match special rules in specified bounds of string }
  procedure MatchSpecRules(Range: TSynRange; Start, Count: Integer);
  var
    I, J: Integer;

    procedure DoMatchRule(Rule: TSynSpecialRule);
    var
      I, K: Integer;
      Match: IMatch;

      { Solution for chaning group indexes }
      procedure ValidIndexFromGroup;
      var
        J: Integer;
        Group: IGroup;
      begin
        K := -1;
        for J := 0 to Pred(Rule.GroupAttributeCount) do
        begin
          Group := Match.Groups.ItemsByName[Copy(Rule.GroupAttributes[J].Name,
            LastDelimiter('.', Rule.GroupAttributes[J].Name) + 1, MAXINT)];
          if (Group.Index > 0) and (Group = Match.Groups[I]) then
          begin
            K := J + 1;
            Break;
          end;
        end;
      end;

    begin
      Match := TRegex.Match(S, Rule.Pattern, GetRegexOptionsEx(Range, Rule),
        J, Count);

      { Work with matches }
      while Match.Success do
      begin
        J := Match.Index + Match.Length;
        for I := 0 to Pred(Match.Groups.Count) do
        begin
          if I = 0 then
            K := 0
          else
            ValidIndexFromGroup;
          if K > -1 then
            AddSpecRuleMatch(fSpecMap^, Match.Groups[I].Index,
              Match.Groups[I].Length, K, Rule);
        end;
        if J >= Count then Break;
        Match := Match.NextMatch;
      end
    end;

  begin
    { Try to match every special rule within that bounds }
    for I := 0 to Range.SpecialRuleCount - 1 do
    begin
      J := Start;
      DoMatchRule(Range.SpecialRules[I]);
    end;

    { Look in links for special rules }
    for I := 0 to Range.LinkCount - 1 do
      if (Range.Links[I].Rule <> nil) and
        (Range.Links[I].Rule is TSynSpecialRule) then
      begin
        J := Start;
        DoMatchRule(Range.Links[I].Rule as TSynSpecialRule);
      end;
  end;

  { Match tokens of range and all subranges.
    StartOpen - starting position where child subranges can occur in string.
    StartClose - starting position from where closing token can be found }
  procedure MatchChildren(Range: TSynRange; StartOpen, StartClose,
    Count: Integer; InsideToken: Boolean = False);
  var
    I: Integer;
    Temp: TSynRangeMatchArray;

    procedure DoMatch(Range: TSynRange; Token: TSynToken; Open: Boolean);
    var
      I, K: Integer;
      Match: IMatch;

      { Solution for changing group indexes }
      procedure ValidIndexFromGroup;
      var
        J: Integer;
        Group: IGroup;
      begin
        K := -1;
        for J := 0 to Pred(Token.GroupAttributeCount) do
        begin
          Group := Match.Groups.ItemsByName[Copy(Token.GroupAttributes[J].Name,
            LastDelimiter('.', Token.GroupAttributes[J].Name) + 1, MAXINT)];
          if (Group.Index > 0) and (Group = Match.Groups[I]) then
          begin
            K := J;
            Break;
          end;
        end;
      end;

    var
      Pattern: UTF8String;
    begin
      { From where to search? }
      if Open then
        I := StartOpen
      else
        I := StartClose;

      { Obtain pattern to match }
      Pattern := Token.Pattern;
      if not Open and (stoBackreferenced in Token.Options) and (Length(fCaptureMap^) > 0) then
        Pattern := TRegex.Match(fCaptureMap^[High(fCaptureMap^)].ACapture,
          fCaptureMap^[High(fCaptureMap^)].AToken.Pattern,
          GetRegexOptions(Range)).Result(Token.Pattern)
      else
        Pattern := Token.Pattern;

      { Look for match }
      Match := TRegex.Match(S, Pattern, GetRegexOptions(Range),
        I, Length(S));

      { This check is necessary in case when range opens and closes
        with the same token and these tokens are not inside a range }
      if Match.Success and (Match.Index <= Count)
        and (Length(fMap^) > 0)
        and not (stoInside in Token.Options)
        and (fMap^[High(fMap^)].ARange = Range)
        and (fMap^[High(fMap^)].AOpen = False)
        and (fMap^[High(fMap^)].AIndex = Match.Index)
      then
        Match := TRegex.Match(S, Pattern, GetRegexOptions(Range),
          I + fMap^[High(fMap^)].ALength, Length(S));

      { This check is necessary in case when range opens and closes
        with the same token and these tokens are not inside a range }
      if Match.Success and (Match.Index <= Count)
        and (Length(fMap^) > 0)
        and (stoInside in Token.Options) and not (stoInside in fMap^[High(fMap^)].AToken.Options)
        and (fMap^[High(fMap^)].ARange <> Range)
        and Open and (fMap^[High(fMap^)].AOpen = False)
        and (fMap^[High(fMap^)].AIndex = Match.Index)
        and (not (sroCanBeInsideToken in Range.Options))
      then
        Match := TRegex.Match(S, Pattern, GetRegexOptions(Range),
          I + fMap^[High(fMap^)].ALength, Length(S));

      { Add to temporary maps }
      if Match.Success and
        ((Match.Index <= Count) or (Match.Index = Length(S) + 1)) then
      begin

        { Add match }
        AddRangeMatch(Temp, Match.Index, Match.Length, Range,
          Token, Open, Match.Value);

        { Add captures }
        if Token.GroupAttributeCount > 0 then
          for I := 0 to Pred(Match.Groups.Count) do
          begin
            if I = 0 then
              K := 0
            else
              ValidIndexFromGroup;
            if K > -1 then
              AddSpecRuleMatch(Temp[High(Temp)].AGroupMatches,
                Match.Groups[I].Index, Match.Groups[I].Length,
                K, Token);
          end;
      end;
    end;

    procedure DoMatchRangeOpen(Range: TSynRange);
    var
      I: Integer;
    begin
      { Check if can match }
      if InsideToken and not (sroCanBeInsideToken in Range.Options) then
        Exit;

      { Loop through tokens }
      for I := 0 to Range.OpenTokenCount - 1 do
        DoMatch(Range, Range.OpenTokens[I], True);
    end;

    procedure DoMatchRangeClose(Range: TSynRange; Global: Boolean = False);
    var
      I: Integer;
    begin
      { Check for invalid input }
      if Range = nil then
        Exit;

      { Loop through tokens }
      for I := 0 to Pred(Range.CloseTokenCount) do
      begin
        if Global and not (stoGlobal in Range.CloseTokens[I].Options) then
          Continue;
        DoMatch(Range, Range.CloseTokens[I], False);
      end;

      { Look for global closures of parent ranges }
      if not (sroOmitGlobalTokens in Range.Options) then
        DoMatchRangeClose(Range.Parent, True);
    end;

  begin
    { Initialize }
    SetLength(Temp, 0);

    { Search for the closest open match of any subrange }
    for I := 0 to Range.RangeCount - 1 do
      DoMatchRangeOpen(Range.Ranges[I]);

    { Find out, where current range closes on that line. If it's a main range,
      then we won't need it since main range closes only at EOF }
    if not InsideToken then
      if Range.Parent <> nil then
        DoMatchRangeClose(Range);

    { No subranges? }
    if Length(Temp) = 0 then
    begin
      { Match special rules }
      MatchSpecRules(Range, StartOpen, Count);

      { Done }
      Exit;
    end;

    { Sort to get closest match }
    SortRangeMap(Temp);

    { Reuse I variable for start boundary }
    I := Temp[0].AIndex;
    if (not (stoInside in Temp[0].AToken.Options) and Temp[0].AOpen) or
      ((stoInside in Temp[0].AToken.Options) and not Temp[0].AOpen)
    then
      I := Temp[0].AIndex + Temp[0].ALength;

    { Grab spec rules from parent range and up to subrange match }
    MatchSpecRules(Range, StartOpen, Pred(I));

    { The closeset match is ready to be placed in map }
    with Temp[0] do
    begin

      { Place in map }
      AddRangeMatch(fMap^, AIndex, ALength, ARange, AToken, AOpen, ACapture);

      { Open token in spec map }
      for I := 0 to High(AGroupMatches) do
        AddSpecRuleMatch(fSpecMap^, AGroupMatches[I].AIndex,
          AGroupMatches[I].ALength,
          AGroupMatches[I].AGroupIndex,
          AGroupMatches[I].ARule);

      { Remember last open capture }
      if AOpen and (stoBackreferenced in AToken.Options) then
      begin
        SetLength(fCaptureMap^, Succ(Length(fCaptureMap^)));
        fCaptureMap^[High(fCaptureMap^)].ACapture := ACapture;
        fCaptureMap^[High(fCaptureMap^)].AToken := AToken;
      end

      { Finalize last capture on closure }
      else if not AOpen and
        (Length(fCaptureMap^) > 0) and
        (AToken.Range = fCaptureMap^[High(fCaptureMap^)].AToken.Range)
      then
        SetLength(fCaptureMap^, High(fCaptureMap^));
    end;

    { Now, if subrange token is not inside subrange, we need to
      match other subranges on that token which can be there }
    if (not (stoInside in Temp[0].AToken.Options) and Temp[0].AOpen) or
      ((stoInside in Temp[0].AToken.Options) and not Temp[0].AOpen) then
    begin
      { Remember added match index }
      StartOpen := High(fMap^);

      { Search for subranges on this token }
      MatchChildren(Range, Temp[0].AIndex, -1,
        Temp[0].AIndex + Temp[0].ALength - 1, True);

      { The following item will be required for analyze }
      AddSwapItem(StartOpen, High(fMap^));
    end;

    { Find out which range to analyze next }
    if I <= Count then
    begin
      { Choose range }
      if Temp[0].AOpen then
        Range := Temp[0].ARange
      else
        Range := Temp[0].ARange.Parent;

      { Choose bounds }
      StartOpen := Temp[0].AIndex;
      StartClose := Temp[0].AIndex;
      if (not (stoInside in Temp[0].AToken.Options) and Temp[0].AOpen) or
        ((stoInside in Temp[0].AToken.Options) and not Temp[0].AOpen) then
      begin
        StartOpen := Temp[0].AIndex + Temp[0].ALength;
        StartClose := Temp[0].AIndex + Temp[0].ALength;
      end
      else if Temp[0].AOpen and (stoInside in Temp[0].AToken.Options) then
        StartClose := Temp[0].AIndex + Temp[0].ALength;

      { Grab subranges }
      MatchChildren(Range, StartOpen, StartClose, Count);
    end;

    { Clean }
    DoClean(Temp);
  end;

  { Since Regex engine works on UTF8 string and SynEdit string in memory is
    UTF-16 representation, we need to get real indexes and lengths already put
    in maps }
  procedure FixMaps;
  var
    I, J, K, P: Integer;
    Temp: TSynRangeMatch;
  begin
    { Is map empty? }
    if Length(fMap^) > 0 then
    begin
      { Get First map item, set position on start, zero real length }
      I := 0;
      K := 0;
      P := 1;

      { Walk through the string until range map ends }
      while I < Length(fMap^) do
      begin
        J := fMap^[I].AIndex;
        K := K + CountUTF8Chars(S, J, P);
        P := J + 1;

        { Several adjanced map items can have similiar start index }
        while (I < Length(fMap^)) and (fMap^[I].AIndex = J) do
        begin
          { Insert corrected values into map item }
          with fMap^[I] do
          begin
            AIndex := K;
            ALength := CountUTF8Chars(S, J + ALength - 1, J);
          end;

          { Next }
          Inc(I);
        end;
      end;

      { Swap }
      for I := High(SwapMap) downto 0 do
      begin
        J := SwapMap[I][0];
        K := SwapMap[I][1];
        Temp := fMap^[J];
        for P := J + 1 to K do
          fMap^[P - 1] := fMap^[P];
        fMap^[K] := Temp;
      end;
    end;

    { Is map empty? }
    if Length(fSpecMap^) = 0 then
      Exit;

    { Sort }
    SortRuleMap(fSpecMap^);

    { Get First map item, set position on start, zero real length }
    I := 0;
    K := 0;
    P := 1;

    { Walk through the string until special rules map ends }
    while I < Length(fSpecMap^) do
    begin
      J := fSpecMap^[I].AIndex;
      K := K + CountUTF8Chars(S, J, P);
      P := J + 1;

      { Several adjanced map items can have similiar start index }
      while (I < Length(fSpecMap^)) and (fSpecMap^[I].AIndex = J) do
      begin
        { Insert corrected values into map item }
        with fSpecMap^[I] do
        begin
          AIndex := K;
          ALength := CountUTF8Chars(S, J + ALength - 1, J);
        end;

        { Next }
        Inc(I);
      end;
    end;
  end;

var
  I: Integer;
  Captures: PSynCaptureArray;
begin
  { Initialize }
  if Start = 1 then
  begin
    fMap := PSynUniCacheItem(fCache[fLineNumber])^.AMap;
    fSpecMap := PSynUniCacheItem(fCache[fLineNumber])^.ASpecMap;
    if PSynUniCacheItem(fCache[fLineNumber])^.ACached and not Force then
    begin
      fLineDone := True;
      Exit;
    end;

    { Reset }
    SetLength(fMap^, 0);
    SetLength(fSpecMap^, 0);
    SetLength(SwapMap, 0);

    { Get last capture }
    if fLineNumber > 0 then
    begin
      Captures := PSynUniCacheItem(fCache[Pred(fLineNumber)])^.ACaptureMap;
      SetLength(fCaptureMap^, Length(Captures^));
      for I := 0 to High(fCaptureMap^) do
        fCaptureMap^[I] := Captures^[I];
    end
    else
      SetLength(fCaptureMap^, 0);

    { Test on invalid input or blank range }
    if (fLineLen = 0) or ((fCurrRange = fRules) and (fCurrRange.RangeCount = 0)
      and (fCurrRange.KeyWordsCount = 0) and (fCurrRange.SetCount = 0)
      and (fCurrRange.LinkCount = 0)) then
    begin
      PSynUniCacheItem(fCache[fLineNumber])^.ACached := True;
      Captures := PSynUniCacheItem(fCache[fLineNumber])^.ACaptureMap;
      SetLength(Captures^, Length(fCaptureMap^));
      for I := 0 to High(fCaptureMap^) do
        Captures^[I] := fCaptureMap^[I];
      Exit;
    end;
  end

  { Clean all previously detected ranges and rules after start }
  else begin
    I := High(fMap^);
    if I > -1 then
      while (fMap^[I].AIndex >= Start) and (I > -1) do Dec(I);
    SetLength(fMap^, I + 1);
    I := High(fSpecMap^);
    if I > -1 then
      while (fSpecMap^[I].AIndex >= Start) and (I > -1) do Dec(I);
    SetLength(fSpecMap^, I + 1);
    SetLength(SwapMap, 0);
  end;

  { Get UTF8 encoded string }
  SetLength(S, fLineLen * 3);
  SetLength(S, UnicodeToUTF8(@S[1], Length(S), fLine, fLineLen){DELPHI2010 - 1});

  { Get matches for children ranges and their subranges }
  MatchChildren(fCurrRange, Start, Start, Length(S));

  { Final touches }
  FixMaps;

  { Cache }
  if Start = 1 then
  begin
    PSynUniCacheItem(fCache[fLineNumber])^.ACached := True;
    PSynUniCacheItem(fCache[fLineNumber])^.AOutlined := False;
    Captures := PSynUniCacheItem(fCache[fLineNumber])^.ACaptureMap;
    SetLength(Captures^, Length(fCaptureMap^));
    for I := 0 to High(fCaptureMap^) do
      Captures^[I] := fCaptureMap^[I];
  end;
end;

// -----------------------------------------------------------------------------
// Fill TSynCustomHighlighter with attributes
procedure TSynUniSyn.ActivateAttributes(Range: TSynRange);
var
  I, J: Integer;
begin
  if Range = fRules then fAttributes.Clear;
  with Range do
  begin

    { Tokens' group attributes }
    if not Range.Linked or Range.LinksRules then
    begin
      for I := 0 to Pred(OpenTokenCount) do
        for J := 0 to Pred(OpenTokens[I].GroupAttributeCount) do
          AddAttribute(OpenTokens[I].GroupAttributes[J]);
      for I := 0 to Pred(CloseTokenCount) do
        for J := 0 to Pred(CloseTokens[I].GroupAttributeCount) do
          AddAttribute(CloseTokens[I].GroupAttributes[J]);
    end;

    { General attributes }
    if not Range.Linked then
    begin
      AddAttribute(Attributes);
      AddAttribute(NumberAttributes);
      AddAttribute(SymbolAttributes);
    end;

    { KeyWords }
    for I := 0 to Pred(KeyWordsCount) do
      AddAttribute(KeyWords[I].Attributes);

    { Sets }
    for I := 0 to Pred(SetCount) do
      AddAttribute(Sets[I].Attributes);

    { SpecialRules }
    for I := 0 to Pred(SpecialRuleCount) do
    begin
      AddAttribute(SpecialRules[I].Attributes);
      for J := 0 to Pred(SpecialRules[I].GroupAttributeCount) do
        AddAttribute(SpecialRules[I].GroupAttributes[J]);
    end;

    { Ranges }
    for I := 0 to Pred(RangeCount) do
      ActivateAttributes(Ranges[I]);
  end;
end;

// -----------------------------------------------------------------------------
// Converts all clNone colors in attributes to parent defined values recursively
procedure TSynUniSyn.ActivateColors(Range: TSynRange);
var
  I, J, K: Integer;
begin
  { Subranges }
  for I := 0 to Pred(Range.RangeCount) do
    with Range.Ranges[I] do
    begin
      { Skip links }
      if Range.Ranges[I].Linked then
        Continue;

      { General attributes }
      if Attributes.Foreground = clNone then
        Attributes.Foreground := Range.Attributes.Foreground;

      { Number and symbol }
      if NumberAttributes.Foreground = clNone then
        NumberAttributes.Foreground := Attributes.Foreground;
      if SymbolAttributes.Foreground = clNone then
        SymbolAttributes.Foreground := Attributes.Foreground;

      { Tokens' group attributes }
      for J := 0 to Pred(OpenTokenCount) do
        for K := 0 to Pred(OpenTokens[J].GroupAttributeCount) do
          if OpenTokens[J].GroupAttributes[K].Foreground = clNone then
            OpenTokens[J].GroupAttributes[K].Foreground := Attributes.Foreground;
      for J := 0 to Pred(CloseTokenCount) do
        for K := 0 to Pred(CloseTokens[J].GroupAttributeCount) do
          if CloseTokens[J].GroupAttributes[K].Foreground = clNone then
            CloseTokens[J].GroupAttributes[K].Foreground := Attributes.Foreground;

      { Same thing on subranges }
      ActivateColors(Range.Ranges[I]);
    end;

  { Keywords }
  for I := 0 to Pred(Range.KeyWordsCount) do
    with Range.KeyWords[I] do
      if Attributes.Foreground = clNone then
        Attributes.Foreground := Range.Attributes.Foreground;

  { Sets }
  for I := 0 to Pred(Range.SetCount) do
    with Range.Sets[I] do
      if Attributes.Foreground = clNone then
        Attributes.Foreground := Range.Attributes.Foreground;

  { Special rules }
  for I := 0 to Pred(Range.SpecialRuleCount) do
    with Range.SpecialRules[I] do
    begin
      if Attributes.Foreground = clNone then
        Attributes.Foreground := Range.Attributes.Foreground;
      for J := 0 to Pred(GroupAttributeCount) do
        if GroupAttributes[J].Foreground = clNone then
          GroupAttributes[J].Foreground := Attributes.Foreground;
    end;
end;

// -----------------------------------------------------------------------------
// Assigns real rules to links according to links' rule name.
// Ranges are always recursively copied into new instances with their rules
// copied as links (instead of subranges as said)
procedure TSynUniSyn.ActivateLinks(Range, OriginalRange: TSynRange;
  const Lookup: TSynUniLinkLookupArray; LookupStart: Integer = 0);

  { Does a copy of range }
  procedure CopyRange(const Link: TSynRuleLink);
  var
    NewRange: TSynRange;
  begin
    with Link do
    begin
      if sloOmitTokens in Options then
      begin
        if not (sloChildrenOnly in Options) then
        begin
          Range.Linked := True;
          Range.LinksRules := True;
        end;
        Range.LinkRange(Rule as TSynRange, LinkOpenTokens, LinkCloseTokens,
          Options);
      end
      else begin
        NewRange := TSynRange.Create(True, False);
        NewRange.LinkRange(Rule as TSynRange, LinkOpenTokens, LinkCloseTokens,
          Options);
        Range.AddRule(NewRange);
      end;
    end;
  end;

  { Checks if a rule linked can actually be linked }
  function CanBeLinked(Link, Parent: TSynRange): Boolean;
  begin
    Result := True;
    if (Parent = Link) or (Parent.LinkSource = Link) then
    begin
      Result := False;
      Exit;
    end;
    while Parent <> nil do
    begin
      Parent := Parent.Parent;
      if (Parent = Link) or ((Parent <> nil) and (Parent.LinkSource = Link)) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;

  procedure ActivateGroup(Attr: TSynHighlighterAttributes);
  begin
    with Attr do
    begin
      if TagString = EmptyStr then Exit;
      Rule := OriginalRange.GetRuleByName(TagString, True, True);
      if Rule <> nil then
      begin
        Tag := Integer(Rule);
        TagString := EmptyStr;
      end;
    end;
  end;

var
  I, J, K: Integer;
begin
  { Activate self links }
  for I := Pred(Range.LinkCount) downto 0 do
  begin

    { Activate groups }
    for J := 0 to Pred(Range.OpenTokenCount) do
      for K := 0 to Pred(Range.OpenTokens[J].GroupAttributeCount) do
        ActivateGroup(Range.OpenTokens[J].GroupAttributes[K]);
    for J := 0 to Pred(Range.CloseTokenCount) do
      for K := 0 to Pred(Range.CloseTokens[J].GroupAttributeCount) do
        ActivateGroup(Range.CloseTokens[J].GroupAttributes[K]);

    { Assign linked rule }
    if Range.Links[I].Rule = nil then
    begin
      if Lookup <> nil then
        Range.Links[I].Rule := GetRuleFromLookup(Range.Links[I].Link, LookupStart)
      else
        Range.Links[I].Rule := OriginalRange.GetRuleByName(Range.Links[I].Link, True, True);
    end;

    { Range? Create a new instance for it and link everything to existing one }
    if (Range.Links[I].Rule <> nil) and (Range.Links[I].Rule is TSynRange) then
    begin
      if not CanBeLinked((Range.Links[I].Rule as TSynRange), Range) then
        Range.RemoveRule(Range.Links[I])
      else
        with Range do
        begin
          CopyRange(Links[I]);
          RemoveRule(Links[I]);
        end;
    end;
  end;

  { Activate subrule groups' triggers }
  for I := Pred(Range.SpecialRuleCount) downto 0 do
    for J := 0 to Pred(Range.SpecialRules[I].GroupAttributeCount) do
      ActivateGroup(Range.SpecialRules[I].GroupAttributes[J]);

  { Activate keywords' and sets' triggers }
  for I := Pred(Range.KeyWordsCount) downto 0 do
    ActivateGroup(Range.KeyWords[I].Attributes);
  for I := Pred(Range.SetCount) downto 0 do
    ActivateGroup(Range.Sets[I].Attributes);

  { Activate subranges links }
  for I := Pred(Range.RangeCount) downto 0 do
    ActivateLinks(Range.Ranges[I], OriginalRange, Lookup);
end;

// -----------------------------------------------------------------------------

function TSynUniSyn.GetTokenKindByRuleName(const Name: UTF8String): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to fAttributes.Count - 1 do
    if fAttributes[I] = Name then
    begin
      Result := Integer(fAttributes.Objects[I]);
      Break;
    end;
end;

function TSynUniSyn.GetRuleNameByTokenKind(TokenKind: Integer): UTF8String;
var
  I: Integer;
begin
  Result := EmptyAnsiStr;
  if TokenKind = -1 then
    Exit;
  for I := 0 to fAttributes.Count - 1 do
    if Integer(fAttributes.Objects[I]) = TokenKind then
    begin
      Result := fAttributes[I];
      Break;
    end;
end;

function TSynUniSyn.HasInParents(Rule: TSynRule; Parent: TSynRange): Boolean;

  { TODO: Find a way(s) to optimize this thing }

  function SearchChildren(Parent: TSynRange): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    if Parent = Rule then
    begin
      Result := True;
      Exit;
    end;
    for I := 0 to Pred(Parent.SpecialRuleCount) do
      if Parent.SpecialRules[I] = Rule then
      begin
        Result := True;
        Exit;
      end;
    for I := 0 to Pred(Parent.KeyWordsCount) do
      if Parent.KeyWords[I] = Rule then
      begin
        Result := True;
        Exit;
      end;
    for I := 0 to Pred(Parent.SetCount) do
      if Parent.Sets[I] = Rule then
      begin
        Result := True;
        Exit;
      end;
    for I := 0 to Pred(Parent.LinkCount) do
      if Parent.Links[I].Rule = Rule then
      begin
        Result := True;
        Exit;
      end;
    for I := 0 to Pred(Parent.RangeCount) do
      if Parent.Ranges[I] = Rule then
      begin
        Result := True;
        Exit;
      end;
    if not Result then
      for I := 0 to Pred(Parent.RangeCount) do
      begin
        Result := SearchChildren(Parent.Ranges[I]);
        if Result then
          Exit;
      end;
  end;

begin
  Result := SearchChildren(Parent);
  if not Result and Parent.Linked then
    Result := SearchChildren(Parent.LinkSource);
  if not Result then
  begin
    if (Rule is TSynRange) and (Rule as TSynRange).Linked then
    begin
      Rule := (Rule as TSynRange).LinkSource;
      Result := SearchChildren(Parent);
      if not Result and Parent.Linked then
        Result := SearchChildren(Parent.LinkSource);
    end;
  end;
  Exit;
end;

// -----------------------------------------------------------------------------
// Tests if range with sroOpenOnBol can be opened inside specified range
function TSynUniSyn.RangeSealed(Range: TSynRange): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(fSilenceMap) do
    if fSilenceMap[I] = Range.Attributes then
    begin
      Result := True;
      Break;
    end;
end;

// -----------------------------------------------------------------------------
// (Un)Marks range so that subranges with sroOpenOnBol can't be opened anymore
procedure TSynUniSyn.SealRange(Range: TSynRange; Unseal: Boolean = False);
var
  I: Integer;
begin
  if Unseal then
    for I := 0 to High(fSilenceMap) do
      if fSilenceMap[I] = Range.Attributes then
      begin
        fSilenceMap[I] := nil;
        Break;
      end else
  else
    if not RangeSealed(Range) then
    begin
      SetLength(fSilenceMap, Length(fSilenceMap) + 1);
      fSilenceMap[High(fSilenceMap)] := Range.Attributes;
    end;
end;

// -----------------------------------------------------------------------------
// Looks in what range we are
procedure TSynUniSyn.RangeProc;
var
  I, J, Pos: Integer;

  { Check out range injection }
  procedure CheckInjection;
  begin
    if Assigned(fTriggerRange) and (Pos = fTriggerPos) then
    begin
      { Test on silence }
      if sroOpenOnBol in fTriggerRange.Options then
        if RangeSealed(fTriggerRange) then
          Exit;

      { Open }
      fCurrRange := fTriggerRange;
      fTriggerRange := nil;

      { Refresh map }
      if not fLineDone then
        CreateRangeAndRuleMaps(True, Pos);
    end;
  end;

begin
  { Get string coords from PChar coords }
  Pos := Run + 1;
  fCurrRangePrev := fCurrRange;

  { Not on token }
  Exclude(fCurrStates, susOnTkn);

  { Check for injection first }
  CheckInjection;

  { Empty? }
  if (fMap = nil) or (Length(fMap^) = 0) then
    Exit;

  { Behind map? Nothing to look then }
  if Pos < fMap^[0].AIndex then
    Exit;

  { After map? Nothing to look then }
  if Pos > fMap^[High(fMap^)].AIndex + fMap^[High(fMap^)].ALength then
    Exit;

  { First index }
  J := 0;
  while Pos < fMap^[J].AIndex do
    Inc(J);

  { Map }
  for I := J to High(fMap^) do
  begin

    { We are at some kind of match, at it's end }
    if Pos = fMap^[I].AIndex + fMap^[I].ALength then
    begin

      { First, check if it's a closing match and it closes current range }
      if not fMap^[I].AOpen and (stoInside in fMap^[I].AToken.Options) then
      begin
        SealRange(fCurrRange, True);
        fCurrRange := fMap^[I].ARange.Parent;
        fCurrRangePrev := fCurrRange;
      end

      { Second, check if it's an open match and it starts a subrange }
      else if fMap^[I].AOpen and not (stoInside in fMap^[I].AToken.Options) then
      begin
        if sroOpenOnBol in fMap^[I].ARange.Options then
          if RangeSealed(fCurrRange) then
            Continue;
        fCurrRangePrev := fCurrRange;
        fCurrRange := fMap^[I].ARange;
        fCurrStates := [];
      end;
    end;

    { We are at some kind of match, at it's start }
    if Pos = fMap^[I].AIndex then
    begin

      { First, check if it's a closing match and it closes current range }
      if not fMap^[I].AOpen and not (stoInside in fMap^[I].AToken.Options) then
      begin
        SealRange(fCurrRange, True);
        fCurrRange := fMap^[I].ARange.Parent;
        fCurrRangePrev := fCurrRange;
      end

      { Second, check if it's an open match and it starts a subrange }
      else if fMap^[I].AOpen and (stoInside in fMap^[I].AToken.Options) then
      begin
        if sroOpenOnBol in fMap^[I].ARange.Options then
          if RangeSealed(fCurrRange) then
            Continue;
        fCurrRangePrev := fCurrRange;
        fCurrRange := fMap^[I].ARange;
        fCurrStates := [];
      end;
    end;

    { On token? }
    if (Pos >= fMap^[I].AIndex) and (Pos < fMap^[I].AIndex + fMap^[I].ALength) and
      ((not (stoInside in fMap^[I].AToken.Options) and (fMap^[I].ARange.Parent = fCurrRange)) or
      ((stoInside in fMap^[I].AToken.Options) and (fMap^[I].ARange = fCurrRange)))
    then
      Include(fCurrStates, susOnTkn);

    { Out of map? }
    if (Pos < fMap^[I].AIndex) and (Pos < fMap^[I].AIndex + fMap^[I].ALength) then
      Break;

  end;
end;

// -----------------------------------------------------------------------------

procedure TSynUniSyn.NullProc;
begin
  fCurrToken := fCurrRange;
  fCurrAttr := fCurrToken.Attributes;
  fTokenNature := tknnUnknown;
  Inc(Run);
  fLineDone := True;
end;

procedure TSynUniSyn.NbspProc;
begin
  fCurrToken := fCurrRange;
  fCurrAttr := fCurrToken.Attributes;
  fTokenNature := tknnNbsp;
  Inc(Run);
  while (fLine[Run] = WideNbsp) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynUniSyn.SpaceProc;
begin
  fCurrToken := fCurrRange;
  fCurrAttr := fCurrToken.Attributes;
  fTokenNature := tknnSpace;
  Inc(Run);
  while IsWhiteChar(fLine[Run]) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynUniSyn.CRProc;
begin
  fCurrToken := fCurrRange;
  fCurrAttr := fCurrToken.Attributes;
  fTokenNature := tknnUnknown;
  Inc(Run);
  if fLine[Run + 1] = #10 then Inc(Run);
end;

procedure TSynUniSyn.LFProc;
begin
  fCurrToken := fCurrRange;
  fCurrAttr := fCurrToken.Attributes;
  fTokenNature := tknnUnknown;
  Inc(Run);
end;

procedure TSynUniSyn.IdentProc;
begin
  fCurrToken := fCurrRange;
  fCurrAttr := fCurrToken.Attributes;
  fTokenNature := tknnIdent;

  { Find token end }
  while not IsWordBreakChar(fLine[Run]) do
    Inc(Run);

  { See if it's a number }
  NumberProc;

  { See if it's one of keywords' }
  if fCurrAttr = fCurrRange.Attributes then
    KeywordProc;

  { Check special rule match }
  SpecialProc;

  { States }
  if not (susOnTkn in fCurrStates) then
    SealRange(fCurrRange);
end;

procedure TSynUniSyn.NumberProc;
var
  I: Integer;
begin
  for I := fTokenPos to Run - 1 do
    if not IsDigitChar(fLine[I]) then
    begin
      if I - fTokenPos > 0 then
      begin
        if fCurrRange.NumberAttributes.FriendlyName <> fCurrRange.Attributes.FriendlyName then
          fCurrAttr := fCurrRange.NumberAttributes;
        fTokenNature := tknnNumber;
        Run := I;
      end;
      Exit;
    end;
  if fCurrRange.NumberAttributes.FriendlyName <> fCurrRange.Attributes.FriendlyName then
    fCurrAttr := fCurrRange.NumberAttributes;
  fTokenNature := tknnNumber;
end;

procedure TSynUniSyn.KeywordProc;
var
  I, J: Integer;
  P: PChar;

  function DoMatchKeywords(Keywords: TSynKeywords): Boolean;
  begin
    { Matches? }
    if (skoOpenOnBol in Keywords.Options) and RangeSealed(fCurrRange) then
      Result := False
    else
      Result := Keywords.Match(P, J, MurmurHash2(P, J, HashSeed or Keywords.Count));

    { Add to range map }
    if Result and (Keywords.Attributes.Tag <> 0) then
    begin
      fTriggerPos := Run + 1;
      fTriggerRange := TSynRange(Keywords.Attributes.Tag);
    end;
  end;

  procedure FreeIdent;
  begin
    if not (sroCaseSensitive in fCurrRange.Options) then FreeMem(P);
  end;

begin
  { Prepare for comparison }
  J := GetTokenLen * SizeOf(Char);
  if not (sroCaseSensitive in fCurrRange.Options) then
  begin
    P := AllocMem(J);
    Move((fLine + fTokenPos)^, P^, J);
    CharLowerBuffW(P, GetTokenLen);
  end
  else
    P := (fLine + fTokenPos);

  { Look current range keywords' for a match }
  for I := 0 to Pred(fCurrRange.KeyWordsCount) do
    if DoMatchKeywords(fCurrRange.KeyWords[I]) then
    begin
      fCurrToken := fCurrRange.KeyWords[I];
      fCurrAttr := fCurrRange.KeyWords[I].Attributes;
      FreeIdent;
      Exit;
    end;

  { Look current range links for a keywords to try a match }
  for I := 0 to Pred(fCurrRange.LinkCount) do
    if (fCurrRange.Links[I].Rule <> nil) and (fCurrRange.Links[I].Rule is TSynKeywords)
      and DoMatchKeywords(fCurrRange.Links[I].Rule as TSynKeywords) then
    begin
      fCurrToken := fCurrRange.Links[I].Rule;
      fCurrAttr := fCurrToken.Attributes;
      FreeIdent;
      Exit;
    end;

  { Not found }
  FreeIdent;
end;

procedure TSynUniSyn.SpecialProc;
var
  I, J, Pos: Integer;
  SpecialRule: TSynSpecialRule;
  RangeToken: TSynToken;
begin
  { Empty? }
  if (fSpecMap = nil) or (Length(fSpecMap^) = 0) then
    Exit;

  { Initialzie }
  Pos := fTokenPos + 1;

  { Behind map? Nothing to look then }
  if Pos < fSpecMap^[0].AIndex then
    Exit;

  { Do not check for 'after map' condition: last element isn't always the most
    far one }

  { First index }
  J := 0;
  while (Pos < fSpecMap^[J].AIndex) and (J < Length(fSpecMap^)) do
    Inc(J);

  { Loop through map }
  for I := J to High(fSpecMap^) do
  begin

    { Token inside a special rule match? }
    if (Pos >= fSpecMap^[I].AIndex) and (Pos < fSpecMap^[I].AIndex + fSpecMap^[I].ALength) then
    begin

      { Is it a range token match or a special rule match? }
      if fSpecMap^[I].ARule is TSynSpecialRule then
      begin
        SpecialRule := fSpecMap^[I].ARule as TSynSpecialRule;

        { Open on Bol? }
        if (ssroOpenOnBol in SpecialRule.Options) and RangeSealed(fCurrRange) then
          Continue;

        { On keyword? }
        if (fCurrToken is TSynKeywords) then
          if not (ssroPriorityOverKeywords in SpecialRule.Options) then
            Continue;

        { Is it already a part of rule token list? }
        if SpecialRule.Style <> EmptyAnsiStr then
        begin
          fCurrToken := SpecialRule;
          fCurrAttr := fCurrToken.Attributes;

          { Swallow in token }
          if SpecialRule.GroupAttributeCount = 0 then
            Run := fSpecMap^[I].AIndex + fSpecMap^[I].ALength - 1;
        end;

        { Set possible trigger pos }
        if (Pos >= fSpecMap^[I].AIndex) and (Pos < fSpecMap^[I].AIndex + fSpecMap^[I].ALength) and (fSpecMap^[I].AGroupIndex = 0) then
          fTriggerPos := fSpecMap^[I].AIndex + fSpecMap^[I].ALength;

        { Is it just inside a general match or at a valid group match? }
        if (Pos >= fSpecMap^[I].AIndex) and (Pos < fSpecMap^[I].AIndex + fSpecMap^[I].ALength) and (fSpecMap^[I].AGroupIndex > 0) then
        begin
          if SpecialRule.Style <> EmptyAnsiStr then
            fCurrToken := SpecialRule;

          if SpecialRule.GroupAttributeCount > fSpecMap^[I].AGroupIndex - 1 then
            fCurrAttr := SpecialRule.GroupAttributes[fSpecMap^[I].AGroupIndex - 1]
          else
            fCurrAttr := SpecialRule.Attributes;

          if fCurrAttr.Tag <> 0 then
            fTriggerRange := TSynRange(fCurrAttr.Tag);

          if fCurrAttr.FriendlyName = EmptyAnsiStr then
            fCurrAttr := SpecialRule.Attributes;
          if fCurrAttr.FriendlyName = EmptyAnsiStr then
            fCurrAttr := fCurrRange.Attributes;

          { Swallow in token }
          Run := fSpecMap^[I].AIndex + fSpecMap^[I].ALength - 1;
        end;
      end

      { It's a range token }
      else begin
        RangeToken := fSpecMap^[I].ARule as TSynToken;

        { Is it already a part of rule token? }
        if (RangeToken.GroupAttributeCount > 0) and (fSpecMap^[I].AGroupIndex = 0) then
          if RangeToken.GroupAttributes[0].FriendlyName <> EmptyStr then
          begin
            fCurrToken := fCurrRange;

            { Check if was captured by higher }
            J := 1;
            while J <= Pred(RangeToken.GroupAttributeCount) do
            begin
              if fCurrAttr = RangeToken.GroupAttributes[J] then
              begin
                Inc(J);
                Break;
              end;
              Inc(J);
            end;
            if J = 1 then
              fCurrAttr := RangeToken.GroupAttributes[0];

            { Swallow in token }
            if RangeToken.GroupAttributeCount = 1 then
              Run := fSpecMap^[I].AIndex + fSpecMap^[I].ALength - 1;
          end;

        { Is it just inside a general match or at a valid group match? }
        if (Pos >= fSpecMap^[I].AIndex) and (Pos < fSpecMap^[I].AIndex + fSpecMap^[I].ALength) and (fSpecMap^[I].AGroupIndex > 0)
          and (RangeToken.GroupAttributeCount > 1) then
        begin
          fCurrToken := fCurrRange;
          if RangeToken.GroupAttributeCount > fSpecMap^[I].AGroupIndex then
            fCurrAttr := RangeToken.GroupAttributes[fSpecMap^[I].AGroupIndex]
          else
            if RangeToken.GroupAttributes[0].FriendlyName <> EmptyStr then
              fCurrAttr := RangeToken.GroupAttributes[0]
            else
              fCurrAttr := fCurrRange.Attributes;

          { Swallow in token }
          Run := fSpecMap^[I].AIndex + fSpecMap^[I].ALength - 1;
        end;
      end;

      { Out of map? }
      if (Pos < fSpecMap^[I].AIndex) and (Pos < fSpecMap^[I].AIndex + fSpecMap^[I].ALength) then
        Break;

    end;
  end;
end;

procedure TSynUniSyn.SymbolProc;
begin
  fCurrToken := fCurrRange;
  fCurrAttr := fCurrRange.SymbolAttributes;
  fTokenNature := tknnSeparator;

  { First, check special rule match }
  SpecialProc;

  { See if it's one of sets' }
  if fCurrAttr = fCurrRange.SymbolAttributes then
    SetProc;

  { Next }
  if fTokenPos = Run then
    Inc(Run);

  { State }
  if not (susOnTkn in fCurrStates) then
    SealRange(fCurrRange);
end;

procedure TSynUniSyn.SetProc;

  function DoMatchSet(ASet: TSynSet): Boolean;
  begin
    { Matches? }
    Result := ASet.Match(fLine[Run]);

    { Add to range map }
    if Result and (ASet.Attributes.Tag <> 0) then
    begin
      fTriggerPos := Run + 2;
      fTriggerRange := TSynRange(ASet.Attributes.Tag);
    end;
  end;

var
  I: Integer;
begin
  { Look current range sets for a match }
  for I := 0 to Pred(fCurrRange.SetCount) do
    if DoMatchSet(fCurrRange.Sets[I]) then
    begin
      fCurrToken := fCurrRange.Sets[I];
      fCurrAttr := fCurrRange.Sets[I].Attributes;
      Exit;
    end;

  { Look links for a set }
  for I := 0 to Pred(fCurrRange.LinkCount) do
    if (fCurrRange.Links[I].Rule <> nil) and (fCurrRange.Links[I].Rule is TSynSet)
      and DoMatchSet(fCurrRange.Links[I].Rule as TSynSet) then
    begin
      fCurrToken := fCurrRange.Links[I].Rule;
      fCurrAttr := fCurrToken.Attributes;
      Exit;
    end;
end;

procedure TSynUniSyn.UnknownProc;
begin
  fCurrToken := fCurrRange;
  fCurrAttr := fCurrRange.Attributes;
  Inc(Run);
end;

// -----------------------------------------------------------------------------

function TSynUniSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

// -----------------------------------------------------------------------------

function TSynUniSyn.GetRange: Pointer;
begin
  Result := Pointer(fCurrRange);
end;

function TSynUniSyn.GetPrevRange: Pointer;
begin
  Result := Pointer(fCurrRangePrev);
end;

function TSynUniSyn.GetRule;
begin
  Result := fCurrToken;
end;

// -----------------------------------------------------------------------------

function TSynUniSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  Result := fCurrAttr;
end;

// -----------------------------------------------------------------------------

function TSynUniSyn.GetTokenKind: Integer;
begin
  Result := Integer(fCurrAttr);
end;

//------------------------------------------------------------------------------
// After each Next() call Run is placed after current token and fTokenPos
// is placed on current token first char
procedure TSynUniSyn.Next;
begin
  { Get current range }
  RangeProc;

  { Start of the current token is an end of previous }
  fTokenPos := Run;
  fTokenNature := tknnUnknown;

  { Apply current range rules }
  case fLine[Run] of

    { Null and EOLs }
    #0: NullProc;
    #13: CRProc;
    #10: LFProc;
    WideNbsp: NbspProc;

    { If current chat is a whitespace }
    else if IsWhiteChar(fLine[Run]) then
      SpaceProc

    { If current char is a part of an identifier }
    else if IsIdentChar(fLine[Run]) then
      IdentProc

    { If current char is a symbol }
    else if IsWordBreakChar(fLine[Run]) then
      SymbolProc

    { Couldn't decide action }
    else
      UnknownProc;
  end;

  inherited;
end;

// -----------------------------------------------------------------------------
// Retrieve current range from SynEdit line pointer
procedure TSynUniSyn.SetRange(AValue: Pointer);
begin
  if AValue = nil then
    fCurrRange := fRules
  else
    fCurrRange := TSynRange(AValue);
  fCurrToken := fCurrRange;
  fCurrAttr := fCurrRange.Attributes;
end;

// -----------------------------------------------------------------------------
// Reset current range to main rules
procedure TSynUniSyn.ResetRange;
begin
  fCurrRange := fRules;
  fCurrToken := fRules;
  fCurrAttr := fRules.Attributes;
end;

// -----------------------------------------------------------------------------
// Fixes colors after applying new stylesheet
procedure TSynUniSyn.LoadFromFile(ThemeFile: TSynHighlighterTheme);
begin
  inherited;
  ActivateColors(fRules);
end;

// -----------------------------------------------------------------------------

function TSynUniSyn.IsWordBreakChar(AChar: Char): Boolean;
begin
  Result := inherited IsWordBreakChar(AChar);
  if Result then
    Result := Pos(AChar, fCurrRange.RemoveDelimiters) < 1
  else
    Result := Pos(AChar, fCurrRange.AddDelimiters) > 0;
end;

// -----------------------------------------------------------------------------
// Cache
procedure TSynUniSyn.LinesInserted(AIndex: Integer; ACount: Integer);
begin
  GrowCache(AIndex, ACount);
end;

procedure TSynUniSyn.LinesDeleted(AIndex: Integer; ACount: Integer);
begin
  GrowCache(AIndex - 1, - ACount);
end;

procedure TSynUniSyn.LinesPutted(AIndex: Integer; ACount: Integer);
begin
  if AIndex = fCache.Count then
    GrowCache(AIndex, 1)
  else
    GrowCache(AIndex, 0);
end;

// -----------------------------------------------------------------------------
// Parse highlighter's grammar file
procedure TSynUniSyn.LoadGrammar(const AFileName: String;
  EnableLinks: Boolean = True; ExternalParent: TSynRange = nil;
  TargetName: UTF8String = '';
  Options: TSynUniLoadOptions = [suloExternalGrammars];
  PrependScope: UTF8String = '');

// Since 3.0 RC1 there's an ability to link whole eternal grammars inside
// the one being parsed. ExternalParent is a range inside which external grammar
// must be placed. TargetName is a range name inside external grammar which
// must be placed into ExternalParent (replacing it's name, style, tokens, etc).
//
// AsEmbedded is a param indicating whether external range must be threated as
// an embedded one. sometimes we want to load external range as an embedded in
// order to style it as an embedded. And sometimes we want to load it as it is
// without touching name and styling. When AsEmbedded is true, external range
// and all of it's subrules will have their names prepended with source range
// suffix (e.g. sql => php.sql when embedding sql inside php string) and all
// their style names will be prepended with embedded.<name> suffix if they
// aren't already embedded (e.g. keyword => embedded.sql.keyword when reading
// external sql range, embedded.keyword => won't change as it already has
// embedded suffix in it's name).
//
// The format of external link is:
//   {<uuid>}(:<target>)?(/<options>)?
//
// Where <uuid> is a unique id of a grammar to read, <target> is a name of a
// range to link and it's optional, <options> defines behaiour of linking and
// it's optional. Current option switch is only e which defines embedded
// linking.

var
  F: TextFile;
  S, NameSuffix, StyleSuffix: UTF8String;
  P: PAnsiChar;
  Valid, bSilent, bSilentRules, bTargetReaded: Boolean;
  LookupStart, LookupAttrStart: Integer;

  {$INCLUDE UFormatInput.inc}

  { Reads grammar info and sample {... }
  procedure ReadInfo;

    procedure ReadFilter(S: String);
    begin
      if bSilent then Exit;
      if EnableLinks then
      begin
        S := '*.' + StringReplace(S, ' ', ';*.', [rfReplaceAll]);
        fDefaultFilter := fFriendlyLanguageName + ' Files (' + S + ')|' + S;
      end
      else
        fDefaultFilter := S;
    end;

  var
    J: Integer;
  begin
    RdLn;
    if IsTagRead(sUID) then begin if not bSilent then Info.UID := GetValue(sUID); RdLn; end else Exit;
    Valid := IsTagRead(sName); if not Valid then Exit;
    if not bSilent then fFriendlyLanguageName := GetValue(sName);
    RdLn;
    if IsTagRead(sFilter) then begin ReadFilter(GetValue(sFilter)); RdLn; end;
    if IsTagRead(sDeveloper) then begin if not bSilent then Info.Developer := GetValue(sDeveloper); RdLn; end;
    if IsTagRead(sContacts) then begin if not bSilent then Info.Contacts := GetValue(sContacts); RdLn; end;
    if IsTagRead(sVersion) then begin if not bSilent then Info.Version := GetValue(sVersion); RdLn; end;
    if IsTagRead(sFirstLineParam) then begin if not bSilent then Info.FirstLineMatch := GetValue(sFirstLineParam); RdLn; end;
    if IsTagRead(sSample) then
    begin
      J := 0;
      if not bSilent then SetLength(Info.Sample, 1024);
      ReadLn(F, S);
      while (S <> sSampleEnd) and not Eof(F) do
      begin
        if not bSilent then
        begin
          if Length(Info.Sample) < J + Length(S) then
            SetLength(Info.Sample, (J + Length(S)) shl 1);
          Move(S[1], Info.Sample[Succ(J)], Length(S));
          Inc(J, Length(S));
          Move(SAnsiLineBreak[1], Info.Sample[Succ(J)], Length(SAnsiLineBreak));
          Inc(J, Length(SAnsiLineBreak));
        end;
        ReadLn(F, S);
      end;
      RdLn; // Skip to range {
      if not bSilent then SetLength(Info.Sample, J - Length(SAnsiLineBreak));
    end;
  end;

  { Reads environment {... }
  procedure ReadEnvironment;
  var
    sName, sValue: UTF8String;
  begin
    if IsTagRead(sEnvironmentTag) then
    begin
      RdLn;
      while not IsTagRead(sEnd) do
      begin
        sName := GetName;
        sValue := GetValue(sName + ' = ''');
        Environment.Add(UTF8ToUnicodeString(sName + #9 + sValue));
        RdLn;
      end;
      RdLn;
    end;
  end;

  { Reads range {... }
  procedure ReadRules(Parent: TSynRange; var TargetReaded: Boolean);

    { Reads rule properies }
    procedure ReadRuleProperties(Element: TObject);
    var
      sNameVal, sLinkVal, sStyleVal, sAttrVal: UTf8String;
    begin
      Valid := False;
      if IsTag(sName) then
      begin
        sNameVal := GetValue(sName);
        sAttrVal := NameSuffix + sNameVal;

        if fAttrLookupPos + 3 >= Length(fAttrLookup) then
          SetLength(fAttrLookup, fAttrLookupPos * 2 + 3);
        with fAttrLookup[fAttrLookupPos] do
        begin
          AHash := MurmurHash2(@sAttrVal[1], Length(sAttrVal), HashSeed);
          if Element is TSynRule then
            ARule := (Element as TSynRule).Attributes
          else
            ARule := Element;
        end;
        Inc(fAttrLookupPos);

        if Element <> nil then
        if Element is TSynRule then
        begin
          if not bTargetReaded and bSilentRules and
            (Element is TSynRange) and (sNameVal = TargetName) then
          begin
            bSilentRules := False;
            TargetReaded := True;
          end;
          if not bSilentRules then
          begin
            if (Element = ExternalParent) and (suloAsEmbedded in Options) then
              sLinkVal := (Element as TSynRule).Name
            else
              sLinkVal := sNameVal;

            if fRuleLookupPos >= Length(fRuleLookup) then
              SetLength(fRuleLookup, fRuleLookupPos * 2);
            with fRuleLookup[fRuleLookupPos] do
            begin
              AHash := MurmurHash2(@sLinkVal[1], Length(sLinkVal), HashSeed);
              ARule := (Element as TSynRule);
            end;

            Inc(fRuleLookupPos);
            (Element as TSynRule).Name := NameSuffix + sNameVal;
            (Element as TSynRule).LinkName := sLinkVal;

            if Element is TSynRange then
            begin
              with fAttrLookup[fAttrLookupPos] do
              begin
                AHash := MurmurHash2(@(Element as TSynRange).NumberAttributes.Name[1], Length((Element as TSynRange).NumberAttributes.Name), HashSeed);
                ARule := (Element as TSynRange).NumberAttributes;
              end;
              Inc(fAttrLookupPos);
              with fAttrLookup[fAttrLookupPos] do
              begin
                AHash := MurmurHash2(@(Element as TSynRange).SymbolAttributes.Name[1], Length((Element as TSynRange).SymbolAttributes.Name), HashSeed);
                ARule := (Element as TSynRange).SymbolAttributes;
              end;
              Inc(fAttrLookupPos);
            end;

          end;
        end
        else if Element is TSynHighlighterAttributes then
        begin
          if not bSilentRules then
            (Element as TSynHighlighterAttributes).Name := NameSuffix + sNameVal;
        end;
      end
      else
        Exit;
      if IsTag(sStyle) then
      begin
        if Element <> nil then
        if not bSilentRules then
        begin
          sStyleVal := GetValue(sStyle);
          if Element is TSynRule then
            (Element as TSynRule).LinkStyle := sStyleVal;
          if not bSilent or not (suloAsEmbedded in Options) then
            //sStyleVal := GetValue(sStyle)
          else
            sStyleVal := AddStyleSuffix(sStyleVal, PrependScope,
              suloEmbedIntoParentScope in Options);
          if Element is TSynRule then
            (Element as TSynRule).Style := sStyleVal
          else if Element is TSynHighlighterAttributes then
            (Element as TSynHighlighterAttributes).FriendlyName := sStyleVal;
        end;
      end
      else
        Exit;
      Valid := True;
    end;

    { Reads additional range properties }
    procedure ReadRangeProperties(Range: TSynRange);

      { Parses range options param }
      function StrToRangeOptions(const S: UTF8String): TSynRangeOptions;
      var
        V: Byte;
      begin
        Result := [];
        V := StrToInt(S);
        if GetBitState(V, 0) then Include(Result, sroCaseSensitive);
        if GetBitState(V, 1) then Include(Result, sroOpenOnBol);
        if GetBitState(V, 2) then Include(Result, sroCanBeInsideToken);
        if GetBitState(V, 3) then Include(Result, sroOmitGlobalTokens);
        if GetBitState(V, 4) then Include(Result, sroSpellCheck);
      end;

    begin
      RdLn;
      Parent.Options := [];
      if IsTagRead(sOptions) then begin if not bSilentRules then Parent.Options := StrToRangeOptions(GetValue(sOptions)); RdLn; end;
      if IsTagRead(sStyleSym) then
      begin
        if not bSilentRules then
          if suloAsEmbedded in Options then
            Parent.SymbolAttributes.FriendlyName := AddStyleSuffix(GetValue(sStyleSym), Parent.SymbolAttributes.Name)
          else
            Parent.SymbolAttributes.FriendlyName := GetValue(sStyleSym);
        RdLn;
      end;
      if IsTagRead(sStyleNum) then
      begin
        if not bSilentRules then
          if suloAsEmbedded in Options then
            Parent.NumberAttributes.FriendlyName := AddStyleSuffix(GetValue(sStyleNum), Parent.NumberAttributes.Name)
          else
            Parent.NumberAttributes.FriendlyName := GetValue(sStyleNum);
        RdLn;
      end;
      if IsTagRead(sDelimAdd) then begin if not bSilentRules then Parent.AddDelimiters := UTF8ToUnicodeString(GetValue(sDelimAdd)); RdLn; end;
      if IsTagRead(sDelimRem) then begin if not bSilentRules then Parent.RemoveDelimiters := UTF8ToUnicodeString(GetValue(sDelimRem)); RdLn; end;
    end;

    { Reads additional special rule properties }
    procedure ReadSpecialRuleProperties(SpecialRule: TSynSpecialRule);

      { Parses special rule options param }
      function StrToSpecialRuleOptions(const S: UTF8String): TSynSpecialRuleOptions;
      var
        V: Byte;
      begin
        Result := [];
        V := StrToInt(S);
        if GetBitState(V, 0) then Include(Result, ssroCaseSensitive);
        if GetBitState(V, 1) then Include(Result, ssroOpenOnBol);
        if GetBitState(V, 2) then Include(Result, ssroPriorityOverKeywords);
        if GetBitState(V, 3) then Include(Result, ssroSymbol);
      end;

    begin
      RdLn;
      if IsTagRead(sOptions) then begin if not bSilentRules then SpecialRule.Options := StrToSpecialRuleOptions(GetValue(sOptions)); RdLn; end;
      if IsTagRead(sReplacePattern) then begin if not bSilentRules then SpecialRule.ReplacePattern := GetValue(sReplacePattern); RdLn; end;
    end;

    { Reads additional keywords properties }
    procedure ReadKeywordsProperties(Keywords: TSynKeywords);

      { Parses keywords options param }
      function StrToKeywordsOptions(const S: UTF8String): TSynKeywordsOptions;
      var
        V: Byte;
      begin
        Result := [];
        V := StrToInt(S);
        if GetBitState(V, 0) then Include(Result, skoOpenOnBol);
      end;

    begin
      RdLn;
      if IsTagRead(sOptions) then begin if not bSilentRules then Keywords.Options := StrToKeywordsOptions(GetValue(sOptions)); RdLn; end;
    end;

    { Reads open {... and close {... }
    procedure ReadToken(Open: Boolean);
    var
      NewToken: TSynToken;
      NewGroup: TSynHighlighterAttributes;

      { Parses token options param }
      procedure StrToTokenOptions(const S: UTF8String);
      var
        V: Byte;
      begin
        NewToken.Options := [];
        V := StrToInt(S);
        if V and 1 <> 0 then NewToken.Options := NewToken.Options + [stoInside];
        if not Open then if V and 2 <> 0 then NewToken.Options := NewToken.Options + [stoGlobal];
        if V and 4 <> 0 then NewToken.Options := NewToken.Options + [stoBackreferenced];
      end;

    begin
      if not bSilentRules then
        if Open then
          NewToken := Parent.AddOpenToken(EmptyAnsiStr, False)
        else
          NewToken := Parent.AddCloseToken(EmptyAnsiStr, False, False);
      Valid := False;
      if IsTag(sPattern) then begin if not bSilentRules then NewToken.Pattern := GetValue(sPattern); RdLn; end else Exit;
      if IsTagRead(sPriority) then begin if not bSilentRules then NewToken.Priority := StrToIntDef(GetValue(sPattern), 0); RdLn; end;
      if IsTagRead(sOptions) then begin if not bSilentRules then StrToTokenOptions(GetValue(sOptions)); RdLn; end;
      if IsTagRead(sGroups) then
      begin
        RdLn; // Group's starting {
        while not IsTagRead(sEnd) do
        begin
          if not bSilentRules then
            NewGroup := NewToken.AddGroupAttribute(EmptyStr, EmptyStr)
          else
            NewGroup := nil;
          ReadRuleProperties(NewGroup);
          RdLn;
          if IsTagRead(sTrigger) then begin if not bSilentRules then NewGroup.TagString := GetValue(sTrigger); RdLn; end;
          RdLn; // Group's ending }
        end;
        RdLn;
      end;
      Valid := IsTagRead(sEnd);
      if Valid then
        RdLn;
    end;

    { Reads rule {... }
    procedure ReadSpecialRule;
    var
      NewSpecialRule: TSynSpecialRule;
      NewGroup: TSynHighlighterAttributes;
    begin
      if not bSilentRules then
        NewSpecialRule := TSynSpecialRule.Create
      else
        NewSpecialRule := nil;
      ReadRuleProperties(NewSpecialRule); if not Valid then Exit;
      ReadSpecialRuleProperties(NewSpecialRule);
      Valid := False;
      if IsTagRead(sPattern) then begin if not bSilentRules then NewSpecialRule.Pattern := GetValue(sPattern); RdLn; end else Exit;

      { Invalid pattern? }
      if not bSilentRules then
        if NewSpecialRule.Pattern = EmptyAnsiStr then
        begin
          FreeAndNil(NewSpecialRule);
          Exit;
        end;

      if IsTagRead(sPriority) then begin if not bSilentRules then NewSpecialRule.Priority := StrToIntDef(GetValue(sPriority), 0); RdLn; end;

      if IsTagRead(sGroups) then
      begin
        RdLn; // Group's starting {
        while not IsTagRead(sEnd) do
        begin
          if not bSilentRules then
            NewGroup := NewSpecialRule.AddGroupAttribute(EmptyStr, EmptyStr)
          else
            NewGroup := nil;
          ReadRuleProperties(NewGroup);
          RdLn;
          if IsTagRead(sTrigger) then begin if not bSilentRules then NewGroup.TagString := GetValue(sTrigger); RdLn; end;
          RdLn; // Group's ending }
        end;
        RdLn;
      end;

      if not bSilentRules then
        Parent.AddRule(NewSpecialRule);

      Valid := IsTagRead(sEnd);
      if Valid then
        RdLn;
    end;

    { Reads keywords {... }
    procedure ReadKeywords;
    var
      NewKeywords: TSynKeywords;
    begin
      if not bSilentRules then
      begin
        NewKeywords := TSynKeywords.Create;
        Parent.AddRule(NewKeywords);
      end
      else
        NewKeywords := nil;
      ReadRuleProperties(NewKeywords); if not Valid then Exit;
      ReadKeywordsProperties(NewKeywords);
      Valid := IsTagRead(sBegin); if not Valid then Exit;
      if not bSilentRules then
        NewKeywords.BeginUpdate;
      try
        while True do
        begin
          RdLn;
          if IsTagRead(sEnd) then
            Break;
          if not bSilentRules then
            NewKeywords.AddKey(RdULn);
        end;
      finally
        if not bSilentRules then
          NewKeywords.EndUpdate;
      end;
      Valid := IsTagRead(sEnd);
      if Valid then RdLn else Exit;
      if IsTagRead(sTrigger) then begin if not bSilentRules then NewKeywords.Attributes.TagString := GetValue(sTrigger); RdLn; end;
      Valid := IsTagRead(sEnd);
      if Valid then RdLn;
    end;

    { Reads set {... }
    procedure ReadSet;
    var
      NewSet: TSynSet;
    begin
      if not bSilentRules then
      begin
        NewSet := TSynSet.Create;
        Parent.AddRule(NewSet);
      end
      else
        NewSet := nil;
      ReadRuleProperties(NewSet); if not Valid then Exit;
      Valid := False;
      if IsTag(sPattern) then begin if not bSilentRules then NewSet.Symbols := GetValue(sPattern); RdLn; end else Exit;
      if IsTagRead(sTrigger) then begin if not bSilentRules then NewSet.Attributes.TagString := GetValue(sTrigger); RdLn; end;
      Valid := IsTagRead(sEnd);
      if Valid then RdLn;
    end;

    { Reads link {... }
    procedure ReadLink;

      procedure ReadLinkArray(var Arr: TIntegerArray; const S: String);
      var
        B, E: Integer;

        procedure AddItem;
        begin
          SetLength(Arr, Length(Arr) + 1);
          Arr[High(Arr)] := StrToInt(Copy(S, B, E - B));
        end;

      begin
        B := 1;
        E := Pos(#32, S);
        while E > 0 do
        begin
          AddItem;
          B := E + 1;
          E := PosEx(#32, S, B);
        end;
        E := Length(S) + 1;
        AddItem;
      end;

    var
      NewLink: TSynRuleLink;

      procedure ReadExternalGrammar;
      var
        nTargetPos, nOptionsPos: Integer;
        sLink, sFileName, sTarget, sOptions, sPrepend: UnicodeString;
        bCreatePlaceholder: Boolean;
        NewRange: TSynRange;
        NewOptions: TSynUniLoadOptions;
      begin
        if bSilentRules then Exit;
        if EnableLinks and Assigned(fOnExternalGrammar) and
          (NewLink.Link[1] = '{') then
        begin
          { Check if allowed to link external grammars }
          if not (suloExternalGrammars in Options) then
          begin
            Parent.RemoveRule(NewLink);
            Exit;
          end;

          { Get file name }
          sLink := UTF8ToUnicodeString(NewLink.Link);
          sFileName := Copy(sLink, 1, Pos('}', sLink));
          fOnExternalGrammar(Self, sFileName);
          if not FileExists(sFileName) then Exit;

          { Read external grammar }
          try

            { Initialize options }
            NewOptions := [suloExternalGrammars];

            { Get options }
            nOptionsPos := Pos('/', sLink);
            if nOptionsPos > 0 then
              sOptions := Copy(sLink, Succ(nOptionsPos), MAXINT)
            else begin
              sOptions := EmptyStr;
              nOptionsPos := Length(sLink) + 1;
            end;
            if (Pos('e', sOptions) > 0) or (sloAsEmbedded in NewLink.Options) then
              Include(NewOptions, suloAsEmbedded);
            if Pos('r', sOptions) > 0 then
              Exclude(NewOptions, suloExternalGrammars);
            sPrepend := EmptyAnsiStr;
            if sloEmbedIntoParentScope in NewLink.Options then
            begin
              sPrepend := Parent.LinkStyle;
              Include(NewOptions, suloEmbedIntoParentScope);
            end;

            { Get targeted range }
            nTargetPos := Pos(':', sLink);
            if nTargetPos > 0 then
              sTarget := Copy(sLink, Succ(nTargetPos), nOptionsPos - nTargetPos - 1)
            else
              sTarget := EmptyStr;
            bCreatePlaceholder := {sTarget <> EmptyStr}False;

            { Disable link }
            Parent.RemoveRule(NewLink);

            { Choose where to place external range }
            if bCreatePlaceholder then
            begin
              { Create new placeholder range for linked one and load inside }
              NewRange := TSynRange.Create;
              LoadGrammar(sFileName, True, NewRange, sTarget, NewOptions, sPrepend);
            end
            else
              { Load inside the current range }
              LoadGrammar(sFileName, True, Parent, sTarget, NewOptions, sPrepend);

          finally
          end;
        end;
      end;

      function ReadLinkOptions(const S: UTF8String): TSynLinkOptions;
      var
        V: Byte;
      begin
        Result := [];
        V := StrToIntDef(S, 0);
        if V and 1 <> 0 then Include(Result, sloOmitTokens);
        if V and 2 <> 0 then Include(Result, sloChildrenOnly);
        if V and 4 <> 0 then Include(Result, sloAsEmbedded);
        if V and 8 <> 0 then Include(Result, sloEmbedIntoParentScope);
      end;

    begin
      NewLink := nil;
      if not bSilentRules then
      begin
        NewLink := TSynRuleLink.Create;
        SetLength(NewLink.LinkOpenTokens, 0);
        SetLength(NewLink.LinkCloseTokens, 0);
      end;
      Valid := False;
      if IsTag(sRuleLink) then begin if not bSilentRules then NewLink.Link := GetValue(sRuleLink); RdLn; end else Exit;
      if IsTagRead(sOptions) then begin if not bSilentRules then NewLink.Options := ReadLinkOptions(GetValue(sOptions)); RdLn; end;
      if IsTagRead(sOpenPtrn) then begin if not bSilentRules then ReadLinkArray(NewLink.LinkOpenTokens, GetValue(sOpenPtrn)); RdLn; end;
      if IsTagRead(sClosePtrn) then begin if not bSilentRules then ReadLinkArray(NewLink.LinkCloseTokens, GetValue(sClosePtrn)); RdLn; end;
      if NewLink <> nil then
        Parent.AddRule(NewLink);
      Valid := IsTagRead(sEnd);
      if Valid then RdLn else Exit;
      ReadExternalGrammar;
    end;

    { Reads range {... }
    procedure ReadRange;
    var
      NewRange: TSynRange;
      bReadedTarget: Boolean;
    begin
      if not bSilentRules then
      begin
        NewRange := TSynRange.Create;
        Parent.AddRule(NewRange);
        ReadRules(NewRange, bReadedTarget);
      end
      else begin
        ReadRules(Parent, bReadedTarget);
        if bReadedTarget then
        begin
          bTargetReaded := True;
          bSilentRules := True;
        end;
      end;
    end;

  begin
    TargetReaded := False;
    ReadRuleProperties(Parent); if not Valid then Exit;
    ReadRangeProperties(Parent);
    while not IsTagRead(sEnd) do
    begin
      if not Valid then Exit;
      if IsTagRead(sOpen) then          ReadToken(True)
      else if IsTagRead(sClose) then    ReadToken(False)
      else if IsTagRead(sRule) then     ReadSpecialRule
      else if IsTagRead(sKeywords) then ReadKeywords
      else if IsTagRead(sSet) then      ReadSet
      else if IsTagRead(sLink) then     ReadLink
      else if IsTagRead(sRange) then    ReadRange;
    end;
    Valid := IsTagRead(sEnd);
    if Valid then RdLn;
  end;

  function RangeFromAttr(const Attr: Integer): TSynRange;
  begin
    if Attr = -1 then Result := nil else
    Result := TSynHighlighterAttributes(Attr).Rule as TSynRange;
  end;

  { Reads outlining {... }
  procedure ReadOutlining;
  var
    NewRegion: TFoldRegionItem;
    bSkip, bBackreferenced: Boolean;
    sKind: UTF8String;

    { Parses region options param }
    procedure StrToRegionOptions;
    var
      V: Byte;
    begin
      V := StrToIntDef(GetValue(sOptions), 0);
      if GetBitState(V, 0) then NewRegion.CloseBackreference := True;
      if GetBitState(V, 1) then NewRegion.Kind := frikIndentation;
    end;

  begin
    if IsTagRead(sOutlining) then
    begin
      { Initialize }
      Valid := False;
      RdLn; // Group's starting {
      while not IsTagRead(sEnd) do
      begin
        { Reset }
        bSkip := False;
        NewRegion := FoldRegions.Add(EmptyStr, EmptyAnsiStr, EmptyAnsiStr, -1);
        RdLn;

        { Read region properties }
        if IsTagRead(sName) then begin NewRegion.Name := GetValue(sName); RdLn; end;
        if IsTagRead(sOptions) then begin StrToRegionOptions; RdLn; end;
        bBackreferenced := NewRegion.CloseBackreference and (NewRegion.Kind = frikNormal);

        { Read patterns }
        if IsTagRead(sOpenPtrn) then begin NewRegion.Open := GetValue(sOpenPtrn); RdLn; end else if NewRegion.Kind = frikNormal then Exit;
        if IsTagRead(sClosePtrn) then begin NewRegion.Close := GetValue(sClosePtrn); RdLn; end else if NewRegion.Kind = frikNormal then Exit;

        { Read open token kind }
        if IsTagRead(sOpenTkn) then
        begin
          sKind := GetValue(sOpenTkn);
          if bSilentRules and (Pos(TargetName, sKind) <> 1) then
            bSkip := True;
          if not bSkip then
            NewRegion.OpenTokenKind := GetTokenFromLookup(NameSuffix + sKind, LookupAttrStart);
          RdLn;
        end;

        { Read close token kind }
        if IsTagRead(sCloseTkn) then
        begin
          if not bSkip then
          begin
            sKind := GetValue(sCloseTkn);
            if bSilentRules and (Pos(TargetName, sKind) <> 1) then
              bSkip := True;
            if not bSkip then
              NewRegion.CloseTokenKind := GetTokenFromLookup(NameSuffix + sKind, LookupAttrStart);
          end;
          RdLn;
        end
        else
          NewRegion.CloseTokenKind := NewRegion.OpenTokenKind;

        { Read backreference parameters }
        if bBackreferenced then
        begin
          if IsTagRead(sOpenBref) then begin if not bSkip then NewRegion.OpenBref := GetValue(sOpenBref); RdLn; end else Exit;
          if IsTagRead(sCloseBref) then begin if not bSkip then NewRegion.CloseBref := GetValue(sCloseBref); RdLn; end else Exit;
        end;

        { Read open token range }
        if IsTagRead(sOpenRng) then
        begin
          if not bSkip and (NewRegion.Kind = frikIndentation) then
          begin
            sKind := GetValue(sOpenRng);
            if bSilentRules and (Pos(TargetName, sKind) <> 1) then
              bSkip := True;
            if not bSkip then
              NewRegion.OpenRng := Pointer(RangeFromAttr(GetTokenFromLookup(NameSuffix + sKind, LookupAttrStart)));
          end;
          RdLn;
        end;

        { Invalid region? }
        if bSkip then
          FoldRegions.Remove(NewRegion);

        { Next region }
        RdLn; // Group's ending }
      end;
      Valid := IsTagRead(sEnd);
      if Valid then
        RdLn;
    end;
  end;

  { Reads matching {... }
  procedure ReadMatching;
  var
    NewMatch: TSynTokenMatch;
    bSkip: Boolean;
    sKind: UTF8String;
  begin
    if IsTagRead(sMatching) then
    begin
      { Initialize }
      Valid := False;
      RdLn; // Group's starting {
      while not IsTagRead(sEnd) do
      begin
        { Reset }
        bSkip := False;
        SetLength(MatchTokens, Succ(Length(MatchTokens)));
        RdLn;

        { Important: reinit record }
        with NewMatch do
        begin
          OpenTokenKind := -1;
          CloseTokenKind := -1;
          OpenRange := nil;
          CloseRange := nil;
          Backreference := False;
        end;

        { Read patterns }
        if IsTagRead(sOpenPtrn) then begin NewMatch.OpenToken := GetValue(sOpenPtrn); RdLn; end else Exit;
        if IsTagRead(sClosePtrn) then begin NewMatch.CloseToken := GetValue(sClosePtrn); RdLn; end else Exit;

        { Read properties }
        if IsTagRead(sOptions) then begin NewMatch.Backreference := GetValue(sOptions) = '1'; RdLn; end else NewMatch.Backreference := False;

        { Read open token kind }
        if IsTagRead(sOpenTkn) then
        begin
          sKind := GetValue(sOpenTkn);
          if bSilentRules and (Pos(TargetName, sKind) <> 1) then
            bSkip := True;
          if not bSkip then
            NewMatch.OpenTokenKind := GetTokenFromLookup(NameSuffix + sKind, LookupAttrStart);
          RdLn;
        end
        else Exit;

        { Read close token kind }
        if IsTagRead(sCloseTkn) then
        begin
          if not bSkip then
          begin
            sKind := GetValue(sCloseTkn);
            if bSilentRules and (Pos(TargetName, sKind) <> 1) then
              bSkip := True;
            if not bSkip then
              NewMatch.CloseTokenKind := GetTokenFromLookup(NameSuffix + sKind, LookupAttrStart);
          end;
          RdLn;
        end
        else
          NewMatch.CloseTokenKind := NewMatch.OpenTokenKind;

        { Read open range }
        if IsTagRead(sOpenRng) then
        begin
          if not bSkip then
          begin
            sKind := GetValue(sOpenRng);
            if bSilentRules and (Pos(TargetName, sKind) <> 1) then
              bSkip := True;
            if not bSkip then
              NewMatch.OpenRange := Pointer(RangeFromAttr(GetTokenFromLookup(NameSuffix + sKind, LookupAttrStart)));
          end;
          RdLn;
        end;

        { Read close range }
        if IsTagRead(sCloseRng) then
        begin
          if not bSkip then
          begin
            sKind := GetValue(sCloseRng);
            if bSilentRules and (Pos(TargetName, sKind) <> 1) then
              bSkip := True;
            if not bSkip then
              NewMatch.CloseRange := Pointer(RangeFromAttr(GetTokenFromLookup(NameSuffix + sKind, LookupAttrStart)));
          end;
          RdLn;
        end;

        { Invalid match? }
        if bSkip then
          SetLength(MatchTokens, High(MatchTokens))
        else
          MatchTokens[High(MatchTokens)] := NewMatch;

        { Next match }
        RdLn; // Group's ending }
      end;
      Valid := IsTagRead(sEnd);
      if Valid then
        RdLn;
    end;
  end;

  { Reads indentation {... }
  procedure ReadIndentation;
  var
    NewRule: TSynIndentationRule;
    bSkip: Boolean;
    sKind: UTF8String;

    { Parses rule options param }
    procedure StrToIndentationOptions;
    var
      V: Byte;
    begin
      V := StrToIntDef(GetValue(sOptions), 0);
      if GetBitState(V, 0) then Include(NewRule.Options, sioIndent);
      if GetBitState(V, 1) then Include(NewRule.Options, sioOnlyOnce);
    end;

  begin
    if IsTagRead(sIndentation) then
    begin
      { Initialize }
      Valid := False;
      RdLn; // Group's starting {
      while not IsTagRead(sEnd) do
      begin
        { Reset }
        bSkip := False;
        SetLength(IndentationRules, Succ(Length(IndentationRules)));
        RdLn;

        { Important: reinit record }
        with NewRule do
        begin
          Options := [];
          TokenKind := -1;
          Range := nil;
        end;

        { Read properties }
        if IsTagRead(sPattern) then begin NewRule.Pattern := GetValue(sPattern); RdLn; end else Exit;
        if IsTagRead(sOptions) then begin StrToIndentationOptions; RdLn; end;

        { Read token kind }
        if IsTagRead(sTokenKnd) then
        begin
          sKind := GetValue(sTokenKnd);
          if bSilentRules and (Pos(TargetName, sKind) <> 1) then
            bSkip := True;
          if not bSkip then
            NewRule.TokenKind := GetTokenFromLookup(NameSuffix + sKind, LookupAttrStart);
          RdLn;
        end;

        { Read range }
        if IsTagRead(sRng) then
        begin
          if not bSkip then
          begin
            sKind := GetValue(sRng);
            if bSilentRules and (Pos(TargetName, sKind) <> 1) then
              bSkip := True;
            if not bSkip then
              NewRule.Range := Pointer(RangeFromAttr(GetTokenFromLookup(NameSuffix + sKind, LookupAttrStart)));
          end;
          RdLn;
        end;

        { Invalid rule? }
        if bSkip then
          SetLength(IndentationRules, High(IndentationRules))
        else
          IndentationRules[High(IndentationRules)] := NewRule;

        { Next rule }
        RdLn; // Group's ending }
      end;
      Valid := IsTagRead(sEnd);
      if Valid then
        RdLn;
    end;
  end;

  { Reads grammar {... }
  procedure ReadGrammar;
  var
    bDummy: Boolean;
  begin
    Valid := IsTag(sGrammar); if not Valid then Exit;
    ReadInfo; if not Valid then Exit;
    ReadEnvironment;
    Valid := IsTagRead(sRange); if not Valid then Exit;
    ReadRules(ExternalParent, bDummy); if not Valid then Exit;
    if TargetName <> EmptyAnsiStr then bSilentRules := True else bSilentRules := False;
    ReadOutlining; if not Valid then Exit;
    ReadMatching; if not Valid then Exit;
    ReadIndentation; if not Valid then Exit;
    Valid := IsTagRead(sEnd); if not Valid then Exit;
    if EnableLinks then
    begin
      SetLength(fRuleLookup, fRuleLookupPos);
      ActivateLinks(ExternalParent, ExternalParent, fRuleLookup, LookupStart);
    end;
    if not bSilent then ActivateAttributes(ExternalParent);
  end;

var
  Buf: array [1..49152] of AnsiChar;
begin
  { Initialize }
  NameSuffix := EmptyAnsiStr;
  StyleSuffix := EmptyAnsiStr;
  bSilent := False;
  bSilentRules := False;
  bTargetReaded := False;

  { Load }
  AssignFile(F, AFileName);
  try
    SetTextBuf(F, Buf);
    Reset(F);
    if ExternalParent = nil then
    begin
      Clear;
      ExternalParent := fRules;
      SetLength(fRuleLookup, 64);
      SetLength(fAttrLookup, 128);
    end
    else begin
      bSilent := True;
      if suloAsEmbedded in Options then
        if Pos('.', ExternalParent.Name) > 0 then
          NameSuffix := Copy(ExternalParent.Name, 1, LastDelimiter('.', ExternalParent.Name))
        else
          NameSuffix := ExternalParent.Name + '.'
      else
        NameSuffix := EmptyAnsiStr;
      if TargetName <> EmptyAnsiStr then
        bSilentRules := True else bSilentRules := False;
    end;
    LookupStart := fRuleLookupPos;
    LookupAttrStart := fAttrLookupPos;
    ReadGrammar;
  finally
    CloseFile(F);
  end;

  { Error? }
  if not Valid then
  begin
    if not bSilent then
      Clear;
    Exit;
  end;

  { Finish }
  if not bSilent then
  begin
    SetLength(fRuleLookup, 0);
    SetLengtH(fAttrLookup, 0);
    fFileName := AFileName;
    fLanguageName := Copy(AFileName, LastDelimiter(PathDelim, AFileName) + 1, MAXINT);
    if Length(fLanguageName) - Length(sExt) <= 0 then
      fLanguageName := sUndefined
    else
      SetLength(fLanguageName, Length(fLanguageName) - Length(sExt));
  end;
end;

// -----------------------------------------------------------------------------
// Save higlhighter contents to a grammar file
procedure TSynUniSyn.SaveGrammar(const AFileName: String);
var
  I: Integer;
  F: TextFile;

  {$INCLUDE UFormatOutput.inc}

  { Writes grammar info and sample {... }
  procedure WriteInfo;

    procedure WriteFilter;
    var
      J: Integer;
      S: UnicodeString;
    begin
      J := PosEx('|', fDefaultFilter);
      if J > 0 then
      begin
        S := Copy(fDefaultFilter, J + 1, MAXINT);
        S := Trim(StringReplace(StringReplace(S, '*.', EmptyStr, [rfReplaceAll]), ';', #32, [rfReplaceAll]));
      end
      else
        S := fDefaultFilter;
      WrULn(sFilter + S + sTail);
    end;

  begin
    WrLn(sUID + Info.UID + sTail);
    WrULn(sName + fFriendlyLanguageName + sTail);
    if fDefaultFilter <> EmptyStr then
      WriteFilter;
    if Info.Developer <> EmptyAnsiStr then
      WrLn(sDeveloper + Info.Developer + sTail);
    if Info.Contacts <> EmptyAnsiStr then
      WrLn(sContacts + Info.Contacts + sTail);
    if Info.Version <> EmptyAnsiStr then
      WrLn(sVersion + Info.Version + sTail);
    if Info.FirstLineMatch <> EmptyAnsiStr then
      WrLn(sFirstLineParam + Info.FirstLineMatch + sTail);
    if Info.Sample <> EmptyStr then
    begin
      WrLn(sSample);
      Inc(I);
        WrLn(Info.Sample, True);
      Dec(I);
      WrLn(sEnd);
    end;
  end;

  { Writes environment {... }
  procedure WriteEnvironment;
  var
    J: Integer;
  begin
    if Environment.Count = 0 then Exit;
    WrLn(sEnvironmentTag);
    Inc(I);
      for J := 0 to Pred(Environment.Count) do
        WrLn(UnicodeStringToUTF8(Environment.Names[J] + ' = ''' + Environment.ValueFromIndex[J] + ''';'));
    Dec(I);
    WrLn(sEnd);
  end;

  { Writes range {... }
  procedure WriteRules(Parent: TSynRange);

    { Writes rule properties }
    procedure WriteRuleProperties(Rule: TSynRule);
    begin
      WrLn(sName + Rule.Name + sTail);
      WrLn(sStyle + Rule.Style + sTail);
    end;

    { Writes additional range properties }
    procedure WriteRangeProperties(Range: TSynRange);

      function RangeOptionsToString: UTF8String;
      var
        V: Byte;
      begin
        V := 0;
        if sroCaseSensitive in Range.Options then
          SetBitState(V, 0, 1);
        if sroOpenOnBol in Range.Options then
          SetBitState(V, 1, 1);
        if sroCanBeInsideToken in Range.Options then
          SetBitState(V, 2, 1);
        if sroOmitGlobalTokens in Range.Options then
          SetBitState(V, 3, 1);
        if sroSpellCheck in Range.Options then
          SetBitState(V, 4, 1);
        Result := IntToStr(V);
      end;

    begin
      if Range.Options <> [] then
        WrLn(sOptions + RangeOptionsToString + sTail);
      if Range.SymbolAttributes.FriendlyName <> Range.Attributes.FriendlyName then
        WrULn(sStyleSym + Range.SymbolAttributes.FriendlyName + sTail);
      if Range.NumberAttributes.FriendlyName <> Range.Attributes.FriendlyName then
        WrULn(sStyleNum + Range.NumberAttributes.FriendlyName + sTail);
      if Range.AddDelimiters <> EmptyStr then
        WrULn(sDelimAdd + Range.AddDelimiters + sTail);
      if Range.RemoveDelimiters <> EmptyStr then
        WrULn(sDelimRem + Range.RemoveDelimiters + sTail);
    end;

    { Writes additional special rule properties }
    procedure WriteSpecialRuleProperties(SpecialRule: TSynSpecialRule);

      function SpecialRuleOptionsToString: UTF8String;
      var
        V: Byte;
      begin
        V := 0;
        if ssroCaseSensitive in SpecialRule.Options then
          SetBitState(V, 0, 1);
        if ssroOpenOnBol in SpecialRule.Options then
          SetBitState(V, 1, 1);
        if ssroPriorityOverKeywords in SpecialRule.Options then
          SetBitState(V, 2, 1);
        if ssroSymbol in SpecialRule.Options then
          SetBitState(V, 3, 1);
        Result := IntToStr(V);
      end;

    begin
      if SpecialRule.Options <> [] then
        WrLn(sOptions + SpecialRuleOptionsToString + sTail);
      if SpecialRule.ReplacePattern <> EmptyAnsiStr then
        WrLn(sReplacePattern + SpecialRule.ReplacePattern + sTail);
    end;

    { Writes additional keywords properties }
    procedure WriteKeywordsProperties(Keywords: TSynKeywords);

      function KeywordsOptionsToString: UTF8String;
      var
        V: Byte;
      begin
        V := 0;
        if skoOpenOnBol in Keywords.Options then
          SetBitState(V, 0, 1);
        Result := IntToStr(V);
      end;

    begin
      if Keywords.Options <> [] then
        WrLn(sOptions + KeywordsOptionsToString + sTail);
    end;

    { Writes open {... and close {... }
    procedure WriteToken(Token: TSynToken; Open: Boolean);

      procedure TokenOptionsToString;
      var
        V: Byte;
      begin
        V := 0;
        if stoInside in Token.Options then
          V := V or 1;
        if not Open then
          if stoGlobal in Token.Options then
            V := V or 2;
        if stoBackreferenced in Token.Options then
          V := V or 4;
        if V > 0 then
          WrLn(sOptions + IntToStr(V) + sTail);
      end;

    var
      J: Integer;
    begin
      if Open then
        WrLn(sOpen)
      else
        WrLn(sClose);
      Inc(I);
        WrLn(sPattern + Token.Pattern + sTail);
        if Token.Priority <> 0 then
          WrLn(sPriority + IntToStr(Token.Priority) + sTail);
        TokenOptionsToString;
        if Token.GroupAttributeCount > 0 then
        begin
          WrLn(sGroups);
          Inc(I);
          for J := 0 to Token.GroupAttributeCount - 1 do
          begin
            WrLn(sBegin);
            Inc(I);
              WrULn(sName + Token.GroupAttributes[J].Name + sTail);
              WrULn(sStyle + Token.GroupAttributes[J].FriendlyName + sTail);
              if Token.GroupAttributes[J].TagString <> EmptyStr then
                WrLn(sTrigger + Token.GroupAttributes[J].TagString + sTail);
            Dec(I);
            WrLn(sEnd);
          end;
          Dec(I);
          WrLn(sEnd);
        end;
      Dec(I);
      WrLn(sEnd);
    end;

    { Writes rule {... }
    procedure WriteSpecialRule(SpecialRule: TSynSpecialRule);
    var
      J: Integer;
    begin
      WrLn(sRule);
      Inc(I);
        WriteRuleProperties(SpecialRule);
        WriteSpecialRuleProperties(SpecialRule);
        WrLn(sPattern + SpecialRule.Pattern + sTail);
        if SpecialRule.Priority <> 0 then
          WrLn(sPriority + IntToStr(SpecialRule.Priority) + sTail);
        if SpecialRule.GroupAttributeCount > 0 then
        begin
          WrLn(sGroups);
          Inc(I);
          for J := 0 to SpecialRule.GroupAttributeCount - 1 do
          begin
            WrLn(sBegin);
            Inc(I);
              WrULn(sName + SpecialRule.GroupAttributes[J].Name + sTail);
              WrULn(sStyle + SpecialRule.GroupAttributes[J].FriendlyName + sTail);
              if SpecialRule.GroupAttributes[J].TagString <> EmptyStr then
                WrLn(sTrigger + SpecialRule.GroupAttributes[J].TagString + sTail);
            Dec(I);
            WrLn(sEnd);
          end;
          Dec(I);
          WrLn(sEnd);
        end;
      Dec(I);
      WrLn(sEnd);
    end;

    { Writes keywords {... }
    procedure WriteKeywords(Keywords: TSynKeywords);
    var
      J: Integer;
    begin
      WrLn(sKeywords);
      Inc(I);
        WriteRuleProperties(Keywords);
        WriteKeywordsProperties(Keywords);
        WrLn(sBegin);
        for J := 0 to Keywords.Count - 1 do
          WrULn(Keywords.Keys[J], True);
        WrLn(sEnd);
        if Keywords.Attributes.TagString <> EmptyAnsiStr then
          WrLn(sTrigger + Keywords.Attributes.TagString + sTail);
      Dec(I);
      WrLn(sEnd);
    end;

    { Writes set {... }
    procedure WriteSet(ASet: TSynSet);
    begin
      WrLn(sSet);
      Inc(I);
        WriteRuleProperties(ASet);
        WrULn(sPattern + ASet.Symbols + sTail);
        if ASet.Attributes.TagString <> EmptyAnsiStr then
          WrLn(sTrigger + ASet.Attributes.TagString + sTail);
      Dec(I);
      WrLn(sEnd);
    end;

    { Writes link {... }
    procedure WriteLink(Link: TSynRuleLink);
    var
      J: Integer;
      S: UTF8String;

      procedure LinkOptionsToString;
      var
        V: Byte;
      begin
        V := 0;
        if sloOmitTokens in Link.Options then V := V or 1;
        if sloChildrenOnly in Link.Options then V := V or 2;
        if sloAsEmbedded in Link.Options then V := V or 4;
        if sloEmbedIntoParentScope in Link.Options then V := V or 8;
        if V <> 0 then
          WrLn(sOptions + IntToStr(V) + sTail);
      end;

    begin
      WrLn(sLink);
      Inc(I);
        WrLn(sRuleLink + Link.Link + sTail);
        LinkOptionsToString;
        S := EmptyAnsiStr;
        for J := 0 to High(Link.LinkOpenTokens) do
          S := S + IntToStr(Link.LinkOpenTokens[J]) + #32;
        SetLength(S, Pred(Length(S)));
        if S <> EmptyAnsiStr then
          WrLn(sOpenPtrn + S + sTail);
        S := EmptyAnsiStr;
        for J := 0 to High(Link.LinkCloseTokens) do
          S := S + IntToStr(Link.LinkCloseTokens[J]) + #32;
        SetLength(S, Pred(Length(S)));
        if S <> EmptyAnsiStr then
          WrLn(sClosePtrn + S + sTail);
      Dec(I);
      WrLn(sEnd);
    end;

  var
    J: Integer;
  begin
    WrLn(sRange);
    Inc(I);
      WriteRuleProperties(Parent);
      WriteRangeProperties(Parent);
      for J := 0 to Pred(Parent.OpenTokenCount) do
        WriteToken(Parent.OpenTokens[J], True);
      for J := 0 to Pred(Parent.CloseTokenCount) do
        WriteToken(Parent.CloseTokens[J], False);
      for J := 0 to Pred(Parent.SpecialRuleCount) do
        WriteSpecialRule(Parent.SpecialRules[J]);
      for J := 0 to Pred(Parent.KeywordsCount) do
        WriteKeywords(Parent.Keywords[J]);
      for J := 0 to Pred(Parent.SetCount) do
        WriteSet(Parent.Sets[J]);
      for J := 0 to Pred(Parent.LinkCount) do
        WriteLink(Parent.Links[J]);
      for J := 0 to Pred(Parent.RangeCount) do
        WriteRules(Parent.Ranges[J]);
    Dec(I);
    WrLn(sEnd);
  end;

  { Writes outlining {... }
  procedure WriteOutlining;

    procedure RegionOptionsToString(ARegion: TFoldRegionItem);
    var
      V: Byte;
    begin
      V := 0;
      if ARegion.CloseBackreference then
        SetBitState(V, 0, 1);
      if ARegion.Kind = frikIndentation then
        SetBitState(V, 1, 1);
      if V <> 0 then
        WrLn(sOptions + IntToStr(V) + sTail);
    end;

  var
    J: Integer;
  begin
    if FoldRegions.Count = 0 then
      Exit;
    WrLn(sOutlining);
    Inc(I);
      for J := 0 to FoldRegions.Count - 1 do
      begin
        WrLn(sBegin);
        Inc(I);
          if FoldRegions[J].Name <> EmptyStr then
            WrULn(sName + FoldRegions[J].Name + sTail);
          RegionOptionsToString(FoldRegions[J]);
          if (FoldRegions[J].Kind = frikNormal) or ((FoldRegions[J].Kind = frikIndentation) and
            (FoldRegions[J].Open <> EmptyAnsiStr)) then
          WrLn(sOpenPtrn + FoldRegions[J].Open + sTail);
          if FoldRegions[J].Kind = frikNormal then
            WrLn(sClosePtrn + FoldRegions[J].Close + sTail);
          if FoldRegions[J].OpenTokenKind > -1 then
            WrLn(sOpenTkn + GetRuleNameByTokenKind(FoldRegions[J].OpenTokenKind) + sTail);
          if (FoldRegions[J].OpenTokenKind > -1) and (FoldRegions[J].OpenTokenKind <> FoldRegions[J].CloseTokenKind) then
            WrLn(sCloseTkn + GetRuleNameByTokenKind(FoldRegions[J].CloseTokenKind) + sTail);
          if FoldRegions[J].CloseBackreference and (FoldRegions[J].Kind = frikNormal) then
          begin
            WrLn(sOpenBref + FoldRegions[J].OpenBref + sTail);
            WrLn(sCloseBref + FoldRegions[J].CloseBref + sTail);
          end;
          if FoldRegions[J].Kind = frikIndentation then
            WrLn(sOpenRng + GetRuleNameByTokenKind(Integer(TSynRange(FoldRegions[J].OpenRng).Attributes)) + sTail);
        Dec(I);
        WrLn(sEnd);
      end;
    Dec(I);
    WrLn(sEnd);
  end;

  { Writes matching {... }
  procedure WriteMatching;
  var
    J: Integer;
  begin
    if Length(MatchTokens) = 0 then
      Exit;
    WrLn(sMatching);
    Inc(I);
      for J := 0 to High(MatchTokens) do
      begin
        WrLn(sBegin);
        Inc(I);
          WrLn(sOpenPtrn + MatchTokens[J].OpenToken + sTail);
          WrLn(sClosePtrn + MatchTokens[J].CloseToken + sTail);
          if MatchTokens[J].Backreference then WrLn(sOptions + '1' + sTail);
          WrLn(sOpenTkn + GetRuleNameByTokenKind(MatchTokens[J].OpenTokenKind) + sTail);
          if (MatchTokens[J].CloseTokenKind > -1) and (MatchTokens[J].CloseTokenKind <> MatchTokens[J].OpenTokenKind) then
            WrLn(sCloseTkn + GetRuleNameByTokenKind(MatchTokens[J].CloseTokenKind) + sTail);
          if MatchTokens[J].OpenRange <> nil then
            WrLn(sOpenRng + GetRuleNameByTokenKind(Integer(TSynRange(MatchTokens[J].OpenRange).Attributes)) + sTail);
          if MatchTokens[J].CloseRange <> nil then
            WrLn(sCloseRng + GetRuleNameByTokenKind(Integer(TSynRange(MatchTokens[J].CloseRange).Attributes)) + sTail);
        Dec(I);
        WrLn(sEnd);
      end;
    Dec(I);
    WrLn(sEnd);
  end;

  { Writes matching {... }
  procedure WriteIndentation;
  var
    J: Integer;

    procedure IndentationOptionsToString;
    var
      V: Byte;
    begin
      V := 0;
      if sioIndent in IndentationRules[J].Options then
        SetBitState(V, 0, 1);
      if sioOnlyOnce in IndentationRules[J].Options then
        SetBitState(V, 1, 1);
      if V <> 0 then
        WrLn(sOptions + IntToStr(V) + sTail);
    end;

  begin
    if Length(IndentationRules) = 0 then
      Exit;
    WrLn(sIndentation);
    Inc(I);
      for J := 0 to High(IndentationRules) do
      begin
        WrLn(sBegin);
        Inc(I);
          WrLn(sPattern + IndentationRules[J].Pattern + sTail);
          IndentationOptionsToString;
          if (IndentationRules[J].TokenKind > -1) then
            WrLn(sTokenKnd + GetRuleNameByTokenKind(IndentationRules[J].TokenKind) + sTail);
          if IndentationRules[J].Range <> nil then
            WrLn(sRng + GetRuleNameByTokenKind(Integer(TSynRange(IndentationRules[J].Range).Attributes)) + sTail);
        Dec(I);
        WrLn(sEnd);
      end;
    Dec(I);
    WrLn(sEnd);
  end;

  { Writes grammar {... }
  procedure WriteGrammar;
  begin
    WrLn(sGrammar);
    Inc(I);
      WriteInfo;
      WriteEnvironment;
      WriteRules(fRules);
      WriteOutlining;
      WriteMatching;
      WriteIndentation;
    Dec(I);
    WrLn(sEnd);
  end;

begin
  AssignFile(F, AFileName);
  Rewrite(F);
  I := 0;
  try
    WriteGrammar;
  finally
    CloseFile(F);
  end;
end;

end.
