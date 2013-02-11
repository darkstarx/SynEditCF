(*

  Letterpress, Lantern

  Miscellanious methods, classes and constants for SynUni and rules.

  Copyright 2006-2010, initial developers and Garnet

*)

unit SynUniClasses;

interface

uses
  Classes, SysUtils,

  { SynEdit }
  SynEditHighlighter;

const
  { Default TSynUniSyn values }
  sUndefined: String = 'None';
  sExt: String = '.grammar';

  { Standard styles }
  sTextElem: UTF8String = 'text';
  sKeywordElem: UTF8String = 'keyword';
  sCommentElem: UTF8String = 'comment';
  sStringElem: UTF8String = 'string';
  sSelectionElem: UTF8String = 'selection';
  sLineElem: UTF8String = 'line';
  sInvisiblesElem: UTF8String = 'invisibles';
  sMarginElem: UTF8String = 'margin';
  sOutliningElem: UTF8String = 'outlining';
  sDiffAddedElem: UTF8String = 'diff.added';
  sDiffRemovedElem: UTF8String = 'diff.removed';
  sDiffChangedElem: UTF8String = 'diff.changed';
  sEmbedded: UTF8String = 'embedded';

  { Parsing }
  sGrammar: UTF8String = 'grammar {';
  sEnvironmentTag: UTF8String = 'environment {';
  sSample: UTF8String = 'sample {';
  sRange: UTF8String = 'range {';
  sRule: UTF8String = 'rule {';
  sKeywords: UTF8String = 'keywords {';
  sSet: UTF8String = 'set {';
  sLink: UTF8String = 'link {';
  sOpen: UTF8String = 'open {';
  sClose: UTF8String = 'close {';
  sGroups: UTF8String = 'groups {';
  sOutlining: UTF8String = 'outlining {';
  sMatching: UTF8String = 'matching {';
  sIndentation: UTF8String = 'indentation {';
  sBegin: UTF8String = '{';
  sUID: UTF8String = 'uuid = ''';
  sName: UTF8String = 'name = ''';
  sStyle: UTF8String = 'style = ''';
  sOptions: UTF8String = 'options = ''';
  sTrigger: UTF8String = 'trigger = ''';
  sFilter: UTF8String = 'filter = ''';
  sDeveloper: UTF8String = 'developer = ''';
  sContacts: UTF8String = 'contacts = ''';
  sVersion: UTF8String = 'version = ''';
  sStyleSym: UTF8String = 'style_symbol = ''';
  sStyleNum: UTF8String = 'style_number = ''';
  sDelimAdd: UTF8String = 'delim_add = ''';
  sDelimRem: UTF8String = 'delim_rem = ''';
  sPattern: UTF8String = 'pattern = ''';
  sReplacePattern: UTF8String = 'replace_pattern = ''';
  sPriority: UTF8String = 'priority = ''';
  sRuleLink: UTF8String = 'rule = ''';
  sOpenPtrn: UTF8String = 'open = ''';
  sClosePtrn: UTF8String = 'close = ''';
  sTokenKnd: UTF8String = 'token = ''';
  sRng: UTF8String = 'range = ''';
  sOpenRng: UTF8String = 'open_range = ''';
  sCloseRng: UTF8String = 'close_range = ''';
  sTermRng: UTF8String = 'term_range = ''';
  sOpenTkn: UTF8String = 'open_token = ''';
  sCloseTkn: UTF8String = 'close_token = ''';
  sTermTkn: UTF8String = 'term_token = ''';
  sOpenBref: UTF8String = 'open_bref = ''';
  sCloseBref: UTF8String = 'close_bref = ''';
  sFirstLineParam: UTF8String = 'first_line = ''';
  sTail: UTF8String = ''';';
  sSampleEnd: UTF8String = #9'}';
  sEnd: UTF8String = '}';

  { For MurmurHash2 }
  HashSeed = $c58f1a7b;

  { To throw away Graphics.pas }
  clNone = $1FFFFFFF;

type
  PSynHighlighterAttributesArray = ^TSynHighlighterAttributesArray;
  TSynHighlighterAttributesArray = array of TSynHighlighterAttributes;

  { Highlighter states }
  TSynUniState = (
    susOnTkn // The current position in line is inside open / close token
             // of some range
  );

  TSynUniStates = set of TSynUniState;

  { Range options }
  TSynRangeOption = (
    sroCaseSensitive,    // Apply range rules in case sensitive manner
    sroOpenOnBol,        // The range opens only if it's open token is first token in range
    sroCanBeInsideToken, // The range can exist inside a token portion of string
    sroOmitGlobalTokens, // Global parent tokens has no effect inside this range
    sroSpellCheck        // Apply spell check to this range
  );

  TSynRangeOptions = set of TSynRangeOption;

  { Special rule options }
  TSynSpecialRuleOption = (
    ssroCaseSensitive, // Apply pattern in case sensitive manner
    ssroOpenOnBol,     // The rule is active only if it's token is first in range
    ssroPriorityOverKeywords, // If one of rule tokens intersects with keyword identifier,
                              // that keyword is shadowed by rule
    ssroSymbol         // Collect in symbol list
  );

  TSynSpecialRuleOptions = set of TSynSpecialRuleOption;

  { Keywords options }
  TSynKeywordsOption = (skoOpenOnBol);

  TSynKeywordsOptions = set of TSynKeywordsOption;

  { Set options }
  TSynSetOption = (ssoOpenOnBol);

  TSynSetOptions = set of TSynSetOption;

  { Link options }
  TSynLinkOption = (sloOmitTokens, sloChildrenOnly, sloAsEmbedded,
    sloEmbedIntoParentScope);

  TSynLinkOptions = set of TSynLinkOption;

  { Token options }
  TSynTokenOption = (
    { Means token must be lit according to it's range rules. If not set,
      it will be lit accoding to parent range rules and it's range will
      be active strictly after that token}
    stoInside,

    { For closing token to identify that it's range can
      be closed in any of it's children subrange }
    stoGlobal,

    { Means that token pattern contains captures which should be stored in
      order for it's closing counterpart to match correctly }
    stoBackreferenced
  );

  TSynTokenOptions = set of TSynTokenOption;

  { Range boundaries are defined by range tokens which can reside inside
    opened range or open range and still be outside opened range }
  TSynToken = class
  private
    { Range to which token belongs }
    fRange: TObject;

    { Properties }
    fPattern: UTF8String;
    fPriority: Integer;
    fOptions: TSynTokenOptions;

    { Add range open / close token matches to rule map }
    fGroupAttributes: TSynHighlighterAttributesArray;

    { Group attributes (captures) management }
    function GetGroupAttributeCount: Integer;
    function GetGroupAttribute(Index: Integer): TSynHighlighterAttributes;
  public
    constructor Create(Range: TObject);
    destructor Destroy; override;

    { Groups }
    function AddGroupAttribute(const AName,
      AStyle: String): TSynHighlighterAttributes;
    procedure DeleteGroupAttribute(Index: Integer);

    { Properties }
    property Range: TObject read fRange;

    property Pattern: UTF8String read fPattern write fPattern;
    property Priority: Integer read fPriority write fPriority;
    property Options: TSynTokenOptions read fOptions write fOptions;

    property GroupAttributeCount: Integer read GetGroupAttributeCount;
    property GroupAttributes[Index: Integer]: TSynHighlighterAttributes
      read GetGroupAttribute;
  end;

  PSynTokens = ^TSynTokens;
  TSynTokens = array of TSynToken;

  { Highlighter information }
  TSynGrammar = record
    UID,
    Developer,
    Contacts,
    Version,
    Sample,
    FirstLineMatch: UTF8String;
  end;

  { Miscellanious methods }
  procedure FreeList(var AList: TList);
  procedure ClearAttributeArray(var Arr: TSynHighlighterAttributesArray);
  function ExpandGrammarFilter(const sName, sFilter: UTF8String): UnicodeString;
  function MurmurHash2(AData: Pointer; ALength, ASeed: Cardinal): Cardinal;

implementation

{ TSynToken }

// -----------------------------------------------------------------------------

constructor TSynToken.Create(Range: TObject);
begin
  inherited Create;
  fRange := Range;
  fPriority := 0;
  fOptions := [];
  SetLength(fGroupAttributes, 0);
end;

destructor TSynToken.Destroy;
begin
  ClearAttributeArray(fGroupAttributes);
  inherited;
end;

// -----------------------------------------------------------------------------

function TSynToken.GetGroupAttributeCount: Integer;
begin
  Result := Length(fGroupAttributes);
end;

function TSynToken.GetGroupAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  Result := fGroupAttributes[Index];
end;

// -----------------------------------------------------------------------------

function TSynToken.AddGroupAttribute(const AName, AStyle: String): TSynHighlighterAttributes;
begin
  Result := TSynHighlighterAttributes.Create(AName, AStyle);
  Result.Rule := fRange;
  SetLength(fGroupAttributes, Length(fGroupAttributes) + 1);
  fGroupAttributes[High(fGroupAttributes)] := Result;
end;

procedure TSynToken.DeleteGroupAttribute(Index: Integer);
begin
  FreeAndNil(fGroupAttributes[Index]);
  for Index := Index + 1 to High(fGroupAttributes) do
    fGroupAttributes[Pred(Index)] := fGroupAttributes[Index];
  SetLength(fGroupAttributes, Pred(Length(fGroupAttributes)));
end;

{ SynUniClasses.pas }

// -----------------------------------------------------------------------------
// Clears a TList freeing all it's children
procedure FreeList(var AList: TList);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
  begin
    TObject(AList[I]).Free;
    AList[I] := nil;
  end;
  AList.Clear;
  FreeAndNil(AList);
end;

procedure ClearAttributeArray(var Arr: TSynHighlighterAttributesArray);
var
  I: Integer;
begin
  for I := 0 to High(Arr) do
    FreeAndNil(Arr[I]);
  SetLength(Arr, 0);
end;

function ExpandGrammarFilter(const sName, sFilter: UTF8String): UnicodeString;
var
  S: UnicodeString;
begin
  Result := EmptyStr;
  if (sName = EmptyAnsiStr) or (sFilter = EmptyAnsiStr) then Exit;
  S := UTF8ToUnicodeString(sFilter);
  S := '*.' + StringReplace(S, #32, '; *.', [rfReplaceAll]);
  Result := UTF8ToUnicodeString(sName) + ' Files (' + S + ')|' + S;
end;

// -----------------------------------------------------------------------------
// Returns a MurmurHash2 for a given token.
// © Austin Appleby, Delphi BASM implementation © Davy Landman.
//
// Garnet:
// This implementation proved to be the fastest on small data blocks of
// ~128 bytes which keywords are. The tests were completed using
// "Delphi MurmurHash v1.4" test project.
//
// MurmurHash2 guarantees unique hash for a given 4-byte block of data.
// That means all keys in SynKeywords must be at least two characters in length.
//
// Comments to the BASM code added by Garnet
function MurmurHash2(AData: Pointer; ALength, ASeed: Cardinal): Cardinal; assembler;
const
    R = 24;
asm
    push  ebx
    push  esi
    push  edi

    xchg  edx, eax

    mov   edi, eax
    mov   esi, eax
    xor   eax, ecx

    and   edi, 3
    shr   esi, 2
    jz    @Last3

{ General loop }
@LargeLoop:
    cmp   esi, 4
    jb    @Loop

    imul  ecx, [edx], HashSeed
    imul  eax, eax, HashSeed

    mov   ebx, ecx
    shr   ebx, R
    xor   ecx, ebx
    imul  ecx, ecx, HashSeed

    xor   eax, ecx

    imul  ecx, [edx + 4], HashSeed
    imul  eax, eax, HashSeed

    mov   ebx, ecx
    shr   ebx, R
    xor   ecx, ebx
    imul  ecx, ecx, HashSeed

    xor   eax, ecx

    imul  ecx, [edx + 8], HashSeed
    imul  eax, eax, HashSeed

    mov   ebx, ecx
    shr   ebx, R
    xor   ecx, ebx
    imul  ecx, ecx, HashSeed

    xor   eax, ecx

    imul  ecx, [edx + 12], HashSeed
    imul  eax, eax, HashSeed

    mov   ebx, ecx
    shr   ebx, R
    xor   ecx, ebx
    imul  ecx, ecx, HashSeed

    xor   eax, ecx

    add   edx, 16
    sub   esi, 4
    jz    @Last3
    jmp   @LargeLoop

@Loop:
    imul  ecx, [edx], HashSeed
    add   edx, 4
    imul  eax, eax, HashSeed

    mov   ebx, ecx
    shr   ebx, R
    xor   ecx, ebx
    imul  ecx, ecx, HashSeed

    xor   eax, ecx

    dec   esi
    jnz   @Loop

{ Hash remaining 3 bytes }
@Last3:
    test  edi, edi
    jz    @Done
    dec   edi
    jz    @OneLeft
    dec   edi
    jz    @TwoLeft

    movzx ecx, byte ptr [edx + 2]
    shl   ecx, 16
    xor   eax, ecx

{ Hash remaining 2 bytes }
@TwoLeft:
    movzx ecx, word ptr [edx]
    xor   eax, ecx
    jmp   @Last3Done

{ Hash last byte }
@OneLeft:
    movzx ecx, byte ptr [edx]
    xor   eax, ecx

{ Tail hashing is done }
@Last3Done:
    imul  eax, eax, HashSeed

{ As it says }
@Done:
    mov   ecx, eax
    shr   ecx, 13
    xor   eax, ecx
    imul  eax, eax, HashSeed
    mov   ecx, eax
    shr   ecx, 15
    xor   eax, ecx

{ Restore stack & exit }
@Ret:
    pop   edi
    pop   esi
    pop   ebx
    ret
end;

end.
