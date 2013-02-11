(*
  Letterpress

  Helper unit for outlining support.

  Copyright 2010, Garnet
*)

unit SynEditCodeFolding;

interface

uses
	Types, Classes, SysUtils, Graphics,

  { SynEdit }
  SynUnicode;

type
	TSynEditFoldRange = class;
  TSynEditAllFoldRanges = class;
  TFoldRegions = class;

  TFoldRegionItemKind = (frikNormal, frikIndentation);

	TFoldRegionItem = class(TCollectionItem)
  private
    { General }
    fName: UnicodeString;         // Settings variable
    fKind: TFoldRegionItemKind;

    { Patterns }
    fOpen: UTF8String;            // Open lexem
    fClose: UTF8String;           // Close lexem

    { Scope }
    fOpenTokenKind: Integer;      // Region start valid only on this token
    fCloseTokenKind: Integer;     // Region end valid only on this token
    fOpenRng: Pointer;
    fCloseRng: Pointer;

    { Backreference matching support }
    fOpenBref: UTF8String;        // Open lexem to restore from close
    fCloseBref: UTF8String;       // Close lexem, image of fOpen
    fCloseBackreference: Boolean; // True if fClose contains backreference to fOpen
  public
    { General }
    property Name: UnicodeString read fName write fName;
    property Kind: TFoldRegionItemKind read fKind write fKind;

    { Patterns & scope }
    property Open: UTF8String read fOpen write fOpen;
    property Close: UTF8String read fClose write fClose;
    property OpenTokenKind: Integer read fOpenTokenKind write fOpenTokenKind;
    property CloseTokenKind: Integer read fCloseTokenKind write fCloseTokenKind;
    property OpenRng: Pointer read fOpenRng write fOpenRng;

    { Backreference matching support }
    property OpenBref: UTF8String read fOpenBref write fOpenBref;
    property CloseBref: UTF8String read fCloseBref write fCloseBref;
    property CloseBackreference: Boolean read fCloseBackreference
      write fCloseBackreference;
	end;

  TFoldRegions = class(TCollection)
  private
    function GetItem(Index: Integer): TFoldRegionItem;
  public
    function Add(
      AName: UnicodeString;
      AOpenRegEx, ACloseRegEx: UTF8String;
      AOpenTokenKind: Integer;
      ACloseTokenKind: Integer = -1;
      ABackreferenced: Boolean = False;
      AOpenBref: UTF8String = '';
      ACloseBref: UTF8String = ''): TFoldRegionItem;
    procedure Remove(Region: TFoldRegionItem);

  	property Items[Index: Integer]: TFoldRegionItem read GetItem; default;
  end;

	TSynEditFoldRanges = class(TPersistent)
  private
  	fRanges: TList;
    function GetSynEditFoldRange(Index: Integer): TSynEditFoldRange;
    function GetCount: Integer;
  public
  	constructor Create;
    destructor Destroy; override;

    procedure Add(FoldRange: TSynEditFoldRange);
    procedure Clear;

    property Count: Integer read GetCount;
    property FoldRanges[Index: Integer]: TSynEditFoldRange
    	read GetSynEditFoldRange; default;
    property Ranges: TList read fRanges;
  end;

  TSynEditAllFoldRanges = class(TSynEditFoldRanges)
  private
  	fAllRanges: TList;
    function GetAllCount: Integer;
    function GetAllFoldRange(Index: Integer): TSynEditFoldRange;
  public
  	constructor Create;
    destructor Destroy; override;

    procedure ClearAll;
    procedure Delete(Index: Integer); overload;
    procedure Delete(FoldRange: TSynEditFoldRange); overload;
    procedure AddFold(FoldRange: TSynEditFoldRange);

    procedure SetPCOfSubFoldRanges(AFoldRange: TSynEditFoldRange);
    procedure UpdateFoldRanges;
    procedure UpdateChildrenLevel(AParent: TSynEditFoldRange;
      AParentIndex: Integer = -1);

    property AllCount: Integer read GetAllCount;
    property AllFoldRanges[Index: Integer]: TSynEditFoldRange
    	read GetAllFoldRange; default;
    property AllRanges: TList read fAllRanges;
  end;

	TSynEditFoldRange = class
  private
  	fFromLine: Integer; // Beginning line
    fToLine: Integer;   // Ending line
    fLevel: Integer;    // Fold range level
    fParentCollapsed: Boolean;    // Is some parent collapsed?
    fAllFoldRanges: TSynEditAllFoldRanges; // TSynEditAllFoldRanges pointer
    fFoldRegion: TFoldRegionItem; // TFoldRegionItem pointer
  public
  	constructor Create(AParent: TSynEditAllFoldRanges);

    function GetLinesCollapsed: Integer;

    procedure MoveBy(LineCount: Integer);
    procedure Widen(LineCount: Integer);

    property FromLine: Integer read fFromLine write fFromLine;
    property ToLine: Integer read fToLine write fToLine;
    property Level: Integer read fLevel write fLevel;
    property LinesCollapsed: Integer read GetLinesCollapsed;
    property ParentCollapsed: Boolean read fParentCollapsed
    	write fParentCollapsed;
    property FoldRegion: TFoldRegionItem read fFoldRegion write fFoldRegion;
  end;

  TSynCodeFoldingChanges = (fcEnabled, fcRefresh, fcRescan);

  TCodeFoldingChangeEvent = procedure(Event: TSynCodeFoldingChanges) of object;

  TSynCodeFolding = class(TPersistent)
  private
    fHighlighterFoldRegions: Boolean;
    fCollapsedCodeHint: Boolean;
    fIndentGuides: Boolean;
    fShowCollapsedLine: Boolean;
    fCollapsedLineColor: TColor;
    fEnabled: Boolean;
    fHighlightIndentGuides: Boolean;
    fFolderBarColor: TColor;
    fFolderBarLinesColor: TColor;
    fFoldRegions: TFoldRegions;
    fCaseSensitive: Boolean;
    fOnChange: TCodeFoldingChangeEvent;

    procedure SetFolderBarColor(const Value: TColor);
    procedure SetFolderBarLinesColor(const Value: TColor);
    procedure SetEnabled(const Value: Boolean);
    procedure SetCollapsedCodeHint(const Value: Boolean);
    procedure SetCollapsedLineColor(const Value: TColor);
    procedure SetHighlighterFoldRegions(const Value: Boolean);
    procedure SetHighlightIndentGuides(const Value: Boolean);
    procedure SetIndentGuides(const Value: Boolean);
    procedure SetShowCollapsedLine(const Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property CaseSensitive: Boolean read fCaseSensitive write SetCaseSensitive;
    property CollapsedCodeHint: Boolean read fCollapsedCodeHint
    	write SetCollapsedCodeHint default True;
    property CollapsedLineColor: TColor read fCollapsedLineColor
    	write SetCollapsedLineColor default clDefault;
    property Enabled: Boolean read fEnabled write SetEnabled default False;
    property FoldRegions: TFoldRegions read fFoldRegions;
    property FolderBarColor: TColor read fFolderBarColor
    	write SetFolderBarColor default clDefault;
    property FolderBarLinesColor: TColor read fFolderBarLinesColor
    	write SetFolderBarLinesColor default clDefault;
    property HighlighterFoldRegions: Boolean read fHighlighterFoldRegions
    	write SetHighlighterFoldRegions default True;
    property HighlightIndentGuides: Boolean read fHighlightIndentGuides
    	write SetHighlightIndentGuides default True;
    property IndentGuides: Boolean read fIndentGuides write SetIndentGuides
    	default True;
    property ShowCollapsedLine: Boolean read fShowCollapsedLine
    	write SetShowCollapsedLine default True;
    property OnChange: TCodeFoldingChangeEvent read fOnChange write fOnChange;
  end;

implementation

uses
  Math, SynEditTextBuffer;

// -----------------------------------------------------------------------------

{ TSynEditAllFoldRanges }

// -----------------------------------------------------------------------------
// § Garnet
procedure TSynEditAllFoldRanges.AddFold(FoldRange: TSynEditFoldRange);
var
  I, Index: Integer;
begin
  { Because we want fold range list to be sorted, we need to find place
    where to put new fold range }
  Index := fAllRanges.Count;
  for I := 0 to fAllRanges.Count - 1 do
    if TSynEditFoldRange(fAllRanges.Items[I]).FromLine > FoldRange.FromLine then
      if I < Index then
        Index := I;

  { Do add }
  if Index = fAllRanges.Count then
    fAllRanges.Add(FoldRange)
  else
    fAllRanges.Insert(Index, FoldRange);
end;

procedure TSynEditAllFoldRanges.ClearAll;
var
  I: Integer;
begin
  if not Assigned(fAllRanges) then
    Exit;
  for I := 0 to fAllRanges.Count - 1 do
  begin
    TObject(fAllRanges[I]).Free;
    fAllRanges[I] := nil;
  end;
  fAllRanges.Clear;
end;

constructor TSynEditAllFoldRanges.Create;
begin
  inherited;
	fAllRanges := TList.Create;
end;

procedure TSynEditAllFoldRanges.Delete(Index: Integer);
begin
  TObject(fAllRanges[Index]).Free;
  fAllRanges[Index] := nil;
	fAllRanges.Delete(Index);
end;

procedure TSynEditAllFoldRanges.Delete(FoldRange: TSynEditFoldRange);
var
  I: Integer;
begin
  for I := 0 to fAllRanges.Count - 1 do
    if fAllRanges[I] = FoldRange then
    begin
      TObject(fAllRanges[I]).Free;
      fAllRanges[I] := nil;
      fAllRanges.Delete(I);
      Break;
    end;
end;

destructor TSynEditAllFoldRanges.Destroy;
var
  I: Integer;
begin
  if not Assigned(fAllRanges) then
    Exit;
  for I := 0 to fAllRanges.Count - 1 do
    TObject(fAllRanges[I]).Free;
  FreeAndNil(fAllRanges);
	inherited;
end;

function TSynEditAllFoldRanges.GetAllCount: Integer;
begin
	Result := fAllRanges.Count;
end;

function TSynEditAllFoldRanges.GetAllFoldRange(
  Index: Integer): TSynEditFoldRange;
begin
	Result := fAllRanges[Index];
end;

procedure TSynEditAllFoldRanges.SetPCOfSubFoldRanges(AFoldRange: TSynEditFoldRange);
var
	I: Integer;
  FoldRange: TSynEditFoldRange;
begin
  for I := 0 to AllCount - 1 do
  begin
    FoldRange := GetAllFoldRange(I);
    if FoldRange = AFoldRange then
      Continue;
    if FoldRange.FromLine > AFoldRange.ToLine then
      Break;
    if (FoldRange.FromLine > AFoldRange.FromLine) and (FoldRange.FromLine <> AFoldRange.ToLine) then
      FoldRange.ParentCollapsed := True;
  end;
end;

procedure TSynEditAllFoldRanges.UpdateFoldRanges;
var
  I: Integer;
  FoldRange: TSynEditFoldRange;
begin
  for I := 0 to AllCount - 1 do
  begin
    FoldRange := GetAllFoldRange(I);
    FoldRange.ParentCollapsed := False;
  end;
  for I := 0 to AllCount - 1 do
  begin
    FoldRange := GetAllFoldRange(I);
    if not FoldRange.ParentCollapsed then
      SetPCOfSubFoldRanges(FoldRange);
  end;
end;

procedure TSynEditAllFoldRanges.UpdateChildrenLevel(AParent: TSynEditFoldRange;
  AParentIndex: Integer = -1);
var
  I: Integer;
  FoldRange: TSynEditFoldRange;
begin
  { Find start }
  if AParent <> nil then
  begin
    if AParentIndex = -1 then
      AParentIndex := Self.fAllRanges.IndexOf(AParent);
    if AParentIndex = -1 then
      Exit;
  end;

  { Update levels }
  for I := AParentIndex + 1 to fAllRanges.Count - 1 do
  begin
    FoldRange := TSynEditFoldRange(fAllRanges[I]);
    if AParent <> nil then
    begin
      if FoldRange.FromLine > AParent.ToLine then
        Break;
      if FoldRange.Level = -1 then
      begin
        FoldRange.Level := AParent.Level + 1;
        UpdateChildrenLevel(FoldRange, I);
      end;
    end
    else if FoldRange.Level = -1 then
    begin
      FoldRange.Level := 0;
      UpdateChildrenLevel(FoldRange, I);
    end;
  end;
end;

{ TSynEditFoldRanges }

// -----------------------------------------------------------------------------

procedure TSynEditFoldRanges.Add(FoldRange: TSynEditFoldRange);
begin
	fRanges.Add(FoldRange);
end;

procedure TSynEditFoldRanges.Clear;
begin
	fRanges.Clear;
end;

constructor TSynEditFoldRanges.Create;
begin
	fRanges := TList.Create;
end;

destructor TSynEditFoldRanges.Destroy;
begin
  fRanges.Free;
  inherited;
end;

function TSynEditFoldRanges.GetCount: Integer;
begin
	Result := fRanges.Count;
end;

function TSynEditFoldRanges.GetSynEditFoldRange(
  Index: Integer): TSynEditFoldRange;
begin
	Result := fRanges[Index];
end;

{ TSynEditFoldRange }

// -----------------------------------------------------------------------------

function TSynEditFoldRange.GetLinesCollapsed: Integer;
begin
  if fToLine - fFromLine - 2 < 0 then
    Result := 0
  else
    Result := fToLine - fFromLine - 1;
end;

constructor TSynEditFoldRange.Create(AParent: TSynEditAllFoldRanges);
begin
  fAllFoldRanges := AParent;
  fLevel := -1;
end;

procedure TSynEditFoldRange.MoveBy(LineCount: Integer);
begin
	Inc(fFromLine, LineCount);
  Inc(fToLine, LineCount);
end;

procedure TSynEditFoldRange.Widen(LineCount: Integer);
begin
	Inc(fToLine, LineCount);
end;

{ TFoldRegions }

// -----------------------------------------------------------------------------

function TFoldRegions.Add(
  AName: UnicodeString;
  AOpenRegEx, ACloseRegEx: UTF8String;
  AOpenTokenKind: Integer;
  ACloseTokenKind: Integer = -1;
  ABackreferenced: Boolean = False;
  AOpenBref: UTF8String = '';
  ACloseBref: UTF8String = ''): TFoldRegionItem;
begin
	Result := TFoldRegionItem(inherited Add);
  with Result do
  begin
    fName := AName;
    fKind := frikNormal;
    fOpen := AOpenRegEx;
    fClose := ACloseRegEx;
    fOpenTokenKind := AOpenTokenKind;
    if ACloseTokenKind = -1 then
      fCloseTokenKind := AOpenTokenKind
    else
      fCloseTokenKind := ACloseTokenKind;
    fCloseBackreference := ABackreferenced;
    fOpenBref := AOpenBref;
    fCloseBref := ACloseBref;
  end;
end;

procedure TFoldRegions.Remove(Region: TFoldRegionItem);
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
    if GetItem(I) = Region then
    begin
      Delete(I);
      Break;
    end;
end;

function TFoldRegions.GetItem(Index: Integer): TFoldRegionItem;
begin
	Result := TFoldRegionItem(inherited Items[Index]);
end;

// -----------------------------------------------------------------------------

{ TSynCodeFolding }

// -----------------------------------------------------------------------------

procedure TSynCodeFolding.Assign(Source: TPersistent);
begin
  if Source is TSynCodeFolding then
  begin
    with TSynCodeFolding(Source) do
    begin
      Self.fCaseSensitive := fCaseSensitive;
      Self.fCollapsedCodeHint := fCollapsedCodeHint;
      Self.fCollapsedLineColor := fCollapsedLineColor;
      Self.fEnabled := fEnabled;
      Self.fFoldRegions.Assign(fFoldRegions);
      Self.fFolderBarColor := fFolderBarColor;
      Self.fFolderBarLinesColor := fFolderBarLinesColor;
      Self.fHighlighterFoldRegions:= fHighlighterFoldRegions;
      Self.fHighlightIndentGuides := fHighlightIndentGuides;
      Self.fIndentGuides := fIndentGuides;
      Self.fShowCollapsedLine := fShowCollapsedLine;
      if Assigned(Self.OnChange) then
        self.OnChange(fcRescan);
    end;
  end
  else
    inherited;
end;

constructor TSynCodeFolding.Create;
begin
  inherited;
  fCollapsedCodeHint := True;
  fCollapsedLineColor := clDefault;
  fEnabled := False;
  fFolderBarColor := clDefault;
  FolderBarLinesColor := clDefault;
 	fHighlighterFoldRegions := True;
  fHighlightIndentGuides := True;
  fShowCollapsedLine := True;
  fIndentGuides := True;
  fFoldRegions := TFoldRegions.Create(TFoldRegionItem);
end;

destructor TSynCodeFolding.Destroy;
begin
  fFoldRegions.Free;
  inherited;
end;

procedure TSynCodeFolding.SetEnabled(const Value: Boolean);
begin
	fEnabled := Value;

  if Assigned(fOnChange) then fOnChange(fcEnabled);
end;

procedure TSynCodeFolding.SetFolderBarColor(const Value: TColor);
begin
	fFolderBarColor := Value;

  if Assigned(fOnChange) then fOnChange(fcRefresh);
end;

procedure TSynCodeFolding.SetFolderBarLinesColor(const Value: TColor);
begin
	fFolderBarLinesColor := Value;

  if Assigned(fOnChange) then fOnChange(fcRefresh);
end;

procedure TSynCodeFolding.SetCollapsedCodeHint(const Value: Boolean);
begin
	fCollapsedCodeHint := Value;

  if Assigned(fOnChange) then fOnChange(fcRefresh);
end;

procedure TSynCodeFolding.SetCollapsedLineColor(const Value: TColor);
begin
	fCollapsedLineColor := Value;

  if Assigned(fOnChange) then fOnChange(fcRefresh);
end;

procedure TSynCodeFolding.SetHighlighterFoldRegions(const Value: Boolean);
begin
	fHighlighterFoldRegions := Value;

  if Assigned(fOnChange) then fOnChange(fcRescan);
end;

procedure TSynCodeFolding.SetHighlightIndentGuides(const Value: Boolean);
begin
	fHighlightIndentGuides := Value;

  if Assigned(fOnChange) then fOnChange(fcRefresh);
end;

procedure TSynCodeFolding.SetIndentGuides(const Value: Boolean);
begin
	fIndentGuides := Value;

  if Assigned(fOnChange) then fOnChange(fcRefresh);
end;

procedure TSynCodeFolding.SetShowCollapsedLine(const Value: Boolean);
begin
	fShowCollapsedLine := Value;

  if Assigned(fOnChange) then fOnChange(fcRefresh);
end;

procedure TSynCodeFolding.SetCaseSensitive(const Value: Boolean);
begin
	fCaseSensitive := Value;

  if Assigned(fOnChange) then fOnChange(fcRescan);
end;

end.
