{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditTextBuffer.pas, released 2000-04-07.
The Original Code is based on parts of mwCustomEdit.pas by Martin Waldenburg,
part of the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
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

$Id: SynEditTextBuffer.pas,v 1.63.2.13 2008/09/14 16:24:59 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditTextBuffer;

{$I SynEdit.inc}

interface

uses
  Windows, Math, Dialogs, Classes, SysUtils, Graphics,

  { SynEdit }
  SynEditTypes, SynEditMiscProcs, SynUnicode;

const
  STRING_REC_HASTABS   = 0; // Line has been detected to have at least one tab
  STRING_REC_HASNOTABS = 1; // Line has been detected 100% not to have any tabs
  STRING_REC_EXLENUNK  = 2; // EXpanded LENgth UNKnown. (I know it sounds like a stupid nickname from retarted RPG)
  STRING_REC_MODIFIED  = 3; // Line has been changed since last save
  STRING_REC_SAVED     = 4; // Line has been changed and that changes are now saved
  STRING_REC_PARSED    = 5; // Highlighter has set range for this line

type
  TSynEditRange = Pointer;

  { § Garnet:
    Several changes here. Record made to be as small as possible.
    Currently it's 17 bytes. Although set would require single byte,
    it was removed because line state flags are more convenient to manage
    like that }
  PSynEditStringRec = ^TSynEditStringRec;
  TSynEditStringRec = packed record
    fString: UnicodeString;
    fRange: TSynEditRange;
    fAnalyzis: PBArray;
    fExpandedLength: Integer;
    fFlags: Byte;
  end;

const
  SynEditStringRecSize = SizeOf(TSynEditStringRec);
  MaxSynEditStrings = MaxInt div SynEditStringRecSize;

  NullRange = TSynEditRange(-1);

type
  PSynEditStringRecList = ^TSynEditStringRecList;
  TSynEditStringRecList = array[0..MaxSynEditStrings - 1] of TSynEditStringRec;

  TStringListChangeEvent = procedure (Sender: TObject; Index: Integer;
    Count: Integer) of object;

  TSynEditFileFormat = (sffDos, sffUnix, sffMac, sffUnicode);

  TSynEditStringList = class
  private
    fOwner: TObject;
    fCount: Integer;
    fCapacity: Integer;
    fFileFormat: TSynEditFileFormat;
    fConvertTabsProc: TConvertTabsProcEx;
    fIndexOfLongestLine: Integer;
    fLengthOfLongestLine: Integer;
    fTabWidth: Integer;
    fOnChange: TNotifyEvent;
    fOnChanging: TNotifyEvent;
    fOnCleared: TNotifyEvent;
    fOnDeleted: TStringListChangeEvent;
    fOnInserted: TStringListChangeEvent;
    fOnBeforePutted: TStringListChangeEvent;
    fOnPutted: TStringListChangeEvent;
    fLongestLineNeedsUpdate: Boolean;
    fUpdateCount: Integer;
    procedure AnalyzeString(Index: Integer);
    function GetAnalyzis(Index: Integer): PBArray;
    function GetExpandedStringLength(Index: Integer): Integer;
    function GetRange(Index: integer): TSynEditRange;
    procedure InsertItem(Index: integer; const S: UnicodeString);
    procedure PutRange(Index: integer; ARange: TSynEditRange);
    procedure SetFileFormat(const Value: TSynEditFileFormat);
    procedure SetLineModified(AIndex: Integer; AValue: Boolean);
    procedure Grow;
  protected
    fStreaming: Boolean;
    function Get(Index: Integer): UnicodeString;
    function GetCapacity: integer;
    function GetCount: integer;
    function GetTextStr: UnicodeString;
    procedure Put(Index: integer; const S: UnicodeString);
    procedure PutObject(Index: integer; AObject: TObject);
    procedure SetCapacity(NewCapacity: integer);
    procedure SetTabWidth(Value: integer);
    procedure SetUpdateState(Updating: Boolean);
  public
    fList: PSynEditStringRecList;

    constructor Create(AOwner: TObject);
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream; Encoding: TEncoding = nil);
    procedure SaveToStream(Stream: TStream; Encoding: TEncoding = nil);

    procedure BeginUpdate;
    procedure EndUpdate;

    function GetLengthOfLongestLine(AStart, AEnd: Integer): Integer;
    function GetIsLineWhitespaceOnly(AIndex: Integer): Boolean;

    function GetLineFlag(AIndex: Integer; AParam: Byte): Boolean;
    function GetLineStates(AFirst, ALast: Integer): TLineStates;
    procedure SetLineFlag(AIndex: Integer; AParam: Byte; AState: Boolean);
    procedure SetLineStates(AIndex: Integer; AStates: Byte);

    function Add(const S: UnicodeString): Integer;

    { Direct buffer access }
    function AccessBuffer(Index: Integer): PChar;
    function AccessStringLength(Index: Integer): Integer;

    procedure AddStrings(Strings: TStrings; AStart: Integer = -1; AEnd: Integer = -1);
    procedure FreeAnalyze(Index, Count: Integer);
    procedure Clear;
    procedure Delete(Index: integer);
    procedure DeleteLines(Index, NumLines: integer);
    procedure Insert(Index: integer; const S: UnicodeString);
    procedure InsertLines(Index, NumLines: integer; Strings: TStrings = nil; AStart: Integer = -1; AEnd: Integer = -1);
    procedure InsertStrings(Index: integer; NewStrings: TStrings);
    procedure InsertText(Index: integer; NewText: UnicodeString);
    procedure SetTextStr(const Value: UnicodeString);
    procedure FontChanged;

    procedure Assign(Source: TObject);

    property Strings[Index: Integer]: UnicodeString read Get write Put; default;
    property Analyzis[Index: Integer]: PBArray read GetAnalyzis;
    property Text: string read GetTextStr write SetTextStr;
    property ModifiedLines[Index: Integer]: Boolean write SetLineModified;
    property FileFormat: TSynEditFileFormat read fFileFormat write SetFileFormat;
    property ExpandedStringLengths[Index: Integer]: Integer read GetExpandedStringLength;
    property Ranges[Index: integer]: TSynEditRange read GetRange write PutRange;
    property TabWidth: integer read fTabWidth write SetTabWidth;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnChanging: TNotifyEvent read fOnChanging write fOnChanging;
    property OnCleared: TNotifyEvent read fOnCleared write fOnCleared;
    property OnDeleted: TStringListChangeEvent read fOnDeleted write fOnDeleted;
    property OnInserted: TStringListChangeEvent read fOnInserted
      write fOnInserted;
    property OnBeforePutted: TStringListChangeEvent read fOnBeforePutted write fOnBeforePutted;
    property OnPutted: TStringListChangeEvent read fOnPutted write fOnPutted;
    property Owner: TObject read fOwner write fOwner;
    property Count: Integer read fCount;
  end;

  ESynEditStringList = class(Exception);

  { Several undo entries can be chained together via the ChangeNumber
    see also TCustomSynEdit.[Begin|End]UndoBlock methods }
  TSynChangeReason = (
    crInsert,
    crPaste,
    crDragDropInsert,
    crDeleteAfterCursor,
    crDelete,
    crLineBreak,                // User hit Enter while on line (not on start)

    crLineInsert,               // Same as above but to avoid unnecessary (here)
                                // AChangeStr allocation. Used when inserting
                                // blank line during line break
    crIndent,
    crUnindent,
    crSilentDelete,
    crSilentDeleteAfterCursor,
    crAutoCompleteBegin,
    crAutoCompleteEnd,
    //crPasteBegin,               // For pasting, since it
    //crPasteEnd,                 // might do a lot of operations
    //crSpecial1Begin,
    //crSpecial1End,
    //crSpecial2Begin,
    //crSpecial2End,
    crCaret,                    // Just restore the Caret, allowing better Undo behavior
    crSelection,                // Restore selection
    crNothing,
    crGroupBreak,
    crDeleteAll,                // Document was cleared entirely
    crWhiteSpaceAdd             // Whitespace have been added before the caret
  );

  TSynEditUndoItem = class(TPersistent)
  private
    fChangeReason: TSynChangeReason;
    fChangeSelMode: TSynSelectionMode;
    fChangeStartPos: TBufferCoord;
    fChangeEndPos: TBufferCoord;
    fChangeCaret: Integer;
    fChangeStr: UnicodeString;
    fChangeStrStates: TLineStates;
    fChangeNumber: Integer;
    //fChangeData: Pointer;
    fChangeIndex: Integer;
  public
    procedure Assign(Source: TPersistent); override;
    procedure ClearLineStates;
    property ChangeReason: TSynChangeReason read fChangeReason;
    property ChangeSelMode: TSynSelectionMode read fChangeSelMode;
    property ChangeStartPos: TBufferCoord read fChangeStartPos write fChangeStartPos;
    property ChangeEndPos: TBufferCoord read fChangeEndPos write fChangeEndPos;
    property ChangeCaret: Integer read fChangeCaret write fChangeCaret;
    property ChangeStr: UnicodeString read fChangeStr write fChangeStr;
    property ChangeStrStates: TLineStates read fChangeStrStates;
    property ChangeNumber: integer read fChangeNumber;
    //property ChangeData: Pointer read fChangeData;
    property ChangeIndex: Integer read fChangeIndex;
  end;

  TSynEditUndoList = class(TPersistent)
  protected
    fBlockChangeNumber: integer;
    fBlockCount: integer;
    fFullUndoImposible: boolean;
    fItems: TList;
    fLockCount: integer;
    fMaxUndoActions: integer;
    fNextChangeNumber: integer;
    fInitialChangeNumber: integer;
    fInsideRedo: boolean;
    fOnAddedUndo: TNotifyEvent;
    procedure EnsureMaxEntries;
    function GetCanUndo: boolean;
    function GetItemCount: Integer;
    procedure SetMaxUndoActions(Value: integer);
    procedure SetInitialState(const Value: boolean);
    function GetInitialState: boolean;
    function GetItems(Index: Integer): TSynEditUndoItem;
    procedure SetItems(Index: Integer; const Value: TSynEditUndoItem);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddChange(AReason: TSynChangeReason; const AStart,
      AEnd: TBufferCoord; ACaret: Integer; const AChangeText: UnicodeString;
      ASelMode: TSynSelectionMode; const ALineStates: TLineStates = nil);

    procedure BeginBlock;
    procedure Clear;
    procedure ClearLineStates;
    procedure EndBlock;
    procedure Lock;
    function PeekItem: TSynEditUndoItem;
    function PopItem: TSynEditUndoItem;
    procedure PushItem(Item: TSynEditUndoItem);
    procedure Unlock;
    function LastChangeReason: TSynChangeReason;
  public
    procedure Assign(Source: TPersistent); override;
    procedure AddGroupBreak;
    procedure DeleteItem(AIndex: Integer);
    property BlockChangeNumber: integer read fBlockChangeNumber
      write fBlockChangeNumber;
    property CanUndo: boolean read GetCanUndo;
    property FullUndoImpossible: boolean read fFullUndoImposible;
    property InitialState: boolean read GetInitialState write SetInitialState;
    property Items[Index: Integer]: TSynEditUndoItem read GetItems write SetItems;
    property ItemCount: integer read GetItemCount;
    property BlockCount: integer read fBlockCount;
    property MaxUndoActions: integer read fMaxUndoActions
      write SetMaxUndoActions;
    property InsideRedo: boolean read fInsideRedo write fInsideRedo;
    property OnAddedUndo: TNotifyEvent read fOnAddedUndo write fOnAddedUndo;
  end;

implementation

uses
  SynEdit;

const
  SListIndexOutOfBounds = 'Invalid stringlist index %d';
  SInvalidCapacity = 'Stringlist capacity cannot be smaller than count';

// -----------------------------------------------------------------------------

{ TSynEditStringList }

// -----------------------------------------------------------------------------

procedure ListIndexOutOfBounds(Index: integer);
begin
  raise ESynEditStringList.CreateFmt(SListIndexOutOfBounds, [Index]);
end;

constructor TSynEditStringList.Create(AOwner: TObject);
begin
  fCount := 0;
  fOwner := AOwner;
  fUpdateCount := 0;
  fIndexOfLongestLine := -1;
  fLengthOfLongestLine := 0;
  fLongestLineNeedsUpdate := False;
  SetFileFormat(sffDos);
  TabWidth := 4;
  Add(EmptyStr);
end;

destructor TSynEditStringList.Destroy;
begin
  fOnChange := nil;
  fOnChanging := nil;
  if fCount > 0 then
  begin
    FreeAnalyze(0, fCount);
    Finalize(fList^[0], fCount);
  end;
  fCount := 0;
  SetCapacity(0);
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

function TSynEditStringList.Add(const S: UnicodeString): Integer;
begin
  Result := fCount;
  InsertItem(Result, S);
  if Assigned(OnInserted) and (fUpdateCount = 0) then
    OnInserted(Self, Result, 1);
end;

//------------------------------------------------------------------------------
// § Garnet
// Analyzes SynEdit text buffer from AStart line to AEnd line and checks
// if there is line longer than currently set one. Note that the AStart and
// AEnd parameters are NOT index-based
function TSynEditStringList.GetLengthOfLongestLine(AStart,
  AEnd: Integer): Integer;
var
  I: Integer;
  Rec: PSynEditStringRec;
begin
  if fStreaming then
  begin
    Result := 0;
    Exit;
  end;
  Dec(AStart); Dec(AEnd);
  AEnd := Min(AEnd, fCount - 1);
  for I := AStart to AEnd do
  begin
    Rec := @fList^[I];
    if GetBitState(Rec^.fFlags, STRING_REC_EXLENUNK) then
      AnalyzeString(I);
    if Rec^.fExpandedLength > fLengthOfLongestLine then
    begin
      fIndexOfLongestLine := I;
      fLengthOfLongestLine := Rec^.fExpandedLength;
    end;
  end;
  Result := fLengthOfLongestLine;
end;

function TSynEditStringList.GetIsLineWhitespaceOnly(AIndex: Integer): Boolean;
var
  I, Len: Integer;
begin
  Result := True;
  if (AIndex < 0) or (AIndex > fCount - 1) then
    Exit;
  with fList^[AIndex] do
  begin
    Len := Length(fString);
    if Len = 0 then
      Exit;
    I := 1;
    while I <= Len do
    begin
      if fString[I] > #33 then
      begin
        Result := False;
        Break;
      end;
      Inc(I);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TSynEditStringList.AccessBuffer(Index: Integer): PChar;
begin
  Result := nil;
  if (Index < 0) or (Index > fCount - 1) then
    Exit;
  Result := PChar(fList^[Index].fString);
end;

function TSynEditStringList.AccessStringLength(Index: Integer): Integer;
begin
  Result := 0;
  if (Index < 0) or (Index > fCount - 1) then
    Exit;
  Result := Length(fList^[Index].fString);
end;

// § Garnet
//------------------------------------------------------------------------------
procedure TSynEditStringList.AddStrings(Strings: TStrings; AStart: Integer = -1;
  AEnd: Integer = -1);
var
  I, FirstAdded: integer;
begin
  if AStart = -1 then
    AStart := 0;
  if AEnd = -1 then
    AEnd := Strings.Count;
  if Strings.Count > 0 then
  begin
    BeginUpdate;
    try
      I := fCount + Strings.Count;
      if I > fCapacity then
        SetCapacity((I + 15) and (not 15));
      FirstAdded := fCount;
      for I := AStart to AEnd do
      begin
        with fList^[fCount] do
        begin
          Pointer(fString) := nil;
          fString := Strings[I];
          fRange := NullRange;
          fExpandedLength := -1;
          fFlags := 0;
          SetBitState(fFlags, STRING_REC_EXLENUNK, 1);
        end;
        Inc(fCount);
      end;
      if Assigned(OnInserted) then
        OnInserted(Self, FirstAdded, Strings.Count);
    finally
      EndUpdate;
    end;

    { Longest line }
    if fOwner <> nil then
      TSynEdit(fOwner).UpdateLongestLine;
  end;
end;

procedure TSynEditStringList.FreeAnalyze(Index, Count: Integer);
begin
  for Index := Index to Pred(Count) do
  begin
    SetLength(fList^[Index].fAnalyzis^, 0);
    Dispose(fList^[Index].fAnalyzis);
  end;
end;

procedure TSynEditStringList.Clear;
begin
  if fCount > 0 then
  begin
    BeginUpdate;
    try
      FreeAnalyze(0, fCount);
      Finalize(fList^[0], fCount);
      fCount := 0;
      SetCapacity(0);
    if Assigned(fOnCleared) then
      fOnCleared(Self);
    finally
      EndUpdate;
    end;
  end;

  { Clear information about longest line }
  fIndexOfLongestLine := -1;
  fLengthOfLongestLine := 0;
end;

procedure TSynEditStringList.Delete(Index: integer);
begin
  if (Index < 0) or (Index > fCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  try
    FreeAnalyze(Index, Index + 1);
    Finalize(fList^[Index]);
    Dec(fCount);
    if Index < fCount then
      System.Move(fList^[Index + 1], fList^[Index],
        (fCount - Index) * SynEditStringRecSize);
  finally
    EndUpdate;
  end;

  { Longest }
  if fIndexOfLongestLine = Index then
  begin
    fLengthOfLongestLine := 0;
    if fUpdateCount = 0 then
      if fOwner <> nil then
        TSynEdit(fOwner).UpdateLongestLine
      else
    else
      fLongestLineNeedsUpdate := True;
  end;

  if Assigned(fOnDeleted) then
    fOnDeleted(Self, Index, 1);
end;

procedure TSynEditStringList.DeleteLines(Index, NumLines: Integer);
var
  LinesAfter: integer;
begin
  if NumLines > 0 then
  begin
    if (Index < 0) or (Index > fCount) then
      ListIndexOutOfBounds(Index);
    LinesAfter := fCount - (Index + NumLines - 1);
    if LinesAfter < 0 then
      NumLines := fCount - Index - 1;
    FreeAnalyze(Index, Index + NumLines);
    Finalize(fList^[Index], NumLines);

    if LinesAfter > 0 then
    begin
      BeginUpdate;
      try
        System.Move(fList^[Index + NumLines], fList^[Index],
          LinesAfter * SynEditStringRecSize);
      finally
        EndUpdate;
      end;
    end;
    Dec(fCount, NumLines);

    { Were longest line deleted? }
    if (fIndexOfLongestLine >= Index) and
      (fIndexOfLongestLine <= Index + NumLines - 1) then
    begin
      fLengthOfLongestLine := 0;
      if fUpdateCount = 0 then
        if fOwner <> nil then
          TSynEdit(fOwner).UpdateLongestLine
        else
      else
        fLongestLineNeedsUpdate := True;
    end;

    if Assigned(fOnDeleted) then
      fOnDeleted( Self, Index, NumLines);
  end;
end;

procedure TSynEditStringList.AnalyzeString(Index: Integer);
var
  I: Integer;
begin
  fList^[Index].fExpandedLength := 0;
  SetLength(fList^[Index].fAnalyzis^, Length(fList^[Index].fString));
  for I := 1 to Length(fList^[Index].fAnalyzis^) do
  begin
    if fList^[Index].fString[I] = #9 then
      fList^[Index].fAnalyzis^[I - 1] := fTabWidth - (fList^[Index].fExpandedLength mod fTabWidth)
    else
      fList^[Index].fAnalyzis^[I - 1] := CharWidthTable(fList^[Index].fString[I]);
    Inc(fList^[Index].fExpandedLength, fList^[Index].fAnalyzis^[I - 1]);
  end;
  SetBitState(fList^[Index].fFlags, STRING_REC_EXLENUNK, 0);
end;

function TSynEditStringList.GetLineFlag(AIndex: Integer; AParam: Byte): Boolean;
begin
  Result := GetBitState(fList^[AIndex].fFlags, AParam);
end;

function TSynEditStringList.GetLineStates(AFirst, ALast: Integer): TLineStates;
var
  I: Integer;
begin
  if AFirst > ALast then
    SwapInt(AFirst, ALast);
  I := 0;
  SetLength(Result, ALast - AFirst + 1);
  while AFirst <= ALast do
  begin
    SetBitState(Result[I], STRING_REC_MODIFIED,
      Byte(GetBitState(fList[AFirst].fFlags, STRING_REC_MODIFIED)));
    SetBitState(Result[I], STRING_REC_SAVED,
      Byte(GetBitState(fList[AFirst].fFlags, STRING_REC_SAVED)));
    Inc(AFirst);
    Inc(I);
  end;
end;

// -----------------------------------------------------------------------------

procedure TSynEditStringList.SetLineFlag(AIndex: Integer; AParam: Byte;
  AState: Boolean);
begin
  if AState then
    SetBitState(fList^[AIndex].fFlags, AParam, 1)
  else
    SetBitState(fList^[AIndex].fFlags, AParam, 0);
end;

procedure TSynEditStringList.SetLineStates(AIndex: Integer; AStates: Byte);
begin
  SetBitState(fList^[AIndex].fFlags, STRING_REC_MODIFIED,
    Byte(GetBitState(AStates, STRING_REC_MODIFIED)));
  SetBitState(fList^[AIndex].fFlags, STRING_REC_SAVED,
    Byte(GetBitState(AStates, STRING_REC_SAVED)));
end;

function TSynEditStringList.Get(Index: integer): UnicodeString;
begin
  if (Index > -1) and (Index < fCount) then
    Result := fList^[Index].fString
  else
    Result := '';
end;

function TSynEditStringList.GetCapacity: integer;
begin
  Result := fCapacity;
end;

function TSynEditStringList.GetCount: integer;
begin
  Result := fCount;
end;

function TSynEditStringList.GetAnalyzis(Index: Integer): PBArray;
begin
  if (Index < 0) or (Index >= fCount) then
  begin
    Result := nil;
    Exit;
  end;
  if GetBitState(fList^[Index].fFlags, STRING_REC_EXLENUNK) then
    AnalyzeString(Index);
  Result := fList^[Index].fAnalyzis;
end;

function TSynEditStringList.GetExpandedStringLength(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < fCount) then
  begin
    if GetBitState(fList^[Index].fFlags, STRING_REC_EXLENUNK) then
      AnalyzeString(Index);
    Result := fList^[Index].fExpandedLength;
  end
  else
    Result := 0;
end;

function TSynEditStringList.GetRange(Index: integer): TSynEditRange;
begin
  if (Index >= 0) and (Index < fCount) then
    Result := fList^[Index].fRange
  else
    Result := nil;
end;

function TSynEditStringList.GetTextStr: UnicodeString;
var
  I, L, Size, Count: Integer;
  P: PChar;
  S, LB: string;
begin
  Count := GetCount;
  Size := 0;
  case FfileFormat of
    sffDos: LB := WideCRLF;
    sffUnix: LB := WideLF;
    sffMac: LB := WideCR;
    sffUnicode: LB := WideLineSeparator;
  end;
  for I := 0 to Count - 1 do Inc(Size, Length(Get(I)) + Length(LB));
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L * SizeOf(Char));
      Inc(P, L);
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      System.Move(Pointer(LB)^, P^, L * SizeOf(Char));
      Inc(P, L);
    end;
  end;
end;

procedure TSynEditStringList.Grow;
var
  Delta: Integer;
begin
  if fCapacity > 64 then
    Delta := fCapacity div 4
  else
    Delta := 16;
  SetCapacity(fCapacity + Delta);
end;

procedure TSynEditStringList.Insert(Index: integer; const S: UnicodeString);
begin
  if (Index < 0) or (Index > fCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  InsertItem(Index, S);
  if Assigned(fOnInserted) then
    fOnInserted( Self, Index, 1 );
  EndUpdate;
end;

// -----------------------------------------------------------------------------
// § Garnet
procedure TSynEditStringList.InsertItem(Index: Integer; const S: UnicodeString);
begin
  if fCount = fCapacity then
    Grow;

  if Index < fCount then
    System.Move(fList^[Index], fList^[Index + 1],
      (fCount - Index) * SynEditStringRecSize);

  with fList^[Index] do
  begin
    Pointer(fString) := nil;
    fString := S;
    fRange := NullRange;
    fExpandedLength := -1;
    fFlags := 0;
    New(fAnalyzis);
    SetBitState(fFlags, STRING_REC_EXLENUNK, 1);
  end;
  Inc(fCount);
end;

// -----------------------------------------------------------------------------
// § Garnet
procedure TSynEditStringList.InsertLines(Index, NumLines: Integer;
  Strings: TStrings = nil; AStart: Integer = -1; AEnd: Integer = -1);
var
  i, c_Line: Integer;
begin
  if (Index < 0) or (Index > fCount) then
    ListIndexOutOfBounds(Index);
  if NumLines > 0 then
  begin
    BeginUpdate;
    try
      SetCapacity(fCount + NumLines);
      if Index < fCount then
      begin
        System.Move(fList^[Index], fList^[Index + NumLines],
          (fCount - Index) * SynEditStringRecSize);
      end;
      i := 0;
      for c_Line := Index to Index + NumLines -1 do
        with fList^[c_Line] do
        begin
          Pointer(fString) := nil;
          if Assigned(Strings) then
            fString := Strings[AStart+i];
          inc(i);
          fRange := NullRange;
          fExpandedLength := -1;
          fFlags := 0;
          New(fAnalyzis);
          SetBitState(fFlags, STRING_REC_EXLENUNK, 1);
        end;
      Inc(fCount, NumLines);
    finally
      EndUpdate;
    end;

    if not fStreaming then
      if fUpdateCount = 0 then
        if fOwner <> nil then
          TSynEdit(fOwner).UpdateLongestLine
        else
      else
        fLongestLineNeedsUpdate := True;

    if Assigned(OnInserted) then
        OnInserted(Self, Index, NumLines);
  end;
end;

procedure TSynEditStringList.InsertStrings(Index: integer;
  NewStrings: TStrings);
var
  Cnt: integer;
begin
  Cnt := NewStrings.Count;
  if Cnt = 0 then exit;

  BeginUpdate;
  try
    InsertLines(Index, Cnt);
  finally
    EndUpdate;
  end;
end;

procedure TSynEditStringList.InsertText(Index: integer;
  NewText: UnicodeString);
var
  TmpStringList: TStringList;
begin
  if NewText = '' then exit;

  TmpStringList := TStringList.Create;
  try
    TmpStringList.Text := NewText;
    InsertStrings(Index, TmpStringList);
  finally
    TmpStringList.Free;
  end;
end;

procedure TSynEditStringList.LoadFromStream(Stream: TStream;
  Encoding: TEncoding = nil);
var
  Size: Integer;
  Buffer: TBytes;
  StrBuffer: UnicodeString;
begin
  fStreaming := True;

  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    if Encoding <> nil then
    begin
      SetLength(Buffer, Size);
      Stream.Read(Buffer[0], Size);
      Size := TEncoding.GetBufferEncoding(Buffer, Encoding);
      SetTextStr(Encoding.GetString(Buffer, Size, Length(Buffer) - Size));
    end
    else begin
      SetLength(StrBuffer, Size shr 1);
      Stream.ReadBuffer(StrBuffer[1], Size);
      SetTextStr(StrBuffer);
    end;
  finally
    EndUpdate;
  end;

  if Assigned(OnInserted) then
    OnInserted(Self, 0, fCount);

  fStreaming := False;
end;

procedure TSynEditStringList.SaveToStream(Stream: TStream; Encoding: TEncoding = nil);
var
  Buffer, Preamble: TBytes;
begin
  fStreaming := True;

  if Encoding = nil then
    Encoding := TEncoding.Default;
  Buffer := Encoding.GetBytes(GetTextStr);
  Preamble := Encoding.GetPreamble;
  if Length(Preamble) > 0 then
    Stream.WriteBuffer(Preamble[0], Length(Preamble));
  Stream.WriteBuffer(Buffer[0], Length(Buffer));

  fStreaming := False;
end;

// -----------------------------------------------------------------------------

procedure TSynEditStringList.BeginUpdate;
begin
  Inc(fUpdateCount);
end;

procedure TSynEditStringList.EndUpdate;
begin
  Dec(fUpdateCount);
  if fUpdateCount = 0 then
    if fLongestLineNeedsUpdate then
    begin
      fLongestLineNeedsUpdate := False;
      if fOwner <> nil then
        TSynEdit(fOwner).UpdateLongestLine;
    end;
end;

// -----------------------------------------------------------------------------
// § Garnet
procedure TSynEditStringList.Put(Index: integer; const S: UnicodeString);
begin
  if ((Index = 0) and (fCount = 0)) or (fCount = Index) then
    Add(S)
  else begin
    if (Index < 0) or (Index >= fCount) then
      ListIndexOutOfBounds(Index);
    if Assigned(OnBeforePutted) then
      OnBeforePutted(Self, Index, 1);
    with fList^[Index] do
    begin
      SetBitState(fFlags, STRING_REC_EXLENUNK, 1);
      SetBitState(fFlags, STRING_REC_HASTABS, 0);
      SetBitState(fFlags, STRING_REC_HASNOTABS, 0);
      fString := S;
    end;
    if Index = fIndexOfLongestLine then
    begin
      AnalyzeString(Index);
      fLengthOfLongestLine := fList^[Index].fExpandedLength;
    end
    else if fUpdateCount > 0 then
      fLongestLineNeedsUpdate := True
    else
      if fOwner <> nil then
        TSynEdit(fOwner).UpdateLongestLine;
    if Assigned(fOnPutted) then
      fOnPutted(Self, Index, 1);
  end;
end;

procedure TSynEditStringList.PutObject(Index: integer; AObject: TObject);
begin
  { Do nothing }
end;

procedure TSynEditStringList.PutRange(Index: integer; ARange: TSynEditRange);
begin
  if (Index < 0) or (Index >= fCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  fList^[Index].fRange := ARange;
  EndUpdate;
end;

procedure TSynEditStringList.SetCapacity(NewCapacity: integer);
begin
  if NewCapacity < Count then
    EListError.Create( SInvalidCapacity );
  ReallocMem(fList, NewCapacity * SynEditStringRecSize);
  fCapacity := NewCapacity;
end;

procedure TSynEditStringList.SetFileFormat(const Value: TSynEditFileFormat);
begin
  fFileFormat := Value;
end;

procedure TSynEditStringList.SetLineModified(AIndex: Integer; AValue: Boolean);
begin
  SetLineFlag(AIndex, STRING_REC_MODIFIED, AValue);
end;

// -----------------------------------------------------------------------------
// § Garnet
procedure TSynEditStringList.SetTabWidth(Value: Integer);
var
  I: Integer;
begin
  if Value <> fTabWidth then
  begin
    fTabWidth := Value;
    fConvertTabsProc := GetBestConvertTabsProcEx(fTabWidth);
    fLengthOfLongestLine := -1;
    for I := 0 to fCount - 1 do
      with fList^[I] do
      begin
        fExpandedLength := -1;
        SetBitState(fFlags, STRING_REC_HASNOTABS, 0);
        SetBitState(fFlags, STRING_REC_EXLENUNK, 1);
      end;
  end;
end;

procedure TSynEditStringList.SetTextStr(const Value: UnicodeString);
var
  Size, iPos: Integer;
  S: UnicodeString;
  P, Start: PChar;
  fCR, fLF, fLineSeparator, fFormatDetected: Boolean;
begin
  fLineSeparator := False;
  fCR := False;
  fLF := False;
  fFormatDetected := False;

  BeginUpdate;
  try
    Clear;
    Size := Length(Value);
    P := PChar(Value);
    if P <> nil then
    begin
      iPos := 0;
      while (iPos < Size) do
      begin
        Start := P;
        while (iPos < Size) and (not CharInSet(P^, [#10, #13])) and
          (P^ <> WideLineSeparator) do
        begin
          Inc(P);
          Inc(iPos);
        end;
        SetString(S, Start, P - Start);
        Add(S);
        if not fFormatDetected then
        begin
          if P^ = WideLineSeparator then
          begin
            fLineSeparator := True;
            Inc(P);
            Inc(iPos);
            fFormatDetected := True;
          end
          else if P^ = WideCR then
          begin
            fCR := True;
            Inc(P);
            Inc(iPos);

            { Check for Windows right here }
            if (iPos < Size) and (P^ = WideLF) then
            begin
              fLF := True;
              Inc(P);
              Inc(iPos);
            end;

            fFormatDetected := True;
          end
          else if P^ = WideLF then
          begin
            fLF := True;
            Inc(P);
            Inc(iPos);
            fFormatDetected := True;
          end;
        end
        else begin
          Inc(P);
          Inc(iPos);
          if (fCR and fLF) and (iPos < Size) and (P^ = WideLF) then
          begin
            Inc(P);
            Inc(iPos);
          end;
        end;
      end;
    end;
  finally
    EndUpdate;
  end;

  { Set LBR format }
  if fLineSeparator then
    fFileFormat := sffUnicode
  else if fCR and not fLF then
    fFileFormat := sffMac
  else if fLF and not fCR then
    fFileFormat := sffUnix
  else
    fFileFormat := sffDos;

  { Send changes to SynEdit }
  if (fUpdateCount = 0) and Assigned(fOnInserted) then
    fOnInserted(Self, 0, fCount);
end;

procedure TSynEditStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then
    if Assigned(fOnChanging) then
      fOnChanging(Self)
    else
  else
    if Assigned(fOnChange) then
      fOnChange(Self);
end;

procedure TSynEditStringList.FontChanged;
var
  I, iStart, iEnd: Integer;
begin
  iStart := 0;
  iEnd := fCount - 1;

  for I := iStart to iEnd do
    with fList^[I] do
    begin
      fExpandedLength := -1;
      SetBitState(fFlags, STRING_REC_HASNOTABS, 0);
      SetBitState(fFlags, STRING_REC_EXLENUNK, 1);
    end;

  { Longest line }
  if fIndexOfLongestLine > -1 then
  begin
    AnalyzeString(fIndexOfLongestLine);
    fLengthOfLongestLine := fList^[fIndexOfLongestLine].fExpandedLength;
  end;
end;

procedure TSynEditStringList.Assign(Source: TObject);
begin
  if Source is TSynEditStringList then
  begin
    BeginUpdate;
    try
      //Clear;
      //AddStrings(TStrings(Source));
    finally
      EndUpdate;
    end;
  end
  else if Source is TStrings then
  begin
    BeginUpdate;
    try
      //Clear;
      //AddStrings(TStrings(Source));
    finally
      EndUpdate;
    end;
  end;
end;

{ TSynEditUndoItem }

procedure TSynEditUndoItem.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if (Source is TSynEditUndoItem) then
  begin
    fChangeReason:=TSynEditUndoItem(Source).fChangeReason;
    fChangeSelMode:=TSynEditUndoItem(Source).fChangeSelMode;
    fChangeStartPos:=TSynEditUndoItem(Source).fChangeStartPos;
    fChangeEndPos:=TSynEditUndoItem(Source).fChangeEndPos;
    fChangeStr:=TSynEditUndoItem(Source).fChangeStr;
    SetLength(fChangeStrStates, Length(TSynEditUndoItem(Source).fChangeStrStates));
    for I := 0 to Length(fChangeStrStates) - 1 do
      fChangeStrStates[I] := TSynEditUndoItem(Source).fChangeStrStates[I];
    fChangeNumber:=TSynEditUndoItem(Source).fChangeNumber;
  end
  else
    inherited Assign(Source);
end;

procedure TSynEditUndoItem.ClearLineStates;
begin
  SetLength(fChangeStrStates, 0);
end;

// -----------------------------------------------------------------------------

{ TSynEditUndoList }

// -----------------------------------------------------------------------------

constructor TSynEditUndoList.Create;
begin
  inherited Create;
  fItems := TList.Create;
  fMaxUndoActions := 1024;
  fNextChangeNumber := 1;
  fInsideRedo := False;
end;

destructor TSynEditUndoList.Destroy;
begin
  Clear;
  fItems.Free;
  inherited Destroy;
end;

procedure TSynEditUndoList.Assign(Source: TPersistent);
var
  i: Integer;
  UndoItem: TSynEditUndoItem;
begin
  if (Source is TSynEditUndoList) then
  begin
    Clear;
    for i:=0 to TSynEditUndoList(Source).fItems.Count-1 do
    begin
      UndoItem:=TSynEditUndoItem.Create;
      UndoItem.Assign(TSynEditUndoList(Source).fItems[i]);
      fItems.Add(UndoItem);
    end;
    fBlockChangeNumber:=TSynEditUndoList(Source).fBlockChangeNumber;
    fBlockCount:=TSynEditUndoList(Source).fBlockCount;
    fFullUndoImposible:=TSynEditUndoList(Source).fFullUndoImposible;
    fLockCount:=TSynEditUndoList(Source).fLockCount;
    fMaxUndoActions:=TSynEditUndoList(Source).fMaxUndoActions;
    fNextChangeNumber:=TSynEditUndoList(Source).fNextChangeNumber;
    fInsideRedo:=TSynEditUndoList(Source).fInsideRedo;
  end
  else
    inherited Assign(Source);
end;

procedure TSynEditUndoList.AddChange(AReason: TSynChangeReason; const AStart,
  AEnd: TBufferCoord; ACaret: Integer; const AChangeText: UnicodeString;
  ASelMode: TSynSelectionMode; const ALineStates: TLineStates = nil);
var
  I: Integer;
  NewItem: TSynEditUndoItem;
begin
  if fLockCount = 0 then
  begin
    NewItem := TSynEditUndoItem.Create;
    try
      with NewItem do
      begin
        fChangeReason := AReason;
        fChangeSelMode := ASelMode;
        fChangeStartPos := AStart;
        fChangeEndPos := AEnd;
        fChangeStr := AChangeText;

        SetLength(fChangeStrStates, Length(ALineStates));
        for I := 0 to Length(ALineStates) - 1 do
          fChangeStrStates[I] := ALineStates[I];

        if fBlockChangeNumber <> 0 then
          fChangeNumber := fBlockChangeNumber
        else begin
          fChangeNumber := fNextChangeNumber;
          if fBlockCount = 0 then begin
            Inc(fNextChangeNumber);
            if fNextChangeNumber = 0 then
              Inc(fNextChangeNumber);
          end;
        end;
      end;
      PushItem(NewItem);
    except
      NewItem.Free;
      raise;
    end;
  end;
end;

procedure TSynEditUndoList.BeginBlock;
begin
  Inc(fBlockCount);
  fBlockChangeNumber := fNextChangeNumber;
end;

procedure TSynEditUndoList.Clear;
var
  i: integer;
begin
  for i := 0 to fItems.Count - 1 do
    TSynEditUndoItem(fItems[i]).Free;
  fItems.Clear;
  fFullUndoImposible := False;
end;

procedure TSynEditUndoList.ClearLineStates;
var
  I: integer;
begin
  for I := 0 to fItems.Count - 1 do
    TSynEditUndoItem(fItems[I]).ClearLineStates;
end;

procedure TSynEditUndoList.EndBlock;
var
  iBlockID: integer;
begin
  if fBlockCount > 0 then
  begin
    Dec(fBlockCount);
    if fBlockCount = 0 then
    begin
      iBlockID := fBlockChangeNumber;
      fBlockChangeNumber := 0;
      Inc(fNextChangeNumber);
      if fNextChangeNumber = 0 then
        Inc(fNextChangeNumber);
      if (fItems.Count > 0) and (PeekItem.ChangeNumber = iBlockID) and
        Assigned(OnAddedUndo) then
      begin
        OnAddedUndo( Self );
      end;
    end;
  end;
end;

procedure TSynEditUndoList.EnsureMaxEntries;
var
  Item: TSynEditUndoItem;
begin
  if fItems.Count > fMaxUndoActions then 
  begin
    fFullUndoImposible := True;                                                 
    while fItems.Count > fMaxUndoActions do
    begin
      Item := fItems[0];
      Item.Free;
      fItems.Delete(0);
    end;
  end;
end;

function TSynEditUndoList.GetCanUndo: boolean;
begin
  Result := fItems.Count > 0;
end;

function TSynEditUndoList.GetItemCount: integer;
begin
  Result := fItems.Count;
end;

procedure TSynEditUndoList.Lock;
begin
  Inc(fLockCount);
end;

function TSynEditUndoList.PeekItem: TSynEditUndoItem;
var
  iLast: integer;
begin
  Result := nil;
  iLast := fItems.Count - 1;
  if iLast >= 0 then
    Result := fItems[iLast];
end;

function TSynEditUndoList.PopItem: TSynEditUndoItem;
var
  iLast: integer;
begin
  Result := nil;
  iLast := fItems.Count - 1;
  if iLast >= 0 then begin
    Result := fItems[iLast];
    fItems.Delete(iLast);
  end;
end;

procedure TSynEditUndoList.PushItem(Item: TSynEditUndoItem);
begin
  if Assigned(Item) then begin
    fItems.Add(Item);
    EnsureMaxEntries;
    if (Item.ChangeReason <> crGroupBreak) and Assigned(OnAddedUndo) then
      OnAddedUndo(Self);
  end;
end;

procedure TSynEditUndoList.SetMaxUndoActions(Value: integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> fMaxUndoActions then begin
    fMaxUndoActions := Value;
    EnsureMaxEntries;
  end;
end;

procedure TSynEditUndoList.Unlock;
begin
  if fLockCount > 0 then
    Dec(fLockCount);
end;

function TSynEditUndoList.LastChangeReason: TSynChangeReason;
begin
  if fItems.Count = 0 then
    result := crNothing
  else
    result := TSynEditUndoItem(fItems[fItems.Count - 1]).fChangeReason;
end;

procedure TSynEditUndoList.AddGroupBreak;
var
  vDummy: TBufferCoord;
begin
  // Add the GroupBreak even if ItemCount = 0. Since items are stored in
  // reverse order in TCustomSynEdit.fRedoList, a GroupBreak could be lost
  if LastChangeReason <> crGroupBreak then
    AddChange(crGroupBreak, vDummy, vDummy, -1, '', smNormal);
end;

procedure TSynEditUndoList.SetInitialState(const Value: boolean);
begin
  if Value then
  begin
    if ItemCount = 0 then
      fInitialChangeNumber := 0
    else
      fInitialChangeNumber := PeekItem.ChangeNumber;
  end
  else
    if ItemCount = 0 then
    begin
      if fInitialChangeNumber = 0 then
        fInitialChangeNumber := -1;
    end
    else if PeekItem.ChangeNumber = fInitialChangeNumber then
      fInitialChangeNumber := -1;
end;

function TSynEditUndoList.GetInitialState: boolean;
begin
  if ItemCount = 0 then
    Result := fInitialChangeNumber = 0
  else
    Result := PeekItem.ChangeNumber = fInitialChangeNumber;
end;

function TSynEditUndoList.GetItems(Index: Integer): TSynEditUndoItem;
begin
  Result := TSynEditUndoItem(fItems[Index]);
end;

procedure TSynEditUndoList.SetItems(Index: Integer;
  const Value: TSynEditUndoItem);
begin
  fItems[Index] := Value;
end;

procedure TSynEditUndoList.DeleteItem(AIndex: Integer);
begin
  TSynEditUndoItem(fItems[AIndex]).Free;
  fItems.Delete(AIndex);
end;

end.
