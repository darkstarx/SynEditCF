{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditKeyCmds.pas, released 2000-04-07.
The Original Code is based on the mwKeyCmds.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Brad Stowers.
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

$Id: SynEditKeyCmds.pas,v 1.23.2.4 2008/09/14 16:24:58 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditKeyCmds;

{$I SynEdit.inc}

interface

uses
  Menus, Classes, SysUtils,

  { SynEdit }
  SynUnicode;

const

  // ---------------------------------------------------------------------------

  // Note! If you add an editor command, you must also update the
  // EditorCommandStrs constant array in implementation section below, or the
  // command will not show up in the IDE.

  // Key strokes are translated from a table into these commands.
  // I used constants instead of a set so that additional commands could be
  // added in descendants (you can't extend a set)

  // There are two ranges of editor commands: the ecViewXXX commands are always
  // valid, while the ecEditXXX commands are ignored when the editor is in
  // read-only mode

  // ---------------------------------------------------------------------------

  { Helpers }
  ecNone             =    0; // Nothing. Useful for user event to handle command
  ecViewCommandFirst =    0;
  ecViewCommandLast  =  500;
  ecEditCommandFirst =  501;
  ecEditCommandLast  = 1000;
  ecUserCommandFirst = 1001;

  { Navigation }
  ecLeft            = 1;     // Move cursor left one char
  ecRight           = 2;     // Move cursor right one char
  ecUp              = 3;     // Move cursor up one line
  ecDown            = 4;     // Move cursor down one line
  ecWordLeft        = 5;     // Move cursor left one word
  ecWordRight       = 6;     // Move cursor right one word
  ecLineStart       = 7;     // Move cursor to beginning of line
  ecLineEnd         = 8;     // Move cursor to end of line
  ecPageUp          = 9;     // Move cursor up one page
  ecPageDown        = 10;    // Move cursor down one page
  ecPageLeft        = 11;    // Move cursor right one page
  ecPageRight       = 12;    // Move cursor left one page
  ecPageTop         = 13;    // Move cursor to top of page
  ecPageBottom      = 14;    // Move cursor to bottom of page
  ecEditorTop       = 15;    // Move cursor to absolute beginning
  ecEditorBottom    = 16;    // Move cursor to absolute end
  ecGotoXY          = 17;    // Move cursor to specific coordinates, Data = PBufferCoord
  ecOffsetCaret     = 18;    // Moves caret to current caret + offset, Data = PBufferCoord
  ecGotoMatchPair   = 19;    // Moves caret to matching pairing element
  ecGotoMatchFoPair = 20;    // Moves caret to matching folding pairing element

  { Selection }
  ecSelection       = 100;   // Add this to ecXXX command to get equivalent
                             // command, but with selection enabled. This is not
                             // a command itself

  { Same as commands above, except they affect selection, too }
  ecSelLeft         = ecLeft + ecSelection;
  ecSelRight        = ecRight + ecSelection;
  ecSelUp           = ecUp + ecSelection;
  ecSelDown         = ecDown + ecSelection;
  ecSelWordLeft     = ecWordLeft + ecSelection;
  ecSelWordRight    = ecWordRight + ecSelection;
  ecSelLineStart    = ecLineStart + ecSelection;
  ecSelLineEnd      = ecLineEnd + ecSelection;
  ecSelPageUp       = ecPageUp + ecSelection;
  ecSelPageDown     = ecPageDown + ecSelection;
  ecSelPageLeft     = ecPageLeft + ecSelection;
  ecSelPageRight    = ecPageRight + ecSelection;
  ecSelPageTop      = ecPageTop + ecSelection;
  ecSelPageBottom   = ecPageBottom + ecSelection;
  ecSelEditorTop    = ecEditorTop + ecSelection;
  ecSelEditorBottom = ecEditorBottom + ecSelection;
  ecSelGotoXY       = ecGotoXY + ecSelection;      // Data = PBufferCoord
  ecSelOffsetCaret  = ecOffsetCaret + ecSelection; // Data = PBufferCoord
  ecSelGotoMatchPair = ecGotoMatchPair + ecSelection;
  ecSelGotoMatchFoPair = ecGotoMatchFoPair + ecSelection;
  ecSelScope        = ecSelection + 21;
  ecSelWord         = ecSelection + 22; // Select word
  ecSelectAll       = ecSelection + 23; // Select entire contents of editor, cursor to end

  ecScrollUp        = 211;   // Scroll up one line leaving cursor position unchanged
  ecScrollDown      = 212;   // Scroll down one line leaving cursor position unchanged
  ecScrollLeft      = 213;   // Scroll left one char leaving cursor position unchanged
  ecScrollRight     = 214;   // Scroll right one char leaving cursor position unchanged

  ecInsertMode      = 221;   // Set insert mode
  ecOverwriteMode   = 222;   // Set overwrite mode
  ecToggleMode      = 223;   // Toggle ins/ovr mode

  ecNormalSelect    = 231;   // Normal selection mode
  ecColumnSelect    = 232;   // Column selection mode
  ecLineSelect      = 233;   // Line selection mode

  { Outlining }
  ecOutliningCollapse = 251;
  ecOutliningExpand   = 252;
  ecOutliningToggle   = 253;

  { Bookmarks }
  ecGotoMarker0     = 301;   // Goto marker 0
  ecGotoMarker1     = 302;   // Goto marker 1
  ecGotoMarker2     = 303;   // Goto marker 2
  ecGotoMarker3     = 304;   // Goto marker 3
  ecGotoMarker4     = 305;   // Goto marker 4
  ecGotoMarker5     = 306;   // Goto marker 5
  ecGotoMarker6     = 307;   // Goto marker 6
  ecGotoMarker7     = 308;   // Goto marker 7
  ecGotoMarker8     = 309;   // Goto marker 8
  ecGotoMarker9     = 310;   // Goto marker 9
  ecSetMarker0      = 351;   // Set marker 0 on current pos
  ecSetMarker1      = 352;   // Set marker 1 on current pos
  ecSetMarker2      = 353;   // Set marker 2 on current pos
  ecSetMarker3      = 354;   // Set marker 3 on current pos
  ecSetMarker4      = 355;   // Set marker 4 on current pos
  ecSetMarker5      = 356;   // Set marker 5 on current pos
  ecSetMarker6      = 357;   // Set marker 6 on current pos
  ecSetMarker7      = 358;   // Set marker 7 on current pos
  ecSetMarker8      = 359;   // Set marker 8 on current pos
  ecSetMarker9      = 360;   // Set marker 9 on current pos

  ecGotFocus        = 480;
  ecLostFocus       = 481;

  ecContextHelp     = 490;   // Help on Word

  ecDeleteLastChar  = 501;   // Delete last char (i.e. backspace key)
  ecDeleteChar      = 502;   // Delete char at cursor (i.e. delete key)
  ecDeleteWord      = 503;   // Delete from cursor to end of word
  ecDeleteLastWord  = 504;   // Delete from cursor to start of word
  ecDeleteBOL       = 505;   // Delete from cursor to beginning of line
  ecDeleteEOL       = 506;   // Delete from cursor to end of line
  ecDeleteLine      = 507;   // Delete current line
  ecClearAll        = 508;   // Delete everything
  ecLineBreak       = 509;   // Break line at current position, move caret to new line
  ecInsertLine      = 510;   // Break line at current position, leave caret
  ecChar            = 511;   // Insert a character at current position

  ecImeStr          = 550;   // Insert character(s) from IME

  ecUndo            = 601;   // Perform undo if available
  ecRedo            = 602;   // Perform redo if available
  ecCut             = 603;   // Cut selection to clipboard
  ecCopy            = 204;   // Copy selection to clipboard
  ecPaste           = 605;   // Paste clipboard to current position

  ecBlockIndent     = 610;   // Indent selection
  ecBlockUnindent   = 611;   // Unindent selection
  ecTab             = 612;   // Tab key
  ecShiftTab        = 613;   // Shift+Tab key
  ecHardTab         = 614;   // Shift+Ctrl+Tab key

  ecString          = 630;   // Insert a whole string

  ecAutoCompletion  = 650;

  ecMoveLineUp      = 701;
  ecMoveLineDown    = 702;
  ecMoveCharLeft    = 703;
  ecMoveCharRight   = 704;

  { User (Letterpress) commands }
  ecExecuteRoutine = ecUserCommandFirst + 1;

const
  scsShift = $2000;
  scsCtrl = $4000;
  scsAlt = $8000;
  scsWin = $A000;
  scsNone = 0;

type
  ESynKeyError = class(Exception);

  TSynEditorCommand = type Word;

  TSynShiftState = set of (ssShift, ssAlt, ssCtrl, ssWin);
  TSynShortcut = Low(Word)..High(Word);

  TSynEditKeyStroke = class(TCollectionItem)
  private
    FKey: Word; // Virtual keycode, i.e. VK_X
    FShift: TSynShiftState;
    FKey2: Word;
    FShift2: TSynShiftState;
    FCommand: TSynEditorCommand;

    function GetShortCut: TSynShortCut;
    function GetShortCut2: TSynShortCut;
    procedure SetCommand(const Value: TSynEditorCommand);
    procedure SetKey(const Value: Word);
    procedure SetKey2(const Value: Word);
    procedure SetShift(const Value: TSynShiftState);
    procedure SetShift2(const Value: TSynShiftState);
    procedure SetShortCut(const Value: TSynShortCut);
    procedure SetShortCut2(const Value: TSynShortCut);
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);

    { No duplicate checking is done if assignment made via these properties! }
    property Key: Word read FKey write SetKey;
    property Key2: Word read FKey2 write SetKey2;
    property Shift: TSynShiftState read FShift write SetShift;
    property Shift2: TSynShiftState read FShift2 write SetShift2;
  published
    property Command: TSynEditorCommand read FCommand write SetCommand;
    property ShortCut: TSynShortCut read GetShortCut write SetShortCut
      default 0;
    property ShortCut2: TSynShortCut read GetShortCut2 write SetShortCut2
      default 0;
  end;

  TSynEditKeyStrokes = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TSynEditKeyStroke;
    procedure SetItem(Index: Integer; Value: TSynEditKeyStroke);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TSynEditKeyStroke;
    procedure AddKey(const ACmd: TSynEditorCommand; const AKey: word;
       const AShift: TSynShiftState);
    procedure Assign(Source: TPersistent); override;
    function FindCommand(Cmd: TSynEditorCommand): integer;
    function FindKeycode(Code: word; SS: TSynShiftState): integer;
    function FindKeycode2(Code1: word; SS1: TSynShiftState;
      Code2: word; SS2: TSynShiftState): integer;
    function FindShortcut(SC: TSynShortcut): integer;
    function FindShortcut2(SC, SC2: TSynShortcut): integer;
    procedure LoadFromStream(AStream: TStream);
    procedure ResetDefaults;
    procedure SaveToStream(AStream: TStream);
  public
    property Items[Index: Integer]: TSynEditKeyStroke read GetItem
      write SetItem; default;
  end;

{ Syn shortcut methods }
function AssembleSynShortcut(Key: Word; Shift: TSynShiftState): TSynShortcut;
procedure DisasmSynShortcut(Shortcut: TSynShortcut; out Key: Word; out Shift: TSynShiftState);
function ShortcutToSynShortcut(Value: TShortcut): TSynShortcut;
function ShiftStateToSynShiftState(Value: TShiftState;
  WinPressed: Boolean = False): TSynShiftState;
function IsWinPressed: Boolean;

{ These are mainly for the TSynEditorCommand property editor, but could be
  useful elsewhere }
function EditorCommandToCodeString(Cmd: TSynEditorCommand): String;
function IdentToEditorCommand(const Ident: String; var Cmd: longint): Boolean;
function EditorCommandToIdent(Cmd: longint; var Ident: String): Boolean;
function ConvertCodeStringToCommand(AString: String): TSynEditorCommand;
function IndexToEditorCommand(const AIndex: Integer): Integer;
procedure GetEditorCommandValues(Proc: TGetStrProc);

implementation

uses
  Windows,

  { SynEdit }
  SynEditKeyConst, SynEditStrConst;

{ Command mapping routines }

const
  EditorCommandStrs: array[0..106] of TIdentMapEntry = (
    (Value: ecNone; Name: 'ecNone'),
    (Value: ecLeft; Name: 'ecLeft'),
    (Value: ecRight; Name: 'ecRight'),
    (Value: ecUp; Name: 'ecUp'),
    (Value: ecDown; Name: 'ecDown'),
    (Value: ecWordLeft; Name: 'ecWordLeft'),
    (Value: ecWordRight; Name: 'ecWordRight'),
    (Value: ecLineStart; Name: 'ecLineStart'),
    (Value: ecLineEnd; Name: 'ecLineEnd'),
    (Value: ecPageUp; Name: 'ecPageUp'),
    (Value: ecPageDown; Name: 'ecPageDown'),
    (Value: ecPageLeft; Name: 'ecPageLeft'),
    (Value: ecPageRight; Name: 'ecPageRight'),
    (Value: ecPageTop; Name: 'ecPageTop'),
    (Value: ecPageBottom; Name: 'ecPageBottom'),
    (Value: ecEditorTop; Name: 'ecEditorTop'),
    (Value: ecEditorBottom; Name: 'ecEditorBottom'),
    (Value: ecGotoXY; Name: 'ecGotoXY'),
    (Value: ecOffsetCaret; Name: 'ecOffsetCaret'),
    (Value: ecGotoMatchPair; Name: 'ecGotoMatchPair'),
    (Value: ecGotoMatchFoPair; Name: 'ecGotoMatchFoPair'),
    (Value: ecSelLeft; Name: 'ecSelLeft'),
    (Value: ecSelRight; Name: 'ecSelRight'),
    (Value: ecSelUp; Name: 'ecSelUp'),
    (Value: ecSelDown; Name: 'ecSelDown'),
    (Value: ecSelWordLeft; Name: 'ecSelWordLeft'),
    (Value: ecSelWordRight; Name: 'ecSelWordRight'),
    (Value: ecSelLineStart; Name: 'ecSelLineStart'),
    (Value: ecSelLineEnd; Name: 'ecSelLineEnd'),
    (Value: ecSelPageUp; Name: 'ecSelPageUp'),
    (Value: ecSelPageDown; Name: 'ecSelPageDown'),
    (Value: ecSelPageLeft; Name: 'ecSelPageLeft'),
    (Value: ecSelPageRight; Name: 'ecSelPageRight'),
    (Value: ecSelPageTop; Name: 'ecSelPageTop'),
    (Value: ecSelPageBottom; Name: 'ecSelPageBottom'),
    (Value: ecSelEditorTop; Name: 'ecSelEditorTop'),
    (Value: ecSelEditorBottom; Name: 'ecSelEditorBottom'),
    (Value: ecSelGotoXY; Name: 'ecSelGotoXY'),
    (Value: ecSelOffsetCaret; Name: 'ecSelOffsetCaret'),
    (Value: ecSelGotoMatchPair; Name: 'ecSelGotoMatchPair'),
    (Value: ecSelGotoMatchFoPair; Name: 'ecGotoMatchFoPair'),
    (Value: ecSelScope; Name: 'ecSelScope'),
    (Value: ecSelWord; Name: 'ecSelWord'),
    (Value: ecSelectAll; Name: 'ecSelectAll'),
    (Value: ecScrollUp; Name: 'ecScrollUp'),
    (Value: ecScrollDown; Name: 'ecScrollDown'),
    (Value: ecScrollLeft; Name: 'ecScrollLeft'),
    (Value: ecScrollRight; Name: 'ecScrollRight'),
    (Value: ecOutliningCollapse; Name: 'ecOutliningCollapse'),
    (Value: ecOutliningExpand; Name: 'ecOutliningExpand'),
    (Value: ecOutliningToggle; Name: 'ecOutliningToggle'),
    (Value: ecDeleteLastChar; Name: 'ecDeleteLastChar'),
    (Value: ecDeleteChar; Name: 'ecDeleteChar'),
    (Value: ecDeleteWord; Name: 'ecDeleteWord'),
    (Value: ecDeleteLastWord; Name: 'ecDeleteLastWord'),
    (Value: ecDeleteBOL; Name: 'ecDeleteBOL'),
    (Value: ecDeleteEOL; Name: 'ecDeleteEOL'),
    (Value: ecDeleteLine; Name: 'ecDeleteLine'),
    (Value: ecClearAll; Name: 'ecClearAll'),
    (Value: ecLineBreak; Name: 'ecLineBreak'),
    (Value: ecInsertLine; Name: 'ecInsertLine'),
    (Value: ecChar; Name: 'ecChar'),
    (Value: ecImeStr; Name: 'ecImeStr'),
    (Value: ecUndo; Name: 'ecUndo'),
    (Value: ecRedo; Name: 'ecRedo'),
    (Value: ecCut; Name: 'ecCut'),
    (Value: ecCopy; Name: 'ecCopy'),
    (Value: ecPaste; Name: 'ecPaste'),
    (Value: ecInsertMode; Name: 'ecInsertMode'),
    (Value: ecOverwriteMode; Name: 'ecOverwriteMode'),
    (Value: ecToggleMode; Name: 'ecToggleMode'),
    (Value: ecBlockIndent; Name: 'ecBlockIndent'),
    (Value: ecBlockUnindent; Name: 'ecBlockUnindent'),
    (Value: ecTab; Name: 'ecTab'),
    (Value: ecShiftTab; Name: 'ecShiftTab'),
    (Value: ecNormalSelect; Name: 'ecNormalSelect'),
    (Value: ecColumnSelect; Name: 'ecColumnSelect'),
    (Value: ecLineSelect; Name: 'ecLineSelect'),
    (Value: ecAutoCompletion; Name: 'ecAutoCompletion'),
    (Value: ecUserCommandFirst; Name: 'ecUserFirst'),
    (Value: ecContextHelp; Name: 'ecContextHelp'),
    (Value: ecGotoMarker0; Name: 'ecGotoMarker0'),
    (Value: ecGotoMarker1; Name: 'ecGotoMarker1'),
    (Value: ecGotoMarker2; Name: 'ecGotoMarker2'),
    (Value: ecGotoMarker3; Name: 'ecGotoMarker3'),
    (Value: ecGotoMarker4; Name: 'ecGotoMarker4'),
    (Value: ecGotoMarker5; Name: 'ecGotoMarker5'),
    (Value: ecGotoMarker6; Name: 'ecGotoMarker6'),
    (Value: ecGotoMarker7; Name: 'ecGotoMarker7'),
    (Value: ecGotoMarker8; Name: 'ecGotoMarker8'),
    (Value: ecGotoMarker9; Name: 'ecGotoMarker9'),
    (Value: ecSetMarker0; Name: 'ecSetMarker0'),
    (Value: ecSetMarker1; Name: 'ecSetMarker1'),
    (Value: ecSetMarker2; Name: 'ecSetMarker2'),
    (Value: ecSetMarker3; Name: 'ecSetMarker3'),
    (Value: ecSetMarker4; Name: 'ecSetMarker4'),
    (Value: ecSetMarker5; Name: 'ecSetMarker5'),
    (Value: ecSetMarker6; Name: 'ecSetMarker6'),
    (Value: ecSetMarker7; Name: 'ecSetMarker7'),
    (Value: ecSetMarker8; Name: 'ecSetMarker8'),
    (Value: ecSetMarker9; Name: 'ecSetMarker9'),
    (Value: ecString; Name: 'ecString'),
    (Value: ecExecuteRoutine; Name: 'ecExecuteRoutine'),
    (Value: ecMoveLineUp; Name: 'ecMoveLineUp'),
    (Value: ecMoveLineDown; Name: 'ecMoveLineDown'),
    (Value: ecMoveCharLeft; Name: 'ecMoveCharLeft'),
    (Value: ecMoveCharRight; Name: 'ecMoveCharRight')
  );

procedure GetEditorCommandValues(Proc: TGetStrProc);
var
  I: integer;
begin
  for I := Low(EditorCommandStrs) to High(EditorCommandStrs) do
    Proc(EditorCommandStrs[I].Name);
end;

function IdentToEditorCommand(const Ident: string; var Cmd: longint): boolean;
begin
  Result := IdentToInt(Ident, Cmd, EditorCommandStrs);
end;

function EditorCommandToIdent(Cmd: longint; var Ident: string): boolean;
begin
  Result := IntToIdent(Cmd, Ident, EditorCommandStrs);
end;

function EditorCommandToCodeString(Cmd: TSynEditorCommand): String;
begin
  if not EditorCommandToIdent(Cmd, Result) then
    Result := IntToStr(Cmd);
end;

{  }

// -----------------------------------------------------------------------------

function AssembleSynShortcut(Key: Word; Shift: TSynShiftState): TSynShortcut;
begin
  Result := 0;
  if HiByte(Key) <> 0 then Exit;
  Result := Key;
  if ssShift in Shift then Inc(Result, scsShift);
  if ssCtrl in Shift then Inc(Result, scsCtrl);
  if ssAlt in Shift then Inc(Result, scsAlt);
  if ssWin in Shift then Inc(Result, scsWin);
end;

procedure DisasmSynShortcut(Shortcut: TSynShortcut; out Key: Word; out Shift: TSynShiftState);
begin
  { Get key }
  Key := Shortcut and not (scsShift or scsCtrl or scsAlt);

  { Get shift }
  Shift := [];
  if Shortcut and scsShift <> 0 then Include(Shift, ssShift);
  if Shortcut and scsCtrl <> 0 then Include(Shift, ssCtrl);
  if Shortcut and scsAlt <> 0 then Include(Shift, ssAlt);
  if Shortcut and scsWin <> 0 then Include(Shift, ssWin);
end;

function ShortcutToSynShortcut(Value: TShortcut): TSynShortcut;
begin
  { SynShortcut is forward compatible with simple Shortcut, hence we assign }
  Result := Value;
end;

function ShiftStateToSynShiftState(Value: TShiftState;
  WinPressed: Boolean = False): TSynShiftState;
begin
  Result := [];
  if Classes.ssShift in Value then Include(Result, ssShift);
  if Classes.ssCtrl in Value then Include(Result, ssCtrl);
  if Classes.ssAlt in Value then Include(Result, ssAlt);
  if WinPressed then Include(Result, ssWin);
end;

function IsWinPressed: Boolean;
begin
  Result := (GetAsyncKeyState(VK_LWIN) <> 0) or
    (GetAsyncKeyState(VK_RWIN) <> 0);
end;

{ TSynEditKeyStroke }

// -----------------------------------------------------------------------------

procedure TSynEditKeyStroke.Assign(Source: TPersistent);
begin
  if Source is TSynEditKeyStroke then
  begin
    Command := TSynEditKeyStroke(Source).Command;
    Key := TSynEditKeyStroke(Source).Key;
    Key2 := TSynEditKeyStroke(Source).Key2;
    Shift := TSynEditKeyStroke(Source).Shift;
    Shift2 := TSynEditKeyStroke(Source).Shift2;
  end else
    inherited Assign(Source);
end;

function TSynEditKeyStroke.GetDisplayName: String;
begin
  Result := EditorCommandToCodeString(Command) + ' - ' + ShortCutToText(ShortCut);
  if ShortCut <> 0 then
    Result := Result + ' ' + ShortCutToText(ShortCut2);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TSynEditKeyStroke.GetShortCut: TSynShortCut;
begin
  Result := AssembleSynShortcut(fKey, fShift);
end;

procedure TSynEditKeyStroke.SetCommand(const Value: TSynEditorCommand);
begin
  if Value <> FCommand then
    FCommand := Value;
end;

procedure TSynEditKeyStroke.SetKey(const Value: word);
begin
  if Value <> FKey then
    FKey := Value;
end;

procedure TSynEditKeyStroke.SetShift(const Value: TSynShiftState);
begin
  if Value <> FShift then
    FShift := Value;
end;

procedure TSynEditKeyStroke.SetShortCut(const Value: TSynShortCut);
var
  NewKey: Word;
  NewShift: TSynShiftState;
  Dup: Integer;
begin
  { Duplicate values of no shortcut are OK }
  if Value <> 0 then
  begin

    { Check for duplicate shortcut in the collection and disallow if there is }
    Dup := TSynEditKeyStrokes(Collection).FindShortcut2(Value, ShortCut2);
    if (Dup <> -1) and (Dup <> Self.Index) then
      raise ESynKeyError.Create(SYNS_EDuplicateShortCut);
  end;

  { Get shortcut parts }
  DisasmSynShortcut(Value, NewKey, NewShift);

  { Assign if it's different }
  if (NewKey <> Key) or (NewShift <> Shift) then
  begin
    Key := NewKey;
    Shift := NewShift;
  end;
end;

procedure TSynEditKeyStroke.SetKey2(const Value: word);
begin
  if Value <> FKey2 then
    FKey2 := Value;
end;

procedure TSynEditKeyStroke.SetShift2(const Value: TSynShiftState);
begin
  if Value <> FShift2 then
    FShift2 := Value;
end;

procedure TSynEditKeyStroke.SetShortCut2(const Value: TSynShortCut);
var
  NewKey: Word;
  NewShift: TSynShiftState;
  Dup: integer;
begin
  { Duplicate values of no shortcut are OK }
  if Value <> 0 then
  begin

    { Check for duplicate shortcut in the collection and disallow if there is }
    Dup := TSynEditKeyStrokes(Collection).FindShortcut2(ShortCut, Value);
    if (Dup <> -1) and (Dup <> Self.Index) then
      raise ESynKeyError.Create(SYNS_EDuplicateShortCut);
  end;

  { Get shortcut parts }
  DisasmSynShortcut(Value, NewKey, NewShift);

  { Assign it if it's differs }
  if (NewKey <> Key2) or (NewShift <> Shift2) then
  begin
    Key2 := NewKey;
    Shift2 := NewShift;
  end;
end;

function TSynEditKeyStroke.GetShortCut2: TSynShortCut;
begin
  Result := 0;
  if HiByte(Key2) <> 0 then Exit;
  Result := Key2;
  if ssShift in Shift2 then Inc(Result, scsShift);
  if ssCtrl in Shift2 then Inc(Result, scsCtrl);
  if ssAlt in Shift2 then Inc(Result, scsAlt);
  if ssWin in Shift2 then Inc(Result, scsWin);
end;

procedure TSynEditKeyStroke.LoadFromStream(AStream: TStream);
begin
  with AStream do
  begin
    Read(fKey, SizeOf(fKey));
    Read(fShift, SizeOf(fShift));
    Read(fKey2, SizeOf(fKey2));
    Read(fShift2, SizeOf(fShift2));
    Read(fCommand, SizeOf(fCommand));
  end;
end;

procedure TSynEditKeyStroke.SaveToStream(AStream: TStream);
begin
  with AStream do
  begin
    Write(fKey, SizeOf(fKey));
    Write(fShift, SizeOf(fShift));
    Write(fKey2, SizeOf(fKey2));
    Write(fShift2, SizeOf(fShift2));
    Write(fCommand, SizeOf(fCommand));
  end;
end;

{ TSynEditKeyStrokes }

// -----------------------------------------------------------------------------

function TSynEditKeyStrokes.Add: TSynEditKeyStroke;
begin
  Result := TSynEditKeyStroke(inherited Add);
end;

procedure TSynEditKeyStrokes.AddKey(const ACmd: TSynEditorCommand;
  const AKey: Word; const AShift: TSynShiftState);
var
  NewKeystroke: TSynEditKeyStroke;
begin
  NewKeystroke := Add;
  try
    NewKeystroke.Key := AKey;
    NewKeystroke.Shift := AShift;
    NewKeystroke.Command := ACmd;
  except
    NewKeystroke.Free;
    raise;
  end;
end;

procedure TSynEditKeyStrokes.Assign(Source: TPersistent);
var
  X: integer;
begin
  if Source is TSynEditKeyStrokes then
  begin
    Clear;
    for X := 0 to TSynEditKeyStrokes(Source).Count-1 do
    begin
      with Add do
        Assign(TSynEditKeyStrokes(Source)[X]);
    end;
  end
  else
    inherited Assign(Source);
end;

constructor TSynEditKeyStrokes.Create(AOwner: TPersistent);
begin
  inherited Create(TSynEditKeyStroke);
  FOwner := AOwner;
end;

function TSynEditKeyStrokes.FindCommand(Cmd: TSynEditorCommand): Integer;
var
  X: integer;
begin
  Result := -1;
  for X := 0 to Count-1 do
    if Items[X].Command = Cmd then
    begin
      Result := X;
      Break;
    end;
end;

function TSynEditKeyStrokes.FindKeycode(Code: Word; SS: TSynShiftState): Integer;
var
  X: integer;
begin
  Result := -1;
  for X := 0 to Pred(Count) do
    if (Items[X].Key = Code) and (Items[X].Shift = SS) and (Items[X].Key2 = 0)
    then begin
      Result := X;
      Break;
    end;
end;

function TSynEditKeyStrokes.FindKeycode2(Code1: Word; SS1: TSynShiftState;
  Code2: Word; SS2: TSynShiftState): Integer;
var
  X: Integer;
begin
  Result := -1;
  for X := 0 to Pred(Count) do
    if (Items[X].Key = Code1) and (Items[X].Shift = SS1) and
      (Items[X].Key2 = Code2) and (Items[X].Shift2 = SS2) then
    begin
      Result := X;
      Break;
    end;
end;

function TSynEditKeyStrokes.FindShortcut(SC: TSynShortcut): Integer;
var
  X: integer;
begin
  Result := -1;
  for X := 0 to Count-1 do
    if Items[X].Shortcut = SC then
    begin
      Result := X;
      break;
    end;
end;

function TSynEditKeyStrokes.FindShortcut2(SC, SC2: TSynShortcut): Integer;
var
  X: integer;
begin
  Result := -1;
  for X := 0 to Count-1 do
    if (Items[X].Shortcut = SC) and (Items[X].Shortcut2 = SC2) then
    begin
      Result := X;
      break;
    end;
end;

function TSynEditKeyStrokes.GetItem(Index: Integer): TSynEditKeyStroke;
begin
  Result := TSynEditKeyStroke(inherited GetItem(Index));
end;

function TSynEditKeyStrokes.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSynEditKeyStrokes.LoadFromStream(AStream: TStream);
var
  Num: Integer;
begin
  Clear;
  AStream.Read(Num, SizeOf(Num));
  while Num > 0 do begin
    with Add do
      LoadFromStream(AStream);
    Dec(Num);
  end;
end;

procedure TSynEditKeyStrokes.ResetDefaults;
begin
  Clear;

  { Scrolling, caret moving and selection }
  AddKey(ecUp, SYNEDIT_UP, []);
  AddKey(ecSelUp, SYNEDIT_UP, [ssShift]);
  AddKey(ecScrollUp, SYNEDIT_UP, [ssCtrl]);
  AddKey(ecDown, SYNEDIT_DOWN, []);
  AddKey(ecSelDown, SYNEDIT_DOWN, [ssShift]);
  AddKey(ecScrollLeft, SYNEDIT_LEFT, [ssAlt]);
  AddKey(ecScrollRight, SYNEDIT_RIGHT, [ssAlt]);
  AddKey(ecScrollDown, SYNEDIT_DOWN, [ssCtrl]);
  AddKey(ecLeft, SYNEDIT_LEFT, []);
  AddKey(ecSelLeft, SYNEDIT_LEFT, [ssShift]);
  AddKey(ecWordLeft, SYNEDIT_LEFT, [ssCtrl]);
  AddKey(ecSelWordLeft, SYNEDIT_LEFT, [ssShift, ssCtrl]);
  AddKey(ecRight, SYNEDIT_RIGHT, []);
  AddKey(ecSelRight, SYNEDIT_RIGHT, [ssShift]);
  AddKey(ecWordRight, SYNEDIT_RIGHT, [ssCtrl]);
  AddKey(ecSelWordRight, SYNEDIT_RIGHT, [ssShift, ssCtrl]);
  AddKey(ecPageDown, SYNEDIT_NEXT, []);
  AddKey(ecSelPageDown, SYNEDIT_NEXT, [ssShift]);
  AddKey(ecPageBottom, SYNEDIT_NEXT, [ssCtrl]);
  AddKey(ecSelPageBottom, SYNEDIT_NEXT, [ssShift, ssCtrl]);
  AddKey(ecPageUp, SYNEDIT_PRIOR, []);
  AddKey(ecSelPageUp, SYNEDIT_PRIOR, [ssShift]);
  AddKey(ecPageTop, SYNEDIT_PRIOR, [ssCtrl]);
  AddKey(ecSelPageTop, SYNEDIT_PRIOR, [ssShift, ssCtrl]);
  AddKey(ecLineStart, SYNEDIT_HOME, []);
  AddKey(ecSelLineStart, SYNEDIT_HOME, [ssShift]);
  AddKey(ecEditorTop, SYNEDIT_HOME, [ssCtrl]);
  AddKey(ecSelEditorTop, SYNEDIT_HOME, [ssShift, ssCtrl]);
  AddKey(ecLineEnd, SYNEDIT_END, []);
  AddKey(ecSelLineEnd, SYNEDIT_END, [ssShift]);
  AddKey(ecEditorBottom, SYNEDIT_END, [ssCtrl]);
  AddKey(ecSelEditorBottom, SYNEDIT_END, [ssShift, ssCtrl]);
  AddKey(ecGotoMatchPair, Ord('B'), [ssCtrl]);
  AddKey(ecSelGotoMatchPair, Ord('B'), [ssShift, ssCtrl]);
  AddKey(ecGotoMatchFoPair, Ord('B'), [ssCtrl, ssAlt]);
  AddKey(ecSelGotoMatchFoPair, Ord('B'), [ssShift, ssCtrl, ssAlt]);

  { Insert key alone }
  AddKey(ecToggleMode, SYNEDIT_INSERT, []);

  { Alternative edit commands }
  AddKey(ecUndo, SYNEDIT_BACK, [ssAlt]);
  AddKey(ecRedo, SYNEDIT_BACK, [ssAlt,ssShift]);
  AddKey(ecCopy, SYNEDIT_INSERT, [ssCtrl]);
  AddKey(ecCut, SYNEDIT_DELETE, [ssShift]);
  AddKey(ecPaste, SYNEDIT_INSERT, [ssShift]);

  { Deletion }
  AddKey(ecDeleteChar, SYNEDIT_DELETE, []);
  AddKey(ecDeleteLastChar, SYNEDIT_BACK, []);
  AddKey(ecDeleteLastChar, SYNEDIT_BACK, [ssShift]);
  AddKey(ecDeleteLastWord, SYNEDIT_BACK, [ssCtrl]);

  { Enter (return) & Tab }
  AddKey(ecLineBreak, SYNEDIT_RETURN, []);
  AddKey(ecLineBreak, SYNEDIT_RETURN, [ssShift]);
  AddKey(ecTab, SYNEDIT_TAB, []);
  AddKey(ecHardTab, SYNEDIT_TAB, [ssShift, ssCtrl]);
  AddKey(ecShiftTab, SYNEDIT_TAB, [ssShift]);
  AddKey(ecContextHelp, SYNEDIT_F1, []);

  { Standard edit commands }
  AddKey(ecUndo, ord('Z'), [ssCtrl]);
  AddKey(ecRedo, ord('Z'), [ssCtrl,ssShift]);
  AddKey(ecCut, ord('X'), [ssCtrl]);
  AddKey(ecCopy, ord('C'), [ssCtrl]);
  AddKey(ecPaste, ord('V'), [ssCtrl]);
  AddKey(ecSelectAll, ord('A'), [ssCtrl]);

  { Block commands }
  AddKey(ecBlockIndent, ord('I'), [ssCtrl,ssShift]);
  AddKey(ecBlockUnindent, ord('U'), [ssCtrl,ssShift]);

  { Fragment deletion }
  AddKey(ecDeleteWord, ord('T'), [ssCtrl]);

  { Line operations }
  AddKey(ecInsertLine, ord('M'), [ssCtrl]);
  AddKey(ecMoveLineUp, SYNEDIT_UP, [ssCtrl, ssAlt]);
  AddKey(ecMoveLineDown, SYNEDIT_DOWN, [ssCtrl, ssAlt]);
  AddKey(ecDeleteLine, SYNEDIT_DELETE, [ssCtrl]);
  AddKey(ecDeleteEOL, SYNEDIT_DELETE, [ssCtrl, ssShift]);
  AddKey(ecMoveCharLeft, SYNEDIT_LEFT, [ssAlt, ssCtrl]);
  AddKey(ecMoveCharRight, SYNEDIT_RIGHT, [ssAlt, ssCtrl]);

  { Bookmarks }
  AddKey(ecGotoMarker0, ord('0'), [ssCtrl]);
  AddKey(ecGotoMarker1, ord('1'), [ssCtrl]);
  AddKey(ecGotoMarker2, ord('2'), [ssCtrl]);
  AddKey(ecGotoMarker3, ord('3'), [ssCtrl]);
  AddKey(ecGotoMarker4, ord('4'), [ssCtrl]);
  AddKey(ecGotoMarker5, ord('5'), [ssCtrl]);
  AddKey(ecGotoMarker6, ord('6'), [ssCtrl]);
  AddKey(ecGotoMarker7, ord('7'), [ssCtrl]);
  AddKey(ecGotoMarker8, ord('8'), [ssCtrl]);
  AddKey(ecGotoMarker9, ord('9'), [ssCtrl]);
  AddKey(ecSetMarker0, ord('0'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker1, ord('1'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker2, ord('2'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker3, ord('3'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker4, ord('4'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker5, ord('5'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker6, ord('6'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker7, ord('7'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker8, ord('8'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker9, ord('9'), [ssCtrl,ssShift]);

  { Selection modes }
  AddKey(ecNormalSelect, ord('N'), [ssCtrl,ssAlt]);
  AddKey(ecColumnSelect, ord('C'), [ssCtrl,ssAlt]);
  AddKey(ecLineSelect, ord('L'), [ssCtrl,ssAlt]);
end;

procedure TSynEditKeyStrokes.SetItem(Index: Integer; Value: TSynEditKeyStroke);
begin
 inherited SetItem(Index, Value);
end;

procedure TSynEditKeyStrokes.SaveToStream(AStream: TStream);
var
  I, Num: integer;
begin
  Num := Count;
  AStream.Write(Num, SizeOf(Num));
  for I := 0 to Num - 1 do
    Items[I].SaveToStream(AStream);
end;

function IndexToEditorCommand(const AIndex: Integer): Integer;
begin
  Result := EditorCommandStrs[AIndex].Value;
end;

function ConvertCodeStringToCommand(AString: string): TSynEditorCommand;
var
  I: Integer;
begin
  Result := ecNone;
  AString := Uppercase(AString);
  for I := Low(EditorCommandStrs) to High(EditorCommandStrs) do
    if Uppercase(EditorCommandStrs[I].Name) = AString then
    begin
      Result := EditorCommandStrs[I].Value;
      Break;
    end;
end;

initialization
  RegisterIntegerConsts(TypeInfo(TSynEditorCommand), IdentToEditorCommand,
    EditorCommandToIdent);

end.
