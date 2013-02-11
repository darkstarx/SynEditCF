{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N-,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE GUI}
{$WARN SYMBOL_DEPRECATED ON}
{$WARN SYMBOL_LIBRARY ON}
{$WARN SYMBOL_PLATFORM ON}
{$WARN SYMBOL_EXPERIMENTAL ON}
{$WARN UNIT_LIBRARY ON}
{$WARN UNIT_PLATFORM ON}
{$WARN UNIT_DEPRECATED ON}
{$WARN UNIT_EXPERIMENTAL ON}
{$WARN HRESULT_COMPAT ON}
{$WARN HIDING_MEMBER ON}
{$WARN HIDDEN_VIRTUAL ON}
{$WARN GARBAGE ON}
{$WARN BOUNDS_ERROR ON}
{$WARN ZERO_NIL_COMPAT ON}
{$WARN STRING_CONST_TRUNCED ON}
{$WARN FOR_LOOP_VAR_VARPAR ON}
{$WARN TYPED_CONST_VARPAR ON}
{$WARN ASG_TO_TYPED_CONST ON}
{$WARN CASE_LABEL_RANGE ON}
{$WARN FOR_VARIABLE ON}
{$WARN CONSTRUCTING_ABSTRACT ON}
{$WARN COMPARISON_FALSE ON}
{$WARN COMPARISON_TRUE ON}
{$WARN COMPARING_SIGNED_UNSIGNED ON}
{$WARN COMBINING_SIGNED_UNSIGNED ON}
{$WARN UNSUPPORTED_CONSTRUCT ON}
{$WARN FILE_OPEN ON}
{$WARN FILE_OPEN_UNITSRC ON}
{$WARN BAD_GLOBAL_SYMBOL ON}
{$WARN DUPLICATE_CTOR_DTOR ON}
{$WARN INVALID_DIRECTIVE ON}
{$WARN PACKAGE_NO_LINK ON}
{$WARN PACKAGED_THREADVAR ON}
{$WARN IMPLICIT_IMPORT ON}
{$WARN HPPEMIT_IGNORED ON}
{$WARN NO_RETVAL ON}
{$WARN USE_BEFORE_DEF ON}
{$WARN FOR_LOOP_VAR_UNDEF ON}
{$WARN UNIT_NAME_MISMATCH ON}
{$WARN NO_CFG_FILE_FOUND ON}
{$WARN IMPLICIT_VARIANTS ON}
{$WARN UNICODE_TO_LOCALE ON}
{$WARN LOCALE_TO_UNICODE ON}
{$WARN IMAGEBASE_MULTIPLE ON}
{$WARN SUSPICIOUS_TYPECAST ON}
{$WARN PRIVATE_PROPACCESSOR ON}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN OPTION_TRUNCATED ON}
{$WARN WIDECHAR_REDUCED ON}
{$WARN DUPLICATES_IGNORED ON}
{$WARN UNIT_INIT_SEQ ON}
{$WARN LOCAL_PINVOKE ON}
{$WARN MESSAGE_DIRECTIVE ON}
{$WARN TYPEINFO_IMPLICITLY_ADDED ON}
{$WARN RLINK_WARNING ON}
{$WARN IMPLICIT_STRING_CAST ON}
{$WARN IMPLICIT_STRING_CAST_LOSS ON}
{$WARN EXPLICIT_STRING_CAST OFF}
{$WARN EXPLICIT_STRING_CAST_LOSS OFF}
{$WARN CVT_WCHAR_TO_ACHAR ON}
{$WARN CVT_NARROWING_STRING_LOST ON}
{$WARN CVT_ACHAR_TO_WCHAR ON}
{$WARN CVT_WIDENING_STRING_LOST ON}
{$WARN XML_WHITESPACE_NOT_ALLOWED ON}
{$WARN XML_UNKNOWN_ENTITY ON}
{$WARN XML_INVALID_NAME_START ON}
{$WARN XML_INVALID_NAME ON}
{$WARN XML_EXPECTED_CHARACTER ON}
{$WARN XML_CREF_NO_RESOLVE ON}
{$WARN XML_NO_PARM ON}
{$WARN XML_NO_MATCHING_PARM ON}
(*

  Letterpress

  Syntax Editor 3.0a.

  § Added dynamic highlighter (SynUniHighlighter, TSynUniSyn, Lantern);
    ( This addresses: 1641275 )
  § Added outlining (code folding) support;
    — Added outlining support for word wrap mode;
    — Added outlining hint for collapsed ranges;
    — Added gradual painting feature for gutter, based on fold range level;
    — Added dimming feature for block of text for selected level;
    — Fixed rendering bugs for outlining in word wrap mode;
    ( This addresses: 1965432, 1198350, 952274, 646927, 576867, 469756 )
  § Added intelligent line states display on gutter;
  § Added new trim trailing spaces behaviour, old one removed;
    ( This addresses: 810441, 1642122, 1767699 )
  § Added more smart auto-indent logic (takes into account also next lines);
  § Added support for painting with complex blended backgrounds;
  § Added new thin caret style just like standard Windows controls have;
  § Added support for dynamic caert size for ctBlock and ctHorizontalLine;
  § Added ability in EnsureCursorPosVisibleEx() to force to the middle even when
    caret is already visible in viewport;
    ( This addresses: 2940839 )
  $ Added support for unlimited bookmarks;
  § Fixed critical bug related to tabs & wide glyphs rendering together;
  § Fixed word wrap bug caused no multi-monitor systems & wide screens support;
    ( This addresses: 2928443, 2793116 )
  § Fixed undo and hard tab bugs when in normal and trimming modes;
  § Fixed auto-indentation bugs for lines with complex whitespace combinations;
    ( This addresses: 2896463 (looks like), 2354318, 1767699 )
  § Fixed smart tabs bug, behaviour rewritten;
    ( This addresses: 1907042 )
  § Fixed delete after EOL in eoScrollPastEOL mode bug;
  § Fixed block indentation bugs;
    — Added support for smart block indenting inside text (no use of tabs there);
  § Fixed block unindentation bugs;
    — Added support for smart unindenting inside text;
    ( This addresses: 1907555, 1882917, 1741282 )
  § Fixed smColumn insertion, deletion and copying behaviours (to affect only
    lines, not rows);
    — Fixed, with that, bugs related to copying and pasting smColumn way when
      in WordWrap mode;
    ( This addresses: 1041601, 1741282 )
  § Fixed bugs with smLine insertion and pasting;
    ( This addresses: 1814190 )
  § Fixed deletiong bug in ecDeleteWord command when executing on last line full
    of word break chars only;
    ( This addresses: 1755519 )
  § Fixed print preview painting bug;
    ( This addresses: 2278064 )
  § Fixed bug in overwrite mode;
    ( This addresses: 1757994 )
  § Whitespace painting no longer utilizes actual characters (font-independant);
    — Added nice dotted style for whitespace painting;
    — Added intelligent color choice for whitespace painting;
  § Paint routines significantly changed to be faster (now row-based);
  § Countless minor fixes and optimizations.

  Copyright 1998-2010, original authors and Garnet

*)

{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEdit.pas, released 2000-04-07.
The Original Code is based on mwCustomEdit.pas by Martin Waldenburg, part of
the mwEdit component suite.

Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.

Unicode translation by Maлl Hцrz. All Rights Reserved.

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

$Id: SynEdit.pas,v 1.386.2.75 2008/09/17 13:59:11 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net
-------------------------------------------------------------------------------}

unit SynEdit;

{$I SynEdit.inc}
{$UNDEF DEBUG}

interface

uses
  Controls, Contnrs, Graphics, Forms, StdCtrls, ExtCtrls, Windows, Messages,
  StdActns, Dialogs, Themes, Imm, SysUtils, Math, WideStrUtils, Classes,
  StrUtils,

  { SynEdit }
  SynUnicode, SynTextDrawer, SynEditTypes, SynEditKeyConst, SynEditMiscProcs,
  SynEditMiscClasses, SynEditTextBuffer, SynEditKeyCmds, SynEditHighlighter,
  SynEditKbdHandler, SynEditCodeFolding, RegularExpressions,

  { TBX }
  TBXUtils;

const
  { Maximum scroll range }
  MAX_SCROLL = 32767;

  { Max number of book / gutter marks returned from GetEditMarksForLine: that
    really should be enough }
  MAX_MARKS = 16;

type
	TBufferCoord  = SynEditTypes.TBufferCoord;
	TDisplayCoord = SynEditTypes.TDisplayCoord;

  TSmartCaret = record
    bMirror: Boolean;
    nIndex, nNestLevel, nOldCaretX: Integer;
    bcCaret, bcStart, bcEnd: TBufferCoord;
    sSearchPattern, sReplacePattern, sShellCommand: UTF8String;
    ePatternOptions: TRegexOptions;
  end;

  TSmartCarets = array of TSmartCaret;

  TSynEditCaretType = (ctVerticalLine, ctHorizontalLine);

  TSynBorderStyle = TBorderStyle;

  TSynStateFlag = (sfCaretChanged, sfScrollbarChanged, sfLinesChanging,
    sfIgnoreNextChar, sfCaretVisible, sfDblClicked, sfPossibleGutterClick,
    sfWaitForDragging, sfInsideRedo, sfGutterDragging, sfJustIndented);

  TSynStateFlags = set of TSynStateFlag;

  TSynEditorOption = (
    eoAlignedWrap,             // Indented word wrap
    eoHighlightMargin,         // Additional ackground highlight after margin
    eoWrapAgainstMargin,       // Use fRightMargin instead of client width as word wrap boundary
    eoWrapCentered,            // Center editing parts in viewport
    eoAutoIndent,              // Will indent the caret on new lines with the same amount of leading white space as the preceding line(s)
    eoAutoSizeMaxScrollWidth,  // Automatically resizes the MaxScrollWidth property when inserting text
    eoDragDropEditing,         // Allows you to select a block of text and drag it within the document to another location
    eoDropFiles,               // Allows the editor to accept OLE file drops
    eoEnhanceHomeKey,          // Enhances home key positioning, similar to visual studio
    eoEnhanceEndKey,           // Enhances End key positioning, similar to JDeveloper
    eoGroupUndo,               // When undoing/redoing actions, handle all continous changes of the same kind in one call instead undoing/redoing each command separately
    eoHalfPageScroll,          // When scrolling with page-up and page-down commands, only scroll a half page at a time
    eoHideShowScrollbars,      // if enabled, then the scrollbars will only show when necessary.  If you have ScrollPastEOL, then it the horizontal bar will always be there (it uses MaxLength instead)
    eoKeepCaretX,              // When moving through lines w/o Cursor Past EOL, keeps the X position of the cursor
    eoScrollHintFollows,       // The scroll hint follows the mouse when scrolling vertically
    eoScrollPastEof,           // Allows the cursor to go past the end of file marker
    eoScrollPastEol,           // Allows the cursor to go past the last character into the white space at the end of a line
    eoShowScrollHint,          // Shows a hint of the visible line numbers when scrolling vertically
    eoShowSpecialChars,        // Shows the special characters
    eoShowSpecialcharsInSelection, // Shows invisibles in selection only
    eoShowEolSpecialChar,      // Includes end of line character in specials
    eoSmartTabDelete,          // Similar to Smart Tabs, but when you delete characters
    eoSmartTabs,               // When tabbing, the cursor will go to the next non-white space character of the previous line
    eoSpecialLineDefaultFg,    // Disables the foreground text color override when using the OnSpecialLineColor event
    eoTabIndent,               // When active <Tab> and <Shift><Tab> act as block indent, unindent when text is selected
    eoTabsToSpaces,            // Converts a tab character to a specified number of space characters
    eoTrimTrailingSpaces,      // Spaces at the end of lines will be trimmed and not saved
    eoAdaptiveSpecialLine,     // Active line color is choosed by darkening (or lightening) current editor background
    eoAdaptiveSelection,       // Selection background (except active line) is choosen by darkening or lightening editor background
    eoAdaptiveSelectionSpecialLine, // Draw active line on selection
    eoFontForceRoman,          // Ultimately throws away bold style from any token attribute
    eoFontForceBold            // Includes bold style in any token attribute
  );

  TSynEditorOptions = set of TSynEditorOption;

  TScrollHintFormat = (shfTopLineOnly, shfTopToBottom);

  TSynReplaceAction = (raCancel, raSkip, raReplace, raReplaceAll);

  ESynEditError = class(ESynError);

  TDropFilesEvent = procedure(Sender: TObject; X, Y: Integer; AFiles: TStrings)
    of object;

  THookedCommandEvent = procedure(Sender: TObject; AfterProcessing: Boolean;
    var Handled: Boolean; var Command: TSynEditorCommand; var AChar: WideChar;
    Data: pointer; HandlerData: pointer) of object;

  TPaintEvent = procedure(Sender: TObject; ACanvas: TCanvas) of object;

  TProcessCommandEvent = procedure(Sender: TObject;
    var Command: TSynEditorCommand; var AChar: WideChar; Data: pointer) of object;

  TReplaceTextEvent = procedure(Sender: TObject; const ASearch, AReplace:
    UnicodeString; Line, Column: Integer; var Action: TSynReplaceAction) of object;

  TSpecialLineColorsEvent = procedure(Sender: TObject; Line: Integer;
    var Special: Boolean; var FG, BG: TColor) of object;

  TTransientType = (ttBefore, ttAfter);

  TPaintTransient = procedure(Sender: TObject; Canvas: TCanvas;
    TransientType: TTransientType) of object;

  TScrollEvent = procedure(Sender: TObject; ScrollBar: TScrollBarKind) of object;

  TGutterGetTextEvent = procedure(Sender: TObject; aLine: Integer;
    var aText: UnicodeString) of object;

  TGutterPaintEvent = procedure(Sender: TObject; aLine: Integer;
    X, Y: Integer) of object;

const
  SYNEDIT_DEFAULT_OPTIONS = [eoEnhanceHomeKey, eoEnhanceEndKey, eoGroupUndo,
    eoDragDropEditing, eoShowScrollHint, eoTabIndent, eoScrollHintFollows,
    eoHideShowScrollbars, eoDropFiles, eoKeepCaretX];

type
  TSynStatusChange = (scAll, scCaretX, scCaretY, scLeftChar, scTopLine,
    scInsertMode, scModified, scSelection, scReadOnly, scRightEdge);

  TSynStatusChanges = set of TSynStatusChange;

  TContextHelpEvent = procedure(Sender: TObject; word: UnicodeString)
    of object;

  TStatusChangeEvent = procedure(Sender: TObject; Changes: TSynStatusChanges)
    of object;

  TMouseCursorEvent = procedure(Sender: TObject; const aLineCharPos: TBufferCoord;
    var aCursor: TCursor) of object;

  TCustomSynEdit = class;

  TSynEditMark = class
  protected
    fLine, fChar, fImage: Integer;
    fEdit: TCustomSynEdit;
    fVisible: Boolean;
    fBookmark: Integer;
    function GetEdit: TCustomSynEdit; virtual;
    procedure SetChar(const Value: Integer); virtual;
    procedure SetImage(const Value: Integer); virtual;
    procedure SetLine(const Value: Integer); virtual;
    procedure SetVisible(const Value: Boolean);
    function GetIsBookmark: Boolean;
  public
    constructor Create(AOwner: TCustomSynEdit);
    property Line: Integer read fLine write SetLine;
    property Char: Integer read fChar write SetChar;
    property Edit: TCustomSynEdit read fEdit;
    property ImageIndex: Integer read fImage write SetImage;
    property BookmarkNumber: Integer read fBookmark write fBookmark;
    property Visible: Boolean read fVisible write SetVisible;
    property IsBookmark: Boolean read GetIsBookmark;
  end;

  TPlaceMarkEvent = procedure(Sender: TObject; var Mark: TSynEditMark)
    of object;

  TSynEditMarks = array[1..MAX_MARKS] of TSynEditMark;

  { A list of mark objects.
    Each object cause a litle picture to be drawn in the gutter }
  TSynEditMarkList = class(TObjectList)            // It makes more sence to derive from TObjectList,
  protected                                        // as it automatically frees its members
    fEdit: TCustomSynEdit;
    fOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TSynEditMark;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure SetItem(Index: Integer; Item: TSynEditMark);
    property OwnsObjects;                          // This is to hide the inherited property,
  public                                           // because TSynEditMarkList always owns the marks
    constructor Create(AOwner: TCustomSynEdit);
    function First: TSynEditMark;
    function Last: TSynEditMark;
    function Extract(Item: TSynEditMark): TSynEditMark;
    procedure ClearLine(Line: Integer);
    procedure GetMarksForLine(ALine: Integer; var AMarks: TSynEditMarks);
    procedure Place(AMark: TSynEditMark);
  public
    property Items[Index: Integer]: TSynEditMark read GetItem write SetItem; default;
    property Edit: TCustomSynEdit read fEdit;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGutterClickEvent = procedure(Sender: TObject; Button: TMouseButton;
    X, Y, Line: Integer; Mark: TSynEditMark) of object;

  { AIndex parameters of line notifications are 0-based.
    ARow parameter of GetRowLength() is 1-based }
  ISynEditBufferPlugin = interface

    { Conversion methods (caret) }
    function BufferToDisplayPos(const aPos: TBufferCoord): TDisplayCoord;
    function BufferToRealDisplayPos(const aPos: TBufferCoord): TDisplayCoord;
    function DisplayToBufferPos(const aPos: TDisplayCoord): TBufferCoord;
    function RealDisplayToBufferPos(const aPos: TDisplayCoord): TBufferCoord;

    { Conversion methods (lines only) }
    function LineToRow(const ALine: Integer): Integer;
    function LineToRealRow(const ALine: Integer): Integer;
    function RowToLine(const ARow: Integer): Integer;

    { Row methods }

    { With code folding we will need a line associated with this row because
      we will need to skip folded up to this line rows }
    function GetRowLength(ARow, ALine: Integer): Integer;
    function GetRowCount(ALine: Integer): Integer; overload;
    function GetRowCount: Integer; overload;

    { Plugin notifications }
    function LinesInserted(AIndex, ACount: Integer): Integer;
    function LinesDeleted(AIndex, ACount: Integer): Integer;
    function LinesPutted(AIndex, ACount: Integer): Integer;
    function LinesFolded(AFromLine, AToLine: Integer): Integer;
    function LinesUnFolded(AFromLine, AToLine: Integer): Integer;

    { Font or size change }
    procedure DisplayChanged;

    { Pretty clear }
    procedure Reset;
  end;

  TSynEditPlugin = class(TObject)
  private
    fOwner: TCustomSynEdit;
  protected
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: Integer); virtual; abstract;
    procedure LinesInserted(FirstLine, Count: Integer); virtual; abstract;
    procedure LinesDeleted(FirstLine, Count: Integer); virtual; abstract;
  protected
    property Editor: TCustomSynEdit read fOwner;
  public
    constructor Create(AOwner: TCustomSynEdit);
    destructor Destroy; override;
  end;

  TSynEditCodeFoldingPlugin = class(TSynEditPlugin)
  protected
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: Integer); override;
    procedure LinesInserted(FirstLine, Count: integer); override;
    procedure LinesDeleted(FirstLine, Count: Integer); override;
  end;

  TCustomSynEditSearchNotFoundEvent = procedure(Sender: TObject;
    FindText: UnicodeString) of object;

  TSpellCheckEvent = function(Sender: TObject; const Word: UnicodeString): Boolean of object;

  TCustomSynEdit = class(TCustomControl)
  protected
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMCaptureChanged(var Msg: TMessage); message WM_CAPTURECHANGED;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMClear(var Msg: TMessage); message WM_CLEAR;
    procedure WMCopy(var Message: TMessage); message WM_COPY;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMGetText(var Msg: TWMGetText); message WM_GETTEXT;
    procedure WMGetTextLength(var Msg: TWMGetTextLength); message WM_GETTEXTLENGTH;
    procedure WMHScroll(var Msg: TWMScroll); message WM_HSCROLL;
    procedure WMImeChar(var Msg: TMessage); message WM_IME_CHAR;
    procedure WMImeComposition(var Msg: TMessage); message WM_IME_COMPOSITION;
    procedure WMImeNotify(var Msg: TMessage); message WM_IME_NOTIFY;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSetText(var Msg: TWMSetText); message WM_SETTEXT;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMUndo(var Msg: TMessage); message WM_UNDO;
    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
//    procedure WMLMouseButtonDown(var Msg: TMessage); message WM_LBUTTONDOWN;
  private
    fAllFoldRanges: TSynEditAllFoldRanges;
    fCodeFolding: TSynCodeFolding;
    fCodeFoldingPlugin: TSynEditCodeFoldingPlugin;
    fSmartCarets: TSmartCarets;
    fSmartCaretsUpdating: Boolean;

    fCurrCharWidth: Integer;
    fOldGutterWidth: Integer;
    fAlwaysShowCaret: Boolean;
    fBlockBegin: TBufferCoord;
    fBlockEnd: TBufferCoord;
    fCaretX: Integer;
    fLastCaretX: integer;
    fCaretY: Integer;
    fCharsInWindow: Integer;
    fCharWidth: Integer;
    fFontDummy: TFont;
    fInserting: Boolean;
    fLines: TSynEditStringList;
    fOrigLines: TSynEditStringList;
    fOrigUndoList: TSynEditUndoList;
    fOrigRedoList: TSynEditUndoList;
    fLinesInWindow: Integer;
    fLeftChar: Integer;
    fMaxScrollWidth: Integer;
    fPaintLock: Integer;
    fReadOnly: Boolean;
    fRightEdge: Integer;
    fRightEdgeColor: TColor;
    fRightEdgeShow: Boolean;
    fScrollHintColor: TColor;
    fScrollHintFormat: TScrollHintFormat;
    FScrollBars: TScrollStyle;
    fTextHeight: Integer;
    fTextOffset: Integer;
    fCenterOffset: Integer;
    fTopLine: Integer;
    fHighlighter: TSynCustomHighlighter;
    fSelectedColor: TSynSelectedColor;
    fActiveLineBG: TColor;
    fActiveLineFG: TColor;
    fUndoList: TSynEditUndoList;
    fRedoList: TSynEditUndoList;
    fMouseDownX: Integer;
    fMouseDownY: Integer;
    fLastMouseX: Integer;
    fBookMarkOpt: TSynBookMarkOpt;
    fBorderStyle: TSynBorderStyle;
    fHideSelection: Boolean;
    fMouseWheelAccumulator: Integer;
    fOverwriteCaret: TSynEditCaretType;
    fInsertCaret: TSynEditCaretType;
    fCaretOffset: TPoint;
    fKeyStrokes: TSynEditKeyStrokes;
    fModified: Boolean;
    fMarkList: TSynEditMarkList;
    fExtraLineSpacing: Integer;
    fSelectionMode: TSynSelectionMode;
    fActiveSelectionMode: TSynSelectionMode;
    fWantReturns: Boolean;
    fWantTabs: Boolean;
    fWordWrapPlugin: ISynEditBufferPlugin;
    fCaretAtEOL: Boolean;
    fGutter: TSynGutter;
    fTabWidth: Integer;
    fTextDrawer: TheTextDrawer;
    fInvalidateRect: TRect;
    fStateFlags: TSynStateFlags;
    fOptions: TSynEditorOptions;
    fStatusChanges: TSynStatusChanges;
    fLastKey: word;
    fLastShiftState: TSynShiftState;
    fSearchEngine: TSynEditSearchCustom;
    fHookedCommandHandlers: TObjectList;
    fKbdHandler: TSynEditKbdHandler;
    fFocusList: TList;
    fPlugins: TObjectList;
    fScrollTimer: TTimer;
    fOutliningTimer: TTimer;
    fLineSelectionTimer: TTimer;
    fScrollDeltaX, fScrollDeltaY: Integer;
    fDimmed, fRepaintAfterDimNeeded: Boolean;
    fDimFirstLine, fDimLastLine: Integer;

    { Event handlers }
    fOnChange: TNotifyEvent;
    fOnClearMark: TPlaceMarkEvent;
    fOnCommandProcessed: TProcessCommandEvent;
    fOnDropFiles: TDropFilesEvent;
    fOnGutterClick: TGutterClickEvent;
    FOnKeyPressW: TKeyPressWEvent;
    fOnMouseCursor: TMouseCursorEvent;
    fOnPaint: TPaintEvent;
    fOnPlaceMark: TPlaceMarkEvent;
    fOnProcessCommand: TProcessCommandEvent;
    fOnProcessUserCommand: TProcessCommandEvent;
    fOnReplaceText: TReplaceTextEvent;
    fOnSpecialLineColors: TSpecialLineColorsEvent;
    fOnContextHelp: TContextHelpEvent;
    fOnPaintTransient: TPaintTransient;
    fOnScroll: TScrollEvent;
    fOnGutterGetText: TGutterGetTextEvent;
    fOnGutterPaint: TGutterPaintEvent;

    { External list events }
    fOnLinesInserted: TStringListChangeEvent;
    fOnLinesPutted: TStringListChangeEvent;
    fOnLinesDeleted: TStringListChangeEvent;
    fOnLinesRecognized: TStringListChangeEvent;

    fOnStatusChange: TStatusChangeEvent;
    FPaintTransientLock: Integer;
    FIsScrolling: Boolean;
    fRightEdgeMoving, fCodeFoldingHint, fMouseAtRightEdge: Boolean;

    fChainListCleared: TNotifyEvent;
    fChainListDeleted: TStringListChangeEvent;
    fChainListInserted: TStringListChangeEvent;
    fChainListPutted: TStringListChangeEvent;
    fChainLinesChanging: TNotifyEvent;
    fChainLinesChanged: TNotifyEvent;
    fChainedEditor: TCustomSynEdit;
    fChainUndoAdded: TNotifyEvent;
    fChainRedoAdded: TNotifyEvent;
    fCmdDrop: Boolean;

    fOnSpellCheck: TSpellCheckEvent;

    { Built-in search }
    fSearchNotFound: TCustomSynEditSearchNotFoundEvent;
    OnFindBeforeSearch: TNotifyEvent;
    OnReplaceBeforeSearch: TNotifyEvent;
    OnCloseBeforeSearch: TNotifyEvent;
    SelStartBeforeSearch: integer;
    SelLengthBeforeSearch: integer;

    { As it says }
    FWindowProducedMessage: Boolean;

    fSnippet, fColumn, fSelections: Boolean;
    fCurrCaret: Integer;

    fSpellCheck: Boolean;
    fBreakWhitespace: Boolean;
    fCarets: TSmartCarets;
    fRoutineTerminated: Boolean;
    fRoutineCompleted: Boolean;
    fInsertingMirrors: Boolean;
    fRoutineOutput: UnicodeString;
    fUndoRedo: Boolean;
    fSnippetDir: UnicodeString;

    fOutliningAttr,
    fDiffAddedAttr,
    fDiffRemovedAttr,
    fDiffChangedAttr: TSynHighlighterAttributes;

    procedure ExpandCollapsedLine(const ALine: Integer);
    procedure ExpandCollapsedLines(const AFirst, ALast: Integer);

    function DoTrimTrailingSpaces(const S: UnicodeString;
      ATrim: Boolean): UnicodeString; overload;
    function DoTrimTrailingSpaces(ALine: Integer): Integer; overload;
    function GetCollapseMarkRect(FoldRange: TSynEditFoldRange;
    	Row: Integer; Line: Integer; Indent: Integer = 0): TRect;
    procedure SetCodeFolding(Value: TSynCodeFolding);
    procedure CodeFoldingOnChange(Event: TSynCodeFoldingChanges);

    function GetExpandLines: TSynEditStringList;
    procedure SetExpandLines(const Value: TSynEditStringList);

    procedure BookMarkOptionsChanged(Sender: TObject);
    procedure ComputeCaret(X, Y: Integer);
    function InternalComputeCaret(const X, Y: Integer): TBufferCoord;
    procedure ComputeScroll(X, Y: Integer);
    procedure DoBlockIndent;
    procedure DoBlockUnindent;
    procedure DoHomeKey(Selection:boolean);
    procedure DoEndKey(Selection: Boolean);
    procedure DoLinesDeleted(FirstLine, Count: integer);
    procedure DoLinesInserted(FirstLine, Count: integer);
    procedure DoShiftTabKey;
    procedure DoTabKey(AHardTab: Boolean = False);
    procedure SynFontChanged(Sender: TObject);
    function FindHookedCmdEvent(AHandlerProc: THookedCommandEvent): integer;

    function GetBlockBegin: TBufferCoord;
    function GetBlockEnd: TBufferCoord;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCaretXY: TBufferCoord;
    function GetDisplayX: Integer;
    function GetDisplayY: Integer;
    function GetDisplayXY: TDisplayCoord;
    function GetDisplayLineCount: Integer;
    function GetFont: TFont;
    function GetHookedCommandHandlersCount: Integer;
    function GetLineText: UnicodeString;
    function GetMaxUndo: Integer;
    function GetOptions: TSynEditorOptions;
    function GetSelAvail: Boolean;
    function GetSelTabBlock: Boolean;
    function GetSelTabLine: Boolean;
    function GetSelText: UnicodeString;
    function SynGetText: UnicodeString;
    function GetWordAtCursor: UnicodeString;
    function GetWordAtMouse: UnicodeString;
    function GetWordWrap: Boolean;
    function GetCurrentScope: UnicodeString;
    function GetCaretByIndex(const Index: Integer;
      const SearchMirrors: Boolean = False): TBufferCoord;
    function GetOriginalSelectionIndex: Integer;
    function GetAtLeastOneBlockOfSelection: Boolean;

    procedure GutterChanged(Sender: TObject);
    procedure InsertBlock(const BB, BE: TBufferCoord; ChangeStr: PWideChar; AddToUndoList: Boolean);
    function LeftSpaces(const Line: UnicodeString): Integer;
    function LeftSpacesEx(const Line: UnicodeString; WantTabs: Boolean): Integer;
    function GetLeftSpacing(CharCount: Integer; WantTabs: Boolean): UnicodeString;
    procedure LinesChanging(Sender: TObject);
    procedure MoveCaretAndSelection(const ptBefore, ptAfter: TBufferCoord;
      SelectionCommand: Boolean);

    procedure MoveSelections(const DX, DY: Integer;
      const SelectionCommand: Boolean);
    procedure MoveCaretHorz(DX: Integer; SelectionCommand: Boolean);
    procedure MoveCaretVert(DY: Integer; SelectionCommand: Boolean);
    procedure PluginsAfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: Integer);

    procedure ReadAddedKeystrokes(Reader: TReader);
    procedure ReadRemovedKeystrokes(Reader: TReader);
    function ScanFrom(Index: Integer): Integer;
    procedure ScrollTimerHandler(Sender: TObject);
    procedure FoldingTimerHandler(Sender: TObject);
    procedure LineSelectionTimerHandle(Sender: TObject);
    procedure SelectedColorsChanged(Sender: TObject);
    procedure SetBlockBegin(Value: TBufferCoord);
    procedure SetBlockEnd(Value: TBufferCoord);
    procedure SetBorderStyle(Value: TSynBorderStyle);
    procedure SetCaretX(Value: Integer);
    procedure SetCaretY(Value: Integer);
    procedure InternalSetCaretX(Value: Integer);
    procedure InternalSetCaretY(Value: Integer);
    procedure SetInternalDisplayXY(const aPos: TDisplayCoord);
    procedure SetActiveLineBG(Value: TColor);
    procedure SetActiveLineFG(Value: TColor);
    procedure SetExtraLineSpacing(const Value: Integer);
    procedure SetFont(const Value: TFont);
    procedure SetGutter(const Value: TSynGutter);
    procedure SetGutterWidth(Value: Integer);
    procedure SetHideSelection(const Value: Boolean);
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure SetInsertCaret(const Value: TSynEditCaretType);
    procedure SetInsertMode(const Value: Boolean);
    procedure SetKeystrokes(const Value: TSynEditKeyStrokes);
    procedure SetLeftChar(Value: Integer);
    procedure SetLines(Value: TSynEditStringList{TUnicodeStrings});
    procedure SetLineText(Value: UnicodeString);
    procedure SetMaxScrollWidth(Value: Integer);
    procedure SetMaxUndo(const Value: Integer);
    procedure SetModified(Value: Boolean);
    procedure SetOptions(Value: TSynEditorOptions);
    procedure SetOverwriteCaret(const Value: TSynEditCaretType);
    procedure SetRightEdge(Value: Integer);
    procedure SetRightEdgeColor(Value: TColor);
    procedure SetRightEdgeShow(Value: Boolean);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetSearchEngine(Value: TSynEditSearchCustom);
    procedure SetSelectionMode(const Value: TSynSelectionMode);
    procedure SetActiveSelectionMode(const Value: TSynSelectionMode);
    procedure SetSelTextExternal(const Value: UnicodeString);
    procedure SetTabWidth(Value: Integer);
    procedure SynSetText(const Value: UnicodeString);
    procedure SetTopLine(Value: Integer);
    procedure SetWordWrap(const Value: Boolean);
    procedure SizeOrFontChanged(bFont: boolean);
    procedure UpdateModifiedStatus;
    procedure UndoRedoAdded(Sender: TObject);
    procedure UpdateLastCaretX;
    procedure UpdateScrollBars;
    procedure WriteAddedKeystrokes(Writer: TWriter);
    procedure WriteRemovedKeystrokes(Writer: TWriter);
    procedure SetSpellCheck(Value: Boolean);

    procedure UpdateCharsInWindow;
  protected
    FIgnoreNextChar: Boolean;
    FCharCodeString: string;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure InvalidateRect(const aRect: TRect; aErase: Boolean); virtual;
    procedure DblClick; override;
    procedure DecPaintLock;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoChange; virtual;
    procedure DoKeyPressW(var Message: TWMKey);
    procedure DragCanceled; override;
    procedure DragOver(Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    function GetReadOnly: boolean; virtual;
    procedure HighlighterAttrChanged(Sender: TObject);
    procedure IncPaintLock;
    procedure InitializeCaret(DoUpdate: Boolean = True);
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyPressW(var Key: WideChar); virtual;
    procedure LinesChanged(Sender: TObject); virtual;
    procedure ListCleared(Sender: TObject);
    procedure ListDeleted(Sender: TObject; aIndex: Integer; aCount: Integer);
    procedure ListInserted(Sender: TObject; Index: Integer; aCount: Integer);
    procedure ListBeforePutted(Sender: TObject; Index: Integer;
      ACount: integer);
    procedure ListPutted(Sender: TObject; Index: Integer; aCount: Integer);

    { Helper procs to chain list commands }
    procedure ChainListCleared(Sender: TObject);
    procedure ChainListDeleted(Sender: TObject; aIndex: Integer; aCount: Integer);
    procedure ChainListInserted(Sender: TObject; aIndex: Integer; aCount: Integer);
    procedure ChainListPutted(Sender: TObject; aIndex: Integer; aCount: Integer);
    procedure ChainLinesChanging(Sender: TObject);
    procedure ChainLinesChanged(Sender: TObject);
    procedure ChainUndoRedoAdded(Sender: TObject);
    procedure ScanRanges;
    procedure Loaded; override;
    procedure MarkListChange(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;

    procedure AddSmartCaret(const C, B, E: TBufferCoord);
    procedure RemoveSmartCaret(const Index: Integer);
    procedure NormalizeSelections;

    procedure NotifyHookedCommandHandlers(AfterProcessing: Boolean;
      var Command: TSynEditorCommand; var AChar: WideChar; Data: pointer); virtual;
    procedure Paint; override;
    procedure PaintGutter(const AClip: TRect; aFirstRow,
      aLastRow: Integer); virtual;
    procedure PaintTextLines(AClip: TRect; aFirstRow, aLastRow,
      FirstCol, LastCol: Integer); virtual;
    procedure RecalcCharExtent;
    procedure RedoItem;
    procedure InternalSetCaretXY(const Value: TBufferCoord); virtual;
    procedure SetCaretXY(const Value: TBufferCoord); virtual;
    procedure SetCaretXYEx(CallEnsureCursorPos: Boolean; Value: TBufferCoord); virtual;
    procedure SetName(const Value: TComponentName); override;
    procedure SetReadOnly(Value: boolean); virtual;
    procedure SetWantReturns(Value: Boolean);
    procedure SetSelTextPrimitive(const Value: UnicodeString);
    procedure SetSelTextPrimitiveEx(PasteMode: TSynSelectionMode; Value: PChar;
      AddToUndoList: Boolean; CanTrim: Boolean = False; ChangeLines: Boolean = True);
    procedure SetWantTabs(Value: Boolean);
    procedure StatusChanged(AChanges: TSynStatusChanges);
    procedure SetBreakWhitespace(Value: Boolean);
    procedure SetSelectedTextEmpty(const ChangeStr: UnicodeString = '');

    { If the translations requires Data, memory will be allocated for it via a
      GetMem call. The client must call FreeMem on Data if it is not nil }
    function TranslateKeyCode(Code: word; Shift: TSynShiftState;
      var Data: pointer): TSynEditorCommand;
    procedure UndoItem;
    procedure JoinPreviousLine(const S: UnicodeString);
    procedure DeleteCertainLine(const Line: Integer);
    procedure UpdateMouseCursor; virtual;

    function ValidSnippetCmd(Command: TSynEditorCommand): Boolean;
    procedure EnterColumnMode;

    function RunSnippetShellCode(const Command: UnicodeString;
      FromMirror: Boolean; const CurrentVar: UnicodeString = ''): UnicodeString;
    procedure RoutineCommandCompleted(Sender: TObject;
      const Data: UTF8String);

    function CurrentSnippetCaret(Char: Integer = 0; Line: Integer = 0;
      ForceCheck: Boolean = False): Integer;
    procedure InsertedInSnippet(const X, Y, DX, DY: Integer);
    procedure MirrorCommand(Command: Integer; AChar: Char; AData: Pointer;
      CommandPos, CommandBegin, CommandEnd: TBufferCoord; AText: PChar);

    procedure NextSnippetVar(First: Boolean = False);
    procedure PrevSnippetVar;
    procedure CancelSnippet(Check: Boolean = False);
  protected
    procedure HideCaret;
    procedure ShowCaret;
    procedure DoOnClearBookmark(var Mark: TSynEditMark); virtual;
    procedure DoOnCommandProcessed(Command: TSynEditorCommand; AChar: WideChar;
      Data: pointer); virtual;

    { No method DoOnDropFiles, intercept the WM_DROPFILES instead }
    procedure DoOnGutterClick(Button: TMouseButton; X, Y: Integer); virtual;
    procedure DoOnPaint; virtual;
    procedure DoOnPaintTransientEx(TransientType: TTransientType; Lock: Boolean); virtual;
    procedure DoOnPaintTransient(TransientType: TTransientType); virtual;

    procedure DoOnPlaceMark(var Mark: TSynEditMark); virtual;
    procedure DoOnProcessCommand(var Command: TSynEditorCommand;
      var AChar: WideChar; Data: pointer); virtual;
    function DoOnReplaceText(const ASearch, AReplace: UnicodeString;
      Line, Column: Integer): TSynReplaceAction; virtual;
    function DoOnSpecialLineColors(Line: Integer;
      var Foreground, Background: TColor): Boolean; virtual;
    procedure DoOnStatusChange(Changes: TSynStatusChanges); virtual;
    function GetSelEnd: integer;
    function GetSelStart: integer;
    function GetSelLength: integer;
    procedure SetSelEnd(const Value: integer);
    procedure SetSelStart(const Value: integer);
    procedure SetSelLength(const Value: integer);
    procedure SetAlwaysShowCaret(const Value: Boolean);
    procedure LinesHookChanged;
    property InternalCaretX: Integer write InternalSetCaretX;
    property InternalCaretY: Integer write InternalSetCaretY;
    property InternalCaretXY: TBufferCoord write InternalSetCaretXY;
  public
    fBookMarks: array of TSynEditMark;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ExecuteSnippet(const S, Directory: UnicodeString);
    procedure EditEachLineInSelection;
    procedure SyncEditLines;

    procedure UpdateLongestLine;

    function IsAllWhiteUpToCaret(const Ln: UnicodeString; Border: Integer = 0): Boolean;
    function IsAllTabsUpToCaret(const Ln: UnicodeString; Border: Integer = 0): Boolean;

    property MultipleSelections: Boolean read fSelections;
    property OutliningAttr: TSynHighlighterAttributes read fOutliningAttr;
    property DiffAddedAttr: TSynHighlighterAttributes read fDiffAddedAttr;
    property DiffRemovedAttr: TSynHighlighterAttributes read fDiffRemovedAttr;
    property DiffChangedAttr: TSynHighlighterAttributes read fDiffChangedAttr;

    property AllFoldRanges: TSynEditAllFoldRanges read fAllFoldRanges;
    property Canvas;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelEnd: Integer read GetSelEnd write SetSelEnd;
    property AlwaysShowCaret: Boolean read FAlwaysShowCaret
      write SetAlwaysShowCaret;

    function FoldRangeForLine(Line: Integer): TSynEditFoldRange;
    function FoldRangeForLineTo(Line: Integer): TSynEditFoldRange;

    procedure UpdateCaret;
    procedure AddKey(Command: TSynEditorCommand; Key1: Word; SS1: TSynShiftState;
      Key2: Word = 0; SS2: TSynShiftState = []);
    procedure BeginUndoBlock;
    procedure BeginUpdate;
    function CaretInView: Boolean;
    function CharIndexToRowCol(Index: Integer): TBufferCoord;
    procedure Clear;
    procedure ClearAll;
    procedure ClearBookMark(BookMark: Integer);
    procedure ClearSelection;
    procedure CommandProcessor(Command: TSynEditorCommand; AChar: Char;
      Data: Pointer); virtual;
    procedure ClearUndo;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure DoCopyToClipboard(const SText: UnicodeString);
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure EndUndoBlock;
    procedure EndUpdate;
    procedure EnsureCursorPosVisible;
    procedure EnsureCursorPosVisibleEx(ForceToMiddle: Boolean;
      EvenIfVisible: Boolean = False);
    procedure ExecuteCommand(Command: TSynEditorCommand; AChar: Char;
      AData: Pointer); virtual;
    function GetBookMark(BookMark: Integer; var X, Y: Integer): Boolean;

    // § Garnet
    function GetHighlighterAttriAtRowCol(const XY: TBufferCoord;
      var Attri: TSynHighlighterAttributes): Boolean; overload;
    function GetHighlighterAttriAtRowCol(const XY: TBufferCoord; var Token: UnicodeString;
      var Attri: TSynHighlighterAttributes): Boolean; overload;
    // § Garnet

    function GetHighlighterAttriAtRowColEx(const XY: TBufferCoord; var Token: UnicodeString;
      var TokenType, Start: Integer;
      var Attri: TSynHighlighterAttributes): boolean;
    function GetPositionOfMouse(out aPos: TBufferCoord): Boolean;
    function GetWordAtRowCol(XY: TBufferCoord): UnicodeString;
    function GetCharAtRowCol(XY: TBufferCoord): Char;

    procedure SetWordAtRowCol(XY: TBufferCoord; const Value: UnicodeString);
    procedure SetCharAtRowCol(XY: TBufferCoord; Value: Char);

    procedure GotoBookMark(BookMark: Integer); virtual;
    procedure GotoLineAndCenter(ALine: Integer); virtual;
    function IsIdentChar(AChar: Char): Boolean; virtual;
    function IsWordBreakChar(AChar: Char): Boolean; virtual;

    { Paint methods }
    procedure InvalidateGutter;
    procedure InvalidateSelection;

    { Line-based }
    procedure InvalidateGutterLine(ALine: integer);
    procedure InvalidateGutterLines(AFirstLine, ALastLine: integer);
    procedure InvalidateLine(ALine: integer);
    procedure InvalidateLines(AFirstLine, ALastLine: integer);

    { Row-based }
    procedure InvalidateGutterRow(ARow: integer);
    procedure InvalidateGutterRows(AFirstRow, ALastRow: integer);
    procedure InvalidateRow(ARow: integer);
    procedure InvalidateRows(AFirstRow, ALastRow: integer);

    function IsBookmark(BookMark: Integer): Boolean;
    function IsPointInSelection(const Value: TBufferCoord): Boolean;
    procedure LockUndo;

    function BufferToDisplayPos(const P: TBufferCoord): TDisplayCoord;
    function BufferToRealDisplayPos(const P: TBufferCoord): TDisplayCoord;
    function DisplayToBufferPos(const P: TDisplayCoord): TBufferCoord;
    function RealDisplayToBufferPos(const P: TDisplayCoord): TBufferCoord;

    function LineToRow(ALine: Integer): Integer;
    function RowToLine(ARow: Integer): Integer;

    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure PasteFromClipboard;

    function NextWordPos: TBufferCoord; virtual;
    function NextWordPosEx(XY: TBufferCoord): TBufferCoord; virtual;
    function WordStart: TBufferCoord; virtual;
    function WordStartEx(const XY: TBufferCoord): TBufferCoord; virtual;
    function WordEnd: TBufferCoord; virtual;
    function WordEndEx(const XY: TBufferCoord): TBufferCoord; virtual;
    function PrevWordPos: TBufferCoord; virtual;
    function PrevWordPosEx(XY: TBufferCoord): TBufferCoord; virtual;

    function PixelsToRowColumn(AX, AY: Integer): TDisplayCoord;
    function PixelsToNearestRowColumn(AX, AY: Integer): TDisplayCoord;
    procedure Redo;
    procedure RegisterCommandHandler(const AHandlerProc: THookedCommandEvent;
      AHandlerData: pointer);
    function RowColumnToPixels(const RowCol: TDisplayCoord): TPoint;
    function RowColToCharIndex(RowCol: TBufferCoord): Integer;
    function SearchReplace(const ASearch, AReplace: UnicodeString;
      AOptions: TSynSearchOptions): Integer;
    procedure SelectAll;
    procedure SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
    procedure SetCaretAndSelection(const ptCaret, ptBefore,
      ptAfter: TBufferCoord; aCaret: Integer = -1);
    procedure SetDefaultKeystrokes; virtual;
    procedure SetSelWord;
    procedure SetWordBlock(Value: TBufferCoord);
    procedure Undo;
    procedure UnlockUndo;
    procedure UnregisterCommandHandler(AHandlerProc: THookedCommandEvent);

    procedure UpdateWordWrapHiddenOffsets;

//    procedure SetFocus; override;

    procedure AddKeyUpHandler(aHandler: TKeyEvent);
    procedure RemoveKeyUpHandler(aHandler: TKeyEvent);
    procedure AddKeyDownHandler(aHandler: TSynKeyEvent);
    procedure RemoveKeyDownHandler(aHandler: TSynKeyEvent);
    procedure AddKeyPressHandler(aHandler: TKeyPressWEvent);
    procedure RemoveKeyPressHandler(aHandler: TKeyPressWEvent);
    procedure AddFocusControl(aControl: TWinControl);
    procedure RemoveFocusControl(aControl: TWinControl);
    procedure AddMouseDownHandler(aHandler: TMouseEvent);
    procedure RemoveMouseDownHandler(aHandler: TMouseEvent);
    procedure AddMouseUpHandler(aHandler: TMouseEvent);
    procedure RemoveMouseUpHandler(aHandler: TMouseEvent);
    procedure AddMouseCursorHandler(aHandler: TMouseCursorEvent);
    procedure RemoveMouseCursorHandler(aHandler: TMouseCursorEvent);

//    procedure WndProc(var Msg: TMessage); override;

    procedure SetLinesPointer(ASynEdit: TCustomSynEdit);
    procedure RemoveLinesPointer;
    procedure HookTextBuffer(aBuffer: TSynEditStringList;
      aUndo, aRedo: TSynEditUndoList);
    procedure UnHookTextBuffer;

    //### Code Folding ###
    procedure CollapseAll;
    procedure InitCodeFolding;
    procedure UncollapseAll;
    function GetRealLineNumber(aLine: Integer): Integer;
    function GetUnRealLineNumber(aLine: Integer): Integer;
    function IsLineVisible(ALine: Integer): Boolean;
    //### End Code Folding ###

    function FindMatchingPair(var Open, Close: TBufferCoord;
      FoldingOnly: Boolean = False): Boolean;
    procedure SelectCurrentScope;
  public
    fLastMatch: TSynTokenMatched;

    procedure SaveLines;

    //### Code Folding ###
    property CodeFolding: TSynCodeFolding read fCodeFolding write setCodeFolding;
    //### End Code Folding ###

    property RoutineTerminated: Boolean read fRoutineTerminated write fRoutineTerminated;
    property ExpandLines: TSynEditStringList read getExpandLines write SetExpandLines;
    property TextDrawer: TheTextDrawer read fTextDrawer;
    property BlockBegin: TBufferCoord read GetBlockBegin write SetBlockBegin;
    property BlockEnd: TBufferCoord read GetBlockEnd write SetBlockEnd;
    property CanPaste: Boolean read GetCanPaste;
    property CanRedo: Boolean read GetCanRedo;
    property CanUndo: Boolean read GetCanUndo;
    property CaretX: Integer read fCaretX write SetCaretX;
    property CaretY: Integer read fCaretY write SetCaretY;
    property CaretXY: TBufferCoord read GetCaretXY write SetCaretXY;
    property ActiveLineBG: TColor read fActiveLineBG
      write SetActiveLineBG default clNone;
    property ActiveLineFG: TColor read fActiveLineFG write SetActiveLineFG;
    property DisplayX: Integer read GetDisplayX;
    property DisplayY: Integer read GetDisplayY;
    property DisplayXY: TDisplayCoord read GetDisplayXY;
    property DisplayLineCount: Integer read GetDisplayLineCount;
    property CharsInWindow: Integer read fCharsInWindow;
    property CharWidth: Integer read fCharWidth;
    property Color;
    property Font: TFont read GetFont write SetFont;
    property Highlighter: TSynCustomHighlighter
      read fHighlighter write SetHighlighter;
    property LeftChar: Integer read fLeftChar write SetLeftChar;
    property LineHeight: Integer read fTextHeight;
    property LinesInWindow: Integer read fLinesInWindow;
    property LineText: UnicodeString read GetLineText write SetLineText;
    property Lines: TSynEditStringList{TUnicodeStrings} read fLines write SetLines;
    property Marks: TSynEditMarkList read fMarkList;
    property MaxScrollWidth: Integer read fMaxScrollWidth write SetMaxScrollWidth
      default 1024;
    property Modified: Boolean read fModified write SetModified;
    property PaintLock: Integer read fPaintLock;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property SearchEngine: TSynEditSearchCustom read fSearchEngine write SetSearchEngine;
    property SelAvail: Boolean read GetSelAvail;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelTabBlock: Boolean read GetSelTabBlock;
    property SelTabLine: Boolean read GetSelTabLine;
    property SelText: UnicodeString read GetSelText write SetSelTextExternal;
    property StateFlags: TSynStateFlags read fStateFlags;
    property Text: UnicodeString read SynGetText write SynSetText;
    property TopLine: Integer read fTopLine write SetTopLine;
    property WordAtCursor: UnicodeString read GetWordAtCursor;
    property WordAtMouse: UnicodeString read GetWordAtMouse;
    property CurrentScope: UnicodeString read GetCurrentScope;
    property UndoList: TSynEditUndoList read fUndoList;
    property RedoList: TSynEditUndoList read fRedoList;
  public
    //property BoxBorderColor: TColor read fBoxBorderColor write fBoxBorderColor;

    property OnProcessCommand: TProcessCommandEvent
      read FOnProcessCommand write FOnProcessCommand;

    property BookMarkOptions: TSynBookMarkOpt
      read fBookMarkOpt write fBookMarkOpt;
    property BorderStyle: TSynBorderStyle read FBorderStyle write SetBorderStyle
      default bsSingle;
    property ExtraLineSpacing: Integer
      read fExtraLineSpacing write SetExtraLineSpacing default 0;
    property Gutter: TSynGutter read fGutter write SetGutter;
    property HideSelection: Boolean read fHideSelection write SetHideSelection
      default False;
    property InsertCaret: TSynEditCaretType read FInsertCaret
      write SetInsertCaret default ctVerticalLine;
    property InsertMode: boolean read fInserting write SetInsertMode
      default true;
    property IsScrolling : Boolean read FIsScrolling;
    property Keystrokes: TSynEditKeyStrokes
      read FKeystrokes write SetKeystrokes stored False;
    property MaxUndo: Integer read GetMaxUndo write SetMaxUndo default 1024;
    property Options: TSynEditorOptions read GetOptions write SetOptions
      default SYNEDIT_DEFAULT_OPTIONS;
    property OverwriteCaret: TSynEditCaretType read FOverwriteCaret
      write SetOverwriteCaret default ctHorizontalLine;
    property RightEdge: Integer read fRightEdge write SetRightEdge default 80;
    property RightEdgeColor: TColor
      read fRightEdgeColor write SetRightEdgeColor default clSilver;
    property RightEdgeShow: Boolean read fRightEdgeShow write SetRightEdgeShow;
    property ScrollHintColor: TColor read fScrollHintColor
      write fScrollHintColor default clInfoBk;
    property ScrollHintFormat: TScrollHintFormat read fScrollHintFormat
      write fScrollHintFormat default shfTopLineOnly;
    property ScrollBars: TScrollStyle
      read FScrollBars write SetScrollBars default ssBoth;
    property SelectedColor: TSynSelectedColor
      read FSelectedColor write FSelectedColor;
    property SelectionMode: TSynSelectionMode
      read FSelectionMode write SetSelectionMode default smNormal;
    property ActiveSelectionMode: TSynSelectionMode read fActiveSelectionMode
      write SetActiveSelectionMode stored False;
    property TabWidth: integer read fTabWidth write SetTabWidth default 8;
    property WantReturns: boolean read fWantReturns write SetWantReturns default True;
    property WantTabs: boolean read fWantTabs write SetWantTabs default False;
    property WordWrap: boolean read GetWordWrap write SetWordWrap default False;
    property SpellCheck: Boolean read fSpellCheck write SetSpellcheck;
    property BreakWhitespace: Boolean read fBreakWhitespace write SetBreakWhitespace;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClearBookmark: TPlaceMarkEvent read fOnClearMark
      write fOnClearMark;
    property OnCommandProcessed: TProcessCommandEvent
      read fOnCommandProcessed write fOnCommandProcessed;
    property OnContextHelp: TContextHelpEvent
      read fOnContextHelp write fOnContextHelp;
    property OnDropFiles: TDropFilesEvent read fOnDropFiles write fOnDropFiles;
    property OnGutterClick: TGutterClickEvent
      read fOnGutterClick write fOnGutterClick;
    property OnGutterGetText: TGutterGetTextEvent read fOnGutterGetText
      write fOnGutterGetText;
    property OnGutterPaint: TGutterPaintEvent read fOnGutterPaint
      write fOnGutterPaint;
    property OnMouseCursor: TMouseCursorEvent read fOnMouseCursor
      write fOnMouseCursor;
    property OnKeyPress: TKeyPressWEvent read FOnKeyPressW write FOnKeyPressW;
    property OnPaint: TPaintEvent read fOnPaint write fOnPaint;
    property OnPlaceBookmark: TPlaceMarkEvent
      read FOnPlaceMark write FOnPlaceMark;
    property OnProcessUserCommand: TProcessCommandEvent
      read FOnProcessUserCommand write FOnProcessUserCommand;
    property OnReplaceText: TReplaceTextEvent read fOnReplaceText
      write fOnReplaceText;
    property OnSpecialLineColors: TSpecialLineColorsEvent
      read fOnSpecialLineColors write fOnSpecialLineColors;
    property OnStatusChange: TStatusChangeEvent
      read fOnStatusChange write fOnStatusChange;
    property OnPaintTransient: TPaintTransient
      read fOnPaintTransient write fOnPaintTransient;
    property OnScroll: TScrollEvent
      read fOnScroll write fOnScroll;
    property OnLinesInserted: TStringListChangeEvent read fOnLinesInserted
      write fOnLinesInserted;
    property OnLinesPutted: TStringListChangeEvent read fOnLinesPutted
      write fOnLinesPutted;
    property OnLinesDeleted: TStringListChangeEvent read fOnLinesDeleted write
      fOnLinesDeleted;
    property OnLinesRecognized: TStringListChangeEvent read fOnLinesRecognized
      write fOnLinesRecognized;
    property OnSpellcheck: TSpellCheckEvent write fOnSpellCheck;
  published
    property Cursor default crIBeam;
    property OnSearchNotFound: TCustomSynEditSearchNotFoundEvent
      read fSearchNotFound write fSearchNotFound;
  end;

  TSynEdit = class(TCustomSynEdit)
  published
    property Align;
    property Anchors;
    property Constraints;
    property Color;
    property ActiveLineBG;
    property ActiveLineFG;
    property CodeFolding;
    property Ctl3D;
    property ParentCtl3D;
    property Enabled;
    property Font;
    property Height;
    property Name;
    property ParentColor default False;
    property ParentFont default False;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property Width;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnStartDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;

    { TCustomSynEdit }
    property BookMarkOptions;
    property BorderStyle;
    property ExtraLineSpacing;
    property Gutter;
    property HideSelection;
    property Highlighter;
    property ImeMode;
    property ImeName;
    property InsertCaret;
    property InsertMode;
    property Keystrokes;
    property Lines;
    property MaxScrollWidth;
    property MaxUndo;
    property Options;
    property OverwriteCaret;
    property ReadOnly;
    property RightEdge;
    property RightEdgeColor;
    property ScrollHintColor;
    property ScrollHintFormat;
    property ScrollBars;
    property SearchEngine;
    property SelectedColor;
    property SelectionMode;
    property TabWidth;
    property WantReturns;
    property WantTabs;
    property WordWrap;
    property OnChange;
    property OnClearBookmark;
    property OnCommandProcessed;
    property OnContextHelp;
    property OnDropFiles;
    property OnGutterClick;
    property OnGutterGetText;
    property OnGutterPaint;
    property OnMouseCursor;
    property OnPaint;
    property OnPlaceBookmark;
    property OnProcessCommand;
    property OnProcessUserCommand;
    property OnReplaceText;
    property OnScroll;
    property OnSpecialLineColors;
    property OnStatusChange;
    property OnPaintTransient;
  end;

  THintWindow = class(Controls.THintWindow)
  protected
    procedure Paint; override;
  end;

implementation

uses
  Consts, Clipbrd, ShellAPI, URoutine,

  { SynEdit }
  SynEditWordWrap, SynEditStrConst, SynTokenMatch, SynUniHighlighter,
  SynUniRules, SynUniClasses;

var
  ScrollHintWnd,
  CodeFoldingHintWnd,
  RightEdgeMoveHintWnd: THintWindow;

// -----------------------------------------------------------------------------
// Altered paint procedure to correctly paint tabs
procedure THintWindow.Paint;
var
  R, ClipRect: TRect;
  Params: DRAWTEXTPARAMS;
begin
  { Nicely calculate rect }
  R := ClientRect;
  if CheckWin32Version(6) and ThemeServices.ThemesEnabled then
  begin
    ClipRect := R;
    InflateRect(R, 4, 4);
    with ThemeServices do
      DrawElement(Canvas.Handle, GetElementDetails(tttStandardNormal), R,
        ClipRect);
    R := ClipRect;
  end;
  with R do
  begin
    Inc(Left, 2);
    Inc(Top, 2);
  end;

  { Always limit tab size to 2 (to show as much as possible code in hint) }
  FillChar(Params, SizeOf(Params), #0);
  with Params do
  begin
    cbSize := SizeOf(Params);
    iTabLength := 2;
  end;

  { Do draw with tabs expanded }
  Canvas.Font.Color := Screen.HintFont.Color;
  DrawTextEx(Canvas.Handle, PChar(Caption), -1, R, DT_LEFT or DT_NOPREFIX or
    DT_WORDBREAK or DT_EXPANDTABS or DT_TABSTOP or
    DrawTextBiDiModeFlagsReadingOnly, @Params);
end;

{ Hints creation }

// -----------------------------------------------------------------------------

function GetScrollHint: THintWindow;
begin
  if ScrollHintWnd = nil then
  begin
    ScrollHintWnd := THintWindow.Create(Application);
    ScrollHintWnd.DoubleBuffered := True;
  end;

  Result := ScrollHintWnd;
end;

function GetCodeFoldingHint: THintWindow;
begin
  if CodeFoldingHintWnd = nil then
  begin
    CodeFoldingHintWnd := THintWindow.Create(Application);
    CodeFoldingHintWnd.DoubleBuffered := True;
  end;

  Result := CodeFoldingHintWnd;
end;

function GetRightEdgeMovingHint: THintWindow;
begin
  if RightEdgeMoveHintWnd = nil then
  begin
    RightEdgeMoveHintWnd := THintWindow.Create(Application);
    RightEdgeMoveHintWnd.DoubleBuffered := True;
  end;

  Result := RightEdgeMoveHintWnd;
end;

{ THookedCommandHandlerEntry }

// -----------------------------------------------------------------------------

type
  THookedCommandHandlerEntry = class(TObject)
  private
    fEvent: THookedCommandEvent;
    fData: Pointer;
    constructor Create(AEvent: THookedCommandEvent; AData: Pointer);
    function Equals(AEvent: THookedCommandEvent): Boolean; reintroduce;
  end;

constructor THookedCommandHandlerEntry.Create(AEvent: THookedCommandEvent;
  AData: Pointer);
begin
  inherited Create;
  fEvent := AEvent;
  fData := AData;
end;

function THookedCommandHandlerEntry.Equals(AEvent: THookedCommandEvent): Boolean;
begin
  with TMethod(fEvent) do
    Result := (Code = TMethod(AEvent).Code) and (Data = TMethod(AEvent).Data);
end;

{ TSynEditMark }

// -----------------------------------------------------------------------------

constructor TSynEditMark.Create(AOwner: TCustomSynEdit);
begin
  inherited Create;
  fBookmark := -1;
  fEdit := AOwner;
end;

// -----------------------------------------------------------------------------
// Ensure to give right instance
function TSynEditMark.GetEdit: TCustomSynEdit;
begin
  if fEdit <> nil then
  try
    if fEdit.Marks.IndexOf(Self) = -1 then
      fEdit := nil;
  except
    fEdit := nil;
  end;
  Result := fEdit;
end;

// -----------------------------------------------------------------------------
// Numeric bookmark?
function TSynEditMark.GetIsBookmark: Boolean;
begin
  Result := fBookmark >= 0;
end;

// -----------------------------------------------------------------------------

procedure TSynEditMark.SetChar(const Value: Integer);
begin
  FChar := Value;
end;

// -----------------------------------------------------------------------------

procedure TSynEditMark.SetImage(const Value: Integer);
begin
  FImage := Value;
  if fVisible and Assigned(fEdit) then
    fEdit.InvalidateGutterLines(fLine, fLine);
end;

// -----------------------------------------------------------------------------

procedure TSynEditMark.SetLine(const Value: Integer);
begin
  if fVisible and Assigned(fEdit) then
  begin
    if fLine > 0 then
      fEdit.InvalidateGutterLines(fLine, fLine);
    fLine := Value;
    fEdit.InvalidateGutterLines(fLine, fLine);
  end
  else
    fLine := Value;
end;

// -----------------------------------------------------------------------------

procedure TSynEditMark.SetVisible(const Value: Boolean);
begin
  if fVisible <> Value then
  begin
    fVisible := Value;
    if Assigned(fEdit) then
      fEdit.InvalidateGutterLines(fLine, fLine);
  end;
end;

{ TSynEditMarkList }

// -----------------------------------------------------------------------------

procedure TSynEditMarkList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

// -----------------------------------------------------------------------------

function TSynEditMarkList.GetItem(Index: Integer): TSynEditMark;
begin
  Result := TSynEditMark(inherited GetItem(Index));
end;

// -----------------------------------------------------------------------------

procedure TSynEditMarkList.SetItem(Index: Integer; Item: TSynEditMark);
begin
  inherited SetItem(Index, Item);
end;

// -----------------------------------------------------------------------------

constructor TSynEditMarkList.Create(AOwner: TCustomSynEdit);
begin
  inherited Create;
  fEdit := AOwner;
end;

// -----------------------------------------------------------------------------

function TSynEditMarkList.First: TSynEditMark;
begin
  Result := TSynEditMark(inherited First);
end;

// -----------------------------------------------------------------------------

function TSynEditMarkList.Last: TSynEditMark;
begin
  result := TSynEditMark(inherited Last);
end;

// -----------------------------------------------------------------------------

function TSynEditMarkList.Extract(Item: TSynEditMark): TSynEditMark;
begin
  Result := TSynEditMark(inherited Extract(Item));
end;

// -----------------------------------------------------------------------------

procedure TSynEditMarkList.ClearLine(Line: Integer);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if not Items[I].IsBookmark and (Items[I].Line = Line) then
      Delete(I);
end;

// -----------------------------------------------------------------------------
// Returns up to MaxMarks book/gutter marks for a chosen line
procedure TSynEditMarkList.GetMarksForLine(ALine: Integer;
  var AMarks: TSynEditMarks);
var
  Cnt: Integer;
  I: Integer;
begin
  FillChar(AMarks, SizeOf(AMarks), 0);
  Cnt := 0;
  for I := 0 to Count - 1 do
  begin
    if Items[I].Line = ALine then
    begin
      Inc(Cnt);
      AMarks[Cnt] := Items[I];
      if Cnt = MAX_MARKS then
        Break;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TSynEditMarkList.Place(AMark: TSynEditMark);
begin
  if Assigned(fEdit) then
    if Assigned(fEdit.OnPlaceBookmark) then
      fEdit.OnPlaceBookmark(fEdit, AMark);
  if Assigned(AMark) then
    Add(AMark);
end;

// -----------------------------------------------------------------------------

{ TCustomSynEdit }

// -----------------------------------------------------------------------------

constructor TCustomSynEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fLastMatch.TokenKind := -1;
  SetLength(fCarets, 0);
  fCurrCaret := -1;
  fSnippet := False;
  fColumn := False;
  fSelections := False;
  fSmartCaretsUpdating := False;
  fUndoRedo := False;

  { Control }
  ControlStyle := ControlStyle + [csOpaque, csSetCaption, csNeedsBorderPaint];
  Cursor := crIBeam;
  Color := clWindow;

  { Text buffer }
  fLines := TSynEditStringList.Create(Self);
  fOrigLines := fLines;
  with TSynEditStringList(fLines) do
  begin
    OnChange := LinesChanged;
    OnChanging := LinesChanging;
    OnCleared := ListCleared;
    OnDeleted := ListDeleted;
    OnInserted := ListInserted;
    OnBeforePutted := ListBeforePutted;
    OnPutted := ListPutted;
  end;

  { Font }
  fFontDummy := TFont.Create;
  with fFontDummy do
  begin
    Name := 'Courier New';
    Size := 10;
    CharSet := DEFAULT_CHARSET;
    Color := clWindowText;
  end;

  { Undo & Redo }
  fUndoList := TSynEditUndoList.Create;
  fUndoList.OnAddedUndo := UndoRedoAdded;
  fOrigUndoList := fUndoList;
  fRedoList := TSynEditUndoList.Create;
  fRedoList.OnAddedUndo := UndoRedoAdded;
  fOrigRedoList := fRedoList;
  fCmdDrop := False;

  { Active line, selection }
  fActiveLineBG := clNone;
  fSelectedColor := TSynSelectedColor.Create;
  fSelectedColor.OnChange := SelectedColorsChanged;

  { Bookmarks }
  fMarkList := TSynEditMarkList.Create(self);
  fMarkList.OnChange := MarkListChange;
  fBookMarkOpt := TSynBookMarkOpt.Create(Self);
  fBookMarkOpt.OnChange := BookMarkOptionsChanged;

  { fRightEdge has to be set before FontChanged
    is called for the first time }
  fRightEdge := 80;
  fRightEdgeColor := clSilver;
  fOutliningAttr := TSynHighlighterAttributes.Create(sOutliningElem, sOutliningElem);
  with fOutliningAttr do
  begin
    Background := clNone;
    Foreground := fRightEdgeColor;
  end;
  fDiffAddedAttr := TSynHighlighterAttributes.Create(sDiffAddedElem, sDiffAddedElem);
  with fDiffAddedAttr do
  begin
    Background := clNone;
    Foreground := fRightEdgeColor;
  end;
  fDiffRemovedAttr := TSynHighlighterAttributes.Create(sDiffRemovedElem, sDiffRemovedElem);
  with fDiffRemovedAttr do
  begin
    Background := clNone;
    Foreground := fRightEdgeColor;
  end;
  fDiffChangedAttr := TSynHighlighterAttributes.Create(sDiffChangedElem, sDiffChangedElem);
  with fDiffChangedAttr do
  begin
    Background := clNone;
    Foreground := fRightEdgeColor;
  end;

  { Gutter }
  fGutter := TSynGutter.Create(Self);
  fOldGutterWidth := fGutter.Width;
  fGutter.OnChange := GutterChanged;

  { Painting }
  fTextDrawer := TheTextDrawer.Create([fsBold], fFontDummy);
  with Font do
  begin
    Assign(fFontDummy);
    OnChange := SynFontChanged;
  end;
  ParentFont := False;
  ParentColor := False;

  { Text }
  TabStop := True;
  fInserting := True;
  fInsertCaret := ctVerticalLine;
  fOverwriteCaret := ctHorizontalLine;
  fSelectionMode := smNormal;
  fActiveSelectionMode := smNormal;
  fFocusList := TList.Create;
  fKbdHandler := TSynEditKbdHandler.Create;
  fKeystrokes := TSynEditKeyStrokes.Create(Self);
  SetDefaultKeystrokes;
  fWantReturns := True;
  fWantTabs := True;
  fTabWidth := 4;
  fLeftChar := 1;
  fTopLine := 1;
  fCaretX := 1;
  fLastCaretX := 1;
  fCaretY := 1;
  fBlockBegin.Char := 1;
  fBlockBegin.Line := 1;
  fBlockEnd := fBlockBegin;
  fOptions := SYNEDIT_DEFAULT_OPTIONS;

  { Scroll }
  fScrollTimer := TTimer.Create(Self);
  with fScrollTimer do
  begin
    Enabled := False;
    Interval := 40;
    OnTimer := ScrollTimerHandler;
  end;
  fScrollHintColor := clInfoBk;
  fScrollHintFormat := shfTopToBottom;
  fScrollBars := ssBoth;

  { Code folding }
  fAllFoldRanges := TSynEditAllFoldRanges.Create;
  fCodeFoldingPlugin := TSynEditCodeFoldingPlugin.Create(Self);
  fCodeFolding := TSynCodeFolding.Create;
  fCodeFolding.OnChange := CodeFoldingOnChange;
  fOutliningTimer := TTimer.Create(Self);
  with fOutliningTimer do
  begin
    Enabled := False;
    Interval := 100;
    OnTimer := FoldingTimerHandler;
  end;

  { Line selection }
  fLineSelectionTimer := TTimer.Create(Self);
  with fLineSelectionTimer do
  begin
    Enabled := False;
    Interval := GetDoubleClickTime;
    OnTimer := LineSelectionTimerHandle;
  end;

  { Miscellanious }
  fMaxScrollWidth := 1024;
  fBorderStyle := bsSingle;
  fRightEdgeMoving := False;
  fMouseAtRightEdge := False;
  fExtraLineSpacing := 2;
  fCurrCharWidth := 1;
  fRepaintAfterDimNeeded := False;
  fDimmed := False;
  fBreakWhitespace := True;

  { Do update character constraints }
  SynFontChanged(nil);

  { Text }
  if eoWrapCentered in fOptions then
    fCenterOffset := Max((ClientWidth - 3 - (fGutter.Width * Ord(Gutter.Visible)) - (fCharWidth * fRightEdge)), 0) shr 1
  else
    fCenterOffset := 0;
  fTextOffset := fCenterOffset + fOldGutterWidth + 2;
end;

// -----------------------------------------------------------------------------

destructor TCustomSynEdit.Destroy;
begin
  CancelSnippet;
  fHighlighter := nil;
  if (fChainedEditor <> nil) or (fLines <> fOrigLines) then
    RemoveLinesPointer;

  inherited Destroy;

  FreeAndNil(fOutliningAttr);
  FreeAndNil(fDiffAddedAttr);
  FreeAndNil(fDiffRemovedAttr);
  FreeAndNil(fDiffChangedAttr);

  { Free listeners while other fields are still valid.
    do not use FreeAndNil, it first nils and then frees causing problems with
    code accessing fHookedCommandHandlers while destruction }
  fHookedCommandHandlers.Free;
  fHookedCommandHandlers := nil;

  { Do not use FreeAndNil, it first nils and then frees causing problems with
    code accessing fPlugins while destruction }
  fPlugins.Free;
  fPlugins := nil;
  fMarkList.Free;
  fBookMarkOpt.Free;
  fKeyStrokes.Free;
  fKbdHandler.Free;
  fFocusList.Free;
  fSelectedColor.Free;
  fOrigUndoList.Free;
  fOrigRedoList.Free;
  fGutter.Free;
  fTextDrawer.Free;
  fFontDummy.Free;
  fOrigLines.Free;
  fAllFoldRanges.Free;
  fCodeFolding.Free;
end;

{ Message processing }

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.WMCancelMode(var Message:TMessage);
begin
  { Do not process }
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.WMCaptureChanged(var Msg: TMessage);
begin
  fScrollTimer.Enabled := False;
  fOutliningTimer.Enabled := False;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.WMChar(var Msg: TWMChar);
begin
  DoKeyPressW(Msg);
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.WMClear(var Msg: TMessage);
begin
  if not fReadOnly then
    SelText := '';
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.WMCopy(var Message: TMessage);
begin
  CopyToClipboard;
  Message.Result := Ord(True);
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.WMCut(var Message: TMessage);
begin
  if not fReadOnly then
    CutToClipboard;
  Message.Result := Ord(True);
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.WMDropFiles(var Msg: TMessage);
var
  I, iNumberDropped: Integer;
  FileNameA: array[0..MAX_PATH - 1] of AnsiChar;
  FileNameW: array[0..MAX_PATH - 1] of Char;
  Point: TPoint;
  FilesList: TStringList;
begin
  try
    if Assigned(fOnDropFiles) then
    begin
      FilesList := TStringList.Create;
      try
        iNumberDropped := DragQueryFile(THandle(Msg.wParam), Cardinal(-1),
          nil, 0);
        DragQueryPoint(THandle(Msg.wParam), Point);

        if Win32PlatformIsUnicode then
          for I := 0 to iNumberDropped - 1 do
          begin
            DragQueryFileW(THandle(Msg.wParam), I, FileNameW,
              sizeof(FileNameW) shr 1);
            FilesList.Add(FileNameW)
          end
        else
          for I := 0 to iNumberDropped - 1 do
          begin
            DragQueryFileA(THandle(Msg.wParam), I, FileNameA,
              sizeof(FileNameA));
            FilesList.Add(UnicodeString(FileNameA))
          end;
        fOnDropFiles(Self, Point.X, Point.Y, FilesList);
      finally
        FreeAndNil(FilesList);
      end;
    end;
  finally
    Msg.Result := 0;
    DragFinish(THandle(Msg.wParam));
  end;
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.WMEraseBkgnd(var Msg: TMessage);
begin
  { Do nothing }
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := Msg.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
  if fWantTabs then
    Msg.Result := Msg.Result or DLGC_WANTTAB;
  if fWantReturns then
    Msg.Result := Msg.Result or DLGC_WANTALLKEYS;
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.WMGetText(var Msg: TWMGetText);
begin
  if HandleAllocated and IsWindowUnicode(Handle) then
  begin
    WStrLCopy(PChar(Msg.Text), PChar(Text), Msg.TextMax - 1);
    Msg.Result := WStrLen(PChar(Msg.Text));
  end
  else begin
    StrLCopy(PAnsiChar(Msg.Text), PAnsiChar(AnsiString(Text)), Msg.TextMax - 1);
    Msg.Result := StrLen(PAnsiChar(Msg.Text));
  end;
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.WMGetTextLength(var Msg: TWMGetTextLength);
begin
  if csDocking in ControlState then
    Msg.Result := 0
  else
    Msg.Result := Length(Text);
end;

// -----------------------------------------------------------------------------
// § Garnet
procedure TCustomSynEdit.WMHScroll(var Msg: TWMScroll);
var
  iMaxWidth: integer;
begin
  Msg.Result := 0;

  inherited;

  case Msg.ScrollCode of
    { Scrolls to start / end of the line }
    SB_LEFT: LeftChar := 1;
    SB_RIGHT:
      if eoScrollPastEol in Options then
        LeftChar := MaxScrollWidth - CharsInWindow + 1
      else
        { Simply set LeftChar property to the LengthOfLongestLine,
          it would do the range checking and constrain the value if necessary }
        LeftChar := ExpandLines.GetLengthOfLongestLine(RowToLine(fTopLine), RowToLine(fTopLine + fLinesInWindow));

    { Scrolls one char left / right }
    SB_LINERIGHT: LeftChar := LeftChar + 1;
    SB_LINELEFT: LeftChar := LeftChar - 1;

    { Scrolls one page of chars left / right }
    SB_PAGERIGHT: LeftChar := LeftChar
      + (fCharsInWindow {- Ord(eoScrollByOneLess in fOptions)});
    SB_PAGELEFT: LeftChar := LeftChar
      - (fCharsInWindow {- Ord(eoScrollByOneLess in fOptions)});

    { Scrolls to the current scroll bar position }
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
    begin
      FIsScrolling := True;
      if eoScrollPastEol in Options then
        iMaxWidth := MaxScrollWidth
      else
        iMaxWidth := Max(ExpandLines.GetLengthOfLongestLine(RowToLine(fTopLine),
          RowToLine(TopLine + LinesInWindow)), 1);
      if iMaxWidth > MAX_SCROLL then
        LeftChar := MulDiv(iMaxWidth, Msg.Pos, MAX_SCROLL)
      else
        LeftChar := Msg.Pos;
    end;
    SB_ENDSCROLL: FIsScrolling := False;
  end;
  if Assigned(OnScroll) then OnScroll(Self,sbHorizontal);
end;

procedure TCustomSynEdit.WMImeChar(var Msg: TMessage);
begin
  { Do nothing here, the IME string is retrieved in WMImeComposition.
    Handling the WM_IME_CHAR message stops Windows from sending WM_CHAR
    messages while using the IME }
end;

procedure TCustomSynEdit.WMImeComposition(var Msg: TMessage);

  function IsWindows98OrLater: Boolean;
  begin
    Result := (Win32MajorVersion > 4) or
      (Win32MajorVersion = 4) and (Win32MinorVersion > 0);
  end;

var
  imc: HIMC;
  PW: PChar;
  PA: PAnsiChar;
  PWLength: Integer;
  ImeCount: Integer;
begin
  if (Msg.LParam and GCS_RESULTSTR) <> 0 then
  begin
    imc := ImmGetContext(Handle);
    try
      if IsWindows98OrLater then
      begin
        ImeCount := ImmGetCompositionStringW(imc, GCS_RESULTSTR, nil, 0);
        { ImeCount is always the size in bytes, also for Unicode }
        GetMem(PW, ImeCount + sizeof(WideChar));
        try
          ImmGetCompositionStringW(imc, GCS_RESULTSTR, PW, ImeCount);
          PW[ImeCount div sizeof(WideChar)] := #0;
          CommandProcessor(ecImeStr, #0, PW);
        finally
          FreeMem(PW);
        end;
      end
      else begin
        ImeCount := ImmGetCompositionStringA(imc, GCS_RESULTSTR, nil, 0);
        { ImeCount is always the size in bytes, also for Unicode }
        GetMem(PA, ImeCount + sizeof(AnsiChar));
        try
          ImmGetCompositionStringA(imc, GCS_RESULTSTR, PA, ImeCount);
          PA[ImeCount] := #0;

          PWLength := MultiByteToWideChar(DefaultSystemCodePage, 0, PA, ImeCount,
            nil, 0);
          GetMem(PW, (PWLength + 1) * sizeof(WideChar));
          try
            MultiByteToWideChar(DefaultSystemCodePage, 0, PA, ImeCount,
              PW, PWLength);
            CommandProcessor(ecImeStr, #0, PW);
          finally
            FreeMem(PW);
          end;
        finally
          FreeMem(PA);
        end;
      end;
    finally
      ImmReleaseContext(Handle, imc);
    end;
  end;
  inherited;
end;

procedure TCustomSynEdit.WMImeNotify(var Msg: TMessage);

  function IsWindows98OrLater: Boolean;
  begin
    Result := (Win32MajorVersion > 4) or
      (Win32MajorVersion = 4) and (Win32MinorVersion > 0);
  end;

var
  imc: HIMC;
  LogFontW: TLogFontW;
  LogFontA: TLogFontA;
begin
  with Msg do
  begin
    case WParam of
      IMN_SETOPENSTATUS:
        begin
          imc := ImmGetContext(Handle);
          if imc <> 0 then
          begin
            if IsWindows98OrLater then
            begin
              GetObjectW(Font.Handle, SizeOf(TLogFontW), @LogFontW);
              ImmSetCompositionFontW(imc, @LogFontW);
            end
            else
            begin
              GetObjectA(Font.Handle, SizeOf(TLogFontA), @LogFontA);
              ImmSetCompositionFontA(imc, @LogFontA);
            end;
            ImmReleaseContext(Handle, imc);
          end;
        end;
    end;
  end;
  inherited;
end;

procedure TCustomSynEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  CommandProcessor(ecLostFocus, #0, nil);

  { Added check for focused to prevent caret disappearing problem }
  if Focused or FAlwaysShowCaret or (fFocusList.Count > 0) then Exit;

  HideCaret;
  Windows.DestroyCaret;
  if FHideSelection and SelAvail then InvalidateSelection;
end;

procedure TCustomSynEdit.WMMouseWheel(var Msg: TMessage);
var
  nDelta: Integer;
  nWheelClicks: Integer;
  bCtrl, bShift: Boolean;
begin
  { Check }
  if csDesigning in ComponentState then
    Exit;

  { We always process the message }
	Msg.Result := 1;

  { In some occasions Windows will not properly initialize mouse wheel, but
    will still keep sending WM_MOUSEWHEEL message. Calling inherited procedure
    will re-initialize related properties (i.e. Mouse.WheelScrollLines) }
  inherited;

  { Find modifiers }
  bCtrl := GetKeyState(VK_CONTROL) < 0;
  bShift := GetKeyState(VK_SHIFT) < 0;

  { Zooming? }
  if bCtrl and bShift then
  begin

    { Get current font size }
    nWheelClicks := Font.Size;

    { Get scrolling direction }
    nDelta := SmallInt(Msg.WParamHi) div WHEEL_DELTA;

    { Get new size with constraint }
    if nDelta >= 0 then
      nWheelClicks := Min(Succ(nWheelClicks), 72)
    else
      nWheelClicks := Max(Pred(nWheelClicks), 8);

    { Apply new size if it's changed }
    if Font.Size <> nWheelClicks then
    begin
      Font.Size := nWheelClicks;
      Gutter.Font.Size := nWheelClicks;
      Gutter.UpdateFont;
    end;
  end

  { Scrolling }
  else begin

    { Initial delta }
    if not bCtrl then
      nDelta := Mouse.WheelScrollLines
    else
      nDelta := LinesInWindow shr Ord(eoHalfPageScroll in fOptions);

    { Find scroll delta }
    Inc(fMouseWheelAccumulator, SmallInt(Msg.wParamHi));
    nWheelClicks := fMouseWheelAccumulator div WHEEL_DELTA;
    fMouseWheelAccumulator := fMouseWheelAccumulator mod WHEEL_DELTA;
    if (nDelta = Integer(WHEEL_PAGESCROLL)) or (nDelta > LinesInWindow) then
      nDelta := LinesInWindow;

    { Apply delta }
    TopLine := TopLine - (nDelta * nWheelClicks);

    { Immidiately move caret extending selection if left mouse button is pressed }
    if GetAsyncKeyState(VK_LBUTTON) = -32768 then
      MouseDown(mbLeft, [Classes.ssShift], ScreenToClient(Mouse.CursorPos).X,
        ScreenToClient(Mouse.CursorPos).Y);

    if Assigned(OnScroll) then OnScroll(Self,sbVertical);
  end;
end;

procedure TCustomSynEdit.WMPaste(var Message: TMessage);
begin
  if not fReadOnly then
    PasteFromClipboard;
  Message.Result := Ord(True);
end;

procedure TCustomSynEdit.WMSetCursor(var Msg: TWMSetCursor);
begin
  if (Msg.HitTest = HTCLIENT) and (Msg.CursorWnd = Handle) and
    not (csDesigning in ComponentState)
  then
    UpdateMouseCursor
  else
    inherited;
end;

procedure TCustomSynEdit.WMSetFocus(var Msg: TWMSetFocus);
begin
  CommandProcessor(ecGotFocus, #0, nil);

  InitializeCaret;
  if FHideSelection and SelAvail then
    InvalidateSelection;
end;

{
procedure TCustomSynEdit.WMLMouseButtonDown(var Msg: TMessage);
begin
  Beep;
end;
}
procedure TCustomSynEdit.WMSetText(var Msg: TWMSetText);
begin
  LongBool(Msg.Result) := True;
  try
    if HandleAllocated and IsWindowUnicode(Handle) then
      Text := PWideChar(Msg.Text)
    else
      Text := UnicodeString(PAnsiChar(Msg.Text));
  except
    LongBool(Msg.Result) := False;
    raise
  end
end;

procedure TCustomSynEdit.WMSize(var Msg: TWMSize);
begin
  inherited;
  SizeOrFontChanged(False);
end;

procedure TCustomSynEdit.WMUndo(var Msg: TMessage);
begin
  Undo;
end;

procedure TCustomSynEdit.WMVScroll(var Msg: TWMScroll);
var
  S: String;
  Rc: TRect;
  Pt: TPoint;
  ScrollHint: THintWindow;
  ButtonH: Integer;
  ScrollInfo: TScrollInfo;
begin
  Msg.Result := 0;

  case Msg.ScrollCode of
    { Scrolls to start / end of the text }
    SB_TOP: TopLine := 1;
    SB_BOTTOM: TopLine := DisplayLineCount;

    { Scrolls one line up / down }
    SB_LINEDOWN: TopLine := fTopLine + 1;
    SB_LINEUP: TopLine := fTopLine - 1;

    { Scrolls one page of lines up / down }
    SB_PAGEDOWN: TopLine := fTopLine
      + (fLinesInWindow {- Ord(eoScrollByOneLess in fOptions)});
    SB_PAGEUP: TopLine := fTopLine
      - (fLinesInWindow {- Ord(eoScrollByOneLess in fOptions)});

    { Scrolls to the current scroll bar position }
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      begin
        fIsScrolling := True;
        if DisplayLineCount > MAX_SCROLL then
          TopLine := MulDiv(fLinesInWindow + DisplayLineCount - 1, Msg.Pos,
            MAX_SCROLL)
        else
          TopLine := Msg.Pos;

        if eoShowScrollHint in fOptions then
        begin
          ScrollHint := GetScrollHint;
          ScrollHint.Color := fScrollHintColor;
          case FScrollHintFormat of
            shfTopLineOnly:
              S := Format(SYNS_ScrollInfoFmtTop, [RowToLine(fTopLine)]);
            else
              S := Format(SYNS_ScrollInfoFmt, [RowToLine(fTopLine),
                RowToLine(fTopLine + Min(fLinesInWindow, DisplayLineCount - fTopLine))]);
          end;

          Rc := ScrollHint.CalcHintRect(200, S, nil);
          if eoScrollHintFollows in fOptions then
          begin
            ButtonH := GetSystemMetrics(SM_CYVSCROLL);

            FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
            ScrollInfo.cbSize := SizeOf(ScrollInfo);
            ScrollInfo.fMask := SIF_ALL;
            GetScrollInfo(Handle, SB_VERT, ScrollInfo);

            Pt := ClientToScreen(Point(ClientWidth - Rc.Right - 4,
              ((Rc.Bottom - Rc.Top) shr 1) +                       // Half the size of the hint window
              Round((ScrollInfo.nTrackPos / ScrollInfo.nMax) *     // The percentage of the page that has been scrolled
                    (ClientHeight - (ButtonH * 2)))                // The height minus the arrow buttons
                   + ButtonH));                                    // The height of the top button
          end
          else
            Pt := ClientToScreen(Point(ClientWidth - Rc.Right - 4, 10));

          OffsetRect(Rc, Pt.x, Pt.y);
          ScrollHint.ActivateHint(Rc, S);
          ScrollHint.Invalidate; // § Garnet: CHECK
          ScrollHint.Update;
        end;
      end;

    { Ends scrolling }
    SB_ENDSCROLL:
      begin
        FIsScrolling := False;
      if eoShowScrollHint in fOptions then
        ShowWindow(GetScrollHint.Handle, SW_HIDE);
      end;
  end;
  Update;
  if Assigned(OnScroll) then OnScroll(Self,sbVertical);
end;

// -----------------------------------------------------------------------------
// Transforms monitor screen coords into editor display coords.
// (Result is in display coordinates)
function TCustomSynEdit.PixelsToNearestRowColumn(AX,
  AY: Integer): TDisplayCoord;
begin
  { Don't return a partially visible last line }
  if AY >= fLinesInWindow * fTextHeight then
  begin
    AY := fLinesInWindow * fTextHeight - 1;
    if AY < 0 then
      AY := 0;
  end;

  { Return }
  with Result do
  begin
    Column := Max(1, fLeftChar + Round((AX - fGutter.Width - 2 - fCenterOffset) / fCharWidth));
    Row := Max(1, fTopLine + (AY div fTextHeight));
  end;
end;

// -----------------------------------------------------------------------------
// Same as above but less precise
function TCustomSynEdit.PixelsToRowColumn(AX, AY: Integer): TDisplayCoord;
begin
  with Result do
  begin
    Column := Max(1, fLeftChar + Round((AX - fGutter.Width - 2 - fCenterOffset) / fCharWidth));
    Row := Max(1, fTopLine + (AY div fTextHeight));
  end;
end;

// -----------------------------------------------------------------------------

function TCustomSynEdit.RowColumnToPixels(const RowCol: TDisplayCoord): TPoint;
begin
  with Result do
  begin
    X := (RowCol.Column-1) * fCharWidth + fTextOffset;
    Y := (RowCol.Row - fTopLine) * fTextHeight + fExtraLineSpacing shr 1;
  end;
end;

// -----------------------------------------------------------------------------
// Completely sets caret (display & buffer) when click is received from mouse.
// (X and Y are in pixel coordinates.)
procedure TCustomSynEdit.ComputeCaret(X, Y: Integer);
var
  vCaretNearestPos : TDisplayCoord;
begin
  vCaretNearestPos := PixelsToNearestRowColumn(X, Y);
  vCaretNearestPos.Row := MinMax(vCaretNearestPos.Row, 1, DisplayLineCount);
  SetInternalDisplayXY(vCaretNearestPos);
  CancelSnippet(True);
end;

function TCustomSynEdit.InternalComputeCaret(const X, Y: Integer): TBufferCoord;
var
  vCaretNearestPos : TDisplayCoord;
begin
  vCaretNearestPos := PixelsToNearestRowColumn(X, Y);
  vCaretNearestPos.Row := MinMax(vCaretNearestPos.Row, 1, DisplayLineCount);
  Result := DisplayToBufferPos(vCaretNearestPos);
end;

// -----------------------------------------------------------------------------
// X, Y are pixel coordinates
procedure TCustomSynEdit.ComputeScroll(X, Y: Integer);
var
  iScrollBounds: TRect; { Relative to the client area }
begin
  { Don't scroll if dragging text from other control }
  if (not MouseCapture) and (not Dragging) then
  begin
    fScrollTimer.Enabled := False;
    Exit;
  end;

  iScrollBounds := Bounds(fGutter.Width, 0, fCharsInWindow * fCharWidth,
    fLinesInWindow * fTextHeight);
  if fBorderStyle = bsNone then
    InflateRect(iScrollBounds, -2, -2);

  if X < iScrollBounds.Left then
    fScrollDeltaX := (X - iScrollBounds.Left) div fCharWidth - 1
  else if X >= iScrollBounds.Right then
    fScrollDeltaX := (X - iScrollBounds.Right) div fCharWidth + 1
  else
    fScrollDeltaX := 0;

  if Y < iScrollBounds.Top then
    fScrollDeltaY := (Y - iScrollBounds.Top) div fTextHeight - 1
  else if Y >= iScrollBounds.Bottom then
    fScrollDeltaY := (Y - iScrollBounds.Bottom) div fTextHeight + 1
  else
    fScrollDeltaY := 0;

  fScrollTimer.Enabled := (fScrollDeltaX <> 0) or (fScrollDeltaY <> 0);
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.DoCopyToClipboard(const SText: UnicodeString);
begin
  if Length(SText) = 0 then
    Exit;
  SetClipboardText(SText);
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.CopyToClipboard;
var
  S: UnicodeString;
  ChangeTrim: Boolean;
begin
  if SelAvail or (fSelections and GetAtLeastOneBlockOfSelection) then
  begin
    ChangeTrim := (fActiveSelectionMode = smColumn) and
      (eoTrimTrailingSpaces in Options);
    try
      if ChangeTrim then
        Exclude(fOptions, eoTrimTrailingSpaces);
      S := SelText;
    finally
      if ChangeTrim then
        Include(fOptions, eoTrimTrailingSpaces);
    end;
    DoCopyToClipboard(S);
  end;
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.CutToClipboard;
begin
  if not ReadOnly and SelAvail then
  begin
    BeginUndoBlock;
    try
      DoCopyToClipboard(SelText);
      SelText := '';
    finally
      EndUndoBlock;
    end;
  end;
end;

procedure TCustomSynEdit.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  { Clear WindowText to avoid it being used as Caption, or else window creation
    will fail if it's bigger than 64KB. It's useless to set the Caption anyway }
  StrDispose(WindowText);
  WindowText := nil;

  inherited CreateParams(Params);

  with Params do
  begin
    WindowClass.Style := WindowClass.Style and not ClassStylesOff;
    Style := Style or BorderStyles[fBorderStyle] or WS_CLIPCHILDREN;
    if NewStyleControls and Ctl3D and (fBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TCustomSynEdit.DecPaintLock;
var
  vAuxPos: TDisplayCoord;
begin
  Assert(fPaintLock > 0);
  Dec(fPaintLock);
  if (fPaintLock = 0) and HandleAllocated then
  begin
    if sfScrollbarChanged in fStateFlags then
      UpdateScrollbars;

    { Locks the caret inside the visible area }
    if WordWrap and ([scCaretX,scCaretY] * fStatusChanges <> []) then
    begin
      vAuxPos := DisplayXY;

      { This may happen in the last row of a line or in rows which length is
        greater than CharsInWindow (Tabs and Spaces are allowed beyond
        CharsInWindow while wrapping the lines) }
      if (vAuxPos.Column > CharsInWindow +1) and (CharsInWindow > 0) then
      begin
        if fCaretAtEOL then
          fCaretAtEOL := False
        else
        begin
          if scCaretY in fStatusChanges then
          begin
            vAuxPos.Column := CharsInWindow + 1;
            fCaretX := DisplayToBufferPos(vAuxPos).Char;
            Include(fStatusChanges,scCaretX);
            UpdateLastCaretX;
          end;
        end;
        Include(fStateFlags, sfCaretChanged);
      end;
    end;
    if sfCaretChanged in fStateFlags then
      UpdateCaret;
    if fStatusChanges <> [] then
      DoOnStatusChange(fStatusChanges);
  end;
end;

function TCustomSynEdit.GetBlockBegin: TBufferCoord;
begin
  if SelAvail then
    if (fBlockEnd.Line < fBlockBegin.Line)
      or ((fBlockEnd.Line = fBlockBegin.Line) and (fBlockEnd.Char < fBlockBegin.Char))
    then
      Result := fBlockEnd
    else
      Result := fBlockBegin
  else
    Result := CaretXY;
end;

function TCustomSynEdit.GetBlockEnd: TBufferCoord;
begin
  if SelAvail then
    if (fBlockEnd.Line < fBlockBegin.Line)
      or ((fBlockEnd.Line = fBlockBegin.Line) and (fBlockEnd.Char < fBlockBegin.Char))
    then
      Result := fBlockBegin
    else
      Result := fBlockEnd
  else
    Result := CaretXY;
end;

procedure TCustomSynEdit.SynFontChanged(Sender: TObject);
begin
  RecalcCharExtent;
  SizeOrFontChanged(True);
end;

function TCustomSynEdit.GetFont: TFont;
begin
  Result := inherited Font;
end;

function TCustomSynEdit.GetLineText: UnicodeString;
begin
  if (CaretY >= 1) and (CaretY <= Lines.Count) then
    Result := fLines[CaretY - 1]
  else
    Result := '';
end;

function TCustomSynEdit.GetSelAvail: Boolean;
begin
  Result := (fBlockBegin.Char <> fBlockEnd.Char) or
    ((fBlockBegin.Line <> fBlockEnd.Line) and (fActiveSelectionMode <> smColumn));
end;

function TCustomSynEdit.GetSelTabBlock: Boolean;
begin
  Result := (fBlockBegin.Line <> fBlockEnd.Line) and (fActiveSelectionMode <> smColumn);
end;

function TCustomSynEdit.GetSelTabLine: Boolean;
begin
  Result := (BlockBegin.Char <= 1) and (BlockEnd.Char > length(Lines[CaretY - 1])) and SelAvail;
end;

// -----------------------------------------------------------------------------
// § Garnet
function TCustomSynEdit.GetSelText: UnicodeString;

  function CopyPadded(const S: UnicodeString;
    Index, Count: Integer): UnicodeString;
  var
    SrcLen, DstLen, I: Integer;
    P: PChar;
  begin
    SrcLen := Length(S);
    DstLen := Index + Count;
    if SrcLen >= DstLen then
      Result := Copy(S, Index, Count)
    else begin
      SetLength(Result, DstLen);
      P := PWideChar(Result);
      WStrCopy(P, PWideChar(Copy(S, Index, Count)));
      Inc(P, Length(S));
      for I := 0 to DstLen - Srclen - 1 do
        P[I] := #32;
    end;
  end;

  { Returns how much chars actually have been copied. It can happen that it
    it will be less than Count }
  procedure CopyAndForward(const S: UnicodeString; Index, Count: Integer;
    var P: PChar);
  var
    pSrc: PChar;
    SrcLen: Integer;
    DstLen: Integer;
  begin
    SrcLen := Length(S);
    if (Index <= SrcLen) and (Count > 0) then
    begin
      Dec(Index);
      pSrc := PChar(S) + Index;
      DstLen := Min(SrcLen - Index, Count);
      Move(pSrc^, P^, DstLen * SizeOf(Char));
      Inc(P, DstLen);
      P^ := #0;
    end;
  end;

  { Copies text from Index of Count chars and pads it with spaces if there
    were less than Count copied }
  function CopyPaddedAndForward(const S: UnicodeString; Index, Count: Integer;
    var P: PChar): Integer;
  var
    OldP: PWideChar;
    Len, I: Integer;
  begin
    Result := 0;
    OldP := P;
    CopyAndForward(S, Index, Count, P);
    Len := Count - (P - OldP);

    { Was anything copied at all or Index was behind line length? }
    if not (eoTrimTrailingSpaces in fOptions) and (P - OldP > 0) then
    begin
      for I := 0 to Len - 1 do
        P[I] := #32;
      Inc(P, Len);
    end
    else
      Result:= Len;
  end;

  { Returns true if a row specified is actually a wrapped row, not an unique
    string }
  function IsWrappedRow(ALine, ARow: Integer): Boolean;
  begin
    if WordWrap then
      Result := fWordWrapPlugin.LineToRealRow(ALine) <> ARow
    else
      Result := False;
  end;

  function DoGetSelText: UnicodeString;
  var
    First, Last, TotalLen: Integer;
    ColFrom, ColTo: Integer;
    I, L, R: Integer;
    S: UnicodeString;
    P: PChar;
    cRow: Integer;
    vAuxLineChar: TBufferCoord;
    vAuxRowCol: TDisplayCoord;
    vTrimCount: Integer;
  begin
    ColFrom := BlockBegin.Char;
    First := BlockBegin.Line - 1;
    ColTo := BlockEnd.Char;
    Last := BlockEnd.Line - 1;
    TotalLen := 0;
    case fActiveSelectionMode of
      smNormal:
        if (First = Last) then
          Result := Copy(Lines[First], ColFrom, ColTo - ColFrom)
        else begin
          { Calculate total length of result string }
          TotalLen := Max(0, ExpandLines.AccessStringLength(First) - ColFrom+1);
          for I := First + 1 to Last - 1 do
            Inc(TotalLen, ExpandLines.AccessStringLength(I));
          Inc(TotalLen, ColTo-1);
          Inc(TotalLen, Length(SLineBreak)*(Last - First));

          { Build up result string }
          SetLength(Result, TotalLen);
          P := PWideChar(Result);
          CopyAndForward(Lines[First], ColFrom, MaxInt, P);

          CopyAndForward(SLineBreak, 1, MaxInt, P);

          for I := First + 1 to Last - 1 do
          begin
            CopyAndForward(Lines[I], 1, MaxInt, P);
            CopyAndForward(SLineBreak, 1, MaxInt, P);
          end;
          CopyAndForward(Lines[Last], 1, ColTo - 1, P);
        end;

      { Columnar selections require much complex approach }
      smColumn:
        begin
          with BufferToRealDisplayPos(BlockBegin) do
          begin
            First := Row;
            ColFrom := Column;
          end;
          with BufferToRealDisplayPos(BlockEnd) do
          begin
            Last := Row;
            ColTo := Column;
          end;
          if ColFrom > ColTo then
            SwapInt(ColFrom, ColTo);

          { Pre-allocate string large enough for worst case }
          TotalLen := ((ColTo - ColFrom) + Length(sLineBreak)) * (Last - First + 1);
          SetLength(Result, TotalLen);
          P := PChar(Result);

          { Copy chunks to the pre-allocated string }
          TotalLen := 0;
          for cRow := First to Last do
          begin
            with vAuxRowCol do
            begin
              Row := cRow;
              Column := ColFrom;
            end;
            vAuxLineChar := RealDisplayToBufferPos(vAuxRowCol);
            if IsWrappedRow(vAuxLineChar.Line, cRow) then
              Continue;
            L := vAuxLineChar.Char;
            S := Lines[vAuxLineChar.Line - 1];
            vAuxRowCol.Column := ColTo;
            R := RealDisplayToBufferPos(vAuxRowCol).Char;
            vTrimCount := CopyPaddedAndForward(S, L, R - L, P);
            TotalLen := TotalLen + (R - L) - vTrimCount + Length(sLineBreak);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          SetLength(Result, Max(TotalLen - Length(sLineBreak), 0));
        end;
      smLine:
        begin
          { If block selection includes LastLine,
            line break code(s) of the last line will not be added }

          { Calculate total length of result string }
          for I := First to Last do
            Inc(TotalLen, Length(Lines[I]) + Length(SLineBreak));
          if Last = Pred(Lines.Count) then
            Dec(TotalLen, Length(SLineBreak));

          { Build up resulting string }
          SetLength(Result, TotalLen);
          P := PWideChar(Result);
          for I := First to Last - 1 do
          begin
            CopyAndForward(Lines[I], 1, MaxInt, P);
            CopyAndForward(SLineBreak, 1, MaxInt, P);
          end;
          CopyAndForward(Lines[Last], 1, MaxInt, P);
          if (Last + 1) < Lines.Count then
            CopyAndForward(SLineBreak, 1, MaxInt, P);
        end;
    end;
  end;

var
  I: Integer;
  BB, BE: TBufferCoord;
begin
  if not SelAvail and not fSelections then
    Result := EmptyStr
  else if fSelections then
  begin
    Result := EmptyStr;
    BB := BlockBegin;
    BE := BlockEnd;
    try
      for I := 0 to High(fCarets) do
        if not CaretsEqual(fCarets[I].bcStart, fCarets[I].bcEnd) then
        begin
          fBlockBegin := fCarets[I].bcStart;
          fBlockEnd := fCarets[I].bcEnd;
          Result := Result + DoGetSelText + SLineBreak;
        end;

      { Remove last added EOL }
      if Length(Result) >= Length(SLineBreak) then
        SetLength(Result, Length(Result) - Length(SLineBreak));
    finally
      fBlockBegin := BB;
      fBlockEnd := BE;
    end;
  end
  else
    Result := DoGetSelText;
end;

function TCustomSynEdit.SynGetText: UnicodeString;
begin
  Result := Lines.Text;
end;

function TCustomSynEdit.GetWordAtCursor: UnicodeString;
var
  bBegin: TBufferCoord;
  bEnd: TBufferCoord;
begin
  { Remember old selection }
  bBegin := GetBlockBegin;
  bEnd := GetBlockEnd;

  { Retrieve word }
  SetBlockBegin(WordStart);
  SetBlockEnd(WordEnd);
  Result := SelText;

  { Restore old selection }
  SetBlockBegin(bBegin);
  SetBlockEnd(bEnd);
end;

procedure TCustomSynEdit.HideCaret;
begin
  if sfCaretVisible in fStateFlags then
    if Windows.HideCaret(Handle) then
      Exclude(fStateFlags, sfCaretVisible);
end;

procedure TCustomSynEdit.IncPaintLock;
begin
  inc(fPaintLock);
end;

procedure TCustomSynEdit.InvalidateGutter;
begin
  InvalidateGutterLines(-1, -1);
end;

procedure TCustomSynEdit.InvalidateGutterLine(ALine: Integer);
begin
  if (ALine < 1) or (ALine > Lines.Count) then
    Exit;

  InvalidateGutterLines(ALine, ALine);
end;

// Note: FirstLine and LastLine don't need to be in correct order
// § Garnet
procedure TCustomSynEdit.InvalidateGutterLines(AFirstLine, ALastLine: Integer);
var
  rcInval: TRect;
  cFirstRow, cLastRow: Integer;
begin
  if Visible and HandleAllocated then

    { Repaint whole gutter }
    if (AFirstLine = -1) and (ALastLine = -1) then
    begin
      rcInval := Rect(0, 0, Gutter.Width, ClientHeight);

      if sfLinesChanging in fStateFlags then
        UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
      else
        InvalidateRect(rcInval, False);
    end

    { Repaint according to specified bounds }
    else begin

      { Find the visible lines first }
      if (ALastLine < AFirstLine) then
        SwapInt(ALastLine, AFirstLine);

      { Convert lines to rows }
      cFirstRow := Max(LineToRow(AFirstLine), fTopLine);
      if WordWrap then
        if ALastLine <= fLines.Count then
          cLastRow := Min(LineToRow(ALastLine + 1) - 1, fTopLine + fLinesInWindow)
        else
          cLastRow := fTopLine + fLinesInWindow
      else
        cLastRow := Min(LineToRow(ALastLine + 1) - 1, fTopLine + fLinesInWindow);

      { Any line visible? }
      if (cLastRow >= cFirstRow) then
      begin

        { Calculate repaint rect }
        rcInval := Rect(0, fTextHeight * (cFirstRow - fTopLine),
          Gutter.Width, fTextHeight * (cLastRow - fTopLine + 1));

        { Do repaint }
        if sfLinesChanging in fStateFlags then
          UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
        else
          InvalidateRect(rcInval, False);
      end;
    end;
end;

// -----------------------------------------------------------------------------
// § Garnet
// Note: FirstLine and LastLine don't need to be in correct order
procedure TCustomSynEdit.InvalidateLines(AFirstLine, ALastLine: Integer);
var
  rcInval: TRect;
  cFirstRow, cLastRow: Integer;
begin
  if Visible and HandleAllocated then

    { Repaint all text lines }
    if (AFirstLine = -1) and (ALastLine = -1) then
    begin
      rcInval := ClientRect;
      Inc(rcInval.Left, fGutter.Width);
      if sfLinesChanging in fStateFlags then
        UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
      else
        InvalidateRect(rcInval, False);
    end

    { Repaint according to specified bounds }
    else begin
      AFirstLine := Max(AFirstLine, 1);
      ALastLine := Max(ALastLine, 1);

      { Find the visible lines first }
      if (ALastLine < AFirstLine) then
        SwapInt(ALastLine, AFirstLine);

      { TopLine is in display coordinates, so FirstLine and LastLine must be
        converted previously }
      cFirstRow := Max(LineToRow(AFirstLine), fTopLine);
      if WordWrap then
        if ALastLine < fLines.Count then
          cLastRow  := Min(LineToRow(ALastLine + 1) - 1, fTopLine + fLinesInWindow)
        else
          cLastRow  := Min(LineToRow(ALastLine), fTopLine + fLinesInWindow)
      else
        cLastRow  := Min(LineToRow(ALastLine), fTopLine + fLinesInWindow);

      { Paint empty space beyond last line }
      if ALastLine >= Lines.Count then
        cLastRow := fTopLine + fLinesInWindow;

      { Any line visible? }
      if (ALastLine >= AFirstLine) then
      begin

        { Calculate repaint rect }
        rcInval := Rect(fGutter.Width, fTextHeight * (cFirstRow - fTopLine),
          ClientWidth, fTextHeight * (cLastRow - fTopLine + 1));

        { Do repaint }
        if sfLinesChanging in fStateFlags then
          UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
        else
          InvalidateRect(rcInval, False);
      end;
    end;
end;

procedure TCustomSynEdit.InvalidateGutterRow(ARow: integer);
begin
  if (ARow < 1) or (ARow > GetDisplayLineCount) then
    Exit;

  InvalidateGutterRows(ARow, ARow);
end;

procedure TCustomSynEdit.InvalidateGutterRows(AFirstRow, ALastRow: integer);
var
  rcInval: TRect;
begin
  if Visible and HandleAllocated then

    { Repaint whole gutter }
    if (AFirstRow = -1) and (ALastRow = -1) then
    begin
      rcInval := Rect(0, 0, Gutter.Width, ClientHeight);

      if sfLinesChanging in fStateFlags then
        UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
      else
        InvalidateRect(rcInval, False);
    end

    { Repaint according to specified bounds }
    else begin

      { Find the visible lines first }
      if (ALastRow < AFirstRow) then
        SwapInt(ALastRow, AFirstRow);

      { Convert lines to rows }
      AFirstRow := Max(AFirstRow, fTopLine);
      ALastRow  := Min(ALastRow,  fTopLine + fLinesInWindow);

      { Any line visible? }
      if (ALastRow >= AFirstRow) then
      begin

        { Calculate repaint rect }
        rcInval := Rect(0, fTextHeight * (AFirstRow - fTopLine),
          Gutter.Width, fTextHeight * (ALastRow - fTopLine + 1));

        { Do repaint }
        if sfLinesChanging in fStateFlags then
          UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
        else
          InvalidateRect(rcInval, False);
      end;
    end;
end;

procedure TCustomSynEdit.InvalidateRow(ARow: integer);
begin
  if (not HandleAllocated) or (ARow < 1) or (ARow > GetDisplayLineCount) or
    (not Visible)
  then
    Exit;

  InvalidateRows(ARow, ARow);
end;

procedure TCustomSynEdit.InvalidateRows(AFirstRow, ALastRow: integer);
var
  rcInval: TRect;
begin
  if Visible and HandleAllocated then

    { Repaint all rows }
    if (AFirstRow = -1) and (ALastRow = -1) then
    begin
      rcInval := ClientRect;
      Inc(rcInval.Left, Gutter.Width);
      if sfLinesChanging in fStateFlags then
        UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
      else
        InvalidateRect(rcInval, False);
    end

    { Repaint according to specified bounds }
    else begin

      { Find the visible lines first }
      if (ALastRow < AFirstRow) then
        SwapInt(ALastRow, AFirstRow);

      { TopLine is in display coordinates, so FirstLine and LastLine must be
        converted previously }
      AFirstRow := Max(AFirstRow, fTopLine);
      ALastRow  := Min(ALastRow,  fTopLine + fLinesInWindow);

      { Any row visible? }
      if (ALastRow >= AFirstRow) then
      begin

        { Calculate repaint rect }
        rcInval := Rect(Gutter.Width, fTextHeight*(AFirstRow - fTopLine),
          ClientWidth, fTextHeight*(ALastRow - fTopLine + 1));

        { Do repaint }
        if sfLinesChanging in fStateFlags then
          UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
        else
          InvalidateRect(rcInval, False);
      end;
    end;
end;

procedure TCustomSynEdit.InvalidateSelection;
begin
  InvalidateLines(BlockBegin.Line, BlockEnd.Line);
end;

procedure TCustomSynEdit.KeyUp(var Key: Word; Shift: TShiftState);
var
  CharCode: Integer;
  KeyMsg: TWMKey;
begin
  if (Classes.ssAlt in Shift) and (Key >= VK_NUMPAD0) and (Key <= VK_NUMPAD9) then
    FCharCodeString := FCharCodeString + IntToStr(Key - VK_NUMPAD0);

  if Key = VK_MENU then
  begin
    if (FCharCodeString <> '') and TryStrToInt(FCharCodeString, CharCode) and
      (CharCode >= 256) and (CharCode <= 65535) then
    begin
      KeyMsg.Msg := WM_CHAR;
      KeyMsg.CharCode := CharCode;
      KeyMsg.Unused := 0;
      KeyMsg.KeyData := 0;
      DoKeyPressW(KeyMsg);
      FIgnoreNextChar := True;
    end;
    FCharCodeString := EmptyStr;
  end;

  inherited;

  fKbdHandler.ExecuteKeyUp(Self, Key, Shift);
end;

procedure TCustomSynEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Data: pointer;
  C: WideChar;
  Cmd: TSynEditorCommand;
  SynShift: TSynShiftState;
begin
  inherited;

  if Key = 0 then
  begin
    Include(fStateFlags, sfIgnoreNextChar);
    Exit;
  end;

  if fSnippet and not fColumn then
    if Key = VK_TAB then
    begin
      if Shift = [] then
        NextSnippetVar
      else if Shift = [Classes.ssShift] then
        PrevSnippetVar;
      Key := 0;
    end;

  SynShift := ShiftStateToSynShiftState(Shift, IsWinPressed);
  fKbdHandler.ExecuteKeyDown(Self, Key, SynShift);
  if Key = 0 then
  begin
    Include(fStateFlags, sfIgnoreNextChar);
    Exit;
  end;

  Data := nil;
  C := #0;
  try
    Cmd := TranslateKeyCode(Key, SynShift, Data);
    if Cmd <> ecNone then
    begin
      Key := 0;
      Include(fStateFlags, sfIgnoreNextChar);
      CommandProcessor(Cmd, C, Data);
    end
    else
      Exclude(fStateFlags, sfIgnoreNextChar);
  finally
    if Data <> nil then
      FreeMem(Data);
  end;
end;

procedure TCustomSynEdit.Loaded;
begin
  inherited Loaded;
  GutterChanged(Self);
  UpdateScrollBars;
end;

procedure TCustomSynEdit.KeyPress(var Key: Char);
begin
  { Don't do anything here }
end;

type
  TAccessWinControl = class(TWinControl);

procedure TCustomSynEdit.DoKeyPressW(var Message: TWMKey);
var
  Form: TCustomForm;
  Key: Char;
begin
  if FIgnoreNextChar then
  begin
    FIgnoreNextChar := False;
    Exit;
  end;

  Key := Char(Message.CharCode);

  Form := GetParentForm(Self);
  if (Form <> nil) and (Form <> TWinControl(Self)) and Form.KeyPreview and
    (Key <= High(AnsiChar)) and TAccessWinControl(Form).DoKeyPress(Message)
  then
    Exit;
  Key := Char(Message.CharCode);

  if (csNoStdEvents in ControlStyle) then Exit;

  if Assigned(FOnKeyPressW) then
    FOnKeyPressW(Self, Key);

  if WideChar(Key) <> #0 then
    KeyPressW(Key);
end;

procedure TCustomSynEdit.KeyPressW(var Key: WideChar);
begin
  { Don't fire the event if key is to be ignored }
  if not (sfIgnoreNextChar in fStateFlags) then
  begin
    fKbdHandler.ExecuteKeyPress(Self, Key);
    CommandProcessor(ecChar, Key, nil);
  end
  else
    { Don't ignore further keys }
    Exclude(fStateFlags, sfIgnoreNextChar);
end;

function TCustomSynEdit.LeftSpaces(const Line: UnicodeString): Integer;
begin
  Result := LeftSpacesEx(Line, False);
end;

function TCustomSynEdit.LeftSpacesEx(const Line: UnicodeString; WantTabs: Boolean): Integer;
var
  p: PChar;
begin
  p := PChar(Line);
  if Assigned(p) and (eoAutoIndent in fOptions) then
  begin
    Result := 0;
    while (p^ >= #1) and (p^ <= #32) do
    begin
      if (p^ = #9) then
        if WantTabs then
          Inc(Result, TabWidth)
        else
          Inc(Result)
      else
        Inc(Result);
      Inc(p);
    end;
  end
  else
    Result := 0;
end;

{ Checks if all characters are whitespace on current line up to the current
  caret pos. If it so, we can use smart tabs }
function TCustomSynEdit.IsAllWhiteUpToCaret(const Ln: UnicodeString;
  Border: Integer = 0): Boolean;
var
  J, Len, Caret: Integer;
begin
  Len := Length(Ln);
  if Border = 0 then
    if SelAvail then
      Caret := Min(fBlockBegin.Char, fBlockEnd.Char)
    else
      Caret := fCaretX
  else
    Caret := Border;
  if (Len = 0) or (Caret = 1) then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
  J := 1;
  while (J <= Len) and (J < Caret) do
  begin
    if Ln[J] > #32 then
      Exit;
    Inc(J);
  end;
  Result := True;
end;

function TCustomSynEdit.IsAllTabsUpToCaret(const Ln: UnicodeString;
  Border: Integer = 0): Boolean;
var
  J, Len, Caret: Integer;
begin
  Len := Length(Ln);
  if Border = 0 then
    if SelAvail then
      Caret := Min(fBlockBegin.Char, fBlockEnd.Char)
    else
      Caret := fCaretX
  else
    Caret := Border;
  if (Len = 0) or (Caret = 1) then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
  J := 1;
  while (J <= Len) and (J < Caret) do
  begin
    if Ln[J] <> #9 then
      Exit;
    Inc(J);
  end;
  Result := True;
end;

function TCustomSynEdit.GetLeftSpacing(CharCount: Integer;
  WantTabs: Boolean): UnicodeString;
begin
  if WantTabs and not (eoTabsToSpaces in Options) and
    (CharCount >= TabWidth)
  then
    Result := UnicodeString(StringOfChar(#9, CharCount div TabWidth) +
      StringOfChar(#32, CharCount mod TabWidth))
  else
    Result := UnicodeString(StringOfChar(#32, CharCount));
end;

procedure TCustomSynEdit.LinesChanging(Sender: TObject);
begin
  Include(fStateFlags, sfLinesChanging);
end;

procedure TCustomSynEdit.LinesChanged(Sender: TObject);
var
  vOldMode: TSynSelectionMode;
begin
  Exclude(fStateFlags, sfLinesChanging);
  if HandleAllocated then
  begin
    UpdateScrollBars;
    vOldMode := fActiveSelectionMode;
    SetBlockBegin(CaretXY);
    fActiveSelectionMode := vOldMode;
    InvalidateRect(fInvalidateRect, False);
    FillChar(fInvalidateRect, SizeOf(TRect), 0);
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;
  end;
end;

procedure TCustomSynEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  bWasSel,
  bCanLineSelect,
  bStartDrag: Boolean;
  TmpBegin, TmpEnd: TBufferCoord;
  OldCaretX, OldCaretY: Integer;
begin
  { Remember initial values }
  TmpBegin := BlockBegin;
  TmpEnd := BlockEnd;

  { Remember selection state, as it will be cleared later }
  bWasSel := False;
  bStartDrag := False;
  bCanLineSelect := False;
  if (Button = mbLeft) and not fSelections then
  begin
    bCanLineSelect := not (sfDblClicked in fStateFlags) and
      fLineSelectionTimer.Enabled and
      (Abs(fMouseDownX - X) <= GetSystemMetrics(SM_CXDOUBLECLK)) and
      (Abs(fMouseDownY - Y) <= GetSystemMetrics(SM_CYDOUBLECLK));
    fMouseDownX := X;
    fMouseDownY := Y;
    if SelAvail then
    begin
      bWasSel := True;
    end;
  end;

  { Initialzie }
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and (ssDouble in Shift) then
    Exit;

  { Can move right edge? }
  if fRightEdgeShow and (fRightEdge > 0) then
    if (Button = mbLeft) and (Classes.ssCtrl in Shift) and (Abs(RowColumnToPixels(DisplayCoord(
      fRightEdge + 1, 0)).X - X) < 3) then
    begin
      fRightEdgeMoving := True;
      Exit;
    end;

  { Chained handlers first }
  fKbdHandler.ExecuteMouseDown(Self, Button, Shift, X, Y);

  { Manage columnar selection }
  if fActiveSelectionMode <> smLine then
    if Classes.ssAlt in Shift then
      SelectionMode := smColumn
    else
      SelectionMode := smNormal;

  { Move caret }
  OldCaretX := fCaretX;
  OldCaretY := fCaretY;
  if (Button in [mbLeft, mbRight]) then
  begin
    if fSelections and (Classes.ssCtrl in Shift) then
    begin
      bCanLineSelect := True;
      TmpBegin := InternalComputeCaret(X, Y);
      for OldCaretX := 0 to High(fCarets) do
        if CaretInRange(TmpBegin, fCarets[OldCaretX].bcStart, fCarets[OldCaretX].bcEnd, True) then
        begin
          bCanLineSelect := False;
          Break;
        end;
      if bCanLineSelect then
        AddSmartCaret(TmpBegin, TmpBegin, TmpBegin);
    end
    else begin
      if Button = mbRight then
      begin
        if {(eoRightMouseMovesCursor in Options) and}
          (SelAvail and
            not IsPointInSelection(DisplayToBufferPos(PixelsToRowColumn(X, Y)))
          or not SelAvail) then
        begin
          InvalidateSelection;
          FBlockEnd := FBlockBegin;
          ComputeCaret(X, Y);
        end
        else
          Exit;
      end
      else
        ComputeCaret(X, Y);

      { Can enter snippet mode? }
      if (Classes.ssCtrl in Shift) and (Button = mbLeft) and
        not CaretInRange(CaretXY, TmpBegin, TmpEnd, True) then
      begin
        { Seize current selection }
        SetCaretAndSelection(CaretXY, CaretXY, CaretXY);

        { Enter multiple selections mode }
        fSnippet := True;
        fSelections := True;

        { Hide original caret (it will be substituted during painting) }
        Exclude(fStateFlags, sfCaretVisible);
        DestroyCaret;

        { Add old & new caret }
        AddSmartCaret(BufferCoord(OldCaretX, OldCaretY), TmpBegin, TmpEnd);
        AddSmartCaret(CaretXY, CaretXY, CaretXY);
      end;
    end;
  end;

  { Trim if not modifying selecion }
  if not (Classes.ssShift in Shift) and not fSelections then
  begin
    DoTrimTrailingSpaces(OldCaretY); // Trim line we leaved
    if DoTrimTrailingSpaces(fCaretY) > 0 then
      if not (eoScrollPastEol in fOptions)  then
        CaretX := CaretX; // This is necessary because user could click
                          // on trimmed area and caret would appear behind
                          // line length when not in eoScrollPastEol mode
  end;

  { Line selection? }
  if (OldCaretY = fCaretY) and bCanLineSelect and not fSelections then
  begin
    fLineSelectionTimer.Enabled := False;
    fBlockBegin := BufferCoord(1, fCaretY);
    fBlockEnd := BufferCoord(MaxScrollWidth,
      fCaretY);
    ActiveSelectionMode := smLine;
    InvalidateSelection;
    Exit;
  end;

  if (Button = mbLeft) and not fSelections then
  begin
    { I couldn't track down why, but sometimes (and definately not all the time)
      the block positioning is lost. This makes sure that the block is
      maintained in case they started a drag operation on the block }
    FBlockBegin := TmpBegin;
    FBlockEnd := TmpEnd;

    MouseCapture := True;

    { If mousedown occurred in selected block begin drag operation }
    Exclude(fStateFlags, sfWaitForDragging);
    if bWasSel and (eoDragDropEditing in fOptions) and (X >= Gutter.Width + 2)
      and IsPointInSelection(DisplayToBufferPos(PixelsToRowColumn(X, Y)))
    then
      bStartDrag := True
  end;

  if not fSelections then
  if (Button = mbLeft) and bStartDrag then
    Include(fStateFlags, sfWaitForDragging)
  else begin
    if not (sfDblClicked in fStateFlags) then
    begin
      if Classes.ssShift in Shift then
        { BlockBegin and BlockEnd are restored to their original position in the
          code from above and SetBlockEnd will take care of proper invalidation }
        SetBlockEnd(CaretXY)
      else begin



        { Selection mode must be set before calling SetBlockBegin }
        SetBlockBegin(CaretXY);
      end;
    end;
  end;

  if (X < Gutter.Width) then
    Include(fStateFlags, sfPossibleGutterClick);
  if (sfPossibleGutterClick in fStateFlags) and (Button = mbRight) then
    DoOnGutterClick(Button, X, Y);
end;

// -----------------------------------------------------------------------------
// § Garnet
procedure TCustomSynEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
const
  sEnd: String = '…';           // Character inserted when line exceeds iMax
  sFin: String = '...';         // Cut-string to insert when there's more than 16 lines to display
  iMax: SmallInt = 128;         // Maximum chars per line without leading whitespace
var
  B: TBufferCoord;
  P: TDisplayCoord;             // Code folding hint rect position in editor screen coords
  Pt: TPoint;                   // Real screen coords

  FoldRange: TSynEditFoldRange; // Fold range associated with hint rect under mouse
  CodeFoldingHint: THintWindow; // Hint with excerpt of fold range text
  RightEdgeHint: THintWindow;   // Hint with current column number

  L,                            // Line
  I,                            // Counter
  J,                            // Hint text length
  D,                            // Chars to trim from leading whitespace
  C,                            // Maximum found length of line in pixels (width of hint rect)
  H,                            // Height of hint rect
  S: Integer;                   // Helper (plays various roles)

  jFrom, jTo: Integer;          // For dimming

  jHint: string;                // Hint text
  jRect: TRect;                 // Hint rect

  Ln: PChar;                    // Line being analyzed
  Len: Integer;                 // Length of line being analyzed

  Params: DRAWTEXTPARAMS;       // Provides size of tab stops to DrawTextEx()

  { Hide code folding hint }
  procedure AfterCollapsedHintCleanUp;
  begin
    fCodeFoldingHint := False;
  	ShowWindow(GetCodeFoldingHint.Handle, SW_HIDE);
  end;

begin
  { Parent's stuff }
  inherited MouseMove(Shift, X, Y);

  { Cancel dim? }
  if (X <> fLastMouseX) and fRepaintAfterDimNeeded then
  begin
    L := RowToLine(PixelsToNearestRowColumn(X, Y).Row);
    I := 0;

    { On gutter? }
    if (X < fGutter.Width) and (L > 0) and (L <= fLines.Count) then
    begin
      SynEditFindClosest(Self, L, fHighlighter.FoldRegions, jFrom, jTo);
      if (jFrom <> fDimFirstLine) or (jTo <> fDimLastLine) then
        I := 1;
    end
    else
      I := 1;

    if I = 1 then
    begin
      fRepaintAfterDimNeeded := False;
      Invalidate;
    end;
  end;

  { On gutter? }
  if X < fGutter.Width then
  begin
    if X > fGutter.Width - 14 then
    begin
      { Reset timer }
      with fOutliningTimer do
      begin
        Enabled := False;
        Enabled := True;
      end;
    end
    else
      fOutliningTimer.Enabled := False;
  end
  else
    fOutliningTimer.Enabled := False;

  { Remember this pos. The Windows will constantly resend the MouseOver
    notifications with small interval. We use this variable to
    distinguish such notifications from real mouse moves }
  fLastMouseX := X;

  { Support for dragging right edge with mouse }
  if fRightEdgeShow then
  begin
    fMouseAtRightEdge := (Classes.ssCtrl in Shift) and (Abs(RowColumnToPixels(DisplayCoord(
      fRightEdge + 1, 0)).X - X) < 3);

    If fRightEdgeMoving then
    begin
      RightEdgeHint := GetRightEdgeMovingHint;
      jRect := RightEdgeHint.CalcHintRect(
        RightEdgeHint.Canvas.TextWidth('Column: ') +
          Length(IntToStr(fLeftChar + fCharsInWindow)) + 20,
        Format(SYNS_RightEdgeInfoFmt, [fRightEdge]), nil
      );
      with jRect do
      begin
        Left := X + ClientToScreen(ClientRect.TopLeft).X;
        Top := Y + ClientToScreen(ClientRect.TopLeft).Y;
        Right := Left + Right;
        Bottom := Top + Bottom;
      end;

      { Show hint }
      with RightEdgeHint do
      begin
        Color := fScrollHintColor;
        ActivateHint(jRect, Format(SYNS_RightEdgeInfoFmt, [fRightEdge]));
        Update;
      end;
      RightEdge := MinMax(PixelsToRowColumn(X, Y).Column, 48, 512);
      Exit;
    end
    else
      if GetRightEdgeMovingHint <> nil then
        ShowWindow(GetRightEdgeMovingHint.Handle, SW_HIDE);
  end;

  { Code folding hint }
  if (Shift = []) and CodeFolding.Enabled and not fMouseAtRightEdge then
  begin
    P := PixelsToNearestRowColumn(X, Y);
    FoldRange := FoldRangeForLine(RowToLine(P.Row));
    if Assigned(FoldRange) then
    begin
      S := (LeftChar-1)*fCharWidth;
      Pt.X := X;
      Pt.Y := Y;
      jRect := GetCollapseMarkRect(FoldRange, P.Row, RowToLine(P.Row));
      if jRect.Right - S > 0 then
      begin
        { Shrink rect }
        OffsetRect(jRect, -S, 0);

        { Check if pointer is inside rect }
        if PtInRect(jRect, Pt) then
        begin
          { Access hint }
          CodeFoldingHint := GetCodeFoldingHint;
          CodeFoldingHint.Canvas.Font.Name := Font.Name;
          CodeFoldingHint.Canvas.Font.Size := 8;
          if not Assigned(CodeFoldingHint) then
            Exit;
          fCodeFoldingHint := True;

          { Find possible length to trim of white chars in the beginning. I
            know I sound like I hadn't slept for a day }
          D := -1;
          for I := FoldRange.FromLine to Min(FoldRange.ToLine,
            FoldRange.FromLine + 16) do
          begin
            Len := ExpandLines.AccessStringLength(I - 1);
            if Len > 0 then
            begin
              S := 0;
              Ln := ExpandLines.AccessBuffer(I-1);
              while (Ln[S] < #33) and (S < Len) do
                Inc(S);
              if D = -1 then
                D := S;
              if (S < D) then
                D := S;
            end;
          end;

          { Calculate hint string length }
          J := 0;
          for I := FoldRange.FromLine to Min(FoldRange.ToLine,
            FoldRange.FromLine + 16) do
          begin
            Len := ExpandLines.AccessStringLength(I - 1);
            if Len > 0 then
            begin
              if Len - D > iMax then
                Inc(J, iMax+1)
              else
                Inc(J, Len - D);
            end;
            if I < Min(FoldRange.ToLine, FoldRange.FromLine + 16) then
              Inc(J, Length(sLineBreak));
          end;
          if FoldRange.ToLine > FoldRange.FromLine + 16 then
            Inc(J, Length(sFin) + Length(sLineBreak));

          { Assemble hint string and calculate hint rect. DrawTextEx() by some
            reason doesn't support correct calculation of rect width on
            multiline strings. Blame Microsoft. Actually, this may be even
            faster since we don't need DT_WORDBREAK and can skip calculatuion
            on some lines sometimes. It's just multiple calls to DrawTextEx
            which I think *possibly* can be slow. Also, we use pointers and
            constant lengths which adds to speed }
          S := 1;
          SetLength(jHint, J);
          J := CodeFoldingHint.Canvas.TextHeight('Wj');
          C := 0; H := 0;
          FillChar(Params, SizeOf(Params), #0);
          with Params do
          begin
            cbSize := SizeOf(Params);
            iTabLength := 2; // Since hint doesn't display in fixed-pitch font
                             // there's no reason to use editor's tab size as
                             // it won't provide correct text alignment either
          end;

          { Loop once again }
          for I := FoldRange.FromLine to Min(FoldRange.ToLine,
            FoldRange.FromLine + 16) do
          begin
            Len := ExpandLines.AccessStringLength(I-1);
            if Len > 0 then
            begin
              Ln := ExpandLines.AccessBuffer(I-1);
              jRect := Rect(0, 0, 0, 0);

              if Len - D > iMax then
              begin
                Move(Ln[D], jHint[S], iMax*SizeOf(Char));
                Move(sEnd[1], jHint[S+iMax], Length(sEnd)*SizeOf(Char));

                DrawTextEx(CodeFoldingHint.Canvas.Handle, @jHint[S],
                  iMax+1, jRect,
                  DT_CALCRECT or DT_LEFT or DT_NOPREFIX or DT_EXPANDTABS or
                  DT_TABSTOP or DrawTextBiDiModeFlagsReadingOnly,
                  @Params);

                C := Max(C, jRect.Right);
                Inc(H, jRect.Bottom);

                Inc(S, iMax+1);
              end
              else begin
                Move(Ln[D], jHint[S], (Len-D)*SizeOf(Char));

                DrawTextEx(CodeFoldingHint.Canvas.Handle, @jHint[S],
                  Len-D, jRect,
                  DT_CALCRECT or DT_LEFT or DT_NOPREFIX or DT_EXPANDTABS or
                  DT_TABSTOP or DrawTextBiDiModeFlagsReadingOnly,
                  @Params);

                C := Max(C, jRect.Right);
                Inc(H, jRect.Bottom);

                Inc(S, Len-D);
              end;
            end
            else
              Inc(H, J);
            if I < Min(FoldRange.ToLine, FoldRange.FromLine + 16) then
            begin
              Move(sLineBreak[1], jHint[S], Length(sLineBreak) * SizeOf(Char));
              Inc(S, Length(sLineBreak));
            end;
          end;
          if FoldRange.ToLine > FoldRange.FromLine + 16 then
          begin
            Move(sLineBreak[1], jHint[S], Length(sLineBreak) * SizeOf(Char));
            Move(sFin[1], jHint[S + Length(sLineBreak)],
              Length(sFin) * SizeOf(Char));

            Inc(H, J);
          end;

          { Find left & top position to show hint }
          Pt     := RowColumnToPixels(DisplayCoord(P.Column, P.Row + 1));
          Inc(Pt.X, ClientToScreen(ClientRect.TopLeft).X);
          Inc(Pt.Y, ClientToScreen(ClientRect.TopLeft).Y);

          { Get hint rect }
          Inc(C, 6);
          Inc(H, 4);
          jRect := Rect(0, 0, C, H);
          with jRect do
          begin
            Inc(Left, Pt.X);
            Inc(Top, Pt.Y);
            Inc(Right, Pt.X);
            Inc(Bottom, Pt.Y);
          end;

          { Show hint }
          with CodeFoldingHint do
          begin
            Color := fScrollHintColor;
            ActivateHint(jRect, jHint);
            Update;
          end;
        end
        else
          AfterCollapsedHintCleanUp;
      end
      else
        AfterCollapsedHintCleanUp;
    end
    else
      AfterCollapsedHintCleanUp
  end
  else
    AfterCollapsedHintCleanUp;

  { Drag & Drop }
  if MouseCapture and (sfWaitForDragging in fStateFlags) and not fSelections then
  begin
    if (Abs(fMouseDownX - X) >= GetSystemMetrics(SM_CXDRAG))
      or (Abs(fMouseDownY - Y) >= GetSystemMetrics(SM_CYDRAG)) then
    begin
      Exclude(fStateFlags, sfWaitForDragging);
      BeginDrag(False);
    end;
  end
  else if (ssLeft in Shift) and MouseCapture then
  begin
    { Should we begin scrolling? }
    ComputeScroll(X, Y);

    { Compute new caret }
    P := PixelsToNearestRowColumn(X, Y);
    P.Row := MinMax(P.Row, 1, DisplayLineCount);
    if fScrollDeltaX <> 0 then
      P.Column := DisplayX;
    if fScrollDeltaY <> 0 then
      P.Row := DisplayY;
    if fSelections and (fCurrCaret > -1) and (fCurrCaret < Length(fCarets)) then
    with fCarets[fCurrCaret] do
    try

      { Compute new buffer coord }
      B := DisplayToBufferPos(P);

      { Reset block bounds }
      if CaretsEqual(bcCaret, bcStart) then
        bcStart := B
      else
        bcEnd := B;

      { Fix bounds errors }
      if (bcStart.Line > bcEnd.Line) or ((bcStart.Line = bcEnd.Line) and
        (bcStart.Char > bcEnd.Char))
      then
        SwapCarets(bcStart, bcEnd);

      { Apply new caret }
      bcCaret := B;

      { Fix each caret X coord }
      bcCaret.Char := MinMax(bcCaret.Char, 1, Succ(fLines.AccessStringLength(Pred(bcCaret.Line))));
      bcCaret.Line := MinMax(bcCaret.Line, 1, fLines.Count);
      bcStart.Char := MinMax(bcStart.Char, 1, Succ(fLines.AccessStringLength(Pred(bcStart.Line))));
      bcStart.Line := MinMax(bcStart.Line, 1, fLines.Count);
      bcEnd.Char := MinMax(bcEnd.Char, 1, Succ(fLines.AccessStringLength(Pred(bcEnd.Line))));
      bcEnd.Line := MinMax(bcEnd.Line, 1, fLines.Count);

      { Redraw }
      NormalizeSelections;
      Invalidate;
    finally
    end
    else begin
      InternalCaretXY := DisplayToBufferPos(P);
      BlockEnd := CaretXY;
      if (sfPossibleGutterClick in fStateFlags) and (FBlockBegin.Line <> CaretXY.Line) then
        Include(fStateFlags, sfGutterDragging);
    end;
  end;
end;

procedure TCustomSynEdit.FoldingTimerHandler(Sender: TObject);
var
  jLine, jFrom, jTo: Integer;
begin
  if fRepaintAfterDimNeeded then
    Exit;
  fOutliningTimer.Enabled := False;

  jLine := DisplayToBufferPos(PixelsToRowColumn(fGutter.Width + 2,
    Self.ScreenToClient(Mouse.CursorPos).Y)).Line;
  if (jLine < 1) or (jLine > fLines.Count) then
    Exit;

  SynEditFindClosest(Self, jLine, fHighlighter.FoldRegions, jFrom, jTo);
  if (jFrom > 0) and (jTo > 0) then
  begin
    fRepaintAfterDimNeeded := False;
    fDimmed := True;
    fDimFirstLine := jFrom;
    fDimLastLine := jTo;
    Invalidate;
  end;
end;

// -----------------------------------------------------------------------------
// Stop line selection timer
procedure TCustomSynEdit.LineSelectionTimerHandle(Sender: TObject);
begin
  fLineSelectionTimer.Enabled := False;
end;

procedure TCustomSynEdit.ScrollTimerHandler(Sender: TObject);
var
  iMousePos: TPoint;
  C: TDisplayCoord;
  X, Y: Integer;
  vCaret: TBufferCoord;
begin
  GetCursorPos(iMousePos);
  iMousePos := ScreenToClient(iMousePos);
  C := PixelsToRowColumn(iMousePos.X, iMousePos.Y);
  C.Row := MinMax(C.Row, 1, DisplayLineCount);
  if fScrollDeltaX <> 0 then
  begin
    LeftChar := LeftChar + fScrollDeltaX;
    X := LeftChar;
    if fScrollDeltaX > 0 then  // Scrolling right
      Inc(X, CharsInWindow);
    C.Column := X;
  end;
  if fScrollDeltaY <> 0 then
  begin
    if GetKeyState(SYNEDIT_SHIFT) < 0 then
      TopLine := TopLine + fScrollDeltaY * LinesInWindow
    else
      TopLine := TopLine + fScrollDeltaY;
    Y := TopLine;
    if fScrollDeltaY > 0 then  // scrolling down?
      Inc(Y, LinesInWindow - 1);
    C.Row := MinMax(Y, 1, DisplayLineCount);
  end;

  if not fSelections then // GARNET: dirty fix?
  begin
    vCaret := DisplayToBufferPos(C);
    if (CaretX <> vCaret.Char) or (CaretY <> vCaret.Line) then
    begin
      // changes to line / column in one go
      IncPaintLock;
      try
        InternalCaretXY := vCaret;
        // if MouseCapture is True we're changing selection. otherwise we're dragging
        if MouseCapture then
          SetBlockEnd(CaretXY);
      finally
        DecPaintLock;
      end;
    end;
  end;
  ComputeScroll(iMousePos.x, iMousePos.y);
end;

procedure TCustomSynEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  fKbdHandler.ExecuteMouseUp(Self, Button, Shift, X, Y);

  if fRightEdgeMoving then
  begin
    fRightEdgeMoving := False;
    StatusChanged([scRightEdge]);
    Exit;
  end;

  EnterColumnMode;

  fScrollTimer.Enabled := False;
  if (Button = mbRight) and (Shift = [ssRight]) and Assigned(PopupMenu) then
    exit;
  MouseCapture := False;
  if (sfPossibleGutterClick in fStateFlags) and (X < Gutter.Width) and (Button <> mbRight) then
  begin
    DoOnGutterClick(Button, X, Y)
  end
  else if fStateFlags * [sfDblClicked, sfWaitForDragging] = [sfWaitForDragging] then
  begin
    ComputeCaret(X, Y);
    if not(Classes.ssShift in Shift) then
      SetBlockBegin(CaretXY);
    SetBlockEnd(CaretXY);
    Exclude(fStateFlags, sfWaitForDragging);
  end;
  Exclude(fStateFlags, sfDblClicked);
  Exclude(fStateFlags, sfPossibleGutterClick);
  Exclude(fStateFlags, sfGutterDragging);
end;

procedure TCustomSynEdit.AddSmartCaret(const C, B, E: TBufferCoord);
var
  I, J: Integer;
begin
  { Add element }
  SetLength(fCarets, Succ(Length(fCarets)));

  { Find a place where to insert }
  I := High(fCarets);
  while I > 0 do
  begin
    Dec(I);
    if CaretBefore(fCarets[I].bcCaret, C) then
    begin
      Inc(I);
      Break;
    end;
  end;

  { Sort }
  for J := I to Pred(High(fCarets)) do
    fCarets[Succ(J)] := fCarets[J];

  { Fill it }
  with fCarets[I] do
  begin
    fCurrCaret := I;
    {$IFDEF DEBUG}
    WriteLn('Added smart caret with index ', fCurrCaret);
    {$ENDIF}
    nIndex := 1;
    nNestLevel := 1;
    bMirror := Length(fCarets) > 1;
    bcCaret := C;
    bcStart := B;
    bcEnd := E;
    nOldCaretX := C.Char;
  end;
  Invalidate;
end;

procedure TCustomSynEdit.RemoveSmartCaret(const Index: Integer);
var
  I: Integer;
  B1, B2, B3: TBufferCoord;
begin
  { Check }
  if (Index < 0) or (Index > High(fCarets)) then
    Exit;

  { Fix current caret index }
  if fCurrCaret = Index then
    fCurrCaret := -1
  else if fCurrCaret > Index then
    Dec(fCurrCaret);

  { Remove caret from array }
  for I := Index to Pred(High(fCarets)) do
    fCarets[Index] := fCarets[Succ(Index)];
  SetLength(fCarets, High(fCarets));

  { One caret left? }
  if Length(fCarets) = 1 then
  begin

    { Remember last caret bounds before leaving snippet mode }
    B1 := fCarets[0].bcCaret;
    B2 := fCarets[0].bcStart;
    B3 := fCarets[0].bcEnd;

    { Leave snuppet mode }
    CancelSnippet;

    { Apply selection bounds }
    if CaretsEqual(B1, B2) then
      MoveCaretAndSelection(B3, B2, True)
    else
      MoveCaretAndSelection(B2, B3, True);

    { Done }
    Exit;
  end;

  { Check if non-mirror exists }
  B1.Char := 0;
  for I := 0 to High(fCarets) do
    if not fCarets[I].bMirror then
    begin
      B1.Char := 1;
      Break;
    end;
  if B1.Char = 0 then
    fCarets[I].bMirror := False;

  { Fix current caret }
  if fCurrCaret < 0 then
    fCurrCaret := 0;
end;

procedure TCustomSynEdit.NormalizeSelections;

  function MustBeMerged(const C1, C2: Integer): Boolean;
  begin
    { Initialize }
    Result := False;

    { Start caret inside another block }
    if ((fCarets[C1].bcStart.Line > fCarets[C2].bcStart.Line) or
      ((fCarets[C1].bcStart.Line = fCarets[C2].bcStart.Line) and
      (fCarets[C1].bcStart.Char > fCarets[C2].bcStart.Char)))
      and
      ((fCarets[C1].bcStart.Line < fCarets[C2].bcEnd.Line) or
      ((fCarets[C1].bcStart.Line = fCarets[C2].bcEnd.Line) and
      (fCarets[C1].bcStart.Char < fCarets[C2].bcEnd.Char)))
    then
      Result := True

    { End caret inside another block }
    else if ((fCarets[C1].bcEnd.Line > fCarets[C2].bcStart.Line) or
      ((fCarets[C1].bcEnd.Line = fCarets[C2].bcStart.Line) and
      (fCarets[C1].bcEnd.Char > fCarets[C2].bcStart.Char)))
      and
      ((fCarets[C1].bcEnd.Line < fCarets[C2].bcEnd.Line) or
      ((fCarets[C1].bcEnd.Line = fCarets[C2].bcEnd.Line) and
      (fCarets[C1].bcEnd.Char < fCarets[C2].bcEnd.Char)))
    then
      Result := True

    { Both blocks start / end on the same pos }
    else if CaretsEqual(fCarets[C1].bcStart, fCarets[C2].bcStart) or
      CaretsEqual(fCarets[C1].bcEnd, fCarets[C2].bcEnd)
    then
      Result := True

    { Another block inside our block }
    else if ((fCarets[C2].bcStart.Line > fCarets[C1].bcStart.Line) or
      ((fCarets[C2].bcStart.Line = fCarets[C1].bcStart.Line) and
      (fCarets[C2].bcStart.Char > fCarets[C1].bcStart.Char)))
      and
      ((fCarets[C2].bcStart.Line < fCarets[C1].bcEnd.Line) or
      ((fCarets[C2].bcStart.Line = fCarets[C1].bcEnd.Line) and
      (fCarets[C2].bcStart.Char < fCarets[C1].bcEnd.Char)))
      and
      ((fCarets[C2].bcEnd.Line > fCarets[C1].bcStart.Line) or
      ((fCarets[C2].bcEnd.Line = fCarets[C1].bcStart.Line) and
      (fCarets[C2].bcEnd.Char > fCarets[C1].bcStart.Char)))
      and
      ((fCarets[C2].bcEnd.Line < fCarets[C1].bcEnd.Line) or
      ((fCarets[C2].bcEnd.Line = fCarets[C1].bcEnd.Line) and
      (fCarets[C2].bcEnd.Char < fCarets[C1].bcEnd.Char)))
    then
      Result := True;
  end;

  procedure DoMerge(const C1, C2: Integer);
  var
    bAtEnd: Boolean;
  begin
    bAtEnd := CaretsEqual(fCarets[C1].bcCaret, fCarets[C1].bcEnd);
    fCarets[C1].bcStart := MinCaret(fCarets[C1].bcStart, fCarets[C2].bcStart);
    fCarets[C1].bcEnd := MaxCaret(fCarets[C1].bcEnd, fCarets[C2].bcEnd);
    if bAtEnd then
      fCarets[C1].bcCaret := fCarets[C1].bcEnd
    else
      fCarets[C1].bcCaret := fCarets[C1].bcStart;
    RemoveSmartCaret(C2);
  end;

var
  I, J: Integer;
begin
  { Initialize }
  if Length(fCarets) < 2 then
    Exit;

  { First, work with current caret }
  if fCurrCaret > -1 then
    for I := High(fCarets) downto 0 do
    begin
      if I = fCurrCaret then
        Continue;
      if MustBeMerged(fCurrCaret, I) then
        DoMerge(fCurrCaret, I);
    end;

  { Then, with all others }
  for I := High(fCarets) downto 0 do
  begin
    if I = fCurrCaret then
      Continue;
    for J := High(fCarets) downto 0 do
    begin
      if I = J then
        Continue;
      if J = fCurrCaret then
        Continue;
      if MustBeMerged(I, J) then
        DoMerge(I, J);
    end;
  end;
end;

procedure TCustomSynEdit.DoOnGutterClick(Button: TMouseButton; X, Y: Integer);
var
  I     : Integer;
  Offs  : Integer;
  Line  : Integer;
  Allmrk: TSynEditMarks;
  Mark  : TSynEditMark;
  Tgl   : Boolean;
begin
  Line := RowToLine(PixelsToRowColumn(X, Y).Row);

  { Check if user clicked on folding controls }
  if CodeFolding.Enabled and (X >= fGutter.Width - 12) then
  begin
    { Collapse / expand if possible }
    SynEditToggleFoldingRange(Self, Line, fHighlighter.FoldRegions, Tgl);

    { Invalidate lines }
    if Tgl then
    begin
      if GetDisplayLineCount < fTopLine + fLinesInWindow then
        TopLine := fTopLine - Max(fTopLine + fLinesInWindow - GetDisplayLineCount - 1, 0);
      if not CaretInView then
        EnsureCursorPosVisible;
      Invalidate;
      Exit;
    end

    { Try to select lines in outlining range scope }
    else begin
      SynEditFindClosest(Self, Line, fHighlighter.FoldRegions, I, Offs);
      if (I > 0) and (Offs > 0) then
        SetCaretAndSelection(BufferCoord(fCaretX, fCaretY),
          BufferCoord(1, I),
          BufferCoord(Lines.AccessStringLength(Pred(Offs)) + 1, Offs));
    end;
  end;

  { Fire assigned click event with corresponding mark }
  if Assigned(fOnGutterClick) then
  begin
    if Line <= Lines.Count then
    begin
      Marks.GetMarksForLine(Line, Allmrk);
      Offs := 0;
      Mark := nil;
      for I := 1 to MAX_MARKS do
      begin
        if Assigned(Allmrk[I]) then
        begin
          Inc(Offs, BookMarkOptions.XOffset);
          if X < Offs then
          begin
            Mark := Allmrk[I];
            Break;
          end;
        end;
      end;
      fOnGutterClick(Self, Button, X, Y, Line, Mark);
    end
    else
      fOnGutterClick(Self, Button, X, Y, 0, nil);
  end;
end;

// -----------------------------------------------------------------------------
// § Garnet
// Main paint routine. Draws gutter and text
procedure TCustomSynEdit.Paint;
var
  rcClip, rcDraw: TRect;
  nL1, nL2, nC1, nC2: Integer;
begin
  { Get the invalidated rect. Compute the invalid area in lines / columns }
  rcClip := Canvas.ClipRect;

  { Columns }
  nC1 := fLeftChar;
  if (rcClip.Left > fGutter.Width + 2) then
    Inc(nC1, (rcClip.Left - fGutter.Width - 2) div fCharWidth);
  nC2 := fLeftChar +
    (rcClip.Right - fGutter.Width - 2 + fCharWidth - 1) div fCharWidth;

  { Rows }
  nL1 := Max(fTopLine + rcClip.Top div fTextHeight, fTopLine);
  nL2 := MinMax(fTopLine + (rcClip.Bottom + fTextHeight - 1) div fTextHeight,
    1, GetDisplayLineCount);

  { Now, paint everything while the caret is hidden }
  HideCaret;
  try
    { Paint the text area if it was (partly) invalidated }
    if (rcClip.Right > fGutter.Width) then
    begin
      rcDraw := rcClip;
      rcDraw.Left := Max(rcDraw.Left, fGutter.Width);
      PaintTextLines(rcDraw, nL1, nL2, nC1, nC2);
    end;

    { Send canvas to plugins }
    PluginsAfterPaint(Canvas, rcClip, nL1, nL2);

    { If there are custom paint handlers, call them }
    DoOnPaint;
    DoOnPaintTransient(ttAfter);

    { Paint the gutter area if it was (partly) invalidated.
      Always paint gutter last because there could be drawn something
      by plugins or user in text under gutter }
    if (rcClip.Left < fGutter.Width) then
    begin
      rcDraw := rcClip;
      rcDraw.Right := fGutter.Width;
      PaintGutter(rcDraw, nL1, nL2);
    end;

    { Switch off dim }
    if fDimmed then
    begin
      fDimmed := False;
      fRepaintAfterDimNeeded := True;
    end;
  finally
    UpdateCaret;
  end;
end;

procedure TCustomSynEdit.PaintGutter(const AClip: TRect;
  aFirstRow, aLastRow: Integer);

  function CanPaintFoldEnd(const ALine: Integer): Boolean;
  begin
    Result := FoldRangeForLineTo(ALine) = nil;
  end;

  procedure DrawMark(aMark: TSynEditMark; var aGutterOff: Integer;
    aMarkRow: Integer);
  var
    jY: Integer;
  begin
    if Assigned(fBookMarkOpt.BookmarkImages) then
    begin
      if aMark.ImageIndex <= fBookMarkOpt.BookmarkImages.Count then
      begin
        if aMark.IsBookmark = BookMarkOptions.DrawBookmarksFirst then
          aGutterOff := 0
        else if aGutterOff = 0 then
          aGutterOff := fBookMarkOpt.XOffset;
        if fTextHeight > fBookMarkOpt.BookmarkImages.Height then
          jY := fTextHeight shr 1 - fBookMarkOpt.BookmarkImages.Height shr 1
        else
          jY := 0;
        with fBookMarkOpt do
          BookmarkImages.Draw(Canvas, LeftMargin + aGutterOff,
            (aMarkRow - TopLine) * fTextHeight + jY, aMark.ImageIndex);
        Inc(aGutterOff, fBookMarkOpt.XOffset);
      end;
    end;
  end;

var
  cLine: Integer;      // Line iterator
  cRow: Integer;       // Row iterator
  vFirstLine: Integer; // Detect line of starting row (for painting)
  vLastLine: Integer;  // Detect line of ending row (for marks)
  vStartRow: Integer;  // Starting row of current line being painted
  vEndRow: Integer;    // Ending row of current line being painted
  cRowCount: Integer;  // vEndRow - cRow (number of rows to paint for that line)
  rcLine: TRect;       // Line rect
  cMark: Integer;      // Mark iterator
  rcFoldMark: TRect;
  aGutterOffs: PIntArray;
  bHasOtherMarks,
  bOpenFoldOnLine,
  bCloseFoldOnLine: Boolean;
  S: UnicodeString;
  vMarkRow: Integer;
  vGutterRow: Integer;
  X, H: Integer;
  TextSize: TSize;
  FoldRange: TSynEditFoldRange;
begin
  { Changed to use fTextDrawer.BeginDrawing and fTextDrawer.EndDrawing only
    when absolutely necessary.  Note: Never change brush / pen / font of the
    canvas inside of this block (only through methods of fTextDrawer)!
    If we have to draw the line numbers then we don't want to erase
    the background first. Do it line by line with TextRect instead
    and fill only the area after the last visible line }
  with fTextDrawer do
  begin
    SetBaseFont(fGutter.Font);
    BeginDrawing(Canvas.Handle);
  end;
  try
    { Prepare the rect initially }
    rcLine := AClip;
    with rcLine do
      Bottom := Top;

    { Loop through lines }
    aFirstRow := fTopLine;
    aLastRow := fTopLine + fLinesInWindow;
    cLine := RowToLine(aFirstRow);
    vFirstLine := cLine;
    cRow := aFirstRow;
    while cRow <= aLastRow do
    begin
      { Get fold range for line to skip folded lines }
      if fCodeFolding.Enabled then
      begin
        FoldRange := FoldRangeForLine(cLine);
        if FoldRange = nil then
          SynEditGetOpenCloseFoldingRange(Self, cLine, fHighlighter.FoldRegions,
            bOpenFoldOnLine, bCloseFoldOnLine);
      end;

      { Retrieve current being painted line rows. It's always necessary
        because line can be wrapped to multiple rows in word wrap mode }
      if WordWrap then
      begin
        vStartRow := LineToRow(cLine);
        if fCodeFolding.Enabled and Assigned(FoldRange) then
          vEndRow := Min(LineToRow(FoldRange.ToLine) - 1, aLastRow)
        else
          vEndRow := Min(LineToRow(cLine + 1) - 1, aLastRow);
      end
      else begin
        vStartRow := cRow;
        vEndRow := cRow;
      end;
      cRowCount := vEndRow - cRow + 1;

      { Next line rect }
      with rcLine do
      begin
        Top    := (cRow - fTopLine) * fTextHeight;
        Bottom := Top + fTextHeight * cRowCount;
      end;

      { Find delta because of visible controls }
      H := 0;
      if fGutter.ShowLineNumbers then
        Inc(H, SEC_LineStatesBarWidth);

      { Set BG color }
      if (cLine = fCaretY) and (eoAdaptiveSpecialLine in fOptions) then
        fTextDrawer.SetBackColor(Lighten(fGutter.Color, 7))
      else
        fTextDrawer.SetBackColor(fGutter.Color);

      { Paint line numbers }
      if fGutter.ShowLineNumbers and (cRow = vStartRow) then
      begin
        { Calculate the number to show on gutter }
        S := fGutter.FormatLineNumber(cLine);

        { User override }
        if Assigned(OnGutterGetText) then
          OnGutterGetText(Self, cLine, S);

        { Draw text }
        if cLine mod 10 = 0 then
          fTextDrawer.SetForeColor(clBlack)
        else
          fTextDrawer.SetForeColor(fGutter.Font.Color);
        TextSize := GetTextSize(Canvas.Handle, PWideChar(S), Length(S));
        Windows.ExtTextOutW(Canvas.Handle,
          (fGutter.Width - fGutter.RightOffset - H - 2) - TextSize.cx,
          rcLine.Top + ((fTextHeight - Integer(TextSize.cy)) shr 1), ETO_OPAQUE,
          @rcLine, PWideChar(S), Length(S), nil);
      end
      else
        fTextDrawer.ExtTextOut(Left, Top, [tooOpaque], rcLine, '', 0);

      { Draw word wrap glyphs }
      if WordWrap and (cRowCount > 0) then
        with fGutter do
          for X := vStartRow + 1 to vEndRow do
          begin
            fTextDrawer.SetForeColor(fGutter.Font.Color);
            fTextDrawer.ExtTextOut(
              (Width - RightOffset - H - 2) - fCharWidth,
              (X - fTopLine) * fTextHeight, [tooOpaque],
              Rect(rcLine.Left, (X - fTopLine) * fTextHeight, rcLine.Right, (X - fTopLine + 1) * fTextHeight),
              #$2026, 1);
          end;

      { Paint line state }
      if fGutter.ShowLineStates and (fLines.Count > 0) then
      begin
        X := 0;
        if ExpandLines.GetLineFlag(cLine - 1, STRING_REC_MODIFIED) then
        begin
          X := 1;
          if ExpandLines.GetLineFlag(cLine - 1, STRING_REC_SAVED) then
            Canvas.Pen.Color := $0000BBFF
          else
            Canvas.Pen.Color := $0000DDFF
        end
        else if ExpandLines.GetLineFlag(cLine - 1, STRING_REC_SAVED) then
        begin
          X := 1;
          Canvas.Pen.Color := $0000EE64;
        end;
        if X = 1 then
          with Canvas, Pen, AClip, fGutter do
          begin
            MoveTo(Right - RightOffset - 1, rcLine.Top);
            LineTo(Right - RightOffset - 1, rcLine.Bottom);
            MoveTo(Right - RightOffset - 2, rcLine.Top);
            LineTo(Right - RightOffset - 2, rcLine.Bottom);
          end;
      end;

      { Paint folding bar, dim if needed }
      if fCodeFolding.Enabled
      then
        with fTextDrawer, Canvas, rcLine do
        begin
          Left := AClip.Right - 12;
          if fDimmed and (cLine >= fDimFirstLine) and
            (cLine <= fDimLastLine)
          then
            SetBackColor(clWhite)
          else
            SetBackColor(fCodeFolding.FolderBarColor);
          ExtTextOut(Left, Top, [tooOpaque], rcLine, '', 0);
          SetBackColor(fGutter.Color);
          Left := AClip.Left;
        end;

      { Paint folding controls }
      if fCodeFolding.Enabled then
      begin
        { Can paint fold range end? }
        if (FoldRange = nil) and CanPaintFoldEnd(cLine) and bCloseFoldOnLine
          and not bOpenFoldOnLine
        then begin
          { Prevent top mark to be painted }
          FoldRange := nil;

          if cRow = vStartRow then

          with Canvas, rcLine do
          begin
            { Prepare rect }
            vEndRow := Bottom;
            Left   := AClip.Right - 11;
            Right  := Left + 9;
            X      := Right - Left;
            Top := Top + (fTextHeight - X) shr 1 + 1;
            Bottom    := Top + X;
            X      := X shr 1 + Left;
            H      := (Bottom - Top) shr 1 + Top - 3;

            { Folding arrows }
            Pen.Color := clGrayText;
            MoveTo(Left + 1, H + 4);
            LineTo(Right - 1, H + 4);
            MoveTo(Left  + 2, H + 3);
            LineTo(Right - 2, H + 3);
            MoveTo(Left + 3, H + 2);
            LineTo(Right - 3, H + 2);
            MoveTo(Left + 4, H + 1);
            LineTo(Right - 4, H + 1);
            Inc(Bottom);

            { Restore rect }
            with rcLine do
            begin
              Left := AClip.Left;
              Right := AClip.Right;
              Bottom := vEndRow;
            end;
          end;
        end;

        { Can paint fold range start? }
        if (FoldRange <> nil) or bOpenFoldOnLine then
        begin

          { Skip folded lines (check for word wrap done separately) }
          if Assigned(FoldRange) then
            Inc(cLine, FoldRange.LinesCollapsed);

          { Can paint rect? }
          if cRow = vStartRow then
          begin

            { Prepare rect }
            with rcLine do
            begin
              vEndRow := Bottom;
              Left   := AClip.Right - 11;
              Right  := Left + 9;
              X := Right - Left;
              Top := (fTextHeight - X) shr 1 + Top - 1;
              Bottom := Top + X;
              X      := X shr 1 + Left;
              H      := (Bottom - Top) shr 1 + Top;
            end;

            { Paint folding arrows }
            with Canvas, rcLine do
            begin

              { Collapsed / expanded mark }
              Pen.Color := clGrayText;
              if Assigned(FoldRange) then
              begin
                MoveTo(Right - 1 - 2, H - 1);
                LineTo(Right - 2 - 2, H - 1);
                MoveTo(Right - 1 - 2, H);
                LineTo(Right - 3 - 2, H);
                MoveTo(Right - 1 - 2, H + 1);
                LineTo(Right - 4 - 2, H + 1);
                MoveTo(Right - 1 - 2, H + 2);
                LineTo(Right - 5 - 2, H + 2);
                MoveTo(Right - 1 - 2, H + 3);
                LineTo(Right - 6 - 2, H + 3);
              end
              else begin
                MoveTo(Left + 1, H);
                LineTo(Right - 1, H);
                MoveTo(Left  + 2, H + 1);
                LineTo(Right - 2, H + 1);
                MoveTo(Left + 3, H + 2);
                LineTo(Right - 3, H + 2);
                MoveTo(Left + 4, H + 3);
                LineTo(Right - 4, H + 3);
              end;
            end;

            { Restore rect }
            with rcLine do
            begin
              Left := AClip.Left;
              Right := AClip.Right;
              Bottom := vEndRow;
            end;
          end;
        end;
      end;

      { Proceed to next line & corresponding row }
      Inc(cLine);
      Inc(cRow, cRowCount);

      { Break at last visible line }
      if cLine > fLines.Count then
        Break;
    end;

    { Now erase the remaining area if any }
    if AClip.Bottom > rcLine.Bottom then
      with rcLine do
      begin
        Top := Bottom;
        Bottom := AClip.Bottom;
        with fTextDrawer do
        begin
          SetBackColor(fGutter.Color);
          ExtTextOut(Left, Top, [tooOpaque], rcLine, '', 0);
        end;
        if fCodeFolding.Enabled then
        begin
          Left := AClip.Right - 12;
          with fTextDrawer do
          begin
            SetBackColor(fCodeFolding.FolderBarColor);
            ExtTextOut(Left, Top, [tooOpaque], rcLine, '', 0);
          end;
        end;
      end;

  finally
    with fTextDrawer do
    begin
      EndDrawing;
      SetBaseFont(Self.Font);
    end;
  end;

  { The gutter separator is visible }
  if (fGutter.BorderColor <> clNone) then
    with Canvas, AClip do
    begin
      { TextDrawer destroys previous pen data.
        We reinitialize it by notifying pen class with pen mode change }
      with Pen do
      begin
        Mode := pmBlack;
        Mode := pmCopy;
        Color := fGutter.BorderColor;
      end;

      { Do paint }
      with AClip do
      begin

        { Line states }
        if fGutter.ShowLineStates then
        begin
          X := Top;
          while X < Bottom do
          begin
            MoveTo(Right - fGutter.RightOffset - 4, X);
            LineTo(Right - fGutter.RightOffset - 4, X + 1);
            Inc(X, 2);
          end;
        end;

        { Folding bar break }
        if fCodeFolding.Enabled then
        begin
          MoveTo(Right - 13, Top);
          LineTo(Right - 13, Bottom);
        end;

        { Border }
        MoveTo(Right - 1, Top);
        LineTo(Right - 1, Bottom);

        { Folding bar additional }
        if fCodeFolding.Enabled then
        begin
          Pen.Color := clWhite;
          MoveTo(Right - 2, Top);
          LineTo(Right - 2, Bottom);
          MoveTo(Right - 12, Top);
          LineTo(Right - 12, Bottom);
        end;
      end;
    end;

  { Now the gutter marks }
  vLastLine := RowToLine(aLastRow);
  if BookMarkOptions.GlyphsVisible and (Marks.Count > 0)
    and (vLastLine >= vFirstLine) then
  begin
    aGutterOffs := AllocMem((aLastRow - aFirstRow + 1) * SizeOf(Integer));
    try
      { Instead of making a two pass loop we look while drawing the bookmarks
        whether there is any other mark to be drawn }
      bHasOtherMarks := False;
      for cMark := 0 to Marks.Count - 1 do with Marks[cMark] do
        if Visible and (Line >= vFirstLine) and (Line <= vLastLine) and
          IsLineVisible(Line) then
        begin
          if IsBookmark <> BookMarkOptions.DrawBookmarksFirst then
            bHasOtherMarks := True
          else begin
            vMarkRow := LineToRow(Line);
            if vMarkRow >= aFirstRow then
              DrawMark(Marks[cMark], aGutterOffs[vMarkRow - aFirstRow], vMarkRow);
          end
        end;
      if bHasOtherMarks then
        for cMark := 0 to Marks.Count - 1 do with Marks[cMark] do
        begin
          if Visible and (IsBookmark <> BookMarkOptions.DrawBookmarksFirst)
            and (Line >= vFirstLine) and (Line <= vLastLine) and
              IsLineVisible(Line) then
          begin
            vMarkRow := LineToRow(Line);
            if vMarkRow >= aFirstRow then
              DrawMark(Marks[cMark], aGutterOffs[vMarkRow - aFirstRow], vMarkRow);
          end;
        end;
      if Assigned(OnGutterPaint) then
        for cLine := vFirstLine to vLastLine do
        begin
          vGutterRow := LineToRow(cLine);
          OnGutterPaint(Self, cLine, aGutterOffs[vGutterRow - aFirstRow],
            (vGutterRow - TopLine) * LineHeight);
        end;
    finally
      FreeMem(aGutterOffs);
    end;
  end
  else if Assigned(OnGutterPaint) then
  begin
    for cLine := vFirstLine to vLastLine do
    begin
      vGutterRow := LineToRow(cLine);
      OnGutterPaint(Self, cLine, 0, (vGutterRow - TopLine) * LineHeight);
    end;
  end;
end;

procedure TCustomSynEdit.PaintTextLines(AClip: TRect; aFirstRow, aLastRow,
  FirstCol, LastCol: Integer);
var
  { Margin }
  bDoRightEdge: Boolean;    // Should paint right edge?
  nRightEdge: Integer;

  { Selection info }
  bAnySelection: Boolean;   // Any selection visible?
  vSelStart: TDisplayCoord; // Start of selected area
  vSelEnd: TDisplayCoord;   // End of selected area

  { Info about normal and selected text and background colors }
  bFirstRow: Boolean;       // First row of a line
  bSpecialLine,
  bLineSelected, bTokenSelected,
  bCurrentLine: Boolean;    // Line being painted is at caret

  { Colors }
  colFG, colBG: TColor;
  colSelFG, colSelBG: TColor;

  { info about selection of the current line }
  nLineSelStart, nLineSelEnd: Integer;
  bComplexLine: Boolean;

  { Painting the background and the text }
  rcLine, rcToken, rcEOL: TRect;

  { Token accumulator }
  TokenAccu: record
    Pos,                  // Real position of token in string
    Len,                  // Real length of token substring
    ExpandedLen,          // Visual length for rcToken
    MaxLen,               // Accumulator capacity (grows)
    CharsBefore: Integer; // Visual offset of token (expanded Pos)
    S: UnicodeString;     // Accumulator contents
    FG, BG: TColor;       // Colors
    Style: TFontStyles;   // Font
  end;

  { Line index for the loop }
  nLine: Integer;
  cRow: Integer;
  nIndent: Integer;

  DC: HDC;

  vFirstLine: Integer;
  vLastLine: Integer;
  vStartRow: Integer;
  vEndRow: Integer;
  bDoCaretWhenToEol: Boolean;

  { Analyzis }
  StrA: PBArray;

  aMisspelled: array of TDisplayCoord;

  { § Garnet: to track down very dark (almost black) BGs }
  function IsVeryDarkColor(C: TColor): Boolean;
  begin
    if C < 0 then C := GetSysColor(C and $FF);
    Result := ((C and $FF) < $21) and ((C shr 8 and $FF) < $21) and
      ((C shr 16 and $FF) < $21);
  end;

  { § Garnet: support for complex blended backgrounds }
  function colEditorBG(Highlight: Boolean = False): TColor;
  const
    Amount = $0E;
  var
    iAttri: TSynHighlighterAttributes;
    jAmount: Integer;
  begin
    jAmount := -9;
    if (eoAdaptiveSpecialLine in fOptions) and (bCurrentLine) then
      if Assigned(fHighlighter) then
      begin
        iAttri := Highlighter.GetTokenAttribute;
        if (iAttri <> nil) and (iAttri.Background <> clNone) then
          Result := iAttri.Background
        else begin
          iAttri := Highlighter.WhitespaceAttribute;
          if (iAttri <> nil) and (iAttri.Background <> clNone) then
            Result := iAttri.Background
          else
            Result := Color;
        end;

        { Very dark colors require more intensity }
        if IsVeryDarkColor(Result) then
          jAmount := -jAmount*4

        { Regular dark colors can have same amount of
          lightness as light colors have darkness }
        else if IsDarkColor(Result) then
          jAmount := -jAmount;

        Result := Lighten(Result, jAmount);
      end
      else begin
        { Very dark colors require more intensity }
        if IsVeryDarkColor(Color) then
          jAmount := -jAmount*4

        { Regular dark colors can have same amount of
          lightness as light colors have darkness }
        else if IsDarkColor(Color) then
          jAmount := -jAmount;

        Result := Lighten(Color, jAmount);
      end
    else if (ActiveLineBG <> clNone) and (bCurrentLine) then
      Result := ActiveLineBG

    else begin
      Result := Color;
      if Highlight then
      begin
        if IsDarkColor(Result) then
          if IsVeryDarkColor(Result) then
            Result := Lighten(Result, Amount * 3)
          else
            Result := Lighten(Result, Amount)
        else
          Result := Lighten(Result, -Amount);
      end
      else if Assigned(fHighlighter) then
      begin
        iAttri := Highlighter.WhitespaceAttribute;
        if (iAttri <> nil) and (iAttri.Background <> clNone) then
          Result := iAttri.Background;
      end;
    end;
  end;

  { Get selection bounds in editor screen coords }
  procedure ComputeSelectionInfo;
  var
    vStart: TBufferCoord;
    vEnd: TBufferCoord;
  begin
    bAnySelection := False;

    { Only if selection is visible anyway }
    if not HideSelection or Self.Focused then
    begin

      { See if selected at all }
      bAnySelection := SelAvail;

      { Detect zero-width column selection }
      if bAnySelection and (fBlockBegin.Line = fBlockEnd.Line) and
        (fActiveSelectionMode = smColumn)
      then
        bAnySelection := fBlockBegin.Char <> fBlockEnd.Char;

      { Test if there is an intersection with the area to be painted }
      if bAnySelection then
      begin

        { Get correct selected area bounds }
        vStart := BlockBegin;
        vEnd := BlockEnd;

        { Don't care if the selection is not visible }
        bAnySelection := (vEnd.Line >= vFirstLine) and (vStart.Line <= vLastLine);
        if bAnySelection then
        begin

          { Transform the selection from text space into screen space }
          vSelStart := BufferToDisplayPos(vStart);
          vSelEnd := BufferToDisplayPos(vEnd);

          { In the column selection mode sort the begin and end of the
            selection, this makes the painting code simpler }
          if (fActiveSelectionMode = smColumn) and (vSelStart.Column > vSelEnd.Column) then
            SwapInt(vSelStart.Column, vSelEnd.Column);
        end;
      end;
    end;
  end;

  { § Garnet: support for complex blended backgrounds }
  procedure SetDrawingColors(Selected: Boolean; Highlight: Boolean = False;
    Invert: Boolean = False);
  const
    Amount = $0E;
  var
    BGColor: TColor;
  begin
    bTokenSelected := Selected;
    with fTextDrawer do

      { Need to apply selection colors? }
      if Selected then
      begin

        { Text color }
        if colSelFG <> clNone then
          SetForeColor(colSelFG)
        else
          if IsDarkColor(colSelBG) then
            SetForeColor(Lighten(colFG, Amount))
          else
            SetForeColor(Lighten(colFG, -Amount));

        { Background color }
        if not bCurrentLine then
          if eoAdaptiveSelection in Options then
            BGColor := Blend(colSelBG, colBG, 50)
          else
            BGColor := colSelBG
        else
          if eoAdaptiveSelectionSpecialLine in Options then
            BGColor := Blend(colSelBG, colBG, 50)
          else
            BGColor := colSelBG;

        { Need to lighten / darken? }
        if Highlight then
          if IsDarkColor(BGColor) then
            if IsVeryDarkColor(BGColor) then
              BGColor := Lighten(BGColor, Amount * 3)
            else
              BGColor := Lighten(BGColor, Amount)
          else
            BGColor := Lighten(BGColor, -Amount);

        { Apply background colors }
        if Invert then BGColor := not BGColor and $00FFFFFF;
        SetBackColor(BGColor);
        Canvas.Brush.Color := BGColor;
      end

      { Normal colors }
      else begin

        { Dimmed? }
        if fDimmed and not ((nLine >= fDimFirstLine) and (nLine <= fDimLastLine)) then
        begin

          { Dim fore color on dark BGs }
          if IsDarkColor(Color) then
          begin

            { Text color }
            if bCurrentLine and (fActiveLineFG <> clNone) then
              SetForeColor(Lighten(fActiveLineFG, -75))
            else
              SetForeColor(Lighten(colFG, -75));

            { Background }
            BGColor := Lighten(colBG, -10);
          end

          { Dim background color on light BGs }
          else begin

            { Text color }
            if bCurrentLine and (fActiveLineFG <> clNone) then
              SetForeColor(fActiveLineFG)
            else
              SetForeColor(colFG);

            { Background }
            BGColor := Lighten(colBG, -10);
          end;

          { Need to lighten / darken? }
          if Highlight then
            if IsDarkColor(colBG) then
              if IsVeryDarkColor(colBG) then
                BGColor := Lighten(BGColor, Amount * 2)
              else
                BGColor := Lighten(BGColor, Amount)
            else
              BGColor := Lighten(BGColor, -Amount);

          { Apply background color }
          if Invert then BGColor := not BGColor and $00FFFFFF;
          SetBackColor(BGColor);
          Canvas.Brush.Color := BGColor;
        end

        { Select normal colors }
        else begin

          { Text color }
          if bCurrentLine and (fActiveLineFG <> clNone) then
            SetForeColor(fActiveLineFG)
          else
            SetForeColor(colFG);

          { Background color }
          BGColor := colBG;

          { Need to lighten / darken? }
          if Highlight then
            if IsDarkColor(BGColor) then
              if IsVeryDarkColor(BGColor) then
                BGColor := Lighten(BGColor, Amount * 3)
              else
                BGColor := Lighten(BGColor, Amount)
            else
              BGColor := Lighten(BGColor, -Amount);

          { Apply colors }
          if Invert then BGColor := not BGColor and $00FFFFFF;
          SetBackColor(BGColor);
          Canvas.Brush.Color := BGColor;
        end;
      end;
  end;

  { Get screen coord of char to be painted }
  function ColumnToXValue(Col: Integer): Integer;
  begin
    Result := fTextOffset + Pred(Col) * fCharWidth;
  end;

  { The PaintToken procedure will take care of invalid parameters
    like empty token rect or invalid indices into TokenLen.
    CharsBefore tells if Token starts at column one or not }
  procedure PaintToken(
    const Token: UnicodeString; // Contents of accumulator
    ExpandedTokenLen,           // As it says
    CharsBefore,                // Expanded Pos
    First,                      // Expanded left
    Last,                       // Expanded right
    RealFirst,                  // Real positions in accumulator
    RealLast: Integer;
    bTransp: Boolean = False);
  var
    Text: UnicodeString;
    I, J, K, Counter, nX, DCBak: Integer;
    rcTab: TRect;
    colTab: TColor;
    ETO: SynTextDrawer.PIntegerArray;
  const
    ETOOptions = [tooOpaque, tooClipped];
  begin
    if (Last >= First) and (rcToken.Right > rcToken.Left) then
    begin
      if WordWrap and (CharsBefore < {0}nIndent) then
      begin
        First := Max(First, FirstCol + nIndent);
        CharsBefore := Max(CharsBefore, {0}nIndent);
      end;
      nX := ColumnToXValue(First);

      Dec(First, CharsBefore);
      Dec(Last, CharsBefore);

      if (First > ExpandedTokenLen) then
      begin
        RealLast := 0;
        Text := '';
      end
      else begin
        { Clip off partially visible glyphs at row begin / end }
        if WordWrap and (fLines.Count > 0) then
        begin
          { There's a portion of token behind visible area }
           if TokenAccu.CharsBefore < nIndent then
            while TokenAccu.CharsBefore + ExpandedPos(TokenAccu.Pos, TokenAccu.Pos + Pred(RealFirst), StrA) < nIndent do
              Inc(RealFirst);

          { Check if there's a portion of token after row length in word wrap mode }
          if cRow < vEndRow then
          begin
            I := fWordWrapPlugin.GetRowLength(cRow, nLine) + nIndent;
            while (RealLast > 0) and (TokenAccu.CharsBefore + ExpandedPos(TokenAccu.Pos, TokenAccu.Pos + Pred(RealFirst) + Pred(RealLast), StrA) >= I)
            do
              Dec(RealLast);
          end;

        end;

        { Get paint text }
        Text := Copy(Token, RealFirst, RealLast);
      end;

      { Do paint token text }
      RealLast := Max(RealLast, 0);
      ETO := AllocMem(RealLast * SizeOf(Integer));
      for I := 0 to RealLast - 1 do
        ETO[I] := StrA^[TokenAccu.Pos + (RealFirst - 1) + I] * fCharWidth;
      if bTransp then
        SetBkMode(Canvas.Handle, TRANSPARENT);
      if bTransp then
        fTextDrawer.ExtTextOut(nX, rcToken.Top + fExtraLineSpacing shr 1,
          [], rcToken, PChar(Text), RealLast, ETO)
      else
        fTextDrawer.ExtTextOut(nX, rcToken.Top + fExtraLineSpacing shr 1,
          ETOOptions, rcToken, PChar(Text), RealLast, ETO);
      if bTransp then
        SetBkMode(Canvas.Handle, OPAQUE);
      FreeMem(ETO);

      { Paint whitespace }
      if (eoShowSpecialChars in fOptions) or (bTokenSelected and (eoShowSpecialcharsInSelection in fOptions)) then
      begin

        { Find color which will be used for painting whitespace }
        colTab := Blend(ColBG, Font.Color, 50);
        if Trunc(ColorDistance(colTab, colBG)) < 150 then
          colTab := Font.Color;

        { Prepare canvas }
        with Canvas do
        begin
          DCBak := SaveDC(Handle);
          Brush.Color := colTab;
          with Pen do
          begin
            Mode := pmBlack;
            Mode := pmCopy;
            Color := colTab;
          end;
        end;

        { Prepare rect }
        K := fCharWidth shr 1;
        J := Max((fTextHeight - 8), 0) shr 4;
        with rcToken do
          nX := Top + (Bottom - Top) shr 1;
        with rcTab do
        begin
          Top := nX - J;
          Bottom := nX + 2 + J;
        end;

        { Find first space position }
        Counter := PosEx(#32, Text, 1);

        { Loop }
        while Counter > 0 do
        begin
          { Prepare rect }
          nX := ColumnToXValue(CharsBefore + Pred(First) +
            ExpandedPos(TokenAccu.Pos + Pred(RealFirst),
            TokenAccu.Pos + Pred(RealFirst) + Counter, StrA));
          with rcTab do
          begin
            Left := nX + K - 1 - J;
            Right := nX + K + 1 + J;
          end;

          { Paint glyph }
          with Canvas, rcTab do
            Rectangle(Left, Top, Right, Bottom);

          { Step }
          Counter := PosEx(#32, Text, Counter + 1);
        end;

        { Prepare rect }
        rcTab := rcToken;

        { Find first tab position }
        Counter := PosEx(#9, Text, 1);

        { Loop }
        while Counter > 0 do
        begin

          { Find, how long the visible part of tab actually is }
          K := StrA^[TokenAccu.Pos + Pred(RealFirst) + Pred(Counter)];

          { Prepare rect }
          nX := ColumnToXValue(CharsBefore + Pred(First) +
            ExpandedPos(TokenAccu.Pos + Pred(RealFirst),
            TokenAccu.Pos + Pred(RealFirst) + Pred(Counter), StrA) + 1);
          with rcTab do
          begin
            Left := nX + 2;
            Right := nX + K * fCharWidth - 3;
          end;

          { Do draw }
          with Canvas, rcTab do
          begin
            nX := (Bottom - Top) shr 1;

            I := Left;
            while I < Right do
            begin
              MoveTo(I, Top + nX);
              LineTo(I + 1, Top + nX);
              Inc(I, 2);
            end;

            MoveTo(I, Top + nX);
            LineTo(I - (nX shr 1), Top + nX - (nX shr 1));
            MoveTo(I, Top + nX);
            LineTo(I - (nX shr 1), Top + nX + (nX shr 1));
          end;

          { Search next }
          Counter := PosEx(#9, Text, Counter + 1);
        end;

        { Release canvas }
        RestoreDC(Canvas.Handle, DCBak);
        with Canvas.Pen do
        begin
          Mode := pmBlack;
          Mode := pmCopy;
          Color := fOutliningAttr.Foreground;
        end;
      end;

      { Prepare for next token }
      rcToken.Left := rcToken.Right;
    end;
  end;

  { Trick to avoid clipping the last pixels of text in italics,
    see also AdjustLastCharWidth() in TheTextDrawer.ExtTextOut() }
  procedure AdjustEndRect;
  var
    LastChar: Cardinal;
    NormalCharWidth, RealCharWidth: Integer;
    CharInfo: TABC;
    tm: TTextMetricA;
  begin
    LastChar := Ord(TokenAccu.S[TokenAccu.Len]);
    NormalCharWidth := fTextDrawer.TextWidth(WideChar(LastChar));
    RealCharWidth := NormalCharWidth;
    if Win32PlatformIsUnicode then
    begin
      if GetCharABCWidthsW(Canvas.Handle, LastChar, LastChar, CharInfo) then
      begin
        RealCharWidth := CharInfo.abcA + Integer(CharInfo.abcB);
        if CharInfo.abcC >= 0 then
          Inc(RealCharWidth, CharInfo.abcC);
      end
      else if LastChar < Ord(High(AnsiChar)) then
      begin
        GetTextMetricsA(Canvas.Handle, tm);
        RealCharWidth := tm.tmAveCharWidth + tm.tmOverhang;
      end;
    end
    else if WideChar(LastChar) <= High(AnsiChar) then
    begin
      if GetCharABCWidths(Canvas.Handle, LastChar, LastChar, CharInfo) then
      begin
        RealCharWidth := CharInfo.abcA + Integer(CharInfo.abcB);
        if CharInfo.abcC >= 0 then
          Inc(RealCharWidth, CharInfo.abcC);
      end
      else if LastChar < Ord(High(AnsiChar)) then
      begin
        GetTextMetricsA(Canvas.Handle, tm);
        RealCharWidth := tm.tmAveCharWidth + tm.tmOverhang;
      end;
    end;
    if RealCharWidth > NormalCharWidth then
      Inc(rcToken.Left, RealCharWidth - NormalCharWidth);
  end;

  procedure PaintHighlightToken(bFillToEOL: Boolean; EolPos: Integer = 0);
  var
    bComplexToken, bCanPaintEolMark: Boolean;
    nC1, nC2, nC1Sel, nC2Sel, nC3, nC4: Integer;
    bU1, bSel, bU2: Boolean;
    nX1, nX2: Integer;
    colEOL: TColor;

    procedure PaintBGBlock(const n1, n2, n3, n4: Integer;
      const Highlighted: Boolean);
    type
      TBGRange = record
        B, E, C: Integer;
      end;
    var
      Bg, En: Integer;
      BGRanges: array of TBGRange;

      procedure AddBGRange(const AB, AE, AC: Integer);
      begin
        SetLength(BGRanges, Succ(Length(BGRanges)));
        with BGRanges[High(BGRanges)] do
        begin
          B := Max(AB, Bg);
          E := Min(AE, En);
          C := AC;
          if C > -1 then
            C := MinMax(AC, Bg, En);
        end;
      end;

      function CharToPixels(const Char: Integer): Integer;
      var
        DP: TDisplayCoord;
      begin
        Result := -1;
        DP := BufferToDisplayPos(BufferCoord(Char, nLine));
        if DP.Row = cRow then
          Result := ColumnToXValue(DP.Column);
      end;

      procedure DoPaintBlock(var AB: Integer; const AE, AC: Integer;
        const Selected: Boolean);
      begin
        { Paint a portion of token background }
        if AE > AB then
        begin
          SetDrawingColors(Selected, Highlighted, False);
          fTextDrawer.ExtTextOut(AB, rcToken.Top, [tooOpaque, tooClipped],
            Rect(AB, rcToken.Top, AE, rcToken.Bottom), nil, 0);
          AB := AE;
        end;

        { Paint caret if it's present and in viewport }
        if (AC > -1) and (AC <= En) then
        begin
          SetDrawingColors(Selected, Highlighted, True);
          fTextDrawer.ExtTextOut(AC, rcToken.Top, [tooOpaque, tooClipped],
            Rect(AC, rcToken.Top, AC + 1, rcToken.Bottom), nil, 0);
          if AC >= AB then
            Inc(AB);
        end;
      end;

    var
      I, J: Integer;
      Whole: Boolean;
    begin
      { Initialize }
      Bg := rcToken.Left;
      En := rcToken.Right;
      SetLength(BGRanges, 0);

      { Analyze all selection blocks }
      for I := 0 to High(fCarets) do
      begin

        { Find out caret position interzection }
        if (nLine = fCarets[I].bcCaret.Line) and InRange(fCarets[I].bcCaret.Char,
          Succ(TokenAccu.Pos), Succ(TokenAccu.Pos) + n4)
        then begin
          J := CharToPixels(fCarets[I].bcCaret.Char);
          if (J > -1) and (fCarets[I].bcCaret.Char = Succ(fLines.AccessStringLength(Pred(fCarets[I].bcCaret.Line)))) then
            bDoCaretWhenToEol := True;
        end
        else
          J := -1;

        { Selection block inside token (bcStart check) }
        if (nLine = fCarets[I].bcStart.Line) and (fCarets[I].bcStart.Char >= Succ(TokenAccu.Pos))
          and (fCarets[I].bcStart.Char <= Succ(TokenAccu.Pos) + n4)
        then
          if fCarets[I].bcEnd.Line > fCarets[I].bcStart.Line then
            AddBGRange(CharToPixels(fCarets[I].bcStart.Char), ColumnToXValue(n2), J)
          else
            AddBGRange(CharToPixels(fCarets[I].bcStart.Char), CharToPixels(fCarets[I].bcEnd.Char), J)

        { Selection block inside token (bcEnd check) }
        else if (nLine = fCarets[I].bcEnd.Line) and (fCarets[I].bcEnd.Char >= Succ(TokenAccu.Pos))
          and (fCarets[I].bcEnd.Char <= Succ(TokenAccu.Pos) + n4)
        then
          if fCarets[I].bcStart.Line < fCarets[I].bcEnd.Line then
            AddBGRange(ColumnToXValue(n1), CharToPixels(fCarets[I].bcEnd.Char), J)
          else
            AddBGRange(CharToPixels(Max(fCarets[I].bcStart.Char, Succ(TokenAccu.Pos))), CharToPixels(fCarets[I].bcEnd.Char), J)

        { A whole token inside selection block }
        else if (nLine >= fCarets[I].bcStart.Line) and (nLine <= fCarets[I].bcEnd.Line)
          then
        begin
          Whole := False;

          if nLine = fCarets[I].bcStart.Line then
            if fCarets[I].bcEnd.Line > fCarets[I].bcStart.Line then
            begin
              if Succ(TokenAccu.Pos) > fCarets[I].bcStart.Char then
                Whole := True;
            end
            else if (Succ(TokenAccu.Pos) > fCarets[I].bcStart.Char) and
              (Succ(TokenAccu.Pos) + n4 < fCarets[I].bcEnd.Char)
            then
              Whole := True
            else
          else if nLine = fCarets[I].bcEnd.Line then
            if fCarets[I].bcEnd.Line > fCarets[I].bcStart.Line then
            begin
              if Succ(TokenAccu.Pos) + n4 < fCarets[I].bcEnd.Char then
                Whole := True;
            end
            else if (Succ(TokenAccu.Pos) > fCarets[I].bcStart.Char) and
              (Succ(TokenAccu.Pos) + n4 < fCarets[I].bcEnd.Char)
            then
              Whole := True
            else
          else if (nLine > fCarets[I].bcStart.Line) and (nLine < fCarets[I].bcEnd.Line) then
            Whole := True;

          if Whole then
            AddBGRange(ColumnToXValue(n1), ColumnToXValue(n2), J);
        end;
      end;

      { Paint all recognized selection ranges and unselected space in the
        beginning and in-between them (if persists) }
      for I := 0 to High(BGRanges) do
      begin
        if BGRanges[I].B > Bg then
          DoPaintBlock(Bg, BGRanges[I].B, -1, False);
        DoPaintBlock(Bg, BGRanges[I].E, BGRanges[I].C, True);
      end;

      { Paint what's left }
      if Bg < En then
        DoPaintBlock(Bg, En, -1, False);
    end;

    procedure DoPaintToken(const n1, n2, n3, n4: Integer;
      const Selected: Boolean);
    var
      DX: Integer;
      bInMargin: Boolean;
    begin
      if WordWrap or not fRightEdgeShow or not (eoHighlightMargin in fOptions)
        or not ((n2 > fRightEdge) and (n1 <= fRightEdge)) then
      begin
        bInMargin := not WordWrap and fRightEdgeShow and (eoHighlightMargin in fOptions)
          and (n1 > fRightEdge);

        { Adjust token rect }
        rcToken.Right := ColumnToXValue(n2);

        { Quick fix for non-break whitespace mode }
        if WordWrap and not BreakWhitespace then
          if eoWrapAgainstMargin in fOptions then
            rcToken.Right := Min(ColumnToXValue(Succ(fRightEdge)), rcToken.Right)
          else
            rcToken.Right := Min(ColumnToXValue(fCharsInWindow), rcToken.Right);

        { Paint }
        if not fSelections then
        begin
          SetDrawingColors(Selected, bInMargin);
          with TokenAccu do
            PaintToken(S, ExpandedLen, CharsBefore, n1, n2, n3, n4);
        end
        else begin
          PaintBGBlock(n1, n2, n3, n4, bInMargin);
          SetDrawingColors(False, False);
          with TokenAccu do
            PaintToken(S, ExpandedLen, CharsBefore, n1, n2, n3, n4, True);
        end;
      end
      else begin

        { Remember token position }
        DX := rcToken.Left;

        { Paint background behind margin }
        rcToken.Right := ColumnToXValue(Succ(fRightEdge));
        if not fSelections then
        begin
          SetDrawingColors(Selected, False);
          fTextDrawer.ExtTextOut(ColumnToXValue(n1), rcToken.Top,
            [tooOpaque, tooClipped], rcToken, nil, 0);
        end
        else
          PaintBGBlock(n1, Succ(fRightEdge), n3, n4, False);

        { Paint background after margin }
        rcToken.Left := rcToken.Right;
        rcToken.Right := ColumnToXValue(n2);
        if not fSelections then
        begin
          SetDrawingColors(Selected, True);
          fTextDrawer.ExtTextOut(rcToken.Left, rcToken.Top,
            [tooOpaque, tooClipped], rcToken, nil, 0);
        end
        else
          PaintBGBlock(Succ(fRightEdge), n2, n3, n4, True);

        { Paint text itself on top }
        rcToken.Left := DX;
        with TokenAccu do
          PaintToken(S, ExpandedLen, CharsBefore, n1, n2, n3, n4, True);
      end;

      { ... }
      if bDoCaretWhenToEol then
      begin
        bDoCaretWhenToEol := False;
        Inc(rcToken.Left);
        Inc(rcToken.Right);
      end;
    end;

  begin
    { Get visual rect bounds }
    nC1 := TokenAccu.CharsBefore + 1;
    nC2 := TokenAccu.CharsBefore + TokenAccu.ExpandedLen + 1;
    nC3 := 1;
    nC4 := TokenAccu.Len;

    { See if a token being painted is partially selected }
    if not bComplexLine or (WordWrap and (not bFirstRow) and
      (fActiveSelectionMode = smColumn)) then
    begin
      bU1 := False;
      bSel := bLineSelected;
      bU2 := False;
      bComplexToken := False;
    end
    else begin
      bU1 := (nC1 < nLineSelStart);
      bSel := (nC1 < nLineSelEnd) and (nC2 > nLineSelStart);
      bU2 := (nC2 > nLineSelEnd);
      bComplexToken := bSel and (bU1 or bU2);
    end;

    { Any token chars accumulated? }
    if (TokenAccu.Len > 0) then
    begin
      { Initialize the colors and the font style }
      if not bSpecialLine then
      begin
        colBG := TokenAccu.BG;
        colFG := TokenAccu.FG;
      end;

      { Roman / Bold }
      if eoFontForceRoman in fOptions then
        fTextDrawer.SetStyle(TokenAccu.Style - [fsBold])
      else if eoFontForceBold in fOptions then
        fTextDrawer.SetStyle(TokenAccu.Style + [fsBold])
      else
        fTextDrawer.SetStyle(TokenAccu.Style);

      { Paint the chars }
      if bComplexToken then
      begin
        nC1Sel := Max(nLineSelStart, nC1);
        nC2Sel := Min(nLineSelEnd, nC2);

        { Get real token position }
        while (TokenAccu.Pos + nc3 <= Length(StrA^)) and
          (Pred(nC1) + ExpandedPos(TokenAccu.Pos, TokenAccu.Pos + nC3, StrA) < nC1Sel)
        do
          Inc(nC3);

        { First unselected part of the token }
        if bU1 then
          DoPaintToken(nC1, nLineSelStart, 1, nC3 - 1, False);

        { Selected part of the token. Get real token position }
        nC4 := nC3;
        while (TokenAccu.Pos + nc4 <= Length(StrA^)) and
          (Pred(nC1) + ExpandedPos(TokenAccu.Pos, TokenAccu.Pos + nC4, StrA) < nC2Sel)
        do
          Inc(nC4);

        { Fix bounds and paint }
        DoPaintToken(nC1Sel, nC2Sel, nC3, nC4 - nC3, True);

        { Second unselected part of the token }
        if bU2 then

          { Fix bounds and paint }
          DoPaintToken(nC2Sel, nC2, nC4, TokenAccu.Len - nC4 + 1, False);
      end

      { Simple, completely (un)selected token }
      else
        DoPaintToken(nC1, nC2, nC3, nC4, bSel);
    end;

    { Fill the background to the end of this line if necessary }
    if bFillToEOL and (rcToken.Left < rcLine.Right) then
    begin
      if fSelections then
      if fLines.AccessStringLength(Pred(nLine)) = 0 then
      begin
        TokenAccu.Pos := 0;
        PaintBGBlock(0, 0, 0, 0, False);
        if bDoCaretWhenToEol then
        begin
          bDoCaretWhenToEol := False;
          Inc(rcToken.Left, 3);
          Inc(rcToken.Right, 3);
        end;
      end;

      if not bSpecialLine then
        colBG := colEditorBG;

      if bComplexLine and
        not (WordWrap and not bFirstRow and (fActiveSelectionMode = smColumn)) then
      begin
        bCanPaintEolMark := InRange(EolPos, nLineSelStart, nLineSelEnd);
        nX1 := ColumnToXValue(nLineSelStart);
        nX2 := ColumnToXValue(nLineSelEnd);

        if fRightEdgeShow and (eoHighlightMargin in fOptions) then
          if WordWrap and not (eoWrapAgainstMargin in fOptions) then
            nC1 := ColumnToXValue(Succ(fCharsInWindow - 1))
          else if not WordWrap then
            nC1 := ColumnToXValue(Succ(fRightEdge))
          else
            nC1 := MaxInt
        else
          nC1 := MaxInt;

        { First unselected part }
        if (rcToken.Left < nX1) then
        begin
          if InRange(nC1, rcToken.Left, nX1) then
          begin
            SetDrawingColors(False);

            rcToken.Right := nC1;
            if (TokenAccu.Len > 0) and (TokenAccu.Style <> []) then
              AdjustEndRect;

            Canvas.FillRect(rcToken);

            SetDrawingColors(False, True);

            rcToken.Left := nC1;
            rcToken.Right := nX1;
            if (TokenAccu.Len > 0) and (TokenAccu.Style <> []) then
              AdjustEndRect;

            Canvas.FillRect(rcToken);
          end
          else begin
            SetDrawingColors(False);

            rcToken.Right := nX1;
            if (TokenAccu.Len > 0) and (TokenAccu.Style <> []) then
              AdjustEndRect;

            Canvas.FillRect(rcToken);
          end;
          rcToken.Left := nX1;
        end;

        { Selected part }
        if (rcToken.Left < nX2) then
        begin
          bCanPaintEolMark := EolPos >= nLineSelStart;
          if InRange(nC1, rcToken.Left, nX2) then
          begin
            SetDrawingColors(True);

            rcToken.Right := nC1;
            if (TokenAccu.Len > 0) and (TokenAccu.Style <> []) then
              AdjustEndRect;

            Canvas.FillRect(rcToken);

            SetDrawingColors(True, True);

            rcToken.Left := nC1;
            rcToken.Right := nX2;
            if (TokenAccu.Len > 0) and (TokenAccu.Style <> []) then
              AdjustEndRect;

            Canvas.FillRect(rcToken);
          end
          else begin
            if rcToken.Left > nC1 then
              SetDrawingColors(True, True)
            else
              SetDrawingColors(True);

            rcToken.Right := nX2;
            if (TokenAccu.Len > 0) and (TokenAccu.Style <> []) then
              AdjustEndRect;

            if WordWrap and (eoWrapAgainstMargin in fOptions) then
              rcToken.Right := ColumnToXValue(fRightEdge + 1);

            Canvas.FillRect(rcToken);

            { Fill to the window }
            if WordWrap and (eoWrapAgainstMargin in fOptions) then
            begin
              fTextDrawer.SetBackColor(Color);
              Canvas.Brush.Color := Color;
              rcToken.Left := rcToken.Right;
              rcToken.Right := ClientWidth;
              Canvas.FillRect(rcToken)
            end;
          end;
          rcToken.Left := nX2;
        end;

        { Second unsselected part }
        if (rcToken.Left < rcLine.Right) then
        begin
          if InRange(nC1, rcToken.Left, rcLine.Right) then
          begin
            SetDrawingColors(False);

            rcToken.Right := nC1;
            if (TokenAccu.Len > 0) and (TokenAccu.Style <> []) then
              AdjustEndRect;

            Canvas.FillRect(rcToken);

            SetDrawingColors(False, True);

            rcToken.Left := nC1;
            rcToken.Right := rcLine.Right;
            if (TokenAccu.Len > 0) and (TokenAccu.Style <> []) then
              AdjustEndRect;

            Canvas.FillRect(rcToken);
          end
          else begin
            if rcToken.Left > nC1 then
              SetDrawingColors(False, True)
            else
              SetDrawingColors(False);

            if WordWrap and (eoWrapAgainstMargin in fOptions) then
              rcToken.Right := ColumnToXValue(fRightEdge + 1)
            else
              rcToken.Right := rcLine.Right;

            if (TokenAccu.Len > 0) and (TokenAccu.Style <> []) then
              AdjustEndRect;

            Canvas.FillRect(rcToken);

            { Fill to the window }
            if WordWrap and (eoWrapAgainstMargin in fOptions) then
            begin
              fTextDrawer.SetBackColor(Color);
              Canvas.Brush.Color := Color;
              rcToken.Left := rcToken.Right;
              rcToken.Right := ClientWidth;
              Canvas.FillRect(rcToken)
            end;
          end;
        end;
      end
      else begin
        bCanPaintEolMark := bLineSelected;
        if WordWrap and (eoWrapAgainstMargin in fOptions) then
        begin

          { Fill to right edge }
          SetDrawingColors(bLineSelected);
          rcToken.Right := ColumnToXValue(Succ(fRightEdge));
          Canvas.FillRect(rcToken);

          { Fill to window }
          fTextDrawer.SetBackColor(Color);
          Canvas.Brush.Color := Color;
          rcToken.Left := rcToken.Right;
          rcToken.Right := ClientWidth;
          Canvas.FillRect(rcToken);
        end
        else begin
          if fRightEdgeShow and (eoHighlightMargin in fOptions) then
            if WordWrap and not (eoWrapAgainstMargin in fOptions) then
              nX1 := ColumnToXValue(Succ(fCharsInWindow - 1))
            else if not WordWrap then
              nX1 := ColumnToXValue(Succ(fRightEdge))
            else
              nX1 := MaxInt
          else
            nX1 := MaxInt;
          rcToken.Right := rcLine.Right;
          if InRange(nX1, rcToken.Left, rcLine.Right) then
          begin
            SetDrawingColors(bLineSelected);

            rcToken.Right := nX1;
            if (TokenAccu.Len > 0) and (TokenAccu.Style <> []) then
              AdjustEndRect;

            Canvas.FillRect(rcToken);

            SetDrawingColors(bLineSelected, True);

            rcToken.Left := nX1;
            rcToken.Right := rcLine.Right;
            if (TokenAccu.Len > 0) and (TokenAccu.Style <> []) then
              AdjustEndRect;

            Canvas.FillRect(rcToken);
          end
          else begin
            if rcToken.Left >= nX1 then
              SetDrawingColors(bLineSelected, True)
            else
              SetDrawingColors(bLineSelected);

            if WordWrap and (eoWrapAgainstMargin in fOptions) then
              rcToken.Right := ColumnToXValue(fRightEdge + 1)
            else
              rcToken.Right := rcLine.Right;

            if (TokenAccu.Len > 0) and (TokenAccu.Style <> []) then
              AdjustEndRect;

            Canvas.FillRect(rcToken);
          end;
        end;
      end;

      { Draw EOL glyph? }
      if (EolPos > 0) and (eoShowEolSpecialChar in fOptions) then
      begin
        if (eoShowSpecialcharsInSelection in fOptions) and not (eoShowSpecialchars in fOptions)
          and not (bCanPaintEolMark) then Exit;

        { Find color which will be used for painting }
        colEOL := Blend(ColBG, Font.Color, 50);
        if Trunc(ColorDistance(colEOL, colBG)) < 150 then
          colEOL := Font.Color;

        { Prepare rect }
        rcEol := rcLine;
        with rcEol do
        begin
          Top := Bottom - fTextHeight;
          Dec(Bottom, 3);
        end;

        { Prepare rect and paint }
        with Canvas, rcEol do
        begin
          Left := ColumnToXValue(EolPos);
          if Left > fGutter.Width - 8 then
          begin
            with Pen do
            begin
              Mode := pmBlack;
              Mode := pmCopy;
              Color := colEOL;
            end;

            nX1 := Top + 2;
            while nX1 < Bottom do
            begin
              MoveTo(Left + 6, nX1);
              LineTo(Left + 6, nx1 + 1);
              Inc(nX1, 2);
            end;

            MoveTo(Left + 6, nX1);
            LineTo(Left + 2, nX1 - 4);
            MoveTo(Left + 6, nX1);
            LineTo(Left + 10, nX1 - 4);
            Pen.Color := fOutliningAttr.Foreground;
          end;
        end;
      end;
    end;
  end;

  { Store the token chars with the attributes in the TokenAccu
    record. This will paint any chars already stored if there is
    a (visible) change in the attributes }
  procedure AddHighlightToken(const Token: UnicodeString;
    CharsBefore, TokenLen, TokenExpandedLen: Integer; Foreground, Background: TColor;
    Style: TFontStyles; P: Integer);
  var
    bCanAppend: Boolean;
    bSpacesTest, bIsSpaces: Boolean;
    I: Integer;

    function TokenIsSpaces: Boolean;
    var
      pTok: PChar;
    begin
      if not bSpacesTest then
      begin
        bSpacesTest := True;
        pTok := PChar(Token);
        while pTok^ <> #0 do
        begin
          if pTok^ > #32 then
            Break;
          Inc(pTok);
        end;
        bIsSpaces := pTok^ = #0;
      end;
      Result := bIsSpaces;
    end;

  begin
    { Fix colors }
    if (Background = clNone) or
      (((ActiveLineBG <> clNone) or
      (eoAdaptiveSpecialLine in Options)) and (bCurrentLine))
    then
      Background := colEditorBG;
    if Foreground = clNone then
      Foreground := Font.Color;

    { Do we have to paint the old chars first, or can we just append? }
    bCanAppend := False;
    bSpacesTest := False;
    if (TokenAccu.Len > 0) then
    begin
      { font style must be the same or token is only spaces }
      if (TokenAccu.Style = Style)
        or (not (fsUnderline in Style) and not (fsUnderline in TokenAccu.Style)
        and TokenIsSpaces) then
      begin
        { Either special colors or same colors }
        if (bSpecialLine and not (eoSpecialLineDefaultFg in fOptions)) or

          { background color must be the same and }
          ((TokenAccu.BG = Background) and

          { foreground color must be the same or token is only spaces }
          ((TokenAccu.FG = Foreground) or TokenIsSpaces))
        then
          bCanAppend := True;
      end;

      { If we can't append it, then we have to paint the old token chars first }
      if not bCanAppend then
      begin
        PaintHighlightToken(False);
        TokenAccu.Pos := P;
      end;
    end;

    { Accumulate chars }
    if bCanAppend then
    begin
      if (TokenAccu.Len + TokenLen > TokenAccu.MaxLen) then
      begin
        TokenAccu.MaxLen := TokenAccu.Len + TokenLen + 32;
        SetLength(TokenAccu.S, TokenAccu.MaxLen);
      end;
      for I := 1 to TokenLen do
        TokenAccu.S[TokenAccu.Len + I] := Token[I];
      Inc(TokenAccu.Len, TokenLen);
      Inc(TokenAccu.ExpandedLen, TokenExpandedLen);
    end

    { Fill with new chars }
    else begin
      with TokenAccu do
      begin
        Pos := P;
        Len := TokenLen;
        ExpandedLen := TokenExpandedLen;
        if (Len > MaxLen) then
        begin
          MaxLen := Len + 32;
          SetLength(S, MaxLen);
        end;
        for I := 1 to TokenLen do
          S[I] := Token[I];
        FG := Foreground;
        BG := Background;
      end;
      TokenAccu.CharsBefore := CharsBefore;
      TokenAccu.Style := Style;
    end;
  end;

  procedure PaintMisspelledWords;
  const
    MW_POINTS: array[0..3] of ShortInt = (0, 1, 2, 1);
  var
    I, TPY, TPX: Integer;
    MaxX,
    NewPoint,
    NewY: Integer;

    procedure DrawPoint;
    var
      savedDC: Integer;
    begin
      with Canvas do
      begin
        savedDC := SaveDC(Handle);
        if NewY = TPY - 1 then
          Exit
        else
          if fDimmed and not ((nLine >= fDimFirstLine) and (nLine <= fDimLastLine)) then
            Pen.Color := Lighten(clRed, -50)
          else
            Pen.Color := clRed;
        Pixels[TPX, NewY] := Pen.Color;
        RestoreDC(Handle, savedDC);
      end;
    end;

  begin
    TPY := rcLine.Bottom - 3;
    for I := 0 to High(aMisspelled) do
    begin
      TPX := ColumnToXValue(Succ(aMisspelled[I].Column));
      NewPoint := 0;
      NewY := TPY + MW_POINTS[NewPoint];
      DrawPoint;
      MaxX := ColumnToXValue(Succ(aMisspelled[I].Column + aMisspelled[I].Row));
      while TPX <= MaxX do
      begin
        DrawPoint;
        Inc(NewPoint);
        if NewPoint > High(MW_POINTS) then
          NewPoint := 0;
        DrawPoint;
        Inc(TPX);
        NewY := TPY + MW_POINTS[NewPoint];
      end;
    end;
    SetLength(aMisspelled, 0);
  end;

  procedure PaintLines;
  var
    nRow: Integer;         // Row index to control correct loop bounds
    sLine: UnicodeString;  // Line being painted
    sToken: UnicodeString; // Current token being painted (can be accumulated)
    nTokenPos, nTokenLen, nOldTokenLen, nAmount: Integer; // Current token visual position and length
    Attr: TSynHighlighterAttributes; // Current token attributes
    vAuxPos: TDisplayCoord;
    vFirstChar: Integer;
    vLastChar: Integer;
    ScrolledXBy: Integer;
    FoldRange: TSynEditFoldRange;
    RowCol: TBufferCoord;
    rcCollapseMark: TRect;
    Dummy: Boolean;
    FillCl: TColor;
  begin
    { Initialize rcLine for drawing. Note that Top and Bottom are updated
      inside the loop. Get only the starting point for this }
    rcLine := AClip;
    rcLine.Left := fGutter.Width;
    rcLine.Bottom := (aFirstRow - fTopLine) * fTextHeight;

    { Make sure the token accumulator string doesn't get reassigned too often }
    if Assigned(fHighlighter) then
    begin
      TokenAccu.MaxLen := Max(128, fCharsInWindow);
      SetLength(TokenAccu.S, TokenAccu.MaxLen);
    end;

    { Calculate indent guides positions }
    ScrolledXBy := (fLeftChar - 1) * fCharWidth;

    { Now loop through all the lines. The indices are valid for fLines }
    nLine := vFirstLine;
    nRow := aFirstRow;
    while nRow <= aLastRow do
    begin

      { Get folding block to skip folded lines }
      if CodeFolding.Enabled then
      	FoldRange := FoldRangeForLine(nLine)
      else
        FoldRange := nil;

      { Retrieve text to paint }
      sLine := fLines[nLine - 1];
      StrA := fLines.Analyzis[nLine - 1];

      { Determine whether will be painted with ActiveLineColor }
      if fSelections then
      begin
        bCurrentLine := False;
        for vFirstChar := 0 to High(fCarets) do
          if fCarets[vFirstChar].bcCaret.Line = nLine then
          begin
            bCurrentLine := True;
            Break;
          end;
      end
      else
        bCurrentLine := fCaretY = nLine;

      { Initialize the text and background colors, maybe the line should
        use special values for them }
      colFG := Font.Color;
      colBG := colEditorBG;
      bSpecialLine := DoOnSpecialLineColors(nLine, colFG, colBG);
      if bSpecialLine then
      begin
        colSelFG := colBG;
        colSelBG := colFG;
      end
      else with fSelectedColor do
      begin
        colSelFG := Foreground;
        colSelBG := Background;
      end;

      { Retrieve current being painted line rows. It's always necessary
        because line can be wrapped to multiple rows in word wrap mode }
      if WordWrap then
      begin
        vStartRow := nRow;
        bFirstRow := nRow = fWordWrapPlugin.LineToRow(nLine);
        if fCodeFolding.Enabled and Assigned(FoldRange) then
          vEndRow := Min(LineToRow(FoldRange.ToLine) - 1, aLastRow)
        else
          vEndRow := Min(LineToRow(nLine + 1) - 1, aLastRow);
      end
      else begin
        vStartRow := nRow;
        vEndRow := nRow;
      end;

      { Loop through line's rows }
      cRow := vStartRow;
      nIndent := 0;
      while cRow <= vEndRow do
      begin
        if WordWrap then
        begin
          vAuxPos.Row := cRow;

          if Assigned(fHighlighter) then
            vAuxPos.Column := FirstCol
          else
            { When no highlighter is assigned, we must always start from the
              first char in a row and PaintToken will do the actual clipping }
            vAuxPos.Column := 1;

          { Apply indent in aligned mode }
          if eoAlignedWrap in fOptions then
            if not bFirstRow and (nIndent = 0) then
              nIndent := GetLeadingExpandedLength(Lines.fList^[nLine - 1].fString, fTabWidth);
          Inc(vAuxPos.Column, nIndent);
          vFirstChar := fWordWrapPlugin.DisplayToBufferPos(vAuxPos).Char;

          vAuxPos.Column := LastCol;
          vLastChar := fWordWrapPlugin.DisplayToBufferPos(vAuxPos).Char;

          Dec(vLastChar);
        end
        else begin
          vFirstChar := FirstCol;
          vLastChar := LastCol;
        end;

        { Get the information about the line selection. Three different parts
          are possible (unselected before, selected, unselected after), only
          unselected or only selected means bComplexLine will be False. Start
          with no selection, compute based on the visible columns }
        bComplexLine := False;
        bDoCaretWhenToEol := False;
        nLineSelStart := 0;
        nLineSelEnd := 0;

        { Does the selection intersect the visible area? }
        if bAnySelection and (cRow >= vSelStart.Row) and (cRow <= vSelEnd.Row) then
        begin

          { Default to a fully selected line. This is correct for the smLine
            selection mode and a good start for the smNormal mode }
          nLineSelStart := FirstCol;
          nLineSelEnd := LastCol + 1;

          { We got to always reassign display coords in columnar mode to avoid
            intersection with expanded chars }
          if (fActiveSelectionMode = smColumn) or
            ((fActiveSelectionMode = smNormal) and (cRow = vSelStart.Row)) then
          begin
            if (vSelStart.Column > LastCol) then
            begin
              nLineSelStart := 0;
              nLineSelEnd := 0;
            end
            else if (vSelStart.Column > FirstCol) then
            begin
              if (fActiveSelectionMode = smColumn) then
                nLineSelStart := BufferToDisplayPos(DisplayToBufferPos(DisplayCoord(vSelStart.Column, cRow))).Column
              else
                nLineSelStart := vSelStart.Column;
              bComplexLine := True;
            end;
          end;
          if (fActiveSelectionMode = smColumn) or
            ((fActiveSelectionMode = smNormal) and (cRow = vSelEnd.Row)) then
          begin
            if (vSelEnd.Column < FirstCol) then
            begin
              nLineSelStart := 0;
              nLineSelEnd := 0;
            end
            else if (vSelEnd.Column < LastCol)  then
            begin
              if (fActiveSelectionMode = smColumn) then
                nLineSelEnd := BufferToDisplayPos(DisplayToBufferPos(DisplayCoord(vSelEnd.Column, cRow))).Column
              else
                nLineSelEnd := vSelEnd.Column;
              bComplexLine := True;
            end;
          end;
        end;

        { Update the rcLine rect to this line }
        with rcLine do
        begin
          Top := Bottom;
          Inc(Bottom, fTextHeight);
        end;

        { Selected? }
        bLineSelected := not bComplexLine and (nLineSelStart > 0);
        rcToken := rcLine;

        { Fill background in centered mode }
        if (eoWrapAgainstMargin in fOptions) and (eoWrapCentered in fOptions) then
        begin
          rcToken.Right := rcToken.Left + fCenterOffset;
          with Canvas do
          begin
            fTextDrawer.SetBackColor(Color);
            Brush.Color := Color;
            FillRect(rcToken);
          end;
          rcToken.Left := rcToken.Right;
          rcToken.Right := rcLine.Right;
        end;

        { Require grammar }
        if not Assigned(fHighlighter) or not fHighlighter.Enabled then
          with Canvas do
          begin
            Brush.Color := Color;
            FillRect(rcToken);
          end

        { Paint with grammar }
        else begin
          { Initialize highlighter with line text and range info. It is
            necessary because we probably did not scan to the end of the last
            line - the internal highlighter range might be wrong }
          if nLine = 1 then
            fHighlighter.ResetRange
          else
            fHighlighter.SetRange(fLines.Ranges[nLine - 2]);
          fHighlighter.SetLine(sLine, nLine - 1);

          { Choose a color }
          FillCl := clNone;
          Attr := fHighlighter.WhitespaceAttribute;
          if Assigned(Attr) then
            FillCl := Attr.Background;
          if FillCl = clNone then
            FillCl := Color;

          { Apply effect in snippet mode }
          if fSnippet and (Length(fCarets) > 0) then
            if (nLine >= fCarets[0].bcStart.Line) and
              (nLine <= fCarets[High(fCarets)].bcEnd.Line) then
            begin
              { Get amount of effect }
              if IsDarkColor(FillCl) then
                nOldTokenLen := 50
              else
                nOldTokenLen := -50;
              FillCl := Lighten(FillCl, nOldTokenLen);
            end;

          { Try to concatenate as many tokens as possible to minimize the count
            of ExtTextOutW calls necessary. This depends on the selection state
            or the line having special colors. For spaces the foreground color
            is ignored as well }
          nTokenPos := 0;
          nOldTokenLen := 0;
          TokenAccu.Len := 0;

          SetLength(aMisspelled, 0);

          { Test first whether anything of this token is visible }
          while not fHighlighter.GetEol do
          begin
            sToken := fHighlighter.GetToken;
            Inc(nTokenPos, nOldTokenLen);
            nTokenLen := ExpandedPos(fHighlighter.GetTokenPos,
              fHighlighter.GetTokenPos + fHighlighter.GetTokenLen, StrA);
            nOldTokenLen := nTokenLen;
            if nTokenPos + nTokenLen >= vFirstChar then
            begin
              if nTokenPos + nTokenLen > vLastChar then
              begin
                if nTokenPos > vLastChar then
                  Break;
                nTokenLen := vLastChar - nTokenPos;
              end;

              { Add to misspelled }
              if fSpellCheck then
                if ((fHighlighter as TSynUniSyn).GetTokenNature = tknnIdent) and
                  ((fHighlighter as TSynUniSyn).GetRule = (fHighlighter as TSynUniSyn).GetRange)
                  and (sroSpellCheck in TSynRange((fHighlighter as TSynUniSyn).GetRange).Options)
                then
                  if (fHighlighter.GetTokenLen > 1) and not fOnSpellCheck(Self, fHighlighter.GetToken) then
                  begin
                    SetLength(aMisspelled, Succ(Length(aMisspelled)));
                    with aMisspelled[High(aMisspelled)] do
                    begin
                      Column := Max(nIndent, nIndent + nTokenPos - (vFirstChar - FirstCol));
                      Row := nTokenLen;
                    end;
                  end;

              { Remove offset generated by tokens already displayed
                (in previous rows) }
              { nTokenPos - (vFirstChar - FirstCol) }

              { It's at least partially visible. Get the token attributes now }
              Attr := fHighlighter.GetTokenAttribute;
              if Assigned(Attr) then
                AddHighlightToken(sToken, nIndent + nTokenPos - (vFirstChar - FirstCol),
                  fHighlighter.GetTokenLen, nTokenLen, Attr.Foreground,
                  Attr.Background, Attr.Style, fHighlighter.GetTokenPos)
              else
                AddHighlightToken(sToken, nIndent + nTokenPos - (vFirstChar - FirstCol),
                  fHighlighter.GetTokenLen, nTokenLen, colFG, colBG, Font.Style,
                  fHighlighter.GetTokenPos);
            end;

            { Let the highlighter scan the next token }
            fHighlighter.Next;
          end;

          { Draw anything that's left in the TokenAccu record. Fill to the end
            of the invalid area with the correct colors }
          if ((eoShowSpecialChars in fOptions) or (eoShowSpecialcharsInSelection in fOptions))
            and fHighlighter.GetEol then
          begin
            if WordWrap then
              if (cRow = vEndRow) and (fLines.Count > 0) then
                PaintHighlightToken(True, fWordWrapPlugin.GetRowLength(vEndRow, nLine) + 1 + nIndent)
              else
                PaintHighlightToken(True)
            else if not WordWrap then
              PaintHighlightToken(True, ExpandLines.ExpandedStringLengths[nLine - 1] + 1);
          end
          else
            PaintHighlightToken(True);

          { Get token rect }
          rcToken.Left := rcLine.Left;
          rcToken.Right := rcToken.Left + 2;

          { Fill }
          with Canvas do
          begin
            fTextDrawer.SetBackColor(FillCl);
            Brush.Color := FillCl;
            FillRect(rcToken);
          end;

          { Adjust the invalid area to not include it }
          rcToken.Left := rcLine.Right;
          rcToken.Right := rcLine.Right;

          { Collapse hint rect painting }
          if fCodeFolding.Enabled and Assigned(FoldRange) then
          begin
            rcCollapseMark := GetCollapseMarkRect(FoldRange, vEndRow, nLine, 1);

            { Get real position of rect }
            with rcCollapseMark do
            begin
              Dec(Left, ScrolledXBy);
              Dec(Right, ScrolledXBy);
              Inc(Left, fCharWidth * nIndent);
              Inc(Right, fCharWidth * nIndent);
            end;

            { Transparently paint folding hint rect if it is visible }
            if (rcCollapseMark.Left > 0) and
              (rcCollapseMark.Right - fGutter.Width < ClientWidth) then
            begin

              { Paint hint mark }
              with Canvas, TextDrawer, rcCollapseMark do
              begin
                nTokenPos := SaveDC(Handle);
                SetBkMode(Handle, TRANSPARENT);
                try
                  SetForeColor(fOutliningAttr.Foreground);
                  if FoldRange.FoldRegion.Name = EmptyStr then
                    TextOut(Left + 1, Top - 1, '...', 3)
                  else
                    TextOut(Left + 1, Top - 1, PChar(FoldRange.FoldRegion.Name),
                      Length(FoldRange.FoldRegion.Name));

                  { Paint beautiful border }
                  Pen.Color := fOutliningAttr.Foreground;
                  MoveTo(Left  + 1, Top);
                  LineTo(Right, Top);
                  MoveTo(Right, Top + 1);
                  LineTo(Right, Bottom);
                  MoveTo(Right - 1, Bottom);
                  LineTo(Left, Bottom);
                  MoveTo(Left, Bottom - 1);
                  LineTo(Left, Top);
                finally
                  SetBkMode(Handle, OPAQUE);
                  RestoreDC(Handle, nTokenPos);
                end;
              end;
            end;
          end;
        end;

        { Paint margin }
        if bDoRightEdge then
        begin
          with Canvas, rcLine do
          begin
            if IsDarkColor(Color) then
              nAmount := -10
            else
              nAmount := -40;
            if bComplexLine then
            begin
              nTokenPos := ColumnToXValue(nLineSelStart);
              nOldTokenLen := ColumnToXValue(nLineSelEnd);
              if InRange(nRightEdge, nTokenPos, nOldTokenLen) then
                if (eoHighlightMargin in fOptions) and bCurrentLine and (eoAdaptiveSelection in fOptions) then
                  Pen.Color := Lighten(Blend(fRightEdgeColor, Lighten(fSelectedColor.Background, 25), 15), nAmount)
                else
                  Pen.Color := Blend(fRightEdgeColor, fSelectedColor.Background, 25)
              else
                Pen.Color := fRightEdgeColor;
            end
            else begin
              if bLineSelected then
                if (eoHighlightMargin in fOptions) and bCurrentLine and (eoAdaptiveSelection in fOptions) then
                  Pen.Color := Lighten(Blend(fRightEdgeColor, Lighten(fSelectedColor.Background, 25), 15), nAmount)
                else
                  Pen.Color := Blend(fRightEdgeColor, fSelectedColor.Background, 25)
              else
                Pen.Color := fRightEdgeColor;
            end;
            MoveTo(nRightEdge, Top);
            LineTo(nRightEdge, Bottom + 1);
            if (eoWrapAgainstMargin in fOptions) and (eoWrapCentered in fOptions) and (fCenterOffset > 0) then
            begin
              if fGutter.Visible then
                nTokenLen := fCenterOffset + fGutter.Width
              else
                nTokenLen := fCenterOffset;
              if bComplexLine then
                if InRange(nTokenLen + 2, nTokenPos, nOldTokenLen) and (nTokenPos <> nOldTokenLen) then
                  if (eoHighlightMargin in fOptions) and bCurrentLine and (eoAdaptiveSelectionSpecialLine in fOptions) then
                    Pen.Color := Lighten(Blend(fRightEdgeColor, Lighten(fSelectedColor.Background, 25), 15), nAmount)
                  else
                    Pen.Color := Blend(fRightEdgeColor, fSelectedColor.Background, 25)
                else
                  Pen.Color := fRightEdgeColor;
              MoveTo(nTokenLen, Top);
              LineTo(nTokenLen, Bottom + 1);
            end;
          end;
        end;

        { Paint misspelled words }
        PaintMisspelledWords;

        { Next row }
        Inc(cRow);
        bFirstRow := False;
      end;
      bCurrentLine := False;

      { Skip folded lines }
      if fCodeFolding.Enabled and Assigned(FoldRange) then
        Inc(nLine, FoldRange.LinesCollapsed);

      { Proceed to next line & corresponding row }
      Inc(nLine);
      Inc(nRow, vEndRow - vStartRow + 1);

      { Check bounds }
      if nLine > fLines.Count then
        Break;
    end;
  end;

begin
  { Retrieve lines associated with rows }
  vFirstLine := RowToLine(aFirstRow);
  vLastLine := RowToLine(aLastRow);

  { WordWrap always start from first column }
  if WordWrap then
    FirstCol := 1;

  { If the right edge is visible and in the invalid area, prepare to paint it.
    Do this first to realize the pen when getting the dc variable }
  bDoRightEdge := False;
  if fRightEdgeShow then
  begin
    if WordWrap and not (eoWrapAgainstMargin in fOptions) then
      nRightEdge := fTextOffset + (fCharsInWindow - 1) * fCharWidth
    else
      nRightEdge := fTextOffset + fRightEdge * fCharWidth;
    if (nRightEdge >= AClip.Left) and (nRightEdge <= AClip.Right) then
      bDoRightEdge := True;
  end;

  { Initialze pen }
  if bDoRightEdge or
    (fCodeFolding.Enabled and (fAllFoldRanges.AllCount > 0))
  then
    Canvas.Pen.Color := fRightEdgeColor;

  { Do everything else with API calls. This (maybe) realizes the new pen color }
  DC := Canvas.Handle;

  { Paint the visible text lines. To make this easier, compute first the
    necessary information about the selected area: is there any visible
    selected area, and what are its lines / columns? }
  if vLastLine >= vFirstLine then
  begin
    ComputeSelectionInfo;
    fTextDrawer.Style := Font.Style;
    fTextDrawer.BeginDrawing(DC);
    try
      PaintLines;
    finally
      fTextDrawer.EndDrawing;
    end;
  end;

  { If there is anything visible below the last line, then fill this as well }
  rcToken := AClip;
  rcToken.Top := (aLastRow - fTopLine + 1) * fTextHeight;
  if (rcToken.Top < rcToken.Bottom) then
    with Canvas do
    begin
      colBG := Color;
      SetDrawingColors(False);
      FillRect(rcToken);
      if not (eoWrapAgainstMargin in fOptions) and (eoHighlightMargin in fOptions) and
        (rcToken.Right > nRightEdge) then
      begin
        colBG := Color;
        SetDrawingColors(False, True);
        rcToken.Left := nRightEdge + 1;
        FillRect(rcToken);
      end;

      { Margin }
      if bDoRightEdge then
      begin
        with Canvas, rcToken do
        begin
          Pen.Color := fRightEdgeColor;
          MoveTo(nRightEdge, Top);
          LineTo(nRightEdge, Bottom + 1);
          if (eoWrapCentered in fOptions) and (fCenterOffset > 0) then
            if fGutter.Visible then
            begin
              MoveTo(fCenterOffset + fGutter.Width, Top);
              LineTo(fCenterOffset + fGutter.Width, Bottom + 1);
            end
            else begin
              MoveTo(fCenterOffset, Top);
              LineTo(fCenterOffset, Bottom + 1);
            end;
        end;
      end;
    end;
end;

procedure TCustomSynEdit.PasteFromClipboard;
var
  AddPasteEndMarker, IncrementCarets: Boolean;
  vStartOfBlock: TBufferCoord;
  vEndOfBlock: TBufferCoord;
  StoredPaintLock: Integer;
  PasteMode: TSynSelectionMode;
  jLineStates: TLineStates;
begin
  if not CanPaste then
    Exit;

  DoOnPaintTransient(ttBefore);
  try
    SetSelectedTextEmpty(GetClipboardText);
  finally
  end;

  { ClientRect can be changed by UpdateScrollBars if eoHideShowScrollBars
    is enabled }
  if eoHideShowScrollBars in Options then
  begin
    StoredPaintLock := fPaintLock;
    try
      fPaintLock := 0;
      UpdateScrollBars;
    finally
      fPaintLock := StoredPaintLock;
    end;
  end;

  EnsureCursorPosVisible;
  StatusChanged([scSelection]);
  DoOnPaintTransient(ttAfter);
end;

procedure TCustomSynEdit.SelectAll;
var
  LastPt: TBufferCoord;
begin
  LastPt.Char := 1;
  LastPt.Line := Lines.Count;
  if LastPt.Line > 0 then
    Inc(LastPt.Char, Length(Lines[LastPt.Line - 1]))
  else
    LastPt.Line  := 1;
  SetCaretAndSelection(LastPt, BufferCoord(1, 1), LastPt);
end;

procedure TCustomSynEdit.SetBlockBegin(Value: TBufferCoord);
var
  nInval1, nInval2: Integer;
  SelChanged: Boolean;
begin
  ActiveSelectionMode := SelectionMode;
  if ((eoScrollPastEol in fOptions) or (fActiveSelectionMode = smColumn)) and not WordWrap then
    Value.Char := MinMax(Value.Char, 1, fMaxScrollWidth + 1)
  else
    Value.Char := Max(Value.Char, 1);
  Value.Line := MinMax(Value.Line, 1, Lines.Count);
  if (fActiveSelectionMode = smNormal) then
    if (Value.Line >= 1) and (Value.Line <= Lines.Count) then
      Value.Char := Min(Value.Char, Length(Lines[Value.Line - 1]) + 1)
    else
      Value.Char := 1;
  if SelAvail then
  begin
    if fBlockBegin.Line < fBlockEnd.Line then
    begin
      nInval1 := Min(Value.Line, fBlockBegin.Line);
      nInval2 := Max(Value.Line, fBlockEnd.Line);
    end
    else
    begin
      nInval1 := Min(Value.Line, fBlockEnd.Line);
      nInval2 := Max(Value.Line, fBlockBegin.Line);
    end;
    fBlockBegin := Value;
    fBlockEnd := Value;

    InvalidateLines(nInval1, nInval2);
    SelChanged := True;
  end
  else
  begin
    SelChanged :=
      (fBlockBegin.Char <> Value.Char) or (fBlockBegin.Line <> Value.Line) or
      (fBlockEnd.Char <> Value.Char) or (fBlockEnd.Line <> Value.Line);
    fBlockBegin := Value;
    fBlockEnd := Value;
  end;
  if SelChanged then
    StatusChanged([scSelection]);
end;

procedure TCustomSynEdit.SetBlockEnd(Value: TBufferCoord);
var
  nLine: Integer;
begin
  ActiveSelectionMode := SelectionMode;
  if ((eoScrollPastEol in fOptions) or (fActiveSelectionMode = smColumn)) and not WordWrap then
    Value.Char := MinMax(Value.Char, 1, fMaxScrollWidth + 1)
  else
    Value.Char := Max(Value.Char, 1);
  Value.Line := MinMax(Value.Line, 1, Lines.Count);
  if (fActiveSelectionMode = smNormal) then
    if (Value.Line >= 1) and (Value.Line <= Lines.Count) then
      Value.Char := Min(Value.Char, Length(Lines[Value.Line - 1]) + 1)
    else
      Value.Char := 1;
  if (Value.Char <> fBlockEnd.Char) or (Value.Line <> fBlockEnd.Line) then
  begin
    if (Value.Char <> fBlockEnd.Char) or (Value.Line <> fBlockEnd.Line) then
    begin
      if (fActiveSelectionMode = smColumn) and (Value.Char <> fBlockEnd.Char) then
      begin
        InvalidateLines(
          Min(fBlockBegin.Line, Min(fBlockEnd.Line, Value.Line)),
          Max(fBlockBegin.Line, Max(fBlockEnd.Line, Value.Line)));
        fBlockEnd := Value;
      end
      else begin
        nLine := fBlockEnd.Line;
        fBlockEnd := Value;
        if (fActiveSelectionMode <> smColumn) or (fBlockBegin.Char <> fBlockEnd.Char) then
          InvalidateLines(nLine, fBlockEnd.Line);
      end;
      StatusChanged([scSelection]);
    end;
  end;
end;

procedure TCustomSynEdit.SetCaretX(Value: Integer);
var
  vNewCaret: TBufferCoord;
begin
  vNewCaret.Char := Value;
  vNewCaret.Line := CaretY;
  SetCaretXY(vNewCaret);
end;

procedure TCustomSynEdit.SetCaretY(Value: Integer);
var
  vNewCaret: TBufferCoord;
begin
  vNewCaret.Line := Value;
  vNewCaret.Char := CaretX;
  SetCaretXY(vNewCaret);
end;

procedure TCustomSynEdit.InternalSetCaretX(Value: Integer);
var
  vNewCaret: TBufferCoord;
begin
  vNewCaret.Char := Value;
  vNewCaret.Line := CaretY;
  InternalSetCaretXY(vNewCaret);
end;

procedure TCustomSynEdit.InternalSetCaretY(Value: Integer);
var
  vNewCaret: TBufferCoord;
begin
  vNewCaret.Line := Value;
  vNewCaret.Char := CaretX;
  InternalSetCaretXY(vNewCaret);
end;

function TCustomSynEdit.GetCaretXY: TBufferCoord;
begin
  Result.Char := CaretX;
  Result.Line := CaretY;
end;

function TCustomSynEdit.GetDisplayX: Integer;
begin
  Result := DisplayXY.Column;
end;

function TCustomSynEdit.GetDisplayY: Integer;
begin
  if not WordWrap then
    Result := GetUnRealLineNumber(CaretY) // § Garnet
  else
    Result := DisplayXY.Row;
end;

Function TCustomSynEdit.GetDisplayXY: TDisplayCoord;
begin
  Result := BufferToDisplayPos(CaretXY);
  if WordWrap and fCaretAtEOL then
  begin
    if Result.Column = 1 then
    begin
      Dec(Result.Row);
      Result.Column := fWordWrapPlugin.GetRowLength(Result.Row, CaretXY.Line) + 1;
    end
    else begin
      // Work-around situations where fCaretAtEOL should have been updated because of
      // text change (it's only valid when Column = 1)
      fCaretAtEOL := False;
    end;
  end;
end;

procedure TCustomSynEdit.SetCaretXY(const Value: TBufferCoord);
//there are two setCaretXY methods.  One Internal, one External.  The published
//property CaretXY (re)sets the block as well
begin
  IncPaintLock;
  try
    Include(fStatusChanges, scSelection);
    SetCaretXYEx(True, Value);
    if SelAvail then
      InvalidateSelection;
    fBlockBegin.Char := fCaretX;
    fBlockBegin.Line := fCaretY;
    fBlockEnd := fBlockBegin;
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.InternalSetCaretXY(const Value: TBufferCoord);
begin
  SetCaretXYEx(True, Value);
end;

procedure TCustomSynEdit.UpdateLastCaretX;
begin
  fLastCaretX := DisplayX;
end;

// -----------------------------------------------------------------------------
// § Garnet
procedure TCustomSynEdit.SetCaretXYEx(CallEnsureCursorPos: Boolean;
  Value: TBufferCoord);
var
  nMaxX: Integer;
  vTriggerPaint: boolean;
begin
  fCaretAtEOL := False;

  vTriggerPaint := HandleAllocated;
  if vTriggerPaint then
    DoOnPaintTransient(ttBefore);

  if WordWrap then
    nMaxX := MaxInt
  else
    nMaxX := MaxScrollWidth + 1;

  if Value.Line > Lines.Count then
    Value.Line := Lines.Count;
  if Value.Line < 1 then
  begin
    { This is just to make sure if Lines stringlist should be empty }
    Value.Line := 1;
    if not (eoScrollPastEol in fOptions) then
      nMaxX := 1;
  end
  else begin
    if not (eoScrollPastEol in fOptions) then
      nMaxX := ExpandLines.AccessStringLength(Value.Line - 1) + 1;
  end;

  if fActiveSelectionMode <> smColumn then
    if (Value.Char > nMaxX) and (not (eoScrollPastEol in Options) or
      not (eoAutoSizeMaxScrollWidth in Options))
    then
      Value.Char := nMaxX;

  if Value.Char < 1 then
    Value.Char := 1;

  if (Value.Char <> fCaretX) or (Value.Line <> fCaretY) then
  begin
    IncPaintLock;
    try
      { Simply include the flags, fPaintLock is > 0 }
      if fCaretX <> Value.Char then
      begin
        fCaretX := Value.Char;
        Include(fStatusChanges, scCaretX);
      end;
      if fCaretY <> Value.Line then
      begin

        { Support for adaptive active line background }
        if (eoAdaptiveSpecialLine in Options) or (ActiveLineBG <> clNone) then
        begin
          InvalidateLine(Value.Line); // Repaints line which will become current
          InvalidateLine(fCaretY);    // Repaints old line
          InvalidateGutterLine(Value.Line);
          InvalidateGutterLine(fCaretY);
        end;

        fCaretY := Value.Line;
        Include(fStatusChanges, scCaretY);
      end;

      { Call UpdateLastCaretX before DecPaintLock because the event handler it
        calls could raise an exception, and we don't want fLastCaretX to be
        left in an undefined state if that happens }
      UpdateLastCaretX;
      if CallEnsureCursorPos then
        EnsureCursorPosVisible;
      Include(fStateFlags, sfCaretChanged);
      Include(fStateFlags, sfScrollbarChanged);
    finally
      DecPaintLock;
    end;
  end
  else begin
    { Also call UpdateLastCaretX if the caret didn't move. Apps don't know
      anything about fLastCaretX and they shouldn't need to. So, to avoid any
      unwanted surprises, always update fLastCaretX whenever CaretXY is
      assigned to.
      Note to SynEdit developers: If this is undesirable in some obscure
      case, just save the value of fLastCaretX before assigning to CaretXY and
      restore it afterward as appropriate. }
    UpdateLastCaretX;
  end;
  if not fSmartCaretsUpdating and not fSelections then
    CancelSnippet(True);

  if vTriggerPaint then
    DoOnPaintTransient(ttAfter);
end;

function TCustomSynEdit.CaretInView: Boolean;
var
  vCaretRowCol: TDisplayCoord;
begin
  vCaretRowCol := DisplayXY;
  Result := (vCaretRowCol.Column >= LeftChar)
    and (vCaretRowCol.Column <= LeftChar + CharsInWindow)
    and (vCaretRowCol.Row >= TopLine)
    and (vCaretRowCol.Row <= TopLine + LinesInWindow);
end;

procedure TCustomSynEdit.SetActiveLineBG(Value: TColor);
begin
  if (fActiveLineBG <> Value) then
  begin
    fActiveLineBG := Value;
    InvalidateLine(CaretY);
  end;
end;

procedure TCustomSynEdit.SetActiveLineFG(Value: TColor);
begin
  if (fActiveLineFG <> Value) then
  begin
    fActiveLineFG := Value;
    InvalidateLine(CaretY);
  end;
end;

procedure TCustomSynEdit.SetFont(const Value: TFont);
var
  DC: HDC;
  Save: THandle;
  Metrics: TTextMetric;
  AveCW, MaxCW: Integer;
begin
  DC := GetDC(0);
  Save := SelectObject(DC, Value.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, Save);
  ReleaseDC(0, DC);
  with Metrics do
  begin
    AveCW := tmAveCharWidth;
    MaxCW := tmMaxCharWidth;
  end;
  case AveCW = MaxCW of
    True: inherited Font := Value;
    False:
      begin
        with fFontDummy do
        begin
          Color := Value.Color;
          Pitch := fpFixed;
          Size := Value.Size;
          Style := Value.Style;
          Name := Value.Name;
        end;
        inherited Font := fFontDummy;
      end;
  end;
  TSynEditStringList(fLines).FontChanged;
  if fGutter.ShowLineNumbers then
    GutterChanged(Self);
end;

procedure TCustomSynEdit.SetGutterWidth(Value: Integer);
begin
  if Gutter.DigitCount <> Value then
  begin
    Gutter.DigitCount := Value;
    if eoWrapCentered in fOptions then
      fCenterOffset := Max((ClientWidth - 3 - (fGutter.Width * Ord(Gutter.Visible)) - (fCharWidth * fRightEdge)), 0) shr 1
    else
      fCenterOffset := 0;
    fTextOffset := fCenterOffset + fGutter.Width + 2 - (fLeftChar - 1) * fCharWidth;
    UpdateCharsInWindow;
  end;
end;

// -----------------------------------------------------------------------------
// § Garnet
procedure TCustomSynEdit.SetLeftChar(Value: Integer);
var
  MaxVal: Integer;
  iDelta: Integer;
  iTextArea: TRect;
begin
  if WordWrap then
    Value := 1;

  if eoScrollPastEol in Options then
  begin
    if eoAutoSizeMaxScrollWidth in Options then
      MaxVal := MaxInt - CharsInWindow
    else
      MaxVal := MaxScrollWidth - CharsInWindow + 1
  end
  else begin
    MaxVal := ExpandLines.GetLengthOfLongestLine(RowToLine(fTopLine), RowToLine(fTopLine + fLinesInWindow));
    if MaxVal > CharsInWindow then
      MaxVal := MaxVal - CharsInWindow + 1
    else
      MaxVal := 1;
  end;

  Value := MinMax(Value, 1, MaxVal);
  if Value <> fLeftChar then
  begin
    iDelta := fLeftChar - Value;
    fLeftChar := Value;
    if eoWrapCentered in fOptions then
      fCenterOffset := Max((ClientWidth - 3 - (fGutter.Width * Ord(Gutter.Visible)) - (fCharWidth * fRightEdge)), 0) shr 1
    else
      fCenterOffset := 0;
    fTextOffset := fCenterOffset + fGutter.Width + 2 - (LeftChar - 1) * fCharWidth;
    if Abs(iDelta) < CharsInWindow then
    begin
      iTextArea := ClientRect;
      Inc(iTextArea.Left, Gutter.Width + 2);
      ScrollWindow(Handle, iDelta * CharWidth, 0, @iTextArea, @iTextArea);
    end
    else
      InvalidateLines(-1, -1);
    if (Options >= [eoAutoSizeMaxScrollWidth, eoScrollPastEol]) and
      (MaxScrollWidth < LeftChar + CharsInWindow) then
    begin
      MaxScrollWidth := LeftChar + CharsInWindow
    end
    else
      UpdateScrollBars;
    StatusChanged([scLeftChar]);
  end;
end;

procedure TCustomSynEdit.SetLines(Value: TSynEditStringList{TUnicodeStrings});
begin
  Lines.Assign(Value);
end;

procedure TCustomSynEdit.SetLineText(Value: UnicodeString);
begin
  if (CaretY >= 1) and (CaretY <= Max(1, Lines.Count)) then
    Lines[CaretY - 1] := Value;
end;

procedure TCustomSynEdit.SetName(const Value: TComponentName);
var
  TextToName: Boolean;
begin
  TextToName := (ComponentState * [csDesigning, csLoading] = [csDesigning])
    and (TrimRight(Text) = Name);
  inherited SetName(Value);
  if TextToName then
    Text := Value;
end;

procedure TCustomSynEdit.SetScrollBars(const Value: TScrollStyle);
begin
  if (FScrollBars <> Value) then
  begin
    FScrollBars := Value;
    UpdateScrollBars;
    Invalidate;
  end;
end;

procedure TCustomSynEdit.SetSelTextPrimitive(const Value: UnicodeString);
begin
  SetSelTextPrimitiveEx(fActiveSelectionMode, PWideChar(Value), True);
end;

// -----------------------------------------------------------------------------
// § Garnet:
// 1. eoTrimTrailingSpaces notice: trimming should only occur when navigating
//    in file (changing caret line pos) and when pasting from clipboard.
//    We can't do trimming while editing because under some circumstances it
//    would corrupt undo flow.
// 2. fCodeFolding notice: All line operations in smColumn mode must be done
//    only after expanding affected ranges.
procedure TCustomSynEdit.SetSelTextPrimitiveEx(PasteMode: TSynSelectionMode;
  Value: PChar; AddToUndoList: Boolean; CanTrim: Boolean = False;
  ChangeLines: Boolean = True);
var
  BB, BE: TBufferCoord;
  TempString: UnicodeString;
  Caret: TBufferCoord;
  DX, DY: Integer;

  { For smColumn. Returns true if a row specified is actually a wrapped row of
    some line }
  function IsWrappedRow(ALine, ARow: Integer): Boolean;
  begin
    if WordWrap then
      Result := fWordWrapPlugin.LineToRealRow(ALine) <> ARow
    else
      Result := False;
  end;

  { Clears selection depending on selection mode }
  procedure DeleteSelection;
  var
    X,          // Counter
    MarkOffset, // Gutter marks delta
    First,      // Starting row
    Last,       // Ending row
    vLine,      // Line corresponding to that row
    DeletePos,
    DisplayDeletePos,
    DeletePosEnd,
    DisplayDeletePosEnd: Integer;
    UpdateMarks: Boolean;
  begin
    UpdateMarks := False;
    MarkOffset := 0;
    case fActiveSelectionMode of
      smNormal:
        begin
          if fLines.Count > 0 then
          begin
            { Create a string that contains everything on the first line up
              to the selection mark, and everything on the last line after
              the selection mark }
            TempString := Copy(Lines[BB.Line - 1], 1, BB.Char - 1) +
              Copy(Lines[BE.Line - 1], BE.Char, MaxInt);

            { Delete all lines in the selection range }
            ExpandLines.DeleteLines(BB.Line, Min(BE.Line - BB.Line, fLines.Count - BB.Line));

            { Put the stuff that was outside of selection back in }
            ExpandLines[BB.Line - 1] := TempString;
            if ChangeLines then
              ExpandLines.ModifiedLines[BB.Line - 1] := True;
          end;
          UpdateMarks := True;
          InternalCaretXY := BB;
        end;

      { The most difficult part }
      smColumn:
        begin
          { Swap caret if needed }
          if BB.Char > BE.Char then
            SwapInt(BB.Char, BE.Char);

          { Get deletion constraints }
          with BufferToRealDisplayPos(BB) do
          begin
            First := Row;
            DisplayDeletePos := Column;
          end;
          with BufferToRealDisplayPos(BE) do
          begin
            Last := Row;
            DisplayDeletePosEnd := Column;
          end;

          { Do expand affected lines }
          ExpandCollapsedLines(BB.Line, BE.Line);

          { Deletion loop }
          for X := First to Last do
          begin
            with RealDisplayToBufferPos(DisplayCoord(DisplayDeletePos, X)) do
            begin
              DeletePos := Char;
              vLine := Line;
            end;
            ExpandCollapsedLine(vLine);

            DeletePosEnd := RealDisplayToBufferPos(DisplayCoord(DisplayDeletePosEnd, X)).Char;
            TempString := fLines.fList[vLine - 1].fString;
            Delete(TempString, DeletePos, DeletePosEnd - DeletePos);
            ExpandLines[vLine - 1] := TempString;
            if ChangeLines then
              ExpandLines.ModifiedLines[vLine - 1] := True;
          end;

          { Lines never get deleted completely, so keep caret at end }
          InternalCaretXY := BufferCoord(BB.Char, fBlockEnd.Line);

          { Column deletion never removes a line entirely, so no mark
            updating is needed here }
        end;
      smLine:
        begin
          ExpandLines.DeleteLines(Pred(BB.Line), (BE.Line - BB.Line) + 1);

          { smLine deletion always resets to first column }
          InternalCaretXY := BufferCoord(1, BB.Line);
          UpdateMarks := True;
          MarkOffset := 0;
        end;
    end;

    { Update marks }
    if UpdateMarks then
      DoLinesDeleted(BB.Line, BE.Line - BB.Line + MarkOffset);
  end;

  procedure InsertText;

    { How much there are EOLs in text being inserted }
    function CountLines(P: PChar): Integer;
    begin
      Result := 0;
      while P^ <> #0 do
      begin
        if P^ = #13 then
          Inc(P);
        if P^ = #10 then
          Inc(P);
        Inc(Result);
        P := GetEOL(P);
      end;
    end;

    { Insert normally (smNormal selection mode) }
    function InsertNormal: Integer;
    var
      sLeftSide: UnicodeString;
      sRightSide: UnicodeString;
      Str, White: UnicodeString;
      Start: PChar;
      P: PChar;
      bIndented: Boolean;
      nLastLineLen: Integer;
    begin
      Result := 0;
      sLeftSide := Copy(LineText, 1, CaretX - 1);
      if CaretX - 1 > Length(sLeftSide) then
      begin
        sLeftSide := sLeftSide + UnicodeString(StringOfChar(#32,
          CaretX - 1 - Length(sLeftSide)));
      end;
      sRightSide := Copy(LineText, CaretX, Length(LineText) - (CaretX - 1));

      bIndented := False;
      if (fLines.Count > 0) and (eoAutoIndent in fOptions) then
        White := GetLeadingWhite(fLines.fList^[Pred(fCaretY)].fString)
      else
        White := '';

      { Step1: insert the first line of Value into current line }
      Start := PWideChar(Value);
      P := GetEOL(Start);
      nLastLineLen := P - Start;
      if P^ <> #0 then
      begin
        Str := sLeftSide + Copy(Value, 1, P - Start);
        ExpandLines[fCaretY - 1] := Str;
        if ChangeLines then
          ExpandLines.ModifiedLines[fCaretY - 1] := True;
        ExpandLines.InsertLines(fCaretY, CountLines(P));
      end
      else begin
        Str := sLeftSide + Value + sRightSide;
        ExpandLines[fCaretY - 1] := Str;
        if ChangeLines then
          ExpandLines.ModifiedLines[fCaretY - 1] := True;
      end;

      { Step2: insert left lines of Value }
      while P^ <> #0 do
      begin
        if P^ = #13 then
          Inc(P);
        if P^ = #10 then
          Inc(P);
        Inc(fCaretY);
        Include(fStatusChanges, scCaretY);
        Start := P;
        P := GetEOL(Start);
        nLastLineLen := P - Start;
        if P = Start then
        begin
          if p^ <> #0 then
            Str := ''
          else
            Str := sRightSide;
        end
        else begin
          SetString(Str, Start, P - Start);
          if p^ = #0 then
            Str := Str + sRightSide
        end;

        if (not IsStringAllWhite(Str) and (GetLeadingExpandedLength(Str, fTabWidth) < GetLeadingExpandedLength(White, fTabWidth))) or bIndented then
        begin
          ExpandLines[fCaretY - 1] := White + Str;
          Inc(nLastLineLen, Length(White));
          bIndented := True;
        end
        else
          ExpandLines[fCaretY - 1] := Str;

        if ChangeLines then
          ExpandLines.ModifiedLines[fCaretY - 1] := True;
        Inc(Result);
      end;
      fCaretX := 1 + Length(ExpandLines[CaretY - 1]) - Length(sRightSide);
      StatusChanged([scCaretX]);

      { Mirror }
      InsertedInSnippet(BB.Char, BB.Line, nLastLineLen, Result);
    end;

    { Insert columnar way (smColumn selecton mode) }
    function InsertColumn: Integer;
    var
      Str: UnicodeString;  // Portion of string to insert on that line
                           // (Used when in normal mode)
      Start: PChar;        // Looper
      P: PChar;            // Runs through the string
      Len,                 // Plays various roles
      jFirst,              // Starting row
      jLine,               // Line associated with that row
      jInsertPos,          // Position in line where we should insert on that row
      jDisplayInsertPos: Integer; // Visual position where we are inserting
      LineBreakPos: TBufferCoord; // For undo, if new lines are added
    begin
      { No new lines added }
      Result := 0;

      { Get display position of insertion point }
      with BufferToRealDisplayPos(CaretXY) do
      begin
        jFirst := Row;
        jDisplayInsertPos := Column;
      end;
      jLine := fCaretY;

      { Do insert text in portions }
      Start := PChar(Value);
      repeat
        { Here we get correct position for insertion on that line }
        with RealDisplayToBufferPos(DisplayCoord(jDisplayInsertPos, jFirst)) do
        begin
          jInsertPos := Char;
          jLine := fCaretY;
        end;
        if IsWrappedRow(jLine, jFirst) then
        begin
          Inc(jFirst);
          Continue;
        end;

        { Get portion of text for that line }
        P := GetEOL(Start);
        if P <> Start then
        begin
          SetLength(Str, P - Start);
          Move(Start^, Str[1], (P - Start) * SizeOf(Char));

          { Stubled upon EOF, need to add lines to insert remaining text }
          if jLine > fLines.Count then
          begin
            Inc(Result);

            { Add whitespace only if there's really something to insert }
            if P - Start > 0 then
            begin
              Len := jInsertPos - 1;
              if eoTabsToSpaces in fOptions then
                TempString := UnicodeString(StringOfChar(#32, Len))
              else
                TempString := UnicodeString(StringOfChar(#9, Len div fTabWidth) +
                  StringOfChar(#32, Len mod fTabWidth));
              Len := Length(TempString);
              TempString := TempString + Str;
            end
            else begin
              Len := 0;
              TempString := '';
            end;

            { Add line }
            ExpandLines.Add('');

            { Reflect our changes in undo list }
            if AddToUndoList then
            begin
              with LineBreakPos do
              begin
                Line := jLine - 1;
                Char := Length(Lines[jLine - 2]) + 1;
              end;
              fUndoList.AddChange(
                crLineBreak,
                LineBreakPos,
                LineBreakPos,
                fCurrCaret,
                '', smNormal
              );
            end;
          end
          else begin
            TempString := ExpandLines[jLine - 1];
            Len := Length(TempString);
            if (Len < jInsertPos) and (P - Start > 0) then
            begin
              TempString :=
                TempString + UnicodeString(StringOfChar(#32, jInsertPos - Len - 1)) + Str;
              fUndoList.AddChange(
                crWhiteSpaceAdd,
                BufferCoord(Len + 1, jLine),
                BufferCoord(jInsertPos, jLine),
                fCurrCaret,
                '', smNormal
              );
            end
            else
              Insert(Str, TempString, jInsertPos);
          end;
          ExpandLines[jLine - 1] := TempString;

          { Add undo change here from PasteFromClipboard }
          if AddToUndoList then
          begin
            fUndoList.AddChange(crPaste, BufferCoord(jInsertPos, jLine),
              BufferCoord(jInsertPos + Length(Str), jLine), fCurrCaret, '',
              smNormal, ExpandLines.GetLineStates(jLine - 1, jLine - 1));
            if ChangeLines then
              ExpandLines.ModifiedLines[jLine - 1] := True;
          end;
        end;

        { New insert portion }
        if P^ = #13 then
        begin
          Inc(P);
          if P^ = #10 then
            Inc(P);
          Inc(jFirst);
          Inc(fCaretY);
          Include(fStatusChanges, scCaretY);
        end;
        Start := P;
      until
        P^ = #0;
      Inc(fCaretX, Length(Str));
      Include(fStatusChanges, scCaretX);
    end;

    { Insert in smLine selection mode. All new lines will be pasted before
      current CaretY }
    function InsertLine: Integer;
    var
      Start: PChar;
      P: PChar;
      Str: UnicodeString;
      vAfter, vReplace, vCaretFix: Boolean;
    begin
      Result := 0;

      { Ensure list isn't empty }
      if fLines.Count = 0 then
        fLines.Add('');

      { Decide, how to insert lines. If fCaretX is after current EOL, then
        user wants them after current Caret Y. Otherwise, he wants them before
        current caret Y }
      if fCaretX = 1 then
        vAfter := False;
      if vAfter then
        vAfter := fCaretX > ExpandLines.AccessStringLength(fCaretY - 1);
      vReplace := ExpandLines.AccessStringLength(fCaretY - 1) = 0;
      vCaretFix := False;

      { Insert strings }
      fCaretX := 1;
      Start := PChar(Value);
      repeat
        P := GetEOL(Start);
        if P <> Start then
        begin
          SetLength(Str, P - Start);
          Move(Start^, Str[1], (P - Start) * SizeOf(Char));
        end
        else
          Str := '';

        if vReplace then
        begin
          vReplace := False;
          ExpandLines[fCaretY - 1] := Str;
          vCaretFix := True;
        end
        else begin
          ExpandLines.Insert(fCaretY - 1 + Ord(vAfter), Str);
          Inc(Result);
        end;
        if ChangeLines then
          ExpandLines.ModifiedLines[fCaretY - 1] := True;

        Inc(fCaretY);

        if P^ = #13 then
          Inc(P);
        if P^ = #10 then
          Inc(P);
        Start := P;
      until
        P^ = #0;
      if Result > 0 then
        Include(fStatusChanges, scCaretY);
      if vCaretFix then
        fCaretX := Length(Str) + 1;
      StatusChanged([scCaretX]);
    end;

  var
    I, StartLine: Integer;
    StartCol: Integer;
    InsertedLines: Integer;
  begin
    if Length(Value) = 0 then
      Exit;

    StartLine := CaretY;
    StartCol := CaretX;
    case PasteMode of
      smNormal:
        InsertedLines := InsertNormal;
      smColumn:
        InsertedLines := InsertColumn;
      smLine:
        InsertedLines := InsertLine;
    else
      InsertedLines := 0;
    end;

    { We delete selected based on the current selection mode, but paste
      what's on the clipboard according to what it was when copied.
      Update marks }
    if InsertedLines > 0 then
    begin
      if ((PasteMode = smNormal) and (StartCol > 1)) or
        ((PasteMode = smLine) and (StartCol > 1))
      then
        Inc(StartLine);

      { Trim trailing spaces }
      if CanTrim then
        for I := StartLine to StartLine + InsertedLines do
          DoTrimTrailingSpaces(I);

      DoLinesInserted(StartLine, InsertedLines);
    end;

    { Force caret reset }
    InternalCaretXY := CaretXY;
    BlockBegin := CaretXY;
    BlockEnd := CaretXY;
  end;

begin
  { Mirror }
  Caret := CaretXY;

  IncPaintLock;
  Lines.BeginUpdate;
  try
    BB := BlockBegin;
    BE := BlockEnd;
    if SelAvail then
    begin
      DeleteSelection;
      InternalCaretXY := BB;
      BlockBegin := BB;
      BlockEnd := BB;

      { Mirror }
      DY := BE.Line - BB.Line;
      if DY > 0 then
        DX := BB.Char - 1
      else
        DX := BB.Char - BE.Char;
      InsertedInSnippet(BE.Char, BE.Line, DX, -DY);
    end
    else
      ExpandCollapsedLine(fCaretY);
    if (Value <> nil) and (Value[0] <> #0) then
      InsertText;
    if CaretY < 1 then
      InternalCaretY := 1;
  finally
    Lines.EndUpdate;
    DecPaintLock;
  end;

  { Mirror }
  {if not fUndoRedo then
    MirrorCommand(ecNone, #0, nil, Caret, BB, BE, Value);}
end;

procedure TCustomSynEdit.SynSetText(const Value: UnicodeString);
begin
  Lines.Text := Value;
end;

procedure TCustomSynEdit.SetTopLine(Value: Integer);
var
  Delta: Integer;
begin
  if (eoScrollPastEof in Options) then
    Value := Min(Value, DisplayLineCount)
  else
    Value := Min(Value, DisplayLineCount - fLinesInWindow + 1);
  Value := Max(Value, 1);
  if Value <> TopLine then
  begin
    Delta := TopLine - Value;
    fTopLine := Value;

    { Auto-size gutter width }
    SetGutterWidth(Length(IntToStr(RowToLine(fTopLine + fLinesInWindow))));

    if Abs(Delta) < fLinesInWindow then
      if fRepaintAfterDimNeeded then
      begin
        fRepaintAfterDimNeeded := False;
        Invalidate;
      end
      else
        ScrollWindow(Handle, 0, fTextHeight * Delta, nil, nil)
    else
      Invalidate;
    UpdateScrollBars;
    StatusChanged([scTopLine]);
  end;
end;

procedure TCustomSynEdit.ShowCaret;
begin
  { When multiple selections, the caret is always hidden }
  if fSelections then
    Exit;

  if not (sfCaretVisible in fStateFlags) then
  begin
    if Windows.ShowCaret(Handle) then
      Include(fStateFlags, sfCaretVisible);
  end;
end;

// -----------------------------------------------------------------------------
// § Garnet
procedure TCustomSynEdit.UpdateCaret;
var
  CX, CY, CW: Integer;
  iClientRect: TRect;
  vCaretDisplay: TDisplayCoord;
  vCaretPix: TPoint;
  CF: TCompositionForm;
  C: Char;
begin
  { When multiple selections, the caret is always hidden }
  if fSelections then
    Exit;

  if (PaintLock <> 0) or not (Focused or FAlwaysShowCaret) then
    Include(fStateFlags, sfCaretChanged)
  else
  begin
    Exclude(fStateFlags, sfCaretChanged);
    vCaretDisplay := DisplayXY;
    if WordWrap and (vCaretDisplay.Column > CharsInWindow + 1) then
      vCaretDisplay.Column := fCharsInWindow + 1;
    vCaretPix := RowColumnToPixels(vCaretDisplay);
    CX := vCaretPix.X + FCaretOffset.X;
    CY := vCaretPix.Y + FCaretOffset.Y;
    iClientRect := GetClientRect;
    Inc(iClientRect.Left, Gutter.Width);
    if (CX >= iClientRect.Left) and (CX < iClientRect.Right)
      and (CY >= iClientRect.Top) and (CY < iClientRect.Bottom) then
    begin
      if ExpandLines.AccessStringLength(fCaretY - 1) >= fCaretX then
      begin
        C := ExpandLines.AccessBuffer(fCaretY - 1)[fCaretX - 1];
        if C = #9 then
          if fCaretX = 1 then
            CW := fTabWidth
          else
            CW := fTabWidth - GetExpandedLengthUpTo(ExpandLines[fCaretY-1], fTabWidth, fCaretX - 1) mod fTabWidth
        else
          CW := CharWidthTable(C);
      end
      else
        CW := 1;
      if CW <> fCurrCharWidth then
      begin
        fCurrCharWidth := Max(CW, 1);
        InitializeCaret(False);
      end;
      SetCaretPos(CX, CY);
      ShowCaret;
    end
    else
    begin
      SetCaretPos(CX, CY);
      HideCaret;
    end;
    CF.dwStyle := CFS_POINT;
    CF.ptCurrentPos := Point(CX, CY);
    ImmSetCompositionWindow(ImmGetContext(Handle), @CF);
  end;
end;

// -----------------------------------------------------------------------------
// § Garnet
procedure TCustomSynEdit.UpdateScrollBars;
var
  nMaxScroll: Integer;
  ScrollInfo: TScrollInfo;
begin
  if not HandleAllocated or (PaintLock <> 0) then
    Include(fStateFlags, sfScrollbarChanged)
  else begin
    Exclude(fStateFlags, sfScrollbarChanged);
    if fScrollBars <> ssNone then
    begin
      ScrollInfo.cbSize := SizeOf(ScrollInfo);
      ScrollInfo.fMask := SIF_ALL;
      if not (eoHideShowScrollbars in Options) then
        ScrollInfo.fMask := ScrollInfo.fMask or SIF_DISABLENOSCROLL;

      { Horizontal }
      if (fScrollBars in [ssBoth, ssHorizontal]) and not WordWrap then
      begin
        if eoScrollPastEol in Options then
          nMaxScroll := MaxScrollWidth
        else
          nMaxScroll := Max(fLines.GetLengthOfLongestLine(RowToLine(fTopLine),
            RowToLine(fTopLine + fLinesInWindow)), 1);
        if nMaxScroll <= MAX_SCROLL then
        begin
          ScrollInfo.nMin := 1;
          ScrollInfo.nMax := nMaxScroll;
          ScrollInfo.nPage := CharsInWindow;
          ScrollInfo.nPos := LeftChar;
        end
        else begin
          ScrollInfo.nMin := 0;
          ScrollInfo.nMax := MAX_SCROLL;
          ScrollInfo.nPage := MulDiv(MAX_SCROLL, CharsInWindow, nMaxScroll);
          ScrollInfo.nPos := MulDiv(MAX_SCROLL, LeftChar, nMaxScroll);
        end;

        ShowScrollBar(Handle, SB_HORZ, not (eoHideShowScrollbars in Options) or
          (ScrollInfo.nMin = 0) or (ScrollInfo.nMax > CharsInWindow));
        SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);

        EnableScrollBar(Handle, SB_HORZ, ESB_ENABLE_BOTH);
      end
      else
        ShowScrollBar(Handle, SB_HORZ, False);

      { Vertical }
      if fScrollBars in [ssBoth, ssVertical] then
      begin
        nMaxScroll := DisplayLineCount;
        if (eoScrollPastEof in Options) then
          Inc(nMaxScroll, LinesInWindow - 1);
        if nMaxScroll <= MAX_SCROLL then
        begin
          ScrollInfo.nMin := 1;
          ScrollInfo.nMax := Max(1, nMaxScroll);
          ScrollInfo.nPage := LinesInWindow;
          ScrollInfo.nPos := TopLine;
        end
        else begin
          ScrollInfo.nMin := 0;
          ScrollInfo.nMax := MAX_SCROLL;
          ScrollInfo.nPage := MulDiv(MAX_SCROLL, LinesInWindow, nMaxScroll);
          ScrollInfo.nPos := MulDiv(MAX_SCROLL, TopLine, nMaxScroll);
        end;

        ShowScrollBar(Handle, SB_VERT, not(eoHideShowScrollbars in Options) or
          (ScrollInfo.nMin = 0) or (ScrollInfo.nMax > LinesInWindow));
        SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);

        EnableScrollBar(Handle, SB_VERT, ESB_ENABLE_BOTH);
      end
      else
        ShowScrollBar(Handle, SB_VERT, False);
    end {endif fScrollBars <> ssNone}
    else
      ShowScrollBar(Handle, SB_BOTH, False);
  end;
end;

function TCustomSynEdit.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
const
  WHEEL_DIVISOR = 120; // Mouse Wheel standard
var
  iWheelClicks: Integer;
  iLinesToScroll: Integer;
begin
  if GetKeyState(VK_CONTROL) < 0 then
    iLinesToScroll := LinesInWindow shr Ord(eoHalfPageScroll in fOptions)
  else
    iLinesToScroll := 3;
  Inc(fMouseWheelAccumulator, WheelDelta);
  iWheelClicks := fMouseWheelAccumulator div WHEEL_DIVISOR;
  fMouseWheelAccumulator := fMouseWheelAccumulator mod WHEEL_DIVISOR;
  TopLine := TopLine - iWheelClicks * iLinesToScroll;
  Update;
  if Assigned(OnScroll) then OnScroll(Self,sbVertical);
  Result := True;
end;

// -----------------------------------------------------------------------------
// Parses line ranges after changes (if it's necessary)
function TCustomSynEdit.ScanFrom(Index: Integer): Integer;
var
  iRange: TSynEditRange;

  { Inform about newely parsed lines }
  procedure DoLinesRecognized;
  begin
    if (Result - Index) > -1 then
      if Assigned(fOnLinesRecognized) then
        fOnLinesRecognized(Self, Index, Max(Result - Index, 1));
  end;

  function ContainsBackreference(const Index: Integer): Boolean;
  var
    Cache: PSynUniCacheItem;
  begin
    Cache := (fHighlighter as TSynUniSyn).Cache[Index];
    Result := Length(Cache^.ACaptureMap^) > 0;
  end;

begin
  { Initialize }
  Result := Index;
  if Result >= Lines.Count then
    Exit;

  { Choose action }
  if Result = 0 then
    fHighlighter.ResetRange
  else
    fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[Result - 1]);

  { Parse again }
  repeat

    { Process line up to EOL to find if range changed }
    fHighlighter.SetLine(Lines[Result], Result, True);
    fHighlighter.NextToEol;

    { Get new range }
    iRange := fHighlighter.GetRange;

    { Remained the same? }
    if (TSynEditStringList(Lines).Ranges[Result] = iRange) and
      not ContainsBackreference(Result) then
    begin
      DoLinesRecognized;
      Exit; // Avoid the final decrement
    end;

    { Assign new range and proceed to the next line }
    TSynEditStringList(Lines).Ranges[Result] := iRange;
    Inc(Result);
  until
    Result = Lines.Count;

  { Done }
  Dec(Result);
  DoLinesRecognized;
end;

procedure TCustomSynEdit.ListCleared(Sender: TObject);
begin
  if WordWrap then
    fWordWrapPlugin.Reset;

  ClearUndo;
  // invalidate the *whole* client area
  FillChar(fInvalidateRect, SizeOf(TRect), 0);
  Invalidate;
  // set caret and selected block to start of text
  CaretXY := BufferCoord(1, 1);
  // scroll to start of text
  TopLine := 1;
  LeftChar := 1;
  Include(fStatusChanges, scAll);
end;

procedure TCustomSynEdit.ListDeleted(Sender: TObject; aIndex: Integer;
  aCount: Integer);
var
  aWas: Integer;
  Runner: Integer;
begin
  if Assigned(fHighlighter) then
  begin
    aWas := aIndex;
    aIndex := Max(aIndex, 1);
    fHighlighter.LinesDeleted(AIndex, ACount);
    if fLines.Count > 0 then
    begin
      Runner := ScanFrom(Pred(aIndex));

      { This is necessary because a line can be deleted with ecDeleteChar
        command and above, what've done, is rescanned a line joined with deleted
        one. But if range on that line hadn't changed, it still could've been
        changed on lines below. In case if range on line with join had changed
        the above rescan already did the job }
      if Runner = Pred(aIndex) then
        ScanFrom(aIndex);
    end;
  end;

  if WordWrap then
    fWordWrapPlugin.LinesDeleted(aWas, aCount);

  InvalidateLines(aWas + 1, MaxInt);
  InvalidateGutterLines(aWas + 1, MaxInt);
end;

procedure TCustomSynEdit.ListInserted(Sender: TObject; Index: Integer;
  aCount: Integer);
var
  L: Integer;
  vLastScan: Integer;
begin
  if Assigned(fHighlighter) and (Lines.Count > 0) then
  begin
    fHighlighter.LinesInserted(Index, ACount);
    vLastScan := Index;
    repeat
      vLastScan := ScanFrom(vLastScan);
      Inc(vLastScan);
    until
      vLastScan >= Index + aCount;
  end;

  if WordWrap then
    fWordWrapPlugin.LinesInserted(Index, aCount);

  if Assigned(fOnLinesInserted) then
    fOnLinesInserted(Self, Index, aCount);

  InvalidateLines(Index + 1, MaxInt);
  InvalidateGutterLines(Index + 1, MaxInt);

  if (eoAutoSizeMaxScrollWidth in fOptions) then
  begin
    L := TSynEditStringList(Lines).ExpandedStringLengths[Index];
    if L > MaxScrollWidth then
      MaxScrollWidth := L;
  end;
end;

procedure TCustomSynEdit.ListBeforePutted(Sender: TObject; Index: Integer;
  ACount: Integer);
begin
  { Do nothing }
end;

procedure TCustomSynEdit.ListPutted(Sender: TObject; Index: Integer;
  aCount: Integer);
var
  L: Integer;
  vEndLine: Integer;
begin
  vEndLine := Min(Index + 1, fLines.Count);
  if Assigned(fHighlighter) then
  begin
    fHighlighter.LinesPutted(Index, ACount);

    vEndLine := Max(vEndLine, ScanFrom(Index) + 1);
    { If this editor is chained then the real owner of text buffer will probably
      have already parsed the changes, so ScanFrom will return immediately }
    if fLines <> fOrigLines then
      vEndLine := MaxInt;
  end;
  if WordWrap then
  begin
    if fWordWrapPlugin.LinesPutted(Index, aCount) <> 0 then
      vEndLine := MaxInt;
    InvalidateGutterLines(Index + 1, vEndLine);
  end;
  InvalidateLines(Index + 1, vEndLine);

  if Assigned(fOnLinesPutted) then
    fOnLinesPutted(Self, Index, aCount);

  if (eoAutoSizeMaxScrollWidth in fOptions) then
  begin
    L := TSynEditStringList(Lines).ExpandedStringLengths[Index];
    if L > MaxScrollWidth then
      MaxScrollWidth := L;
  end;
end;

procedure TCustomSynEdit.ScanRanges;
var
  i: Integer;
begin
  if Assigned(fHighlighter) and (Lines.Count > 0) then begin
    fHighlighter.ResetRange;
    i := 0;
    repeat
      fHighlighter.SetLine(Lines[i], i, True);
      fHighlighter.NextToEol;
      fLines.Ranges[i] := fHighlighter.GetRange;
      Inc(i);
    until i >= Lines.Count;
  end;
end;

procedure TCustomSynEdit.SetWordBlock(Value: TBufferCoord);
var
  vBlockCaret: TBufferCoord;
  vBlockBegin: TBufferCoord;
  vBlockEnd: TBufferCoord;
  sLine: UnicodeString;

  procedure CharScan;

    function CheckSeparator(Value: TBufferCoord): Boolean;
    var
      I: Integer;
    begin
      Result := False;
      if fHighlighter.GetTokenNature = tknnSeparator then
      begin
        for I := Pred(Value.Char) downto fHighlighter.GetTokenPos do
          if not SynEditTypes.IsWordBreakChar(sLine[Succ(I)]) then
            vBlockBegin.Char := Succ(I)
          else
            Break;
        for I := Pred(Value.Char) to fHighlighter.GetTokenPos + fHighlighter.GetTokenLen do
          if not SynEditTypes.IsWordBreakChar(sLine[Succ(I)]) then
            vBlockEnd.Char := Succ(I) + 1
          else
            Break;
        if not CaretsEqual(vBlockBegin, Value) or not CaretsEqual(vBlockEnd, Value) then
          Result := True;
      end;
    end;

  begin
    vBlockCaret := Value;
    vBlockBegin := Value;
    vBlockEnd := Value;
    sLine := fLines.fList^[Pred(Value.Line)].fString;
    GetTokenKind(Self, Value);
    if CheckSeparator(Value) then Exit;
    if not (fHighlighter.GetTokenNature in [tknnIdent, tknnNumber]) then
      GetTokenKind(Self, BufferCoord(Pred(Value.Char), Value.Line));
    if CheckSeparator(BufferCoord(Pred(Value.Char), Value.Line)) then Exit;
    if (fHighlighter.GetTokenNature in [tknnIdent, tknnNumber]) then
    begin
      vBlockBegin.Char := Succ(fHighlighter.GetTokenPos);
      vBlockEnd.Char := vBlockBegin.Char + fHighlighter.GetTokenLen;
    end;
    if not (CaretsEqual(vBlockBegin, vBlockCaret) and CaretsEqual(vBlockEnd, vBlockCaret)) then
    begin
      if vBlockCaret.Char - vBlockBegin.Char >= vBlockEnd.Char - vBlockCaret.Char then
        vBlockCaret.Char := vBlockEnd.Char
      else
        vBlockCaret.Char := vBlockBegin.Char;
    end;
  end;

begin
  { Get current caret pos }
  if (eoScrollPastEol in Options) and not WordWrap then
    Value.Char := MinMax(Value.Char, 1, fMaxScrollWidth + 1)
  else
    Value.Char := Max(Value.Char, 1);
  Value.Line := MinMax(Value.Line, 1, Lines.Count);

  { Caret exeeds line length }
  if Value.Char > Succ(fLines.AccessStringLength(Pred(Value.Line))) then
  begin
    InternalCaretXY := BufferCoord(Succ(fLines.AccessStringLength(Pred(Value.Line))), Value.Line);
    Exit;
  end;

  { Obtain word bounds if a token on caret is word }
  CharScan;

  { Select }
  vBlockBegin.Line := Value.Line;
  vBlockEnd.Line := Value.Line;
  SetCaretAndSelection(vBlockCaret, vBlockBegin, vBlockEnd);
  InvalidateLine(Value.Line);
  StatusChanged([scSelection]);
end;

procedure TCustomSynEdit.DblClick;
var
  PtMouse: TPoint;
  P: TDisplayCoord;
  Dummy: Boolean;
begin
  { Get cursor pos in client coords }
  GetCursorPos(PtMouse);
  PtMouse := ScreenToClient(PtMouse);

  { Check if we are above folding collapsed hint }
  if fCodeFoldingHint then
  begin
    P := PixelsToNearestRowColumn(PtMouse.X, PtMouse.Y);
    SynEditToggleFoldingRange(Self, RowToLine(P.Row), fHighlighter.FoldRegions, Dummy);
    Invalidate;
    Exit;
  end;

  { Check if clicked behind gutter }
  if PtMouse.X >= Gutter.Width + 2 then
  begin
    SetWordBlock(CaretXY);
    inherited;
    Include(fStateFlags, sfDblClicked);
    MouseCapture := False;
    if not fLineSelectionTimer.Enabled then
      fLineSelectionTimer.Enabled := True;
  end
  else
    inherited;
end;

function TCustomSynEdit.GetCanUndo: Boolean;
begin
  result := not ReadOnly and fUndoList.CanUndo;
end;

function TCustomSynEdit.GetCanRedo: Boolean;
begin
  result := not ReadOnly and fRedoList.CanUndo;
end;

function TCustomSynEdit.GetCanPaste;
begin
  Result := not ReadOnly and ClipboardProvidesText;
end;

// -----------------------------------------------------------------------------
// Used by BlockIndent and Redo
procedure TCustomSynEdit.InsertBlock(const BB, BE: TBufferCoord;
  ChangeStr: PWideChar; AddToUndoList: Boolean);
begin
  SetCaretAndSelection(BB, BB, BE);
  ActiveSelectionMode := smColumn;
  SetSelTextPrimitiveEx(smColumn, ChangeStr, AddToUndoList);
  StatusChanged([scSelection]);
end;

procedure TCustomSynEdit.Redo;

  procedure RemoveGroupBreak;
  var
    Item: TSynEditUndoItem;
    OldBlockNumber: Integer;
  begin
    if fRedoList.LastChangeReason = crGroupBreak then
    begin
      OldBlockNumber := UndoList.BlockChangeNumber;
      Item := fRedoList.PopItem;
      try
        UndoList.BlockChangeNumber := Item.ChangeNumber;
        fUndoList.AddGroupBreak;
      finally
        UndoList.BlockChangeNumber := OldBlockNumber;
        Item.Free;
      end;
      UpdateModifiedStatus;
    end;
  end;

var
  Item: TSynEditUndoItem;
  OldChangeNumber: integer;
  SaveChangeNumber: integer;
  FLastChange : TSynChangeReason;
  FAutoComplete: Boolean;
  FPasteAction: Boolean;
  FSpecial1: Boolean;
  FSpecial2: Boolean;
  FKeepGoing: Boolean;
begin
  if ReadOnly then
    Exit;

  FLastChange := FRedoList.LastChangeReason;
  FAutoComplete := FLastChange = crAutoCompleteBegin;

  Item := fRedoList.PeekItem;
  if Item <> nil then
  begin
    OldChangeNumber := Item.ChangeNumber;
    SaveChangeNumber := fUndoList.BlockChangeNumber;
    fUndoList.BlockChangeNumber := Item.ChangeNumber;
    try
      repeat
        RedoItem;
        Item := fRedoList.PeekItem;
        if Item = nil then
          FKeepGoing := False
        else begin
          if FAutoComplete then
             FKeepGoing:= (FRedoList.LastChangeReason <> crAutoCompleteEnd)
          else if Item.ChangeNumber = OldChangeNumber then
             FKeepGoing := True
          else begin
            FKeepGoing := ((eoGroupUndo in FOptions) and
              (FLastChange = Item.ChangeReason) and
              not(FLastChange in [crIndent, crUnindent]));
          end;
          FLastChange := Item.ChangeReason;
        end;
      until not(FKeepGoing);

      { We need to eat the last command since it does nothing
        and also update modified status }
      if (FAutoComplete and (FRedoList.LastChangeReason = crAutoCompleteEnd)){ or
         (FPasteAction and (FRedoList.LastChangeReason = crPasteEnd)) or
         (FSpecial1 and (FRedoList.LastChangeReason = crSpecial1End)) or
         (FSpecial2 and (FRedoList.LastChangeReason = crSpecial2End))} then
      begin
        RedoItem;
        UpdateModifiedStatus;
      end;

    finally
      fUndoList.BlockChangeNumber := SaveChangeNumber;
    end;
    RemoveGroupBreak;
  end;
end;

procedure TCustomSynEdit.RedoItem;
var
  Item: TSynEditUndoItem;
  TempString: UnicodeString;
  CaretPt: TBufferCoord;
  ChangeScrollPastEol: boolean;
  I, Beginning, Ending: integer;
begin
  ChangeScrollPastEol := not (eoScrollPastEol in Options);
  Item := fRedoList.PopItem;
  if Assigned(Item) then
  try
    ActiveSelectionMode := Item.ChangeSelMode;
    IncPaintLock;
    Include(fOptions, eoScrollPastEol);
    fUndoList.InsideRedo := True;
    case Item.ChangeReason of
      crCaret:
        begin
          fUndoList.AddChange(Item.ChangeReason, CaretXY, CaretXY, fCurrCaret, '',
            fActiveSelectionMode, Item.ChangeStrStates);
          InternalCaretXY := Item.ChangeStartPos;
        end;
      crSelection:
        begin
          fUndoList.AddChange(Item.ChangeReason, BlockBegin, BlockEnd, fCurrCaret, '',
            fActiveSelectionMode, Item.ChangeStrStates);
          SetCaretAndSelection(CaretXY, Item.ChangeStartPos, Item.ChangeEndPos);
        end;
      crInsert, crPaste, crDragDropInsert:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeStartPos);

          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, Item.ChangeCaret, SelText, Item.ChangeSelMode,
            ExpandLines.GetLineStates(Item.ChangeStartPos.Line - 1, Item.ChangeEndPos.Line - 1)
          );

          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr),
            False);
          // GARNET: CHECK!
          //InternalCaretXY := Item.ChangeEndPos;

          if Item.ChangeReason = crDragDropInsert then
          begin
            SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
              Item.ChangeEndPos);
          end;

        end;
      crDelete, crSilentDelete, crDeleteAfterCursor, crSilentDeleteAfterCursor:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);

          TempString := SelText;

          fUndoList.AddChange(
            Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, Item.ChangeCaret, TempString, Item.ChangeSelMode,
            ExpandLines.GetLineStates(Item.ChangeStartPos.Line - 1,
              Item.ChangeEndPos.Line - 1));

          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr),
            False);

          // GARNET: CHECK!
          //InternalCaretXY := Item.ChangeEndPos;
        end;
      crLineBreak:
        begin
          CaretPt := Item.ChangeStartPos;
          SetCaretAndSelection(CaretPt, CaretPt, CaretPt);
          CommandProcessor(ecLineBreak, #13, nil);
        end;
      crLineInsert:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeStartPos);
          CommandProcessor(ecLineBreak, #13, nil);
        end;
      crIndent:
        begin
          SetCaretAndSelection(Item.ChangeEndPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, Item.ChangeCaret, Item.ChangeStr, Item.ChangeSelMode,
            Item.ChangeStrStates);
        end;
      crUnindent:
        begin
          { Set caret and selection }
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);

          { Re-delete again (will also fill undo item again) }
          DoBlockUnindent;
         end;
      crWhiteSpaceAdd:
        begin
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
             Item.ChangeEndPos, Item.ChangeCaret, '', Item.ChangeSelMode);
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeStartPos);
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr), False, False, False);
          InternalCaretXY := Item.ChangeStartPos;
        end;
    end;

    { Line states }
    if Length(Item.ChangeStrStates) > 0 then
    begin
      Beginning := Item.ChangeStartPos.Line;
      Ending := Item.ChangeEndPos.Line;
      if Beginning > Ending then
        SwapInt(Beginning, Ending);
      for I := Beginning to Ending do
        ExpandLines.SetLineStates(I - 1, Item.ChangeStrStates[I - Beginning]);
    end;
  finally
    fUndoList.InsideRedo := False;
    if ChangeScrollPastEol then
      Exclude(fOptions, eoScrollPastEol);
    Item.Free;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.Undo;

  procedure RemoveGroupBreak;
  var
    Item: TSynEditUndoItem;
    OldBlockNumber: Integer;
  begin
    if fUndoList.LastChangeReason = crGroupBreak then
    begin
      OldBlockNumber := RedoList.BlockChangeNumber;
      try
        Item := fUndoList.PopItem;
        RedoList.BlockChangeNumber := Item.ChangeNumber;
        Item.Free;
        fRedoList.AddGroupBreak;
      finally
        RedoList.BlockChangeNumber := OldBlockNumber;
      end;
    end;
  end;

var
  Item: TSynEditUndoItem;
  OldChangeNumber: integer;
  SaveChangeNumber: integer;
  FLastChange : TSynChangeReason;
  FAutoComplete: Boolean;
  FPasteAction: Boolean;
  FSpecial1: Boolean;
  FSpecial2: Boolean;
  FKeepGoing: Boolean;
begin
  if ReadOnly then
    Exit;

  RemoveGroupBreak;

  FLastChange := FUndoList.LastChangeReason;
  FAutoComplete := FLastChange = crAutoCompleteEnd;

  Item := fUndoList.PeekItem;
  if Item <> nil then
  begin
    OldChangeNumber := Item.ChangeNumber;
    SaveChangeNumber := fRedoList.BlockChangeNumber;
    fRedoList.BlockChangeNumber := Item.ChangeNumber;

    try
      repeat
        UndoItem;
        Item := fUndoList.PeekItem;
        if Item = nil then
          FKeepGoing := False
        else begin
          if FAutoComplete then
            FKeepGoing := (FUndoList.LastChangeReason <> crAutoCompleteBegin)
          else if Item.ChangeReason = crWhiteSpaceAdd then
            FKeepGoing := True
          else if Item.ChangeNumber = OldChangeNumber then
            FKeepGoing := True
          else begin
            FKeepGoing := ((eoGroupUndo in FOptions) and
              (FLastChange = Item.ChangeReason) and
              not (FLastChange in [crIndent, crUnindent]));
          end;
          FLastChange := Item.ChangeReason;
        end;
      until
        not FKeepGoing;

      { We need to eat the last command since it does nothing
        and also update modified status }
      if (FAutoComplete and (FUndoList.LastChangeReason = crAutoCompleteBegin)) then
      begin
        UndoItem;
        UpdateModifiedStatus;
      end;

    finally
      fRedoList.BlockChangeNumber := SaveChangeNumber;
    end;
  end;
end;

procedure TCustomSynEdit.JoinPreviousLine(const S: UnicodeString);
begin
  InsertedInSnippet(1, fCaretY + 1, fCaretX - 1, -1);
  if fCaretY > 0 then
  begin
    ExpandLines[Pred(fCaretY)] := ExpandLines[Pred(fCaretY)] + S;
    ExpandLines.Delete(fCaretY);
  end
  else
    ExpandLines[Pred(fCaretY)] := S;

  DoLinesDeleted(fCaretY + 1, 1);
end;

procedure TCustomSynEdit.DeleteCertainLine(const Line: Integer);
begin
  fLines.Delete(Pred(Line));
end;

procedure TCustomSynEdit.UndoItem;
var
  Item: TSynEditUndoItem;
  BB, BE, TmpPos: TBufferCoord;
  TmpStr: UnicodeString;
  ChangeScrollPastEol: Boolean;
  I, Len, Beginning, Ending: Integer;
begin
  ChangeScrollPastEol := not (eoScrollPastEol in Options);
  Item := fUndoList.PopItem;
  if Assigned(Item) then
  try
    ActiveSelectionMode := Item.ChangeSelMode;
    IncPaintLock;
    Include(fOptions, eoScrollPastEol);
    case Item.ChangeReason of
      crCaret:
        begin
          fRedoList.AddChange(Item.ChangeReason, CaretXY, CaretXY, Item.ChangeCaret,
            '', fActiveSelectionMode);
          InternalCaretXY := Item.ChangeStartPos;
        end;
      crSelection:
        begin
          { Rstore selection back }
          ActiveSelectionMode := Item.ChangeSelMode;
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos, Item.ChangeEndPos);

          { Fill redo item }
          fRedoList.AddChange(Item.ChangeReason, BlockBegin, BlockEnd, Item.ChangeCaret, '', fActiveSelectionMode);
        end;
      crInsert, crPaste, crDragDropInsert:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos, CurrentSnippetCaret);

          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, Item.ChangeCaret, SelText, Item.ChangeSelMode,
            fLines.GetLineStates(Item.ChangeStartPos.Line - 1,
              Item.ChangeEndPos.Line - 1)
          );

          SetSelTextPrimitiveEx(Item.ChangeSelMode, PChar(Item.ChangeStr),
            False);

          //InternalCaretXY := Item.ChangeStartPos;
        end;
      crDelete, crDeleteAfterCursor,
      crSilentDelete, crSilentDeleteAfterCursor,
      crDeleteAll:
        begin
          fRedoList.AddChange(
            Item.ChangeReason,
            Item.ChangeStartPos,
            Item.ChangeEndPos,
            Item.ChangeCaret,
            EmptyStr,
            Item.ChangeSelMode,
            ExpandLines.GetLineStates(Item.ChangeStartPos.Line - 1, Item.ChangeEndPos.Line - 1)
          );

          { If there's no selection, we have to set
            the Caret's position manualy }
          if Item.ChangeSelMode = smColumn then
            TmpPos := BufferCoord(
              Min(Item.ChangeStartPos.Char, Item.ChangeEndPos.Char),
              Min(Item.ChangeStartPos.Line, Item.ChangeEndPos.Line))
          else
            TmpPos := TBufferCoord(MinPoint(
              TPoint(Item.ChangeStartPos), TPoint(Item.ChangeEndPos)));

          if TmpPos.Line > fLines.Count then
          begin
            fLines.Add(EmptyStr);
            InternalCaretXY := BufferCoord(1, fLines.Count);
          end;

          CaretXY := TmpPos;

          SetSelTextPrimitiveEx(Item.ChangeSelMode, PChar(Item.ChangeStr),
            False);

          if Item.ChangeReason in [crDeleteAfterCursor, crSilentDeleteAfterCursor]
          then
            TmpPos := Item.ChangeStartPos
          else
            TmpPos := Item.ChangeEndPos;

          if Item.ChangeReason in [crSilentDelete, crSilentDeleteAfterCursor]
          then
            InternalCaretXY := TmpPos
          else begin
            SetCaretAndSelection(TmpPos, Item.ChangeStartPos,
              Item.ChangeEndPos);
          end;

          if Item.ChangeReason = crDeleteAll then
          begin
            InternalCaretXY := BufferCoord(1, 1);
            fBlockEnd := BufferCoord(1, 1);
          end;

          EnsureCursorPosVisible;
        end;
      crLineBreak:
        begin
          InternalCaretXY := Item.ChangeStartPos;

          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, Item.ChangeCaret, EmptyStr, Item.ChangeSelMode,
            ExpandLines.GetLineStates(Item.ChangeStartPos.Line - 1,
              Item.ChangeStartPos.Line - 1));

          JoinPreviousLine(Item.ChangeStr);
        end;

      { Used to avoid unnecessary whole line copying }
      crLineInsert:
        begin
          InternalCaretXY := Item.ChangeStartPos;

          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, Item.ChangeCaret, EmptyStr, Item.ChangeSelMode,
            fLines.GetLineStates(Item.ChangeStartPos.Line - 1,
              Item.ChangeStartPos.Line - 1)
          );

          DeleteCertainLine(Item.ChangeStartPos.Line);
        end;

      { DoBlockIndent() }
      crIndent:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, Item.ChangeCaret, Item.ChangeStr, Item.ChangeSelMode);
        end;

      { DoBlockUnIndent() }
      crUnindent:
        begin

          { Find, how much we should add to BlockBegin and BlockEnd chars }
          Beginning := GetEOL(PChar(Item.ChangeStr)) - PChar(Item.ChangeStr);
          if Item.ChangeStartPos.Line <> Item.ChangeEndPos.Line then
          begin
            Len := PInteger(PChar(Item.ChangeStr) - 2)^;
            Ending := (@Item.ChangeStr[Len] - GetEOLBack(@Item.ChangeStr[Len])) shr 1;
          end;

          { Assemble new carets }
          if Item.ChangeStartPos.Line <> Item.ChangeEndPos.Line then
          begin
            BB := BufferCoord(Item.ChangeStartPos.Char + Beginning, Item.ChangeStartPos.Line);
            BE := BufferCoord(Item.ChangeEndPos.Char + Ending, Item.ChangeEndPos.Line);
          end
          else begin
            BB := BufferCoord(Item.ChangeStartPos.Char + Beginning, Item.ChangeStartPos.Line);
            BE := BufferCoord(Item.ChangeEndPos.Char + Beginning, Item.ChangeEndPos.Line);
          end;

          { Send to redo list }
          fRedoList.AddChange(Item.ChangeReason, BB, BE, Item.ChangeCaret, EmptyStr,
            Item.ChangeSelMode, fLines.GetLineStates(BB.Line - 1, BE.Line - 1));

          { Re-insert the string }
          if Item.ChangeSelMode <> smColumn then
            InsertBlock(BufferCoord(1, Item.ChangeStartPos.Line),
              BufferCoord(1, Item.ChangeEndPos.Line),
              PChar(Item.ChangeStr), False)
          else begin
            { Find real start pos }
            Beginning := Min(Item.ChangeStartPos.Char, Item.ChangeEndPos.Char);
            InsertBlock(BufferCoord(Beginning, Item.ChangeStartPos.Line),
              BufferCoord(Beginning, Item.ChangeEndPos.Line),
              PChar(Item.ChangeStr), False);
          end;

          { Set caret pos }
          SetCaretAndSelection(BB, BB, BE);
        end;

      crWhiteSpaceAdd:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          TmpStr := SelText;
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr), False, False, False);
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, Item.ChangeCaret, TmpStr, Item.ChangeSelMode);
          InternalCaretXY := Item.ChangeStartPos;
        end;
    end;

    { Update line states }
    if Length(Item.ChangeStrStates) > 0 then
    begin
      Beginning := Item.ChangeStartPos.Line;
      Ending := Item.ChangeEndPos.Line;
      if Beginning > Ending then
        SwapInt(Beginning, Ending);
      for I := Beginning to Ending do
        ExpandLines.SetLineStates(I - 1, Item.ChangeStrStates[I - Beginning]);
    end;
  finally
    if ChangeScrollPastEol then
      Exclude(fOptions, eoScrollPastEol);
    Item.Free;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.ClearBookMark(BookMark: Integer);
begin
  if assigned(fBookMarks[BookMark]) then
  begin
    DoOnClearBookmark(fBookMarks[BookMark]);
    FMarkList.Remove(fBookMarks[Bookmark]);
    fBookMarks[BookMark] := nil;
  end
end;

procedure TCustomSynEdit.GotoBookMark(BookMark: Integer);
var
  iNewPos: TBufferCoord;
begin
  if Assigned(fBookMarks[BookMark]) and
     (fBookMarks[BookMark].Line <= fLines.Count)
  then
  begin
    iNewPos.Char := fBookMarks[BookMark].Char;
    iNewPos.Line := fBookMarks[BookMark].Line;
    //call it this way instead to make sure that the caret ends up in the middle
    //if it is off screen (like Delphi does with bookmarks)
    SetCaretXYEx(False, iNewPos);
    EnsureCursorPosVisibleEx(True);
    if SelAvail then
      InvalidateSelection;
    fBlockBegin.Char := fCaretX;
    fBlockBegin.Line := fCaretY;
    fBlockEnd := fBlockBegin;
  end;
end;

procedure TCustomSynEdit.GotoLineAndCenter(ALine: Integer);
begin
  SetCaretXYEx( False, BufferCoord(1, ALine) );
  if SelAvail then
    InvalidateSelection;
  fBlockBegin.Char := fCaretX;
  fBlockBegin.Line := fCaretY;
  fBlockEnd := fBlockBegin;
  EnsureCursorPosVisibleEx(True);
end;

procedure TCustomSynEdit.SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
var
  Mark: TSynEditMark;
begin
  if (Y >= 1) and (Y <= Max(1, fLines.Count)) then
  begin
    Mark := TSynEditMark.Create(self);
    with Mark do
    begin
      Line := Y;
      Char := X;
      ImageIndex := Min(Bookmark, 9);
      BookmarkNumber := Bookmark;
      Visible := True;
    end;
    DoOnPlaceMark(Mark);
    if (Mark <> nil) then
    begin
      if Bookmark > High(fBookmarks) then
        SetLength(fBookmarks, Succ(Bookmark));

      if Assigned(fBookMarks[BookMark]) then
        ClearBookmark(BookMark);
      fBookMarks[BookMark] := Mark;
      fMarkList.Add(fBookMarks[BookMark]);
    end;
  end;
end;

function IsTextMessage(Msg: UINT): Boolean;
begin
  Result := (Msg = WM_SETTEXT) or (Msg = WM_GETTEXT) or (Msg = WM_GETTEXTLENGTH);
end;

(*
procedure TCustomSynEdit.WndProc(var Msg: TMessage);
const
  ALT_KEY_DOWN = $20000000;
begin

  { Prevent Alt-Backspace from beeping }
  if (Msg.Msg = WM_SYSCHAR) and (Msg.wParam = VK_BACK) and
    (Msg.lParam and ALT_KEY_DOWN <> 0)
  then
    Msg.Msg := 0;

  { Handle direct WndProc calls that could happen through VCL-methods like Perform }
  if HandleAllocated and IsWindowUnicode(Handle) then
    if not FWindowProducedMessage then
    begin
      FWindowProducedMessage := True;
      if IsTextMessage(Msg.Msg) then
      begin
        with Msg do
          Result := SendMessageA(Handle, Msg, wParam, lParam);
        Exit;
      end;
    end
    else
      FWindowProducedMessage := False;

  inherited;
end;
*)

procedure TCustomSynEdit.ChainListCleared(Sender: TObject);
begin
  if Assigned(fChainListCleared) then
    fChainListCleared(Sender);
  TSynEditStringList(fOrigLines).OnCleared(Sender);
end;

procedure TCustomSynEdit.ChainListDeleted(Sender: TObject; aIndex: Integer;
  aCount: Integer);
begin
  if Assigned(fChainListDeleted) then
    fChainListDeleted(Sender, aIndex, aCount);
  TSynEditStringList(fOrigLines).OnDeleted(Sender, aIndex, aCount);
end;

procedure TCustomSynEdit.ChainListInserted(Sender: TObject; aIndex: Integer;
  aCount: Integer);
begin
  if Assigned(fChainListInserted) then
    fChainListInserted(Sender, aIndex, aCount);
  TSynEditStringList(fOrigLines).OnInserted(Sender, aIndex, aCount);
end;

procedure TCustomSynEdit.ChainListPutted(Sender: TObject; aIndex: Integer;
  aCount: Integer);
begin
  if Assigned(fChainListPutted) then
    fChainListPutted(Sender, aIndex, aCount);
  TSynEditStringList(fOrigLines).OnPutted(Sender, aIndex, aCount);
end;

procedure TCustomSynEdit.ChainLinesChanging(Sender: TObject);
begin
  if Assigned(fChainLinesChanging) then
    fChainLinesChanging(Sender);
  TSynEditStringList(fOrigLines).OnChanging(Sender);
end;

procedure TCustomSynEdit.ChainLinesChanged(Sender: TObject);
begin
  if Assigned(fChainLinesChanged) then
    fChainLinesChanged(Sender);
  TSynEditStringList(fOrigLines).OnChange(Sender);
end;

procedure TCustomSynEdit.ChainUndoRedoAdded(Sender: TObject);
var
  iList: TSynEditUndoList;
  iHandler: TNotifyEvent;
begin
  if Sender = fUndoList then
  begin
    iList := fOrigUndoList;
    iHandler := fChainUndoAdded;
  end
  else { if Sender = fRedoList then }
  begin
    iList := fOrigRedoList;
    iHandler := fChainRedoAdded;
  end;
  if Assigned(iHandler) then
    iHandler(Sender);
  iList.OnAddedUndo(Sender);
end;

procedure TCustomSynEdit.UnHookTextBuffer;
var
  vOldWrap: Boolean;
begin
  Assert(fChainedEditor = nil);
  if fLines = fOrigLines then
    Exit;

  vOldWrap := WordWrap;
  WordWrap := False;

  //first put back the real methods
  with TSynEditStringList(fLines) do
  begin
    OnCleared := fChainListCleared;
    OnDeleted := fChainListDeleted;
    OnInserted := fChainListInserted;
    OnPutted := fChainListPutted;
    OnChanging := fChainLinesChanging;
    OnChange := fChainLinesChanged;
  end;
  fUndoList.OnAddedUndo := fChainUndoAdded;
  fRedoList.OnAddedUndo := fChainRedoAdded;

  fChainListCleared := nil;
  fChainListDeleted := nil;
  fChainListInserted := nil;
  fChainListPutted := nil;
  fChainLinesChanging := nil;
  fChainLinesChanged := nil;
  fChainUndoAdded := nil;

  //make the switch
  fLines := fOrigLines;
  fUndoList := fOrigUndoList;
  fRedoList := fOrigRedoList;
  LinesHookChanged;

  WordWrap := vOldWrap;
end;

procedure TCustomSynEdit.HookTextBuffer(aBuffer: TSynEditStringList;
  aUndo, aRedo: TSynEditUndoList);
var
  vOldWrap: Boolean;
begin
  Assert(fChainedEditor = nil);
  Assert(fLines = fOrigLines);

  vOldWrap := WordWrap;
  WordWrap := False;

  if fChainedEditor <> nil then
    RemoveLinesPointer
  else if fLines <> fOrigLines then
    UnHookTextBuffer;

  { Store the current values and put in the chained methods }
  fChainListCleared := aBuffer.OnCleared;
    aBuffer.OnCleared := ChainListCleared;
  fChainListDeleted := aBuffer.OnDeleted;
    aBuffer.OnDeleted := ChainListDeleted;
  fChainListInserted := aBuffer.OnInserted;
    aBuffer.OnInserted := ChainListInserted;
  fChainListPutted := aBuffer.OnPutted;
    aBuffer.OnPutted := ChainListPutted;
  fChainLinesChanging := aBuffer.OnChanging;
    aBuffer.OnChanging := ChainLinesChanging;
  fChainLinesChanged := aBuffer.OnChange;
    aBuffer.OnChange := ChainLinesChanged;

  fChainUndoAdded := aUndo.OnAddedUndo;
    aUndo.OnAddedUndo := ChainUndoRedoAdded;
  fChainRedoAdded := aRedo.OnAddedUndo;
    aRedo.OnAddedUndo := ChainUndoRedoAdded;

  { Make the switch }
  fLines := aBuffer;
  fUndoList := aUndo;
  fRedoList := aRedo;
  LinesHookChanged;

  WordWrap := vOldWrap;
end;

procedure TCustomSynEdit.LinesHookChanged;
var
  iLongestLineLength: Integer;
begin
  Invalidate;
  if eoAutoSizeMaxScrollWidth in fOptions then
  begin
    iLongestLineLength := ExpandLines.GetLengthOfLongestLine(RowToLine(fTopLine), RowToLine(fTopLine + fLinesInWindow));
    if iLongestLineLength > MaxScrollWidth then
      MaxScrollWidth := iLongestLineLength;
  end;
  UpdateScrollBars;
end;

procedure TCustomSynEdit.SetLinesPointer(ASynEdit: TCustomSynEdit);
begin
  HookTextBuffer(TSynEditStringList(ASynEdit.Lines),
    ASynEdit.UndoList, ASynEdit.RedoList);

  fChainedEditor := ASynEdit;
  ASynEdit.FreeNotification(Self);
end;

procedure TCustomSynEdit.RemoveLinesPointer;
begin
  if Assigned(fChainedEditor) then
    RemoveFreeNotification(fChainedEditor);
  fChainedEditor := nil;

  UnHookTextBuffer;
end;

procedure TCustomSynEdit.DragCanceled;
begin
  fScrollTimer.Enabled := False;
  inherited;
end;

procedure TCustomSynEdit.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  vNewPos: TDisplayCoord;
begin
  inherited;
  if (Source is TCustomSynEdit) and not ReadOnly then
  begin
    Accept := True;

    { Ctrl is pressed => change cursor to indicate copy instead of move }
    if GetKeyState(VK_CONTROL) < 0 then
      DragCursor := crMultiDrag
    else
      DragCursor := crDrag;
    if Dragging then // If the drag source is the SynEdit itself
    begin
      if State = dsDragLeave then // Restore prev caret position
        ComputeCaret(FMouseDownX, FMouseDownY)
      else
      begin
        vNewPos := PixelsToNearestRowColumn(X, Y);
        vNewPos.Column := MinMax(vNewPos.Column, LeftChar, LeftChar + CharsInWindow - 1);
        vNewPos.Row := MinMax(vNewPos.Row, TopLine, TopLine + LinesInWindow - 1);
        InternalCaretXY := DisplayToBufferPos(vNewPos);
        ComputeScroll(X, Y);
      end;
    end
    else // If is dragging from another SynEdit
      ComputeCaret(X, Y); // Position caret under the mouse cursor
  end;
end;

procedure TCustomSynEdit.DragDrop(Source: TObject; X, Y: Integer);
var
  vNewCaret: TBufferCoord;
  DoDrop, DropAfter, DropMove: Boolean;
  vBB, vBE: TBufferCoord;
  vLineStates: TLineStates;
  DragDropText: UnicodeString;
  ChangeScrollPastEOL, ChangeTrim: Boolean;
begin
  if not ReadOnly and ((Source is TCustomSynEdit)
    and TCustomSynEdit(Source).SelAvail) then
  begin
    IncPaintLock;
    try
      inherited;

      { Get caret position from X, Y drop cordinates }
      ChangeScrollPastEOL := not (eoScrollPastEol in fOptions);
      if ChangeScrollPastEOL then
        Include(fOptions, eoScrollPastEol);
      ComputeCaret(X, Y);
      vNewCaret := CaretXY;
      if ChangeScrollPastEOL then
        Exclude(fOptions, eoScrollPastEol);

      { If from other control then move when SHIFT, else copy.
        If from Self then copy when CTRL, else move }
      if Source <> Self then
      begin
        DropMove := GetKeyState(VK_SHIFT) < 0;
        DoDrop := True;
        DropAfter := False;
      end

      { We must calculate here the future position of insertion when moving.
        The checks are diffirent depending on selection mode }
      else begin
        DropMove := (GetKeyState(VK_CONTROL) >= 0) or fCmdDrop;
        vBB := BlockBegin;
        vBE := BlockEnd;

        { Check for normal selection. Also valid for line mode }
        if fActiveSelectionMode <> smColumn then
          DropAfter := (vNewCaret.Line > vBE.Line)
            or ((vNewCaret.Line = vBE.Line) and ((vNewCaret.Char > vBE.Char) or
            (not DropMove and (vNewCaret.Char = vBE.Char))))

        { Check for column mode. Column drag never delete lines, so we only
          should take in account horizontal position }
        else
          DropAfter := (vNewCaret.Char > vBE.Char) and
            InRange(vNewCaret.Line, vBB.Line, vBE.Line);

        { This check is valid for all selection modes }
        DoDrop := DropAfter or (vNewCaret.Line < vBB.Line) or (vNewCaret.Line > vBE.Line)
          or ((vNewCaret.Line = vBB.Line) and ((vNewCaret.Char < vBB.Char) or
          ((not DropMove) and (vNewCaret.Char = vBB.Char))));
      end;

      { Do drop if possible }
      if DoDrop then
      begin
        BeginUndoBlock;
        try

          { Get text being dropped }
          ChangeTrim := eoTrimTrailingSpaces in fOptions;
          if ChangeTrim then
            Exclude(fOptions, eoTrimTrailingSpaces);
          try
            DragDropText := TCustomSynEdit(Source).SelText;
          finally
            if ChangeTrim then
              Include(fOptions, eoTrimTrailingSpaces);
          end;

          { Delete the selected text if necessary }
          if DropMove then
          begin

            { Other editor can be the source }
            if Source <> Self then
              TCustomSynEdit(Source).SelText := ''
            else begin
              SelText := '';

              { Adjust horizontal drop position }
              if fActiveSelectionMode <> smColumn then
                if DropAfter and (vNewCaret.Line = vBE.Line) then
                  Dec(vNewCaret.Char, vBE.Char - vBB.Char);

              if fActiveSelectionMode = smColumn then
                if DropAfter then
                  Dec(vNewCaret.Char, vBE.Char - vBB.Char);

              { Adjust vertical drop position }
              if fActiveSelectionMode <> smColumn then
                if DropAfter and (vBE.Line > vBB.Line) then
                  Dec(vNewCaret.Line, vBE.Line - vBB.Line);

              { smColumn drop never causes vertical change }
            end;
          end;

          { TODO: this is probably already done inside SelText
            insert the selected text }
          ChangeScrollPastEOL := not (eoScrollPastEol in fOptions);
          try
            if ChangeScrollPastEOL then
              Include(fOptions, eoScrollPastEol);
            InternalCaretXY := vNewCaret;
            BlockBegin := vNewCaret;

            { Add the text. Undo is locked so the action is recorded as crDragDropInsert
              instead of crInsert (code right bellow) }
            SelText := DragDropText;
          finally
            if ChangeScrollPastEOL then
              Exclude(fOptions, eoScrollPastEol);
          end;

          { Reset selection }
          BlockEnd := CaretXY;
          CommandProcessor(ecSelGotoXY, #0, @vNewCaret);
        finally
          EndUndoBlock;
        end;
      end;
    finally
      DecPaintLock;
    end;
  end
  else
    inherited;
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.SetRightEdge(Value: Integer);
begin
  if fRightEdge <> Value then
  begin
    fRightEdge := Max(Value, 48);
    if WordWrap then
    begin
      if eoWrapAgainstMargin in fOptions then
        fWordWrapPlugin.Reset;
      if eoWrapCentered in fOptions then
      begin
        fCenterOffset := Max((ClientWidth - 3 - (fGutter.Width * Ord(Gutter.Visible)) - (fCharWidth * fRightEdge)), 0) shr 1;
        fTextOffset := fCenterOffset + fOldGutterWidth + 2;
      end;
    end;

    Invalidate;
  end;
end;

procedure TCustomSynEdit.SetRightEdgeColor(Value: TColor);
var
  nX: Integer;
  rcInval: TRect;
begin
  if fRightEdgeColor <> Value then
  begin
    fRightEdgeColor := Value;
    if HandleAllocated then
    begin
      nX := fTextOffset + fRightEdge * fCharWidth;
      rcInval := Rect(nX - 1, 0, nX + 1, Height);
      InvalidateRect(rcInval, False);
    end;
  end;
end;

procedure TCustomSynEdit.SetRightEdgeShow(Value: Boolean);
var
  nX: Integer;
  rcInval: TRect;
begin
  if fRightEdgeShow <> Value then
  begin
    fRightEdgeShow := Value;
    if HandleAllocated then
    begin
      nX := fTextOffset + fRightEdge * fCharWidth;
      rcInval := Rect(nX - 1, 0, nX + 1, Height);
      if (eoHighlightMargin in fOptions) and (nX < ClientWidth) then
        rcInval.Right := ClientWidth;
      InvalidateRect(rcInval, False);
    end;
  end;
end;

function TCustomSynEdit.GetMaxUndo: Integer;
begin
  result := fUndoList.MaxUndoActions;
end;

procedure TCustomSynEdit.SetMaxUndo(const Value: Integer);
begin
  if Value > -1 then
  begin
    fUndoList.MaxUndoActions := Value;
    fRedoList.MaxUndoActions := Value;
  end;
end;

procedure TCustomSynEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = fSearchEngine then
    begin
      SearchEngine := nil;
    end;

    if AComponent = fHighlighter then
    begin
      Highlighter := nil;
    end;

    if AComponent = fChainedEditor then
    begin
      RemoveLinesPointer;
    end;

    if (fBookmarkOpt <> nil) then
      if (AComponent = fBookmarkOpt.BookmarkImages) then
      begin
        fBookmarkOpt.BookmarkImages := nil;
        InvalidateGutterLines(-1, -1);
      end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.SetHighlighter(const Value: TSynCustomHighlighter);
begin
  if Value <> fHighlighter then
  begin
    if Assigned(fHighlighter) then
    begin
      fHighlighter.UnhookAttrChangeEvent(HighlighterAttrChanged);
      fHighlighter.RemoveFreeNotification(Self);
    end;
    if Assigned(Value) then
    begin
      Value.HookAttrChangeEvent(HighlighterAttrChanged);
      Value.FreeNotification(Self);
    end;
    fHighlighter := Value;
    if not(csDestroying in ComponentState) then
      HighlighterAttrChanged(fHighlighter);
  end;
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.SetBorderStyle(Value: TSynBorderStyle);
begin
  if fBorderStyle <> Value then
  begin
    fBorderStyle := Value;
    RecreateWnd;
  end;
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.SetHideSelection(const Value: Boolean);
begin
  if fHideSelection <> Value then
  begin
    FHideSelection := Value;
    InvalidateSelection;
  end;
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.SetInsertMode(const Value: Boolean);
begin
  if fInserting <> Value then
  begin
    fInserting := Value;
    if not (csDesigning in ComponentState) then
      { Reset the caret }
      InitializeCaret;
    StatusChanged([scInsertMode]);
  end;
end;

// -----------------------------------------------------------------------------
// CreateCaret automatically destroys the previous one, so we don't have to
// worry about cleaning up the old one here with DestroyCaret.
// Ideally, we will have properties that control what these two carets look like.
procedure TCustomSynEdit.InitializeCaret(DoUpdate: Boolean = True);
var
  CT: TSynEditCaretType;
  CW, CH: Integer;
begin
  { When multiple selections, the caret is always hidden }
  if fSelections then
    Exit;

  if InsertMode then
    CT := fInsertCaret
  else
    CT := fOverwriteCaret;

  case CT of
    ctVerticalLine:
      begin
        CW := 1;
        CH := fTextHeight - fExtraLineSpacing shr 1;
        fCaretOffset := Point(0, 0);
      end;
    ctHorizontalLine:
      begin
        CW := fCharWidth * fCurrCharWidth;
        CH := 1;
        fCaretOffset := Point(0, fTextHeight - fExtraLineSpacing shr 1 - 1);
      end;
  end;
  Exclude(fStateFlags, sfCaretVisible);

  if (Focused or fAlwaysShowCaret) then
  begin
    CreateCaret(Handle, 0, CW, CH);
    if DoUpdate then
      UpdateCaret;
  end;
end;

procedure TCustomSynEdit.SetInsertCaret(const Value: TSynEditCaretType);
begin
  if FInsertCaret <> Value then
  begin
    FInsertCaret := Value;
    InitializeCaret;
  end;
end;

procedure TCustomSynEdit.SetOverwriteCaret(const Value: TSynEditCaretType);
begin
  if fOverwriteCaret <> Value then
  begin
    fOverwriteCaret := Value;
    InitializeCaret;
  end;
end;

procedure TCustomSynEdit.SetMaxScrollWidth(Value: Integer);
begin
  Value := MinMax(Value, 1, MaxInt - 1);
  if MaxScrollWidth <> Value then
  begin
    fMaxScrollWidth := Value;
    if eoScrollPastEol in Options then
      UpdateScrollBars;
  end;
end;

procedure TCustomSynEdit.EnsureCursorPosVisible;
begin
  EnsureCursorPosVisibleEx(False);
end;

// -----------------------------------------------------------------------------
// § Garnet
procedure TCustomSynEdit.EnsureCursorPosVisibleEx(ForceToMiddle: Boolean;
  EvenIfVisible: Boolean = False);
var
  TmpMiddle: Integer;
  VisibleX: Integer;
  vCaretRow: Integer;
begin
  HandleNeeded;
  IncPaintLock;
  try
    { Make sure X is visible }
    VisibleX := DisplayX;
    if VisibleX < LeftChar then
      LeftChar := VisibleX
    else if VisibleX >= CharsInWindow + LeftChar then
      LeftChar := VisibleX - CharsInWindow + 1
    else
      LeftChar := LeftChar;

    { Make sure Y is visible }
    vCaretRow := DisplayY;
    if ForceToMiddle then
    begin
      if vCaretRow < (fTopLine - 1) then
      begin
        TmpMiddle := fLinesInWindow shr 1;
        if vCaretRow - TmpMiddle < 0 then
          TopLine := 1
        else
          TopLine := vCaretRow - TmpMiddle + 1;
      end
      else if vCaretRow > (fTopLine + (fLinesInWindow - 2)) then
      begin
        TmpMiddle := fLinesInWindow shr 1;
        TopLine := vCaretRow - (LinesInWindow - 1) + TmpMiddle;
      end

      { Forces to middle even if visible in viewport }
      else if EvenIfVisible then
      begin
        TmpMiddle := fLinesInWindow shr 1;
        TopLine := vCaretRow - TmpMiddle;
      end;
    end
    else begin
      if vCaretRow < TopLine then
        TopLine := vCaretRow
      else if vCaretRow > TopLine + Max(1, LinesInWindow) - 1 then
        TopLine := vCaretRow - (LinesInWindow - 1)
      else
        TopLine := TopLine;
    end;
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.SetKeystrokes(const Value: TSynEditKeyStrokes);
begin
  if Value = nil then
    FKeystrokes.Clear
  else
    FKeystrokes.Assign(Value);
end;

procedure TCustomSynEdit.SetDefaultKeystrokes;
begin
  FKeystrokes.ResetDefaults;
end;

// If the translations requires Data, memory will be allocated for it via a
// GetMem call.  The client must call FreeMem on Data if it is not NIL.

function TCustomSynEdit.TranslateKeyCode(Code: word; Shift: TSynShiftState;
  var Data: pointer): TSynEditorCommand;
var
  I: Integer;
begin
  I := KeyStrokes.FindKeycode2(fLastKey, fLastShiftState, Code, Shift);
  if I >= 0 then
    Result := KeyStrokes[I].Command
  else begin
    I := Keystrokes.FindKeycode(Code, Shift);
    if I >= 0 then
      Result := Keystrokes[I].Command
    else
      Result := ecNone;
  end;
  if (Result = ecNone) and (Code >= VK_ACCEPT) and (Code <= VK_SCROLL) then
  begin
    fLastKey := Code;
    fLastShiftState := Shift;
  end
  else begin
    fLastKey := 0;
    fLastShiftState := [];
  end;
end;

procedure TCustomSynEdit.CommandProcessor(Command: TSynEditorCommand;
  AChar: WideChar; Data: pointer);
begin
  // first the program event handler gets a chance to process the command
  DoOnProcessCommand(Command, AChar, Data);
  if Command <> ecNone then
  begin
    // notify hooked command handlers before the command is executed inside of
    // the class
    NotifyHookedCommandHandlers(False, Command, AChar, Data);
    // internal command handler
    if (Command <> ecNone) and (Command < ecUserCommandFirst) then
      ExecuteCommand(Command, AChar, Data);
    // notify hooked command handlers after the command was executed inside of
    // the class
    if Command <> ecNone then
      NotifyHookedCommandHandlers(True, Command, AChar, Data);
  end;
  DoOnCommandProcessed(Command, AChar, Data);
end;

// -----------------------------------------------------------------------------
// § Garnet
// Editor commands execution center
procedure TCustomSynEdit.ExecuteCommand(Command: TSynEditorCommand; AChar: Char;
  AData: Pointer);

  { When doing a line break on a line there can be whitespace left after,
    so we need to "eat" it into undo item because it can be immidiately
    trimmed (in trimming mode) after, breaking the undo flow }
  function SaveTrimmedWhitespace(const S: String; P: Integer): String;
  var
    I: Integer;
  begin
    I := P-1;
    while (I > 0) and (S[I] < #33) do
      Dec(I);
    Result := Copy(S, I + 1, P - I - 1);
  end;

  { Checks if all characters are whitespace on current line up to the current
    caret pos. If it is so, we can insert tabs in whitespace }
  function AllWhiteUpToCaret(const Ln: UnicodeString;
    Len: Integer): Boolean;
  var
    J: Integer;
  begin
    if (Len = 0) or (fCaretX = 1) then
    begin
      Result := True;
      Exit;
    end;
    Result := False;
    J := 1;
    while (J <= Len) and (J < fCaretX) do
    begin
      if Ln[J] > #32 then
        Exit;
      Inc(J);
    end;
    Result := True;
  end;

var
  I: Integer;
  CX, DX: Integer;
  Len: Integer;
  Temp: UnicodeString;
  Helper: UnicodeString;
  TabBuffer: UnicodeString;
  SpaceBuffer: UnicodeString;
  SpaceCount1: Integer;
  SpaceCount2: Integer;
  VisualSpaceCount1, VisualSpaceCount2: Integer;
  BackCounter: Integer;
  StartOfBlock: TBufferCoord;
  EndOfBlock: TBufferCoord;
  bChangeScroll: Boolean;
  moveBkm: Boolean;
  WP: TBufferCoord;
  Caret: TBufferCoord;
  CaretNew: TBufferCoord;
  Counter: Integer;
  InsDelta: Integer;
  iUndoBegin, iUndoEnd: TBufferCoord;
  vCaretRow: Integer;
  vTabTrim: integer;
  S: UnicodeString;
  bJustIndented: Boolean;
begin
  { Drop indentation flag }
  bJustIndented := sfJustIndented in fStateFlags;
  Exclude(fStateFlags, sfJustIndented);

  { Leave snippet mode if unsupported (by snippet mode) command issued }
  if not ValidSnippetCmd(Command) then
    CancelSnippet;
  if fSelections then
  begin
    InternalCaretXY := GetCaretByIndex(1);
    fCurrCaret := GetOriginalSelectionIndex;
    {$IFDEF DEBUG}
    WriteLn('Current smart caret is ', fCurrCaret);
    {$ENDIF}
  end;

  { Process a comand }
  Helper := EmptyStr;
  IncPaintLock;
  try
    case Command of

      { Horizontal caret movement or selection }
      ecLeft, ecSelLeft:
        MoveCaretHorz(-1, Command = ecSelLeft);
      ecRight, ecSelRight:
        MoveCaretHorz(1, Command = ecSelRight);
      ecPageLeft, ecSelPageLeft:
        MoveCaretHorz(-CharsInWindow, Command = ecSelPageLeft);
      ecPageRight, ecSelPageRight:
        MoveCaretHorz(CharsInWindow, Command = ecSelPageRight);
      ecLineStart, ecSelLineStart:
        DoHomeKey(Command = ecSelLineStart);
      ecLineEnd, ecSelLineEnd:
        DoEndKey(Command = ecSelLineEnd);

      { Vertical caret movement or selection }
      ecUp, ecSelUp:
        begin
          MoveCaretVert(-1, Command = ecSelUp);
          Update;
        end;
      ecDown, ecSelDown:
        begin
          MoveCaretVert(1, Command = ecSelDown);
          Update;
        end;
      ecPageUp, ecSelPageUp, ecPageDown, ecSelPageDown:
        begin
          Counter := fLinesInWindow shr Ord(eoHalfPageScroll in fOptions);
          if (Command in [ecPageUp, ecSelPageUp]) then
            Counter := -Counter;
          TopLine := fTopLine + Counter;
          if Command in [ecSelPageUp, ecSelPageDown] then
            MoveCaretVert(Counter, True);
          Update;
        end;
      ecPageTop, ecSelPageTop:
        begin
          CaretNew := DisplayToBufferPos(DisplayCoord(DisplayX, fTopLine));
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelPageTop);
          Update;
        end;
      ecPageBottom, ecSelPageBottom:
        begin
          CaretNew := DisplayToBufferPos(DisplayCoord(DisplayX, fTopLine + fLinesInWindow - 1));
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelPageBottom);
          Update;
        end;
      ecEditorTop, ecSelEditorTop:
        begin
          with CaretNew do
          begin
            Char := 1;
            Line := 1;
          end;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelEditorTop);
          Update;
        end;
      ecEditorBottom, ecSelEditorBottom:
        begin
          with CaretNew do
          begin
            Char := 1;
            Line := fLines.Count;
            if (Line > 0) then
              Char := ExpandLines.AccessStringLength(Line - 1) + 1;
          end;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelEditorBottom);
          Update;
        end;

      { Go to the given line / column position }
      ecGotoXY, ecSelGotoXY:
        if Assigned(AData) then
        begin
          MoveCaretAndSelection(CaretXY, TBufferCoord(AData^), Command = ecSelGotoXY);
          Update;
        end;

      { Go by given line / column offset }
      ecOffsetCaret, ecSelOffsetCaret:
        if Assigned(AData) then
        begin
          CaretNew := TBufferCoord(AData^);

          { Assemble new caret pos }
          CaretNew := BufferCoord(CaretXY.Char + CaretNew.Char,
            CaretXY.Line + CaretNew.Line);

          { Avoid negative coords }
          with CaretNew do
          begin
            Char := Max(Char, 1);
            Line := MinMax(Line, 1, fLines.Count);
          end;

          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelOffsetCaret);
          Update;
        end;

      { Outlining }
      ecOutliningCollapse: OutliningCollapse(Self, fCaretY);
      ecOutliningExpand: OutliningExpand(Self, fCaretY);
      ecOutliningToggle: OutliningToggle(Self, fCaretY);

      { Goto bookmark }
      ecGotoMarker0..ecGotoMarker9: GotoBookMark((Command - ecGotoMarker0));

      { Set bookmark }
      ecSetMarker0..ecSetMarker9:
        begin
          for I := Pred(fMarkList.Count) downto 0 do
            if fMarkList[I].fBookmark = (Command - ecSetMarker0) then
            begin
              fMarkList.Delete(I);
              Break;
            end;
          SetBookMark((Command - ecSetMarker0), fCaretX, fCaretY);
        end;

      { Word selection, selection }
      ecWordLeft, ecSelWordLeft:
        begin
          CaretNew := WordStart;
          if CaretsEqual(CaretNew, CaretXY) then
            CaretNew := PrevWordPos;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelWordLeft);
        end;
      ecWordRight, ecSelWordRight:
        begin
          CaretNew := WordEnd;
          if CaretsEqual(CaretNew, CaretXY) then
            CaretNew := NextWordPos;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelWordRight);
        end;
      ecSelWord:
      	  SetSelWord;
      ecSelectAll:
          SelectAll;

      { Pairing matching }
      ecGotoMatchPair, ecGotoMatchFoPair:
        begin
          bJustIndented := False;
          if fLastMatch.TokenKind <> -1 then
            if CaretsEqual(CaretXY, fLastMatch.OpenTokenPos) then
            begin
              bJustIndented := True;
              CaretXY := fLastMatch.CloseTokenPos;
              EnsureCursorPosVisibleEx(False, False);
            end
            else if CaretsEqual(CaretXY, fLastMatch.CloseTokenPos) then
            begin
              bJustIndented := True;
              CaretXY := fLastMatch.OpenTokenPos;
              EnsureCursorPosVisibleEx(False, False);
            end;
          if not bJustIndented then
            if FindMatchingPair(StartOfBlock, EndOfBlock, Command = ecGotoMatchFoPair) then
            begin
              CaretXY := StartOfBlock;
              EnsureCursorPosVisibleEx(False, False);
            end;
        end;
      ecSelGotoMatchPair, ecSelGotoMatchFoPair:
        begin
          if FindMatchingPair(StartOfBlock, EndOfBlock, Command = ecSelGotoMatchFoPair) then
          begin
            SetCaretAndSelection(StartOfBlock, StartOfBlock, EndOfBlock);
            EnsureCursorPosVisibleEx(False, False);
          end;
        end;
      ecSelScope: SelectCurrentScope;

      { Backspace command }
      ecDeleteLastChar:
        if not ReadOnly then
        begin
          DoOnPaintTransientEx(ttBefore, True);
          try
            if SelAvail then
              SetSelectedTextEmpty
            else begin
              if fSnippet then
                fUndoList.BeginBlock;
              ExpandCollapsedLine(fCaretY);

              Temp := LineText;
              Len := fLines.AccessStringLength(fCaretY - 1);
              TabBuffer := fLines.Strings[fCaretY - 1];
              Caret := CaretXY;
              vTabTrim := 0;
              bJustIndented := False;

              { Behind EOL? Simply move the cursor }
              if fCaretX > Len + 1 then
              begin
                bJustIndented := True;

                { Handle smart tab deletion }
                if eoSmartTabDelete in fOptions then
                begin
                  { It's at the end of the line, move it to the length }
                  if Len > 0 then
                    InternalCaretX := Len + 1

                  { Move it as if there were normal spaces there }
                  else begin

                    SpaceCount1 := fCaretX - 1; // In that case it will be
                                                // visual length
                    SpaceCount2 := 0;

                    { Unindent }
                    if SpaceCount1 > 0 then
                    begin
                      { Count visual whitespace on previous lines }
                      BackCounter := CaretY - 2;
                      while BackCounter > -1 do
                      begin
                        SpaceCount2 := GetLeadingExpandedLength(ExpandLines[BackCounter],
                          fTabWidth);
                        if SpaceCount2 < SpaceCount1 then
                          Break;
                        Dec(BackCounter);
                      end;
                      if (BackCounter = -1) and (SpaceCount2 > SpaceCount1) then
                        SpaceCount2 := 0;
                    end;
                    if SpaceCount2 = SpaceCount1 then
                      SpaceCount2 := 0;

                    { Move caret }
                    fCaretX := fCaretX - (SpaceCount1 - SpaceCount2);
                    UpdateLastCaretX;
                    fStateFlags := fStateFlags + [sfCaretChanged];
                    StatusChanged([scCaretX]);
                  end;
                end

                { Only move caret one column }
                else
                  InternalCaretX := CaretX - 1;
              end

              { Deleting while on BOL? }
              else if fCaretX = 1 then
              begin
                { Join this line with the last line if possible }
                if fCaretY > 1 then
                begin
                  InternalCaretXY := BufferCoord(ExpandLines.AccessStringLength(fCaretY - 2) + 1, fCaretY - 1);

                  fUndoList.AddChange(crSilentDelete, CaretXY, Caret, CurrentSnippetCaret, sLineBreak,
                    smNormal, ExpandLines.GetLineStates(fCaretY - 1, fCaretY));

                  ExpandLines.Delete(fCaretY);
                  DoLinesDeleted(fCaretY + 1, 1);
                  ExpandLines[fCaretY - 1] := ExpandLines[fCaretY - 1] + Temp;

                  { Mirror }
                  InsertedInSnippet(Caret.Char, Caret.Line, fCaretX - 1, -1);
                end;
              end

              { Delete text before the caret }
              else begin
                SpaceCount1 := LeftSpaces(Temp); // Count physical whitespace
                                                 // chars on a line
                SpaceCount2 := 0;

                { Caret is just behind all line's whitespace }
                if SpaceCount1 = fCaretX - 1 then
                begin

                  { Handle smart tabs deletion }
                  if eoSmartTabDelete in fOptions then
                  begin

                    { Unindent, count visual whitespace on current previous lines }
                    VisualSpaceCount1 := GetLeadingExpandedLength(Temp, fTabWidth);
                    VisualSpaceCount2 := 0; // Shut up Delphi compiler
                    BackCounter := fCaretY - 2;
                    while BackCounter >= 0 do
                    begin
                      VisualSpaceCount2 := GetLeadingExpandedLength(ExpandLines[BackCounter], fTabWidth);
                      if VisualSpaceCount2 < VisualSpaceCount1 then
                      begin
                        SpaceCount2 := LeftSpaces(ExpandLines[BackCounter]);
                        Break;
                      end;
                      Dec(BackCounter);
                    end;

                    { Do checks }
                    if (BackCounter = -1) and (SpaceCount2 > SpaceCount1) then
                      SpaceCount2 := 0;
                    if SpaceCount2 = SpaceCount1 then
                      SpaceCount2 := 0;

                    { There's a line to which we can unindent. Let's do it }
                    if SpaceCount2 > 0 then
                    begin

                      { Find the first char where our line's whitespace is
                        visually less }
                      CX := fCaretX - 2;
                      DX := GetLeadingExpandedLength(Temp, fTabWidth, CX);
                      while DX > VisualSpaceCount2 do
                      begin
                        Dec(CX);
                        DX := GetLeadingExpandedLength(Temp, fTabWidth, CX);
                      end;

                      { CX now points to first char we will leave undeleted.
                        Delete whitespace between CX and fCaretX }
                      Helper := Copy(Temp, CX + 1, SpaceCount1 - CX);
                      Delete(Temp, CX + 1, SpaceCount1 - CX);
                      fUndoList.BeginBlock;
                      try
                        fUndoList.AddChange(
                          crSilentDelete,
                          BufferCoord(CX + 1, fCaretY), // After CX
                          Caret,                        // And up to caret pos
                          CurrentSnippetCaret,
                          Helper,                       // Saved whitespace
                          smNormal,
                          ExpandLines.GetLineStates(fCaretY - 1, fCaretY - 1)
                        );

                        { If there's visual diffirence after deletion, that
                          means there are #32 spaces chars which we need to
                          compensate with #32 spaces. They cannot exceed tab
                          width }
                        if VisualSpaceCount2 - DX > 0 then
                          SpaceBuffer := StringOfChar(#32, VisualSpaceCount2 - DX);
                        Insert(SpaceBuffer, Temp, CX + 1);
                        fUndoList.AddChange(crWhiteSpaceAdd,
                          BufferCoord(CX + 1, fCaretY),
                          BufferCoord(CX + 1 + Length(SpaceBuffer), fCaretY),
                          CurrentSnippetCaret,
                          '', smNormal);
                      finally
                        fUndoList.EndBlock;
                      end;
                      fCaretX := CX + Length(SpaceBuffer) + 1;
                    end

                    { The are no lines where we can unindent or they all
                      don't have leading whitespace. Unindent to the first
                      visual TabLength with mod = 0 }
                    else begin

                      { Now VisualSpaceCount2 contains amount to where we
                        should visually unindent }
                      VisualSpaceCount2 := VisualSpaceCount1 - (VisualSpaceCount1 mod fTabWidth);

                      { If it's equal to our initial whitespace, then
                        unindent to whole TabWidth visually }
                      if VisualSpaceCount2 = VisualSpaceCount1 then
                        VisualSpaceCount2 := Max(VisualSpaceCount2 - fTabWidth, 0);

                      { Find the first char where our line's whitespace is
                        visually less. CX > 0 is essential here }
                      CX := fCaretX - 2;
                      DX := GetLeadingExpandedLength(Temp, fTabWidth, CX);
                      while (CX > 0) and (DX > VisualSpaceCount2) do
                      begin
                        Dec(CX);
                        DX := GetLeadingExpandedLength(Temp, fTabWidth, CX);
                      end;

                      { CX now points to first char we will leave undeleted.
                        Delete whitespace between CX and fCaretX }
                      Helper := Copy(Temp, CX + 1, SpaceCount1 - CX);
                      Delete(Temp, CX + 1, SpaceCount1 - CX);
                      fUndoList.AddChange(
                        crSilentDelete,
                        BufferCoord(CX + 1, fCaretY), // After CX
                        Caret,                        // And up to caret pos
                        CurrentSnippetCaret,
                        Helper,                       // Saved whitespace
                        smNormal,
                        ExpandLines.GetLineStates(fCaretY - 1, fCaretY - 1)
                      );
                      fCaretX := CX + 1;
                    end;
                  end

                  { § Garnet: I don't know what's better here: to delete just
                    one char or behave like in case with smart tab delete.
                    Went for the second though it's very simple to remake to
                    the first approach }
                  else begin

                    { Now VisualSpaceCount2 contains amount to where we
                      should visually unindent }
                    VisualSpaceCount1 := GetLeadingExpandedLength(Temp, fTabWidth);
                    VisualSpaceCount2 := VisualSpaceCount1 - (VisualSpaceCount1 mod fTabWidth);

                    { If it's equal to our initial whitespace, then
                      unindent to whole TabWidth visually }
                    if VisualSpaceCount2 = VisualSpaceCount1 then
                      VisualSpaceCount2 := Max(VisualSpaceCount2 - fTabWidth, 0);

                    { Find the first char where our line's whitespace is
                      visually less. CX > 0 is essential here }
                    CX := fCaretX - 2;
                    DX := GetLeadingExpandedLength(Temp, fTabWidth, CX);
                    while (CX > 0) and (DX > VisualSpaceCount2) do
                    begin
                      Dec(CX);
                      DX := GetLeadingExpandedLength(Temp, fTabWidth, CX);
                    end;

                    { CX now points to first char we will leave undeleted.
                      Delete whitespace between CX and fCaretX }
                    Helper := Copy(Temp, CX + 1, SpaceCount1 - CX);
                    Delete(Temp, CX + 1, SpaceCount1 - CX);
                    fUndoList.AddChange(
                      crSilentDelete,
                      BufferCoord(CX + 1, fCaretY), // After CX
                      Caret,                        // And up to caret pos
                      CurrentSnippetCaret,
                      Helper,                       // Saved whitespace
                      smNormal,
                      ExpandLines.GetLineStates(fCaretY - 1, fCaretY - 1)
                    );
                    fCaretX := CX + 1;
                  end;

                  { Do update editor }
                  ExpandLines[fCaretY - 1] := Temp;
                  UpdateLastCaretX;
                  fStateFlags := fStateFlags + [sfCaretChanged];
                  StatusChanged([scCaretX]);
                end

                { Delete one char }
                else begin

                  { We can fill undo item right now }
                  fUndoList.AddChange(
                    crSilentDelete,
                    BufferCoord(fCaretX - 1, fCaretY),
                    Caret,
                    CurrentSnippetCaret,
                    Copy(Temp, fCaretX - 1, 1),
                    smNormal,
                    ExpandLines.GetLineStates(fCaretY - 1, fCaretY - 1)
                  );

                  { Set line }
                  Delete(Temp, fCaretX - 1, 1);
                  ExpandLines[fCaretY - 1] := Temp;

                  { Set new caret pos }
                  InternalCaretX := fCaretX - 1;
                end;
              end;
              if not bJustIndented then
                ExpandLines.ModifiedLines[fCaretY - 1] := True;

              { Mirror }
              InsertedInSnippet(Caret.Char, Caret.Line, fCaretX - Caret.Char, 0);
            end;
            if not fInsertingMirrors then
              EnsureCursorPosVisible;

            { Mirror }
            MirrorCommand(Command, AChar, AData, Caret, Caret, Caret, nil);
            if fSnippet then
              fUndoList.EndBlock;
          finally
            DoOnPaintTransientEx(ttAfter, True);
          end;
        end;

      { Delete command }
      ecDeleteChar:
        if not ReadOnly then
        begin
          DoOnPaintTransient(ttBefore);

          { Work with selection }
          if SelAvail then
            SetSelectedTextEmpty

          { Delete one character }
          else begin
            if fSnippet then
              fUndoList.BeginBlock;
            ExpandCollapsedLine(fCaretY);
            CaretNew := CaretXY;

            { Call UpdateLastCaretX. Even though the caret doesn't move, the
              current caret position should "stick" whenever text is modified }
            UpdateLastCaretX;
            Temp := LineText;
            Len := Length(Temp);

            { Inside string, delete char }
            if fCaretX <= Len then
            begin
              Helper := Copy(Temp, fCaretX, 1);
              with Caret do
              begin
                Char := CaretX + 1;
                Line := CaretY;
              end;

              { Do set line }
              Delete(Temp, fCaretX, 1);
              ExpandLines[fCaretY - 1] := Temp;

              { Reflect our changes in undo list }
              fUndoList.AddChange(crSilentDeleteAfterCursor, CaretXY, Caret,
                CurrentSnippetCaret, Helper, smNormal);

              { Mirror }
              InsertedInSnippet(CaretNew.Char, CaretNew.Line, -1, 0);
            end

            { After, join line with the line after }
            else begin
              if CaretY < fLines.Count then
              begin
                SpaceCount1 := fCaretX - 1 - Len;
                if eoTabsToSpaces in fOptions then
                  SpaceBuffer := UnicodeString(StringOfChar(#32, SpaceCount1))
                else
                  if AllWhiteUpToCaret(Temp, Len) then
                    SpaceBuffer := UnicodeString(StringOfChar(#9, SpaceCount1 div fTabWidth) +
                      StringOfChar(#32, SpaceCount1 mod fTabWidth))
                  else
                    SpaceBuffer := UnicodeString(StringOfChar(#32, SpaceCount1));

                { Reflect our changes in undo list }
                with Caret do
                begin
                  Char := 1;
                  Line := fCaretY + 1;
                end;

                if SpaceCount1 > 0 then
                begin
                  fUndoList.BeginBlock;
                  fUndoList.AddChange(
                    crWhiteSpaceAdd,
                    BufferCoord(Len + 1, fCaretY),
                    BufferCoord(fCaretX, fCaretY),
                    CurrentSnippetCaret,
                    '', smNormal
                  );
                end;

                Helper := sLineBreak;
                fUndoList.AddChange(crSilentDeleteAfterCursor, CaretXY, Caret,
                  CurrentSnippetCaret, Helper, smNormal,
                  fLines.GetLineStates(CaretXY.Line - 1, Caret.Line - 1));

                if SpaceCount1 > 0 then
                  fUndoList.EndBlock;

                { Do set lines }
                ExpandLines[fCaretY - 1] := Temp + SpaceBuffer + ExpandLines[fCaretY];
                fLines.ModifiedLines[Pred(fCaretY)] := True;
                ExpandLines.Delete(fCaretY);

                { Inform others about deletion }
                DoLinesDeleted(fCaretY + 1, 1);

                { Mirror }
                InsertedInSnippet(Caret.Char, Caret.Line, CaretNew.Char - 1, -1);
              end;
            end;

            { Mirror }
            MirrorCommand(Command, AChar, AData, CaretNew, CaretNew, CaretNew,
              nil);
            if fSnippet then
              fUndoList.EndBlock;
          end;
          DoOnPaintTransient(ttAfter);
        end;

      { Other deletion commands }
      ecDeleteWord, ecDeleteEOL:
        if not ReadOnly then
        begin
          ExpandCollapsedLine(fCaretY);
          DoOnPaintTransient(ttBefore);
          Len := Length(LineText);
          if Command = ecDeleteWord then
          begin
            WP := WordEnd;
            Temp := LineText;
            if (WP.Char < CaretX) or ((WP.Char = CaretX) and (WP.Line < fLines.Count)) then
            begin
              if WP.Char > Len then
              begin
                Inc(WP.Line);
                WP.Char := 1;
                Temp := Lines[WP.Line - 1];
              end
              else if Temp[WP.Char] <> #32 then
                Inc(WP.Char);
            end

            { § Garnet: ecDeleteWord fix }
            else if (WP.Char = fCaretX) and (WP.Line = fCaretY) then
            begin
              WP.Char := Len + 1;
              WP.Line := fCaretY;
            end;
            {$IFOPT R+}
            Temp := Temp + #0;
            {$ENDIF}
            if Temp <> '' then
              while Temp[WP.Char] = #32 do
                Inc(WP.Char);
          end
          else begin
            WP.Char := Len + 1;
            WP.Line := fCaretY;
          end;
          if (WP.Char <> CaretX) or (WP.Line <> CaretY) then
          begin
            SetBlockBegin(CaretXY);
            SetBlockEnd(WP);
            ActiveSelectionMode := smNormal;
            Helper := SelText;
            SetSelTextPrimitive(UnicodeString(StringOfChar(' ', CaretX - BlockBegin.Char)));
            fUndoList.AddChange(crSilentDeleteAfterCursor, CaretXY, WP, CurrentSnippetCaret,
              Helper, smNormal);
            InternalCaretXY := CaretXY;
          end;
        end;
      ecDeleteLastWord, ecDeleteBOL:
        if not ReadOnly then
        begin
          ExpandCollapsedLine(fCaretY);
          DoOnPaintTransient(ttBefore);
          if Command = ecDeleteLastWord then
            WP := PrevWordPos
          else begin
            WP.Char := 1;
            WP.Line := CaretY;
          end;
          if (WP.Char <> CaretX) or (WP.Line <> CaretY) then
          begin
            SetBlockBegin(CaretXY);
            SetBlockEnd(WP);
            ActiveSelectionMode := smNormal;
            Helper := SelText;
            SetSelTextPrimitive('');
            fUndoList.AddChange(crSilentDelete, WP, CaretXY, CurrentSnippetCaret,
              Helper, smNormal);
            InternalCaretXY := WP;
          end;
          DoOnPaintTransient(ttAfter);
        end;
      ecDeleteLine:
        if not ReadOnly and (Lines.Count > 0) and not ((CaretY = Lines.Count) and (Length(Lines[CaretY - 1]) = 0))
        then
        begin
          ExpandCollapsedLine(fCaretY);
          DoOnPaintTransient(ttBefore);
          if SelAvail then
            SetBlockBegin(CaretXY);
          Helper := LineText;
          if CaretY = Lines.Count then
          begin
            Lines[CaretY - 1] := '';
            fUndoList.AddChange(crSilentDeleteAfterCursor, BufferCoord(1, CaretY),
              BufferCoord(Length(Helper) + 1, CaretY), CurrentSnippetCaret,
              Helper, smNormal);
          end
          else begin
            Lines.Delete(CaretY - 1);
            Helper := Helper + #13#10;
            fUndoList.AddChange(crSilentDeleteAfterCursor, BufferCoord(1, CaretY),
              BufferCoord(1, CaretY + 1), CurrentSnippetCaret, Helper, smNormal);
            DoLinesDeleted(CaretY - 1, 1);
          end;
          InternalCaretXY := BufferCoord(1, CaretY); // Like seen in the Delphi editor
        end;

      { Moving }
      ecMoveLineUp:
        begin
          fCmdDrop := True;
          try
            iUndoBegin := BlockBegin;
            iUndoEnd := BlockEnd;
            with StartOfBlock do
            begin
              Char := Min(iUndoBegin.Char, iUndoEnd.Char);
              Line := Min(iUndoBegin.Line, iUndoEnd.Line);
            end;
            StartOfBlock := TBufferCoord(RowColumnToPixels(BufferToDisplayPos(StartOfBlock)));
            Dec(StartOfBlock.Line, fTextHeight);
            DragDrop(Self, StartOfBlock.Char, StartOfBlock.Line);
          finally
            fCmdDrop := False;
          end;
        end;

      ecMoveLineDown:
        begin
          fCmdDrop := True;
          try
            iUndoBegin := BlockBegin;
            iUndoEnd := BlockEnd;
            with StartOfBlock do
            begin
              Char := Min(iUndoBegin.Char, iUndoEnd.Char);
              Line := Max(iUndoBegin.Line, iUndoEnd.Line);
            end;
            StartOfBlock := TBufferCoord(RowColumnToPixels(BufferToDisplayPos(StartOfBlock)));
            Inc(StartOfBlock.Line, fTextHeight);
            DragDrop(Self, StartOfBlock.Char, StartOfBlock.Line);
          finally
            fCmdDrop := False;
          end;
        end;

      ecMoveCharLeft:
        begin
          fCmdDrop := True;
          try
            iUndoBegin := BlockBegin;
            iUndoEnd := BlockEnd;
            with StartOfBlock do
            begin
              Char := Min(iUndoBegin.Char, iUndoEnd.Char);
              Line := Min(iUndoBegin.Line, iUndoEnd.Line);
            end;
            StartOfBlock := TBufferCoord(RowColumnToPixels(BufferToDisplayPos(BufferCoord(StartOfBlock.Char - 1, StartOfBlock.Line))));
            DragDrop(Self, StartOfBlock.Char, StartOfBlock.Line);
          finally
            fCmdDrop := False;
          end;
        end;
      ecMoveCharRight:
        begin
          fCmdDrop := True;
          try
            iUndoBegin := BlockBegin;
            iUndoEnd := BlockEnd;
            with StartOfBlock do
            begin
              Char := Max(iUndoBegin.Char, iUndoEnd.Char);
              Line := Min(iUndoBegin.Line, iUndoEnd.Line);
            end;
            StartOfBlock := TBufferCoord(RowColumnToPixels(BufferToDisplayPos(BufferCoord(StartOfBlock.Char + 1, StartOfBlock.Line))));
            DragDrop(Self, StartOfBlock.Char, StartOfBlock.Line);
          finally
            fCmdDrop := False;
          end;
        end;

      { Delete everything }
      ecClearAll:
        if not ReadOnly then ClearAll;

      { New line insertion / break }
      ecInsertLine, ecLineBreak:
        if not ReadOnly then
        begin
          UndoList.BeginBlock;
          try

          { Clear selection if there is }
          if SelAvail then
            SetSelectedTextEmpty;

          { Fetch current line }
          Temp := LineText;

          { This is sloppy, but the Right Thing would be to track the column
            of markers too, so they could be moved depending on whether they
            are after the caret X... }
          InsDelta := Ord(CaretX = 1);
          Len := Length(Temp);

          { Get caret }
          Caret := CaretXY;

          { Mirror }
          InsertedInSnippet(Caret.Char, Caret.Line, 0, 1);

          { If breaking on line which is not blank, we need to act diffirently
            depending on break position }
          if Len > 0 then
          begin

            { Breaking inside line or on first char? }
            if Len >= fCaretX then
            begin

              { Breaking inside line? }
              if fCaretX > 1 then
              begin
                { When breaking inside line in trim trailing spaces mode
                  they (spaces) will be lost on trimming. We save them and
                  do trimming right here. Saved whitespace then go to UndoItem }
                if eoTrimTrailingSpaces in fOptions then
                  TabBuffer := SaveTrimmedWhitespace(Temp, fCaretX);

                { Count how much whitespace need to be in SpaceBuffer }
                SpaceCount2 := 0; // Shut up Delphi compiler
                if eoAutoIndent in fOptions then
                  if eoTabsToSpaces in fOptions then
                  begin
                    SpaceCount1 := 1; // Use it as a flag now
                    SpaceCount2 := GetLeadingExpandedLength(Temp, fTabWidth);
                  end
                  else
                    SpaceCount1 := LeftSpaces(Temp)
                else
                  SpaceCount1 := 0;

                { Fill space buffer and find it's visual length }
                if SpaceCount1 > 0 then
                begin
                  { Create string of #32 whites }
                  if eoTabsToSpaces in fOptions then
                    SpaceBuffer := GetLeftSpacing(SpaceCount2, False)
                  else
                    SpaceBuffer := Copy(Temp, 1, SpaceCount1);
                end;

                { Set line }
                ExpandLines[fCaretY - 1] := Copy(Temp, 1, fCaretX - 1);

                { Get and set new line }
                Temp := Copy(Temp, fCaretX, MAXINT);
                if (eoAutoIndent in fOptions) and (SpaceCount1 > 0) then
                  Lines.Insert(fCaretY, SpaceBuffer + Temp)
                else
                  Lines.Insert(fCaretY, Temp);

                { Reflect our changes in undo list }
                if (eoTrimTrailingSpaces in fOptions) and (TabBuffer <> '') then
                  fUndoList.AddChange(
                    crLineBreak,
                    Caret,
                    Caret,
                    CurrentSnippetCaret,
                    TabBuffer + Temp,
                    smNormal,
                    ExpandLines.GetLineStates(Caret.Line - 1, Caret.Line - 1))
                else
                  fUndoList.AddChange(
                    crLineBreak,
                    Caret,
                    Caret,
                    CurrentSnippetCaret,
                    Temp,
                    smNormal,
                    ExpandLines.GetLineStates(Caret.Line - 1, Caret.Line - 1));

                { Update line states }
                with ExpandLines do
                begin
                  ModifiedLines[Caret.Line - 1] := True;
                  ModifiedLines[Caret.Line] := True;
                end;

                { Set new caret position }
                if Command = ecLineBreak then
                  if eoTabsToSpaces in fOptions then
                    InternalCaretXY := BufferCoord(SpaceCount2 + 1, fCaretY + 1)
                  else
                    InternalCaretXY := BufferCoord(SpaceCount1 + 1, fCaretY + 1);
              end

              { On first char, simply insert new blank line }
              else begin
                ExpandLines.Insert(fCaretY - 1, EmptyStr);

                { Reflect our changes in undo list }
                fUndoList.AddChange(
                  crLineInsert,
                  Caret,
                  Caret,
                  CurrentSnippetCaret,
                  '',
                  smNormal,
                  ExpandLines.GetLineStates(fCaretY, fCaretY)
                );

                { Update line states }
                with ExpandLines do
                begin
                  ModifiedLines[fCaretY - 1] := True;
                  ModifiedLines[fCaretY] := True;
                end;

                { Set new caret position }
                if Command = ecLineBreak then
                  InternalCaretY := fCaretY + 1;
              end;
            end

            { Breaking after EOL }
            else begin

              { Count whitespace length on previous or next lines.
                Turned off for ecInsertLine when after EOL.
                Maybe should turn on? }
              SpaceCount1 := 0;
              SpaceCount2 := 0;
              BackCounter := fCaretY; // The Line after which new one will be
                                      // inserted
              if (Command = ecLineBreak) and (eoAutoIndent in fOptions) then
              begin
                repeat
                  Dec(BackCounter);
                  if ExpandLines.AccessStringLength(BackCounter) > 0 then
                  begin
                    if eoTabsToSpaces in fOptions then
                    begin
                      SpaceCount1 := 1;
                      SpaceCount2 := GetLeadingExpandedLength(Lines[BackCounter], fTabWidth);
                    end
                    else
                      SpaceCount1 := LeftSpaces(Lines[BackCounter]);
                    Break;
                  end;
                until
                  BackCounter = 0;

                { Try next lines if no success }
                if (SpaceCount1 = 0) and (fCaretY < Lines.Count) then
                begin
                  BackCounter := fCaretY - 1;
                  repeat
                    Inc(BackCounter);
                    if ExpandLines.AccessStringLength(BackCounter) > 0 then
                    begin
                      if eoTabsToSpaces in fOptions then
                      begin
                        SpaceCount1 := 1;
                        SpaceCount2 := GetLeadingExpandedLength(Lines[BackCounter], fTabWidth);
                      end
                      else
                        SpaceCount1 := LeftSpaces(Lines[BackCounter]);
                      Break;
                      Inc(BackCounter); // After line insertion, index will
                                        // increase
                      Break;
                    end;
                  until
                    BackCounter = fLines.Count - 1;
                end;
              end;

              { Do insert new line }
              ExpandLines.Insert(fCaretY, '');

              { Reflect our changes in undo list }
              fUndoList.AddChange(
                crLineBreak,
                Caret,
                Caret,
                CurrentSnippetCaret,
                '',
                smNormal,
                ExpandLines.GetLineStates(fCaretY - 1, fCaretY - 1)
              );

              { Update line states }
              with ExpandLines do
              begin
                ModifiedLines[fCaretY - 1] := True;
                ModifiedLines[fCaretY] := True;
              end;

              { If was breaking }
              if Command = ecLineBreak then
              begin
                { Get space buffer and insert it on new line when
                  auto indenting }
                if SpaceCount1 > 0 then
                begin
                  { Get space buffer }
                  if eoTabsToSpaces in fOptions then
                    SpaceBuffer := GetLeftSpacing(SpaceCount2, False)
                  else
                    SpaceBuffer := Copy(Lines[BackCounter], 1, SpaceCount1);

                  { Set new caret }
                  InternalCaretXY := BufferCoord(1, fCaretY + 1);

                  { Insert spaces into new line char by char }
                  for I := 1 to Length(SpaceBuffer) do
                    CommandProcessor(ecChar, SpaceBuffer[I], nil);
                end

                { Simply set new caret }
                else
                  InternalCaretXY := BufferCoord(1, fCaretY + 1);
              end;
            end;
          end

          { Breaking on blank line }
          else begin
            { Fill with blank line if was editing blank document }
            if fLines.Count = 0 then
              fLines.Add('');

            { Count whitespace length on previous or next lines.
              Turned off for ecInsertLine when after EOL.
              Maybe should turn on? }
            SpaceCount1 := 0;
            SpaceCount2 := 0;
            BackCounter := fCaretY; // The Line after which new one will be
                                    // inserted

            if (Command = ecLineBreak) and (eoAutoIndent in fOptions)
              and (fLines.Count > 1) then
            begin
              repeat
                Dec(BackCounter);
                if ExpandLines.AccessStringLength(BackCounter) > 0 then
                begin
                  if eoTabsToSpaces in fOptions then
                  begin
                    SpaceCount1 := 1;
                    SpaceCount2 := GetLeadingExpandedLength(Lines[BackCounter], fTabWidth);
                  end
                  else
                    SpaceCount1 := LeftSpaces(Lines[BackCounter]);
                  Break;
                end;
              until
                BackCounter = 0;

              { Try next lines if no success }
              if (SpaceCount1 = 0) and (fCaretY < Lines.Count) then
              begin
                BackCounter := fCaretY - 1;
                repeat
                  Inc(BackCounter);
                  if ExpandLines.AccessStringLength(BackCounter) > 0 then
                  begin
                    if eoTabsToSpaces in fOptions then
                    begin
                      SpaceCount1 := 1;
                      SpaceCount2 := GetLeadingExpandedLength(Lines[BackCounter], fTabWidth);
                    end
                    else
                      SpaceCount1 := LeftSpaces(Lines[BackCounter]);
                    Break;
                    Inc(BackCounter); // After line insertion, index will
                                      // increase
                    Break;
                  end;
                until
                  BackCounter = Lines.Count - 1;
              end;
            end;

            { Do insert line }
            Lines.Insert(fCaretY - 1, '');

            { Reflect our changes in undo list }
            fUndoList.AddChange(
              crLineBreak,
              CaretXY,
              CaretXY,
              CurrentSnippetCaret,
              '',
              smNormal,
              ExpandLines.GetLineStates(fCaretY, fCaretY)
            );

            { Update line states }
            with ExpandLines do
            begin
              ModifiedLines[fCaretY - 1] := True;
              ModifiedLines[fCaretY] := True;
            end;

            { If was breaking }
            if Command = ecLineBreak then
            begin
              { Get space buffer and insert it on new line when
                auto indenting }
              if SpaceCount1 > 0 then
              begin
                { Get space buffer }
                if eoTabsToSpaces in fOptions then
                  SpaceBuffer := GetLeftSpacing(SpaceCount2, False)
                else
                  SpaceBuffer := Copy(Lines[BackCounter], 1, SpaceCount1);

                { Set new caret }
                InternalCaretXY := BufferCoord(1, fCaretY + 1);

                { Insert spaces char by char into new line }
                for I := 1 to Length(SpaceBuffer) do
                  CommandProcessor(ecChar, SpaceBuffer[I], nil);
              end

              { Simply set new caret }
              else
                InternalCaretXY := BufferCoord(1, fCaretY + 1);
            end;
          end;

          { *Always* trim a line (needed for correct undo work) }
          DoTrimTrailingSpaces(Caret.Line);

          { Update editor }
          DoLinesInserted(CaretY - InsDelta, 1);
          BlockBegin := CaretXY;
          BlockEnd   := CaretXY;
          EnsureCursorPosVisible;
          UpdateLastCaretX;

          { Mirror }
          if not fUndoRedo then
            MirrorCommand(Command, AChar, AData, Caret, Caret, Caret, nil);

          finally UndoList.EndBlock; end;
        end;

      { Tabbing }
      ecTab:
        if not fReadOnly then DoTabKey;
      ecHardTab:
        if not fReadOnly then DoTabKey(True);
      ecShiftTab:
        if not fReadOnly then DoShiftTabKey;
      ecChar:
        { #127 is Ctrl + Backspace }
        if not ReadOnly and ((AChar >= #32) or (AChar = #9)) and
          (AChar <> #127) then
        begin
          if SelAvail or fSelections then
            SetSelectedTextEmpty(AChar)
          else begin
            ExpandCollapsedLine(fCaretY);
            Caret := CaretXY;
            if fSnippet then
              fUndoList.BeginBlock;

            { Fetch a line }
            Temp := LineText;
            Len := Length(Temp);

            { Count, how many whitespace we should insert if caret is behind
              EOL }
            SpaceCount1 := 0;
            SpaceCount2 := 0;
            if Len < fCaretX - 1 then
            begin
              if eoTabsToSpaces in fOptions then
                SpaceBuffer := UnicodeString(StringOfChar(#32, fCaretX - Len - Ord(fInserting)))
              else
                if AllWhiteUpToCaret(Temp, Len) then
                  SpaceBuffer := UnicodeString(StringOfChar(#9, (fCaretX - Len - Ord(fInserting)) div fTabWidth) +
                    StringOfChar(#32, (fCaretX - Len - Ord(fInserting)) mod fTabWidth))
                else
                  SpaceBuffer := UnicodeString(StringOfChar(#32, fCaretX - Len - Ord(fInserting)));
              SpaceCount1 := Length(SpaceBuffer);
              SpaceCount2 := GetLeadingExpandedLength(SpaceBuffer, fTabWidth);
            end;

            { Added the check for whether or not we're in insert mode.
              If we are, we append one less space than we would in overwrite mode.
              This is because in overwrite mode we have to put in a final space
              character which will be overwritten with the typed character.  If we put the
              extra space in in insert mode, it would be left at the end of the line and
              cause problems unless eoTrimTrailingSpaces is set }

            { Remember caret }
            StartOfBlock := CaretXY;

            { We need to act different according to editing mode }
            if fInserting then
            begin
              { Cancel command if beyound viewport constraints }
              if not WordWrap and not (eoAutoSizeMaxScrollWidth in Options)
                 and (CaretX > MaxScrollWidth)
              then
                Exit;

              { If space was inserted }
              if SpaceCount1 > 0 then
                Temp := Temp + SpaceBuffer + AChar
              else
                Insert(AChar, Temp, fCaretX);

              { Do set line }
              ExpandLines[fCaretY - 1] := Temp;

              { Manage undo list }
              if SpaceCount1 > 0 then
              begin
                BeginUndoBlock;
                try
                  { Inserted whitespace }
                  fUndoList.AddChange(
                    crWhiteSpaceAdd,
                    BufferCoord(StartOfBlock.Char - SpaceCount2, fCaretY),
                    BufferCoord(Len + SpaceCount1 + 1, fCaretY),
                    CurrentSnippetCaret,
                    '',
                    smNormal
                  );

                  { Inserted char }
                  fUndoList.AddChange(
                    crInsert,
                    BufferCoord(Len + SpaceCount1 + 1, fCaretY),
                    BufferCoord(Len + SpaceCount1 + 2, fCaretY),
                    CurrentSnippetCaret,
                    '',
                    smNormal,
                    ExpandLines.GetLineStates(fCaretY - 1, fCaretY - 1)
                  );
                  ExpandLines.SetLineFlag(fCaretY - 1, STRING_REC_MODIFIED, True);

                  { Set new caret pos }
                  InternalCaretX := Len + SpaceCount1 + 2;
                finally
                  EndUndoBlock;
                end;
              end
              else begin
                { Add break after indenting }
                if bJustIndented then
                  fUndoList.AddGroupBreak;

                { Reflect our changes in undo list }
                fUndoList.AddChange(
                  crInsert,
                  StartOfBlock,
                  BufferCoord(fCaretX + 1, fCaretY),
                  CurrentSnippetCaret,
                  '',
                  smNormal,
                  ExpandLines.GetLineStates(fCaretY - 1, fCaretY - 1)
                );
                ExpandLines.SetLineFlag(fCaretY - 1, STRING_REC_MODIFIED, True);

                { Set new caret pos }
                InternalCaretX := fCaretX + 1;
              end;
            end

            { Processing of case character covers on LeadByte }
            else begin

              { Remember char being overwritten }
              if fCaretX <= Len then
                Helper := Copy(Temp, fCaretX, 1);

              { Set line }
              if fCaretX <= Len then
                Temp[fCaretX] := AChar
              else
                if SpaceCount1 > 0 then
                begin
                  SpaceBuffer[SpaceCount1] := AChar;
                  Temp := Temp + SpaceBuffer;
                end
                else
                  Temp := Temp + AChar;
              ExpandLines[fCaretY - 1] := Temp;

              { If space was inserted }
              if SpaceCount1 > 0 then
              begin
                BeginUndoBlock;
                try
                  { Inserted whitespace }
                  fUndoList.AddChange(
                    crWhiteSpaceAdd,
                    BufferCoord(StartOfBlock.Char - SpaceCount2 + 1, fCaretY),
                    BufferCoord(Len + SpaceCount1, fCaretY),
                    CurrentSnippetCaret,
                    '',
                    smNormal
                  );

                  { Added char }
                  fUndoList.AddChange(
                    crInsert,
                    BufferCoord(Len + SpaceCount1, fCaretY),
                    BufferCoord(Len + SpaceCount1 + 1, fCaretY),
                    CurrentSnippetCaret,
                    '',
                    smNormal,
                    ExpandLines.GetLineStates(fCaretY - 1, fCaretY - 1)
                  );
                  ExpandLines.SetLineFlag(fCaretY - 1, STRING_REC_MODIFIED, True);

                  { Set new caret pos }
                  InternalCaretX := Len + SpaceCount1 + 1;
                finally
                  EndUndoBlock;
                end;
              end
              else begin
                { Break after indenting }
                if bJustIndented then
                  fUndoList.AddGroupBreak;

                { Reflect our changes in undo list }
                fUndoList.AddChange(
                  crInsert,
                  StartOfBlock,
                  BufferCoord(fCaretX + 1, fCaretY),
                  CurrentSnippetCaret,
                  Helper,
                  smNormal,
                  ExpandLines.GetLineStates(fCaretY - 1, fCaretY - 1)
                );
                ExpandLines.SetLineFlag(fCaretY - 1, STRING_REC_MODIFIED, True);

                { Set new caret pos }
                InternalCaretX := fCaretX + 1;
              end;
            end;

            { Ensure cursor is visible }
            if CaretX >= LeftChar + fCharsInWindow then
              LeftChar := LeftChar + Min(25, fCharsInWindow - 1);

            { Mirror }
            InsertedInSnippet(Caret.Char, Caret.Line, fCaretX - Caret.Char, 0);
            MirrorCommand(Command, AChar, AData, Caret, Caret, Caret, nil);
            if fSnippet then
              fUndoList.EndBlock;
          end;
          DoOnPaintTransient(ttAfter);
        end;

      { Undo / Redo }
      ecUndo:
        if not ReadOnly then
        begin
          fUndoRedo := True;
          try
            Undo;

            { In snippet mode caret can end up in the mirror of a placeholder
              inside which redo command has been issued. We now need to place
              the caret inside the real placeholder of that mirror }
            MirrorCommand(ecGotoXY, #0, nil, CaretXY, BlockBegin, BlockEnd, nil);
          finally
            fUndoRedo := False;
          end;
        end;
      ecRedo:
        if not ReadOnly then
        begin
          fUndoRedo := True;
          try
            Redo;

            { In snippet mode caret can end up in the mirror of a placeholder
              inside which redo command has been issued. We now need to place
              the caret inside the real placeholder of that mirror }
            MirrorCommand(ecGotoXY, #0, nil, CaretXY, BlockBegin, BlockEnd, nil);
          finally
            fUndoRedo := False;
          end;
        end;

      { Clipboard }
      ecCut:
        if (not ReadOnly) and SelAvail then
          CutToClipboard;
      ecCopy:
        CopyToClipboard;
      ecPaste:
        if not ReadOnly then PasteFromClipboard;

      { Scrolling }
      ecScrollUp: TopLine := fTopLine - 1;
      ecScrollDown: TopLine := fTopLine + 1;
      ecScrollLeft: LeftChar := fLeftChar - 1;
      ecScrollRight: LeftChar := fLeftChar + 1;

      { Editing mode }
      ecInsertMode:
        InsertMode := True;
      ecOverwriteMode:
        InsertMode := False;
      ecToggleMode:
        InsertMode := not InsertMode;

      { Indentation }
      ecBlockIndent:
        if not ReadOnly then DoBlockIndent;
      ecBlockUnindent:
        if not ReadOnly then DoBlockUnindent;

      { Selection mode }
      ecNormalSelect:
        ActiveSelectionMode := smNormal;
      ecColumnSelect:
        ActiveSelectionMode := smColumn;
      ecLineSelect:
        ActiveSelectionMode := smLine;

      { Fires event with word under cursor }
      ecContextHelp:
        if Assigned(fOnContextHelp) then
          fOnContextHelp(Self, WordAtCursor);

      { IME }
      ecImeStr:
        if not ReadOnly then
        begin
          ExpandCollapsedLine(fCaretY);
          SetString(S, PChar(AData), WStrLen(AData));
          if SelAvail then
          begin
            BeginUndoBlock;
            try
              fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd,
                CurrentSnippetCaret, Helper, smNormal);
              StartOfBlock := fBlockBegin;
              SetSelTextPrimitive(S);
              fUndoList.AddChange(crInsert, fBlockBegin, fBlockEnd,
                CurrentSnippetCaret, Helper, smNormal);
            finally
              EndUndoBlock;
            end;
            InvalidateGutterLines(-1, -1);
          end
          else
          begin
            Temp := LineText;
            Len := Length(Temp);
            if Len < CaretX then
              Temp := Temp + UnicodeString(StringOfChar(#32, CaretX - Len - 1));
            bChangeScroll := not (eoScrollPastEol in fOptions);
            try
              if bChangeScroll then Include(fOptions, eoScrollPastEol);
              StartOfBlock := CaretXY;
              Len := Length(S);
              if not fInserting then
              begin
                Helper := Copy(Temp, CaretX, Len);
                Delete(Temp, CaretX, Len);
              end;
              Insert(S, Temp, CaretX);
              InternalCaretX := (CaretX + Len);
              ExpandLines[fCaretY - 1] := Temp;
              if fInserting then
                Helper := '';
              fUndoList.AddChange(crInsert, StartOfBlock, CaretXY,
                CurrentSnippetCaret, Helper, smNormal);
              if CaretX >= LeftChar + fCharsInWindow then
                LeftChar := LeftChar + min(25, fCharsInWindow - 1);
            finally
              if bChangeScroll then Exclude(fOptions, eoScrollPastEol);
            end;
          end;
        end;
    end;
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.DoOnCommandProcessed(Command: TSynEditorCommand;
  AChar: WideChar; Data: pointer);
begin
  if Assigned(fOnCommandProcessed) then
    fOnCommandProcessed(Self, Command, AChar, Data);
end;

procedure TCustomSynEdit.DoOnProcessCommand(var Command: TSynEditorCommand;
  var AChar: WideChar; Data: pointer);
begin
  if Command < ecUserCommandFirst then
  begin
    if Assigned(FOnProcessCommand) then
      FOnProcessCommand(Self, Command, AChar, Data);
  end
  else begin
    if Assigned(FOnProcessUserCommand) then
      FOnProcessUserCommand(Self, Command, AChar, Data);
  end;
end;

procedure TCustomSynEdit.ClearAll;
begin
  AllFoldRanges.ClearAll;
  UpdateWordWrapHiddenOffsets;
  fLines.Clear;
  fMarkList.Clear;
  FillChar(fBookMarks, SizeOf(fBookMarks), 0);
  fUndoList.Clear;
  fRedoList.Clear;
  fModified := False;
end;

procedure TCustomSynEdit.ClearSelection;
begin
  if SelAvail then
    SelText := '';
end;

function TCustomSynEdit.NextWordPosEx(XY: TBufferCoord): TBufferCoord;
var
  I: Integer;
  sLine: UnicodeString;
  bFound: Boolean;
begin
  { Fallback }
  Result := XY;

  { Go over this position }
  GetTokenKind(Self, XY);
  if not fHighlighter.GetEol then
  begin
    fHighlighter.Next;

    { Next token may be a word }
    XY.Char := fHighlighter.GetTokenPos;
  end;

  { Search till the end of file }
  while XY.Line <= fLines.Count do
  begin
    { Get line. We might need it }
    sLine := fLines.fList^[Pred(XY.Line)].fString;

    { Go until word }
    while not fHighlighter.GetEol and not (fHighlighter.GetTokenNature in [tknnIdent, tknnNumber]) do
    begin
      { Remember }
      XY.Char := fHighlighter.GetTokenPos;

      { Look for word chars inside separator token }
      if fHighlighter.GetTokenNature = tknnSeparator then
      begin
        bFound := False;
        for I := XY.Char to XY.Char + fHighlighter.GetTokenLen do
          if not SynEditTypes.IsWordBreakChar(sLine[Succ(I)]) then
          begin
            XY.Char := I;
            bFound := True;
            Break;
          end;
        if bFound then
          Break;
      end;

      { Next token }
      fHighlighter.Next;

      { Next token may be a word }
      XY.Char := fHighlighter.GetTokenPos;
    end;

    { Done? }
    if not fHighlighter.GetEol then
    begin
      with Result do
      begin
        Char := Succ(XY.Char);
        Line := XY.Line;
      end;
      Break;
    end;

    { Next line }
    Inc(XY.Line);
    XY.Char := 1;
    GetTokenKind(Self, XY);
  end;
end;

function TCustomSynEdit.WordStartEx(const XY: TBufferCoord): TBufferCoord;
var
  I: Integer;
  sLine: UnicodeString;
begin
  { Fallback }
  Result := XY;

  { Attempt to get word start }
  GetTokenKind(Self, XY);
  if fHighlighter.GetTokenNature in [tknnIdent, tknnNumber] then
    Result.Char := Succ(fHighlighter.GetTokenPos)
  else if fHighlighter.GetTokenNature = tknnSeparator then
  begin
    sLine := fLines.fList^[Pred(XY.Line)].fString;
    for I := Pred(XY.Char) downto fHighlighter.GetTokenPos do
      if not SynEditTypes.IsWordBreakChar(sLine[Succ(I)]) then
      begin
        Result.Char := Succ(I);
        Break;
      end;
  end;
end;

function TCustomSynEdit.WordEndEx(const XY: TBufferCoord): TBufferCoord;
var
  I: Integer;
  sLine: UnicodeString;
begin
  { Fallback }
  Result := XY;

  { Attempt to get word start }
  GetTokenKind(Self, XY);
  if fHighlighter.GetTokenNature in [tknnIdent, tknnNumber] then
    Result.Char := Succ(fHighlighter.GetTokenPos + fHighlighter.GetTokenLen)
  else if fHighlighter.GetTokenNature = tknnSeparator then
  begin
    sLine := fLines.fList^[Pred(XY.Line)].fString;
    for I := Pred(XY.Char) to fHighlighter.GetTokenPos + fHighlighter.GetTokenLen do
      if not SynEditTypes.IsWordBreakChar(sLine[Succ(I)]) then
      begin
        Result.Char := Succ(I);
        Break;
      end;
  end;
end;

function TCustomSynEdit.PrevWordPosEx(XY: TBufferCoord): TBufferCoord;
var
  I, LastWord: Integer;
  sLine: UnicodeString;
begin
  { Fallback }
  Result := XY;

  { Search till the end of file }
  XY.Char := 1;
  while XY.Line > 0 do
  begin
    { We might need a line }
    sLine := fLines.fList^[Pred(XY.Line)].fString;

    { Begin this line }
    GetTokenKind(Self, XY);

    { Prepare this line }
    LastWord := -1;

    { Go until word }
    while not fHighlighter.GetEol do
    begin
      if (XY.Line <> Result.Line) or ((XY.Line = Result.Line) and (Succ(fHighlighter.GetTokenPos) < Result.Char)) then
        if fHighlighter.GetTokenNature in [tknnIdent, tknnNumber] then
          LastWord := fHighlighter.GetTokenPos
        else if fHighlighter.GetTokenNature = tknnSeparator then
          for I := fHighlighter.GetTokenPos to fHighlighter.GetTokenPos + fHighlighter.GetTokenLen do
          begin
            if (XY.Line = Result.Line) and (Succ(I) >= Result.Char) then
              Break;
            if not SynEditTypes.IsWordBreakChar(sLine[Succ(I)]) then
            begin
              LastWord := I;
              Break;
            end;
          end;
      fHighlighter.Next;
    end;

    { Found? }
    if LastWord > -1 then
    begin
      with Result do
      begin
        Char := Succ(LastWord);
        Line := XY.Line;
      end;
      Break;
    end;

    { Next line }
    Dec(XY.Line);
  end;
end;

procedure TCustomSynEdit.SetSelectionMode(const Value: TSynSelectionMode);
begin
  if fSelectionMode <> Value then
  begin
    fSelectionMode := Value;
    ActiveSelectionMode := Value;
  end;
end;

procedure TCustomSynEdit.SetActiveSelectionMode(const Value: TSynSelectionMode);
begin
  if fActiveSelectionMode <> Value then
  begin
    if SelAvail then
      InvalidateSelection;
    fActiveSelectionMode := Value;
    if SelAvail then
      InvalidateSelection;
    StatusChanged([scSelection]);
  end;
end;

procedure TCustomSynEdit.BeginUndoBlock;
begin
  fUndoList.BeginBlock;
end;

procedure TCustomSynEdit.BeginUpdate;
begin
  IncPaintLock;
end;

procedure TCustomSynEdit.EndUndoBlock;
begin
  fUndoList.EndBlock;
end;

procedure TCustomSynEdit.EndUpdate;
begin
  DecPaintLock;
end;

procedure TCustomSynEdit.AddKey(Command: TSynEditorCommand;
  Key1: word; SS1: TSynShiftState; Key2: word; SS2: TSynShiftState);
var
  Key: TSynEditKeyStroke;
begin
  Key := Keystrokes.Add;
  Key.Command := Command;
  Key.Key := Key1;
  Key.Shift := SS1;
  Key.Key2 := Key2;
  Key.Shift2 := SS2;
end;

{ Called by FMarkList if change }
procedure TCustomSynEdit.MarkListChange(Sender: TObject);
begin
  InvalidateGutter;
end;

function TCustomSynEdit.GetSelStart: integer;
begin
  if GetSelAvail then
    Result := RowColToCharIndex(BlockBegin)
  else
    Result := RowColToCharIndex(CaretXY);
end;

procedure TCustomSynEdit.SetAlwaysShowCaret(const Value: Boolean);
begin
  if FAlwaysShowCaret <> Value then
  begin
    FAlwaysShowCaret := Value;
    if not(csDestroying in ComponentState) and  not(focused) then
    begin
      if Value then
      begin
        InitializeCaret;
      end
      else
      begin
        HideCaret;
        Windows.DestroyCaret;
      end;
    end;
  end;
end;

procedure TCustomSynEdit.SetSelStart(const Value: Integer);
begin
  { if we don't call HandleNeeded, CharsInWindow may be 0 and LeftChar will
  be set to CaretX }
  HandleNeeded;
  InternalCaretXY := CharIndexToRowCol(Value);
  BlockBegin := CaretXY;
end;

function TCustomSynEdit.GetSelEnd: Integer;
begin
  if GetSelAvail then
    Result := RowColToCharIndex(Blockend)
  else
    Result := RowColToCharIndex(CaretXY);
end;

procedure TCustomSynEdit.SetSelEnd(const Value: Integer);
begin
  HandleNeeded;
  BlockEnd := CharIndexToRowCol( Value );
end;

procedure TCustomSynEdit.SetSelWord;
begin
  SetWordBlock(CaretXY);
end;

procedure TCustomSynEdit.SetExtraLineSpacing(const Value: Integer);
begin
  fExtraLineSpacing := Value;
  SynFontChanged(self);
end;

function TCustomSynEdit.GetBookMark(BookMark: Integer; var X, Y: Integer):
  Boolean;
var
  i: Integer;
begin
  Result := False;
  if assigned(Marks) then
    for i := 0 to Marks.Count - 1 do
      if Marks[i].IsBookmark and (Marks[i].BookmarkNumber = BookMark) then
      begin
        X := Marks[i].Char;
        Y := Marks[i].Line;
        Result := True;
        Exit;
      end;
end;

function TCustomSynEdit.IsBookmark(BookMark: Integer): Boolean;
var
  x, y: Integer;
begin
  Result := GetBookMark(BookMark, x, y);
end;

procedure TCustomSynEdit.ClearUndo;
begin
  fUndoList.Clear;
  fRedoList.Clear;
end;

procedure TCustomSynEdit.SetSelTextExternal(const Value: UnicodeString);
var
  //SelMode: TSynSelectionMode;
  StartOfBlock, EndOfBlock: TBufferCoord;
  States: TLineStates;
begin
  if fLines.Count = 0 then
    fLines.Add('');
  BeginUndoBlock;
  try
    //SelMode := fActiveSelectionMode;
    if SelAvail then
    begin
      fUndoList.AddChange(
        crDelete,
        BlockBegin,
        BlockEnd,
        CurrentSnippetCaret,
        SelText,
        fActiveSelectionMode,
        fLines.GetLineStates(BlockBegin.Line - 1, BlockEnd.Line - 1)
      );
      SetLength(States, 0);
    end
    else begin
      ActiveSelectionMode := SelectionMode;
      States := fLines.GetLineStates(BlockBegin.Line - 1, BlockEnd.Line - 1);
    end;
    StartOfBlock := BlockBegin;
    EndOfBlock := BlockEnd;
    fBlockBegin := StartOfBlock;
    fBlockEnd := EndOfBlock;
    SetSelTextPrimitive(Value);
    EndOfBlock := BlockEnd;
    if (Value <> EmptyStr) and (fActiveSelectionMode <> smColumn) then
      fUndoList.AddChange(
        crInsert,
        StartOfBlock,
        EndOfBlock,
        CurrentSnippetCaret,
        EmptyStr,
        fActiveSelectionMode,
        States
      );
  finally
    EndUndoBlock;
  end;
end;

procedure TCustomSynEdit.SetGutter(const Value: TSynGutter);
begin
  fGutter.Assign(Value);
end;

procedure TCustomSynEdit.GutterChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
  begin
    if fOldGutterWidth <> fGutter.Width then
    begin
      { Get new gutter width }
      fOldGutterWidth := fGutter.Width;

      { Find new offset in centered mode }
      if eoWrapCentered in fOptions then
        fCenterOffset := Max((ClientWidth - 3 - (fGutter.Width * Ord(Gutter.Visible)) - (fCharWidth * fRightEdge)), 0) shr 1
      else
        fCenterOffset := 0;

      { Set text offset }
      fTextOffset := fCenterOffset + fOldGutterWidth + 2 - (LeftChar - 1) * fCharWidth;

      { fCharsInWindow has been changed }
      UpdateCharsInWindow;
    end
    else if HandleAllocated then
      InvalidateGutter;
  end;
end;

procedure TCustomSynEdit.LockUndo;
begin
  fUndoList.Lock;
  fRedoList.Lock;
end;

procedure TCustomSynEdit.UnlockUndo;
begin
  fUndoList.Unlock;
  fRedoList.Unlock;
end;

procedure TCustomSynEdit.SetTabWidth(Value: Integer);
begin
  Value := MinMax(Value, 1, 16);
  if (Value <> fTabWidth) then
  begin
    fTabWidth := Value;
    Lines.TabWidth := Value;
    if WordWrap then
      fWordWrapPlugin.Reset;
    Invalidate;
  end;
end;

procedure TCustomSynEdit.SelectedColorsChanged(Sender: TObject);
begin
  InvalidateSelection;
end;

// find / replace

function TCustomSynEdit.SearchReplace(const ASearch, AReplace: UnicodeString;
  AOptions: TSynSearchOptions): Integer;
var
  ptStart, ptEnd: TBufferCoord; // Start and end of the search range
  ptCurrent: TBufferCoord;      // Current search position
  nSearchLen, nReplaceLen, n, nFound: integer;
  nInLine: integer;
  bBackward, bFromCursor: boolean;
  bPrompt: boolean;
  bReplace, bReplaceAll: boolean;
  bEndUndoBlock: boolean;
  nAction: TSynReplaceAction;
  iResultOffset: Integer;

  function InValidSearchRange(First, Last: Integer): Boolean;
  begin
    Result := True;
    if (fActiveSelectionMode = smNormal) or not (ssoSelectedOnly in AOptions) then
    begin
      if ((ptCurrent.Line = ptStart.Line) and (First < ptStart.Char)) or
        ((ptCurrent.Line = ptEnd.Line) and (Last > ptEnd.Char))
      then
        Result := False;
    end
    else
    if (fActiveSelectionMode = smColumn) then
      // solves bug in search/replace when smColumn mode active and no selection
      Result := (First >= ptStart.Char) and (Last <= ptEnd.Char) or (ptEnd.Char - ptStart.Char < 1);
  end;

begin
  { Initialize }
  Result := 0;

  { Check }
  if not Assigned(fSearchEngine) then Exit;
  if Length(ASearch) = 0 then Exit;

  { Get the text range to search in, ignore the "Search in selection only"
    option if nothing is selected }
  bBackward := (ssoBackwards in AOptions);
  bPrompt := (ssoPrompt in AOptions);
  bReplace := (ssoReplace in AOptions);
  bReplaceAll := (ssoReplaceAll in AOptions);
  bFromCursor := not (ssoEntireScope in AOptions);
  if not SelAvail then Exclude(AOptions, ssoSelectedOnly);
  if (ssoSelectedOnly in AOptions) then
  begin
    ptStart := BlockBegin;
    ptEnd := BlockEnd;

    { Search the whole line in the line selection mode }
    if (fActiveSelectionMode = smLine) then
    begin
      ptStart.Char := 1;
      ptEnd.Char := Length(Lines[ptEnd.Line - 1]) + 1;
    end

    { Column mode }
    else if (fActiveSelectionMode = smColumn) then
      { Ignore the cursor position when searching in the selection }
      if bBackward then
        ptCurrent := ptEnd
      else
        ptCurrent := ptStart;
  end
  else begin
    ptStart.Char := 1;
    ptStart.Line := 1;
    ptEnd.Line := Lines.Count;
    ptEnd.Char := Length(Lines[ptEnd.Line - 1]) + 1;
    if bFromCursor then
      if bBackward then ptEnd := CaretXY else ptStart := CaretXY;
    if bBackward then ptCurrent := ptEnd else ptCurrent := ptStart;
  end;

  { Initialize the search engine }
  fSearchEngine.Options := AOptions;
  fSearchEngine.Pattern := ASearch;

  { Search while the current search position is inside of the search range }
  nReplaceLen := 0;
  //DoOnPaintTransient(ttBefore);

  { Need to prepare undo block? }
  if bReplaceAll and not bPrompt then
  begin
    IncPaintLock;
    BeginUndoBlock;
    bEndUndoBlock := True;
  end
  else
    bEndUndoBlock := False;

  { Search while in range }
  try
    while (ptCurrent.Line >= ptStart.Line) and (ptCurrent.Line <= ptEnd.Line) do
    begin
      if bReplace or bReplaceAll then
        fSearchEngine.ReplacePattern := AReplace;
      nInLine := fSearchEngine.FindAll(fLines.fList[ptCurrent.Line - 1].fString);
      iResultOffset := 0;
      if bBackward then
        n := Pred(fSearchEngine.ResultCount)
      else
        n := 0;

      { Operate on all results in this line }
      while nInLine > 0 do
      begin
        // An occurrence may have been replaced with a text of different length
        nFound := fSearchEngine.Results[n] + iResultOffset;
        nSearchLen := fSearchEngine.Lengths[n];
        if bBackward then Dec(n) else Inc(n);
        Dec(nInLine);
        // Is the search result entirely in the search range?
        if not InValidSearchRange(nFound, nFound + nSearchLen) then Continue;
        Inc(Result);
        // Select the text, so the user can see it in the OnReplaceText event
        // handler or as the search result.

        ptCurrent.Char := nFound;
        BlockBegin := ptCurrent;
        // Be sure to use the Ex version of CursorPos so that it appears in the middle if necessary
        SetCaretXYEx(False, BufferCoord(1, ptCurrent.Line));
        EnsureCursorPosVisibleEx(True);
        Inc(ptCurrent.Char, nSearchLen);
        BlockEnd := ptCurrent;
        InternalCaretXY := ptCurrent;
        if bBackward then InternalCaretXY := BlockBegin else InternalCaretXY := ptCurrent;
        // If it's a search only we can leave the procedure now.
        if not (bReplace or bReplaceAll) then exit;
        // Prompt and replace or replace all.  If user chooses to replace
        // all after prompting, turn off prompting.
        if bPrompt and Assigned(fOnReplaceText) then
        begin
          nAction := DoOnReplaceText(ASearch, AReplace, ptCurrent.Line, nFound);
          if nAction = raCancel then
            exit;
        end
        else
          nAction := raReplace;
        if nAction <> raSkip then
        begin
          // user has been prompted and has requested to silently replace all
          // so turn off prompting
          if nAction = raReplaceAll then begin
            if not bReplaceAll or bPrompt then
            begin
              bReplaceAll := True;
              IncPaintLock;
            end;
            bPrompt := False;
            if bEndUndoBlock = false then
              BeginUndoBlock;
            bEndUndoBlock:= true;
          end;
          // Allow advanced substition in the search engine
          if bBackward then
            SelText := fSearchEngine.Replace(SelText, AReplace, Succ(n))
          else
            SelText := fSearchEngine.Replace(SelText, AReplace, Pred(n));
          nReplaceLen := CaretX - nFound;
        end;
        // fix the caret position and the remaining results
        if not bBackward then begin
          InternalCaretX := nFound + nReplaceLen;
          if (nSearchLen <> nReplaceLen) and (nAction <> raSkip) then
          begin
            Inc(iResultOffset, nReplaceLen - nSearchLen);
            if (fActiveSelectionMode <> smColumn) and (CaretY = ptEnd.Line) then
            begin
              Inc(ptEnd.Char, nReplaceLen - nSearchLen);
              BlockEnd := ptEnd;
            end;
          end;
        end;
        if not bReplaceAll then
          exit;
      end;

      { Search next / previous line }
      if bBackward then
        Dec(ptCurrent.Line)
      else
        Inc(ptCurrent.Line);
    end;
  finally
    if bReplaceAll and not bPrompt then DecPaintLock;
    if bEndUndoBlock then EndUndoBlock;
    //DoOnPaintTransient(ttAfter);
  end;
end;

function TCustomSynEdit.IsPointInSelection(const Value: TBufferCoord): boolean;
var
  ptBegin, ptEnd: TBufferCoord;
begin
  ptBegin := BlockBegin;
  ptEnd := BlockEnd;
  if (Value.Line >= ptBegin.Line) and (Value.Line <= ptEnd.Line) and
    ((ptBegin.Line <> ptEnd.Line) or (ptBegin.Char <> ptEnd.Char)) then
  begin
    if fActiveSelectionMode = smLine then
      Result := True
    else if (fActiveSelectionMode = smColumn) then
    begin
      if (ptBegin.Char > ptEnd.Char) then
        Result := (Value.Char >= ptEnd.Char) and (Value.Char < ptBegin.Char)
      else if (ptBegin.Char < ptEnd.Char) then
        Result := (Value.Char >= ptBegin.Char) and (Value.Char < ptEnd.Char)
      else
        Result := False;
    end
    else
      Result := ((Value.Line > ptBegin.Line) or (Value.Char >= ptBegin.Char)) and
        ((Value.Line < ptEnd.Line) or (Value.Char < ptEnd.Char));
  end
  else
    Result := False;
end;
{
procedure TCustomSynEdit.SetFocus;
begin
  if (fFocusList.Count > 0) then
  begin
    TWinControl(fFocusList.Last).SetFocus;
    exit;
  end;
  inherited;
end;
}
procedure TCustomSynEdit.UpdateMouseCursor;
var
  ptCursor: TPoint;
  ptLineCol: TBufferCoord;
  iNewCursor: TCursor;
  bAlt: Boolean;
begin
  { On gutter? }
  GetCursorPos(ptCursor);
  ptCursor := ScreenToClient(ptCursor);
  if (ptCursor.X < Gutter.Width) then
    SetCursor(Screen.Cursors[fGutter.Cursor])

  { In editor }
  else begin

    { Moving right edge or above it }
    if fRightEdgeMoving or fMouseAtRightEdge then
      iNewCursor := crSizeWE

    { Showing folding hint }
    else if fCodeFoldingHint then
      iNewCursor := crHelp

    { Dragging? }
    else begin
      ptLineCol := DisplayToBufferPos(PixelsToRowColumn(ptCursor.X, ptCursor.Y));
      if (eoDragDropEditing in fOptions) and (not MouseCapture) and
        IsPointInSelection(ptLineCol)
      then
        iNewCursor := crArrow
      else
        iNewCursor := Cursor;
    end;

    { User override }
    if Assigned(OnMouseCursor) then
      OnMouseCursor(Self, ptLineCol, iNewCursor);

    { Set cursor }
    fKbdHandler.ExecuteMouseCursor(Self, ptLineCol, iNewCursor);
    SetCursor(Screen.Cursors[iNewCursor]);
  end;
end;

function TCustomSynEdit.ValidSnippetCmd(Command: TSynEditorCommand): Boolean;
const
  ValidSnippetCommands: array[0..18] of Integer = (
    ecCut,
    ecCopy,
    ecPaste,
    ecLineBreak,
    ecChar,
    ecDeleteChar,
    ecDeleteLastchar,
    ecLeft,
    ecUp,
    ecRight,
    ecDown,
    ecSelLeft,
    ecSelUp,
    ecSelRight,
    ecSelDown,
    ecUndo,
    ecRedo,
    ecGotFocus,
    ecLostFocus
  );
var
  I: Integer;
begin
  Result := False;
  for I := Low(ValidSnippetCommands) to High(ValidSnippetCommands) do
    if Command = ValidSnippetCommands[I] then
    begin
      Result := True;
      Break;
    end;
end;

procedure TCustomSynEdit.EnterColumnMode;
var
  I, nRow: Integer;
  B1, B2: TBufferCoord;
  D1, D2: TDisplayCoord;
begin
  if fColumn then
    Exit;
  if fActiveSelectionMode = smColumn then
  begin
    B1 := BlockBegin;
    B2 := BlockEnd;
    D1 := BufferToDisplayPos(B1);
    D2 := BufferToDisplayPos(B2);
    if D1.Column = D2.Column then
    begin
      SetLength(fCarets, Succ(B2.Line - B1.Line));
      for I := B1.Line to B2.Line do
        with fCarets[I - B1.Line] do
        begin
          nIndex := 1;
          if I = B1.Line then
            bMirror := False
          else
            bMirror := True;

          nRow := BufferToDisplayPos(BufferCoord(1, I)).Row;
          bcStart := BufferCoord(DisplayToBufferPos(DisplayCoord(D1.Column, nRow)).Char, I);
          bcEnd := bcStart;

          sSearchPattern := EmptyAnsiStr;
          sReplacePattern := EmptyAnsiStr;
          ePatternOptions := [];
          sShellCommand := EmptyAnsiStr;
        end;
      fSnippet := True;
      fColumn := True;
      SelectionMode := smNormal;
      ActiveSelectionMode := smNormal;
      fSmartCaretsUpdating := True;
      SetCaretAndSelection(fCarets[0].bcStart, fCarets[0].bcStart, fCarets[0].bcStart);
      fSmartCaretsUpdating := False;
    end;
  end;
end;

function TCustomSynEdit.GetExpandLines: TSynEditStringList;
begin
  Result := TSynEditStringList(fLines);
end;

procedure TCustomSynEdit.SetExpandLines(const Value: TSynEditStringList);
begin
  Lines := Value;
end;

procedure TCustomSynEdit.BookMarkOptionsChanged(Sender: TObject);
begin
  InvalidateGutter;
end;

function TCustomSynEdit.GetOptions: TSynEditorOptions;
begin
  Result := fOptions;
end;

procedure TCustomSynEdit.SetOptions(Value: TSynEditorOptions);
const
  ScrollOptions = [eoHideShowScrollbars, eoScrollPastEof, eoScrollPastEol];
var
  bSetDrag, bSetWrap, bSetMargin, bSetCentered, bSetInvisibles: Boolean;
  bUpdateScroll: Boolean;
  vTempBlockBegin, vTempBlockEnd: TBufferCoord;
begin
  if (Value <> fOptions) then
  begin
    bSetDrag := (eoDropFiles in fOptions) <> (eoDropFiles in Value);
    bSetWrap := ((eoWrapAgainstMargin in fOptions) <> (eoWrapAgainstMargin in Value)) or
      ((eoAlignedWrap in fOptions) <> (eoAlignedWrap in Value));
    bSetMargin := (eoHighlightMargin in fOptions) <> (eoHighlightMargin in Value);
    bSetCentered := (eoWrapCentered in fOptions) <> (eoWrapCentered in Value);
    bSetInvisibles := ((eoShowSpecialChars in fOptions) <> (eoShowSpecialChars in Value))
      or ((eoShowSpecialcharsInSelection in fOptions) <> (eoShowSpecialcharsInSelection in Value))
      or ((eoShowEolSpecialChar in fOptions) <> (eoShowEolSpecialChar in Value));

    if not (eoScrollPastEol in Options) then
      LeftChar := LeftChar;
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;

    bUpdateScroll := (Options * ScrollOptions) <> (Value * ScrollOptions);

    fOptions := Value;

    { Constrain caret position to MaxScrollWidth if eoScrollPastEol is enabled }
    InternalCaretXY := CaretXY;
    if (eoScrollPastEol in Options) then
    begin
      vTempBlockBegin := BlockBegin;
      vTempBlockEnd := BlockEnd;
      SetBlockBegin(vTempBlockBegin);
      SetBlockEnd(vTempBlockEnd);
    end;

    { (Un)register HWND as drop target }
    if bSetDrag and not (csDesigning in ComponentState) and HandleAllocated then
      DragAcceptFiles(Handle, (eoDropFiles in fOptions));

    { Rewrap }
    if WordWrap and bSetWrap then
      fWordWrapPlugin.Reset;

    { Recalculate center offset }
    if bSetCentered then
    begin
      if eoWrapCentered in fOptions then
        fCenterOffset := Max((ClientWidth - 3 - (fGutter.Width * Ord(Gutter.Visible)) - (fCharWidth * fRightEdge)), 0) shr 1
      else
        fCenterOffset := 0;
      fTextOffset := fCenterOffset + fOldGutterWidth + 2;
    end;

    if bSetWrap or bSetMargin or bSetCentered or bSetInvisibles then
      Invalidate;

    if bUpdateScroll or bSetWrap then
      UpdateScrollBars;
  end;
end;

procedure TCustomSynEdit.SizeOrFontChanged(bFont: boolean);
begin
  if HandleAllocated and (fCharWidth <> 0) then
  begin
    if eoWrapCentered in fOptions then
      fCenterOffset := Max((ClientWidth - 3 - (fGutter.Width * Ord(Gutter.Visible)) - (fCharWidth * fRightEdge)), 0) shr 1
    else
      fCenterOffset := 0;
    fTextOffset := fCenterOffset + fGutter.Width + 2 - (fLeftChar - 1) * fCharWidth;
    fCharsInWindow := Max(ClientWidth - fGutter.Width - 2, 0) div fCharWidth;
    fLinesInWindow := ClientHeight div fTextHeight;
    if WordWrap or bFont then
    begin
      if WordWrap then
        fWordWrapPlugin.DisplayChanged;
      if bFont then
      begin
        InitializeCaret;
        Exclude(fStateFlags, sfCaretChanged);
      end;
      Invalidate;
    end;
    UpdateScrollBars;
    Exclude(fStateFlags, sfScrollbarChanged);
    if not (eoScrollPastEol in Options) then
      LeftChar := LeftChar;
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;
  end;
end;

procedure TCustomSynEdit.MoveSelections(const DX, DY: Integer;
  const SelectionCommand: Boolean);
var
  I, J: Integer;
begin
  { Check }
  if not fSelections then Exit;

  { Work with all carets }
  for I := 0 to High(fCarets) do
  begin

    { Extend selection? }
    if SelectionCommand then
    begin

      { Extend X coord }
      if DX > 0 then
        if CaretsEqual(fCarets[I].bcCaret, fCarets[I].bcEnd) then
        begin
          Inc(fCarets[I].bcEnd.Char, DX);
          Inc(fCarets[I].bcCaret.Char, DX);
        end
        else begin
          Inc(fCarets[I].bcStart.Char, DX);
          Inc(fCarets[I].bcCaret.Char, DX);
        end
      else if DX < 0 then
        if CaretsEqual(fCarets[I].bcCaret, fCarets[I].bcStart) then
        begin
          Inc(fCarets[I].bcStart.Char, DX);
          Inc(fCarets[I].bcCaret.Char, DX);
        end
        else begin
          Inc(fCarets[I].bcEnd.Char, DX);
          Inc(fCarets[I].bcCaret.Char, DX);
        end;

      { Extend Y coord }
      if DY > 0 then
        if fCarets[I].bcCaret.Line = fCarets[I].bcEnd.Line then
        begin
          Inc(fCarets[I].bcEnd.Line, DY);
          Inc(fCarets[I].bcCaret.Line, DY);
        end
        else begin
          Inc(fCarets[I].bcStart.Line, DY);
          Inc(fCarets[I].bcCaret.Line, DY);
        end
      else
        if fCarets[I].bcCaret.Line = fCarets[I].bcStart.Line then
        begin
          Inc(fCarets[I].bcStart.Line, DY);
          Inc(fCarets[I].bcCaret.Line, DY);
        end
        else begin
          Inc(fCarets[I].bcEnd.Line, DY);
          Inc(fCarets[I].bcCaret.Line, DY);
        end;
    end

    { Simple movement: move and seize blocks to carets }
    else begin
      Inc(fCarets[I].bcCaret.Char, DX);
      Inc(fCarets[I].bcCaret.Line, DY);
      fCarets[I].bcStart := fCarets[I].bcCaret;
      fCarets[I].bcEnd := fCarets[I].bcCaret;
    end;

    { Restore old caret X when moving across lines }
    if (DY <> 0) and (DX = 0) then
    begin
      if CaretsEqual(fCarets[I].bcCaret, fCarets[I].bcStart) then
        fCarets[I].bcStart.Char := fCarets[I].nOldCaretX;
      if CaretsEqual(fCarets[I].bcCaret, fCarets[I].bcEnd) then
        fCarets[I].bcEnd.Char := fCarets[I].nOldCaretX;
      fCarets[I].bcCaret.Char := fCarets[I].nOldCaretX;
    end;

    { Ensure all line bounds are valid }
    fCarets[I].bcCaret.Line := MinMax(fCarets[I].bcCaret.Line, 1, fLines.Count);
    fCarets[I].bcStart.Line := MinMax(fCarets[I].bcStart.Line, 1, fLines.Count);
    fCarets[I].bcEnd.Line := MinMax(fCarets[I].bcEnd.Line, 1, fLines.Count);

    { Ensure caret X coord is valid }
    while True do
    begin
      J := Succ(fLines.AccessStringLength(Pred(fCarets[I].bcCaret.Line)));

      if (DY <> 0) or ((fCarets[I].bcCaret.Char > 0) and
        (fCarets[I].bcCaret.Char <= J))
      then
        Break;

      if fCarets[I].bcCaret.Char < 1 then
      begin
        if fCarets[I].bcCaret.Line = 1 then
          Break;
        Inc(fCarets[I].bcCaret.Char, Succ(fLines.AccessStringLength(fCarets[I].bcCaret.Line - 2)));
        Dec(fCarets[I].bcCaret.Line);
      end
      else if fCarets[I].bcCaret.Char > J then
      begin
        if fCarets[I].bcCaret.Line = fLines.Count then
          Break;
        Dec(fCarets[I].bcCaret.Char, J);
        Inc(fCarets[I].bcCaret.Line);
      end;
    end;
    fCarets[I].bcCaret.Char := Max(Min(fCarets[I].bcCaret.Char, J), 1);

    { Ensure start X coord is valid }
    while True do
    begin
      J := Succ(fLines.AccessStringLength(Pred(fCarets[I].bcStart.Line)));

      if (DY <> 0) or ((fCarets[I].bcStart.Char > 0) and
        (fCarets[I].bcStart.Char <= J))
      then
        Break;

      if fCarets[I].bcStart.Char < 1 then
      begin
        if fCarets[I].bcStart.Line = 1 then
          Break;
        Inc(fCarets[I].bcStart.Char, Succ(fLines.AccessStringLength(fCarets[I].bcStart.Line - 2)));
        Dec(fCarets[I].bcStart.Line);
      end
      else if fCarets[I].bcStart.Char > J then
      begin
        if fCarets[I].bcStart.Line = fLines.Count then
          Break;
        Dec(fCarets[I].bcStart.Char, J);
        Inc(fCarets[I].bcStart.Line);
      end;
    end;
    fCarets[I].bcStart.Char := Max(Min(fCarets[I].bcStart.Char, J), 1);

    { Ensure end X coord is valid }
    while True do
    begin
      J := Succ(fLines.AccessStringLength(Pred(fCarets[I].bcEnd.Line)));

      if (DY <> 0) or ((fCarets[I].bcEnd.Char > 0) and
        (fCarets[I].bcEnd.Char <= J))
      then
        Break;

      if fCarets[I].bcEnd.Char < 1 then
      begin
        if fCarets[I].bcEnd.Line = 1 then
          Break;
        Inc(fCarets[I].bcEnd.Char, Succ(fLines.AccessStringLength(fCarets[I].bcEnd.Line - 2)));
        Dec(fCarets[I].bcEnd.Line);
      end
      else if fCarets[I].bcEnd.Char > J then
      begin
        if fCarets[I].bcEnd.Line = fLines.Count then
          Break;
        Dec(fCarets[I].bcEnd.Char, J);
        Inc(fCarets[I].bcEnd.Line);
      end;
    end;
    fCarets[I].bcEnd.Char := Max(Min(fCarets[I].bcEnd.Char, J), 1);

    { Always reset old caret X when moving horizontally }
    if DX <> 0 then
      fCarets[I].nOldCaretX := fCarets[I].bcCaret.Char;
  end;

  { Repaint all client area }
  NormalizeSelections;
  Invalidate;
end;

// -----------------------------------------------------------------------------
// § Garnet
// Trim trailing whitespace as user "navigates" in file
procedure TCustomSynEdit.MoveCaretHorz(DX: Integer; SelectionCommand: Boolean);
var
  ptO, ptDst: TBufferCoord;
  nLineLen, nBorder: Integer;
  bChangeY: Boolean;
  vCaretRowCol: TDisplayCoord;
begin
  if fSelections then
  begin
    IncPaintLock;
    try
      MoveSelections(DX, 0, SelectionCommand);
      if fSelections then
        InternalCaretXY := GetCaretByIndex(1);
      Include(fStateFlags, sfCaretChanged);
    finally
      DecPaintLock;
    end;
    Exit;
  end;

  if WordWrap then
  begin
    if DX > 0 then
    begin
      if fCaretAtEOL then
      begin
        fCaretAtEOL := False;
        UpdateLastCaretX;
        IncPaintLock;
        Include(fStateFlags, sfCaretChanged);
        DecPaintLock;
        Exit;
      end;
    end

    { DX < 0. Handle ecLeft/ecPageLeft at BOL }
    else begin
      if (not fCaretAtEOL) and (CaretX > 1) and (DisplayX = 1) then
      begin
        fCaretAtEOL := True;
        UpdateLastCaretX;
        if eoWrapAgainstMargin in fOptions then
          nBorder := fRightEdge
        else
          nBorder := fCharsInWindow - 1;
        if DisplayX > nBorder + 1 then
          SetInternalDisplayXY(DisplayCoord(nBorder + 1, DisplayY))
        else begin
          IncPaintLock;
          Include(fStateFlags, sfCaretChanged);
          DecPaintLock;
        end;
        Exit;
      end;
    end;
  end;

  ptO := CaretXY;
  ptDst := ptO;

  { Get line buffers }
  nLineLen := fLines.AccessStringLength(fCaretY - 1);

  { Only moving or selecting one char can change the line }
  bChangeY := not (eoScrollPastEol in fOptions);

  { Should appear at EOL of previous line? }
  if bChangeY and (DX = -1) and (ptO.Char = 1) and (ptO.Line > 1) then
    with ptDst do
    begin
      Line := RowToLine(LineToRow(Line) - 1);
      Char := ExpandLines.AccessStringLength(Line - 1) + 1;
    end

  { Should appear at BOL of next line? }
  else if bChangeY and (DX = 1) and (ptO.Char > nLineLen) and (ptO.Line < Lines.Count) then
    with ptDst do
    begin
      Line := RowToLine(LineToRow(ptDst.Line)+1);
      Char := 1;
    end

  else begin
    ptDst.Char := Max(1, ptDst.Char + DX);

    { Don't go past last char when ScrollPastEol option not set }
    if (DX > 0) and bChangeY then
      ptDst.Char := Min(ptDst.Char, nLineLen + 1);
  end;

  { Trim source and destination lines }
  if not SelectionCommand and (ptDst.Line <> ptO.Line) then
  begin
    DoTrimTrailingSpaces(ptO.Line);
    DoTrimTrailingSpaces(ptDst.Line);
  end;

  { Set caret and block begin / end }
  MoveCaretAndSelection(fBlockBegin, ptDst, SelectionCommand);

  { If caret is beyond CharsInWindow move to next row (this means there are
    spaces/tabs at the end of the row) }
  if WordWrap and (DX > 0) and (CaretX < Length(LineText)) then
  begin
    vCaretRowCol := DisplayXY;
    if (vCaretRowCol.Column = 1) and (LineToRow(CaretY) <> vCaretRowCol.Row) then
    begin
      fCaretAtEOL := True;
      UpdateLastCaretX;
    end
    else if vCaretRowCol.Column > CharsInWindow +1 then
    begin
      Inc(vCaretRowCol.Row);
      vCaretRowCol.Column := 1;
      InternalCaretXY := DisplayToBufferPos(vCaretRowCol);
    end;
  end;
end;

procedure TCustomSynEdit.MoveCaretVert(DY: Integer; SelectionCommand: Boolean);
var
  ptO, ptDst, vEOLTestPos: TDisplayCoord;
  vDstLineChar: TBufferCoord;
  SaveLastCaretX: Integer;
begin
  if fSelections then
  begin
    IncPaintLock;
    try
      MoveSelections(0, DY, SelectionCommand);
      InternalCaretXY := GetCaretByIndex(1, True);
      Include(fStateFlags, sfCaretChanged);
    finally
      DecPaintLock;
    end;
    Exit;
  end;

  ptO := DisplayXY;
  ptDst := ptO;

  Inc(ptDst.Row, DY);
  if DY >= 0 then
  begin
    if RowToLine(ptDst.Row) > Lines.Count then
      ptDst.Row := Max(1, DisplayLineCount);
  end
  else begin
    if ptDst.Row < 1 then
      ptDst.Row := 1;
  end;

  if (ptO.Row <> ptDst.Row) then
  begin
    if eoKeepCaretX in Options then
      ptDst.Column := fLastCaretX;
  end;
  vDstLineChar := DisplayToBufferPos(ptDst);
  SaveLastCaretX := fLastCaretX;

  if not SelectionCommand and (vDstLineChar.Line <> fBlockBegin.Line) then
  begin
    DoTrimTrailingSpaces(fBlockBegin.Line);
    DoTrimTrailingSpaces(vDstLineChar.Line);
  end;

  { Set caret and block begin / end }
  IncPaintLock;
  MoveCaretAndSelection(fBlockBegin, vDstLineChar, SelectionCommand);
  if WordWrap then
  begin
    vEOLTestPos := BufferToDisplayPos(vDstLineChar);
    fCaretAtEOL := (vEOLTestPos.Column = 1) and (vEOLTestPos.Row <> ptDst.Row);
  end;
  DecPaintLock;

  { Restore fLastCaretX after moving caret, since UpdateLastCaretX, called by
    SetCaretXYEx, changes them. This is the one case where we don't want that }
  fLastCaretX := SaveLastCaretX;
end;

procedure TCustomSynEdit.MoveCaretAndSelection(const ptBefore, ptAfter: TBufferCoord;
  SelectionCommand: Boolean);
begin
  if (eoGroupUndo in FOptions) and UndoList.CanUndo then
    fUndoList.AddGroupBreak;

  IncPaintLock;
  if SelectionCommand then
  begin
    if not SelAvail then
      SetBlockBegin(ptBefore);
    SetBlockEnd(ptAfter);
  end
  else
    SetBlockBegin(ptAfter);
  InternalCaretXY := ptAfter;
  DecPaintLock;
end;

procedure TCustomSynEdit.SetCaretAndSelection(const ptCaret, ptBefore,
  ptAfter: TBufferCoord; aCaret: Integer = -1);
var
  vOldMode: TSynSelectionMode;
begin
  { Work with selections }
  if fSnippet and fSelections and (aCaret > -1) then
    if not CaretsEqual(ptCaret, fCarets[aCaret].bcStart) then
    begin
      fCarets[aCaret].bcStart := ptCaret;
      fCarets[aCaret].bcCaret := ptCaret;
    end;

  { Do set values }
  vOldMode := fActiveSelectionMode;
  IncPaintLock;
  try
    InternalCaretXY := ptCaret;
    SetBlockBegin(ptBefore);
    SetBlockEnd(ptAfter);
  finally
    ActiveSelectionMode := vOldMode;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.RecalcCharExtent;
const
  iFontStyles: array[0..3] of TFontStyles = ([], [fsItalic], [fsBold],
    [fsItalic, fsBold]);
var
  iHasStyle: array[0..3] of Boolean;
  cAttr: Integer;
  cStyle: Integer;
  iCurr: TFontStyles;
begin
  FillChar(iHasStyle, SizeOf(iHasStyle), 0);
  if Assigned(fHighlighter) and (fHighlighter.AttrCount > 0) then begin
    for cAttr := 0 to fHighlighter.AttrCount - 1 do
    begin
      iCurr := fHighlighter.Attribute[cAttr].Style * [fsItalic, fsBold];
      for cStyle := 0 to 3 do
        if iCurr = iFontStyles[cStyle] then
        begin
          iHasStyle[cStyle] := True;
          break;
        end;
    end;
  end
  else begin
    iCurr := Font.Style * [fsItalic, fsBold];
    for cStyle := 0 to 3 do
      if iCurr = iFontStyles[cStyle] then
      begin
        iHasStyle[cStyle] := True;
        break;
      end;
  end;

  fTextHeight := 0;
  fCharWidth := 0;
  fTextDrawer.BaseFont := Self.Font;
  for cStyle := 0 to 3 do
    if iHasStyle[cStyle] then
    begin
      fTextDrawer.BaseStyle := iFontStyles[cStyle];
      fTextHeight := Max(fTextHeight, fTextDrawer.CharHeight);
      fCharWidth := Max(fCharWidth, fTextDrawer.CharWidth);
    end;
  Inc(fTextHeight, fExtraLineSpacing);
end;

procedure TCustomSynEdit.HighlighterAttrChanged(Sender: TObject);
begin
  RecalcCharExtent;
  if Sender is TSynCustomHighlighter then
  begin
    Lines.BeginUpdate;
    try
      ScanRanges;
    finally
      Lines.EndUpdate;
    end;
  end
  else
    Invalidate;
  SizeOrFontChanged(True);
end;

procedure TCustomSynEdit.StatusChanged(AChanges: TSynStatusChanges);
begin
  fStatusChanges := fStatusChanges + AChanges;
  if PaintLock = 0 then
    DoOnStatusChange(fStatusChanges);
end;

procedure TCustomSynEdit.SetBreakWhitespace(Value: Boolean);
begin
  if fBreakWhitespace <> Value then
  begin
    fBreakWhitespace := Value;
    if WordWrap then
    begin
      fWordWrapPlugin.Reset;
      Invalidate;
    end;
  end;
end;

{ Puts selected text into undo item and then deletes it }
procedure TCustomSynEdit.SetSelectedTextEmpty(const ChangeStr: UnicodeString = '');
var
  C: Integer;
  vCaret, vStartOfBlock, vUndoBegin, vUndoEnd: TBufferCoord;
begin
  if fSelections then
  begin
    C := Self.CurrentSnippetCaret;
    if C > -1 then
    SetCaretAndSelection(fCarets[C].bcStart, fCarets[C].bcStart, fCarets[C].bcEnd);
  end;
  vCaret := CaretXY;
  vUndoBegin := BlockBegin;
  vUndoEnd := BlockEnd;
  if fSnippet and not fInsertingMirrors then
    fUndoList.BeginBlock;
  if ChangeStr <> EmptyStr then
  begin
    vStartOfBlock := BlockBegin;
    if (fActiveSelectionMode = smLine) then
      vStartOfBlock.Char := 1;
    fUndoList.BeginBlock;
  end;
  fUndoList.AddChange(crDelete, vUndoBegin, vUndoEnd, CurrentSnippetCaret, GetSelText,
    fActiveSelectionMode, ExpandLines.GetLineStates(vUndoBegin.Line - 1, vUndoEnd.Line - 1));
  SetSelTextPrimitive(ChangeStr);
  if ChangeStr <> EmptyStr then
  begin
    fUndoList.AddChange(crInsert, vStartOfBlock, BlockEnd, CurrentSnippetCaret, EmptyStr, smNormal);
    fUndoList.EndBlock;
  end;
  MirrorCommand(ecNone, #0, nil, vCaret, vUndoBegin, vUndoEnd, PChar(ChangeStr));
  if fSnippet and not fInsertingMirrors then
    fUndoList.EndBlock;
end;

// -----------------------------------------------------------------------------
// § Garnet
// Many changes.
// Smart tabs are necessary only when we want to tab until FIRST
// non-whitespace character from previous line. When we are already
// on that position or far - then, it should simply insert one tab,
// not try to tab until next non-whitespace BETWEEN words
// on previous line
procedure TCustomSynEdit.DoTabKey(AHardTab: Boolean = False);
var
  Ln: UnicodeString;  // Line where we are tabbing
  Len: Integer;       // Serves different purpose
  StartOfBlock: TBufferCoord; // Used to fill undo item
  SpaceCount1,        // How much new visual whitespace we need to insert
  SpaceCount2,        // Amount of visual whitespace on line where tabbing
  Counter,            // Lines counter

  iTabs,              // How much tabs there are on line where tabbing
  iSpaces,            // How much whitespaces
  iWhitespace,        // Position of first printable after all whitespace
  iTopLine: Integer;

  Spaces: UnicodeString; // Assembled whitespace string
  NewCaretX: Integer; // What new caret pos after tabbing it will be
  AllWhiteUpToCaret, OldSelTabLine, vIgnoreSmartTabs: Boolean;

  { Collects information about whitespace on current line up to first
    printable character. How many tabs, spaces and what start position of
    first printable. Spaces are only counted up to the caret }
  procedure GetLineInfo(var Tabs, Spaces, First: Integer);
  begin
    Tabs := 0;
    Spaces := 0;
    First := 0;
    if Len = 0 then
      Exit;
    First := 1;
    while (First <= Len) and (Ln[First] < #33) do
    begin
      if Ln[First] = #9 then
        Inc(Tabs)
      else if First < fCaretX then
        Inc(Spaces);
      Inc(First);
    end;
  end;

begin
  { Provide Visual Studio like block indenting }
  OldSelTabLine := SelTabLine;
  if (eoTabIndent in Options) and ((SelTabBlock) or (OldSelTabLine)) then
  begin
    DoBlockIndent;
    if OldSelTabLine then
    begin
      if fBlockBegin.Char < fBlockEnd.Char then
        fBlockBegin.Char := 1
      else
        fBlockEnd.Char := 1;
    end;
    Exit;
  end;

  { Separate from insertion commands }
  if (not SelAvail) and (fUndoList.PeekItem <> nil) and
    (fUndoList.PeekItem.ChangeReason = crInsert)
  then
    fUndoList.AddGroupBreak;

  fUndoList.BeginBlock;
  try

    { Work with selection }
    if SelAvail then
    begin
      fUndoList.AddChange(crDelete,
        fBlockBegin, fBlockEnd, CurrentSnippetCaret, SelText,
        fActiveSelectionMode,
        fLines.GetLineStates(fBlockBegin.Line - 1, fBlockEnd.Line - 1)
      );
      SetSelTextPrimitive('');
    end;

    { Remember original caret pos }
    StartOfBlock := CaretXY;

    { Fetch a line }
    Counter := fCaretY - 1;
    Ln := fLines[Counter];
    Len := Length(Ln);

    { This will happen with eoScrollPastEOL. By doing this we will ensure
      correct undo flow. One can think that it's wiser to insert whitespace
      up to caret but it can insert spaces - and it may happen that
      we don't need any spaces to be inserted }
    if Len < fCaretX - 1 then
      fCaretX := Len + 1;

    { Shut up Delphi compiler }
    AllWhiteUpToCaret := True;
    vIgnoreSmartTabs := True;
    NewCaretX := 1;
    SpaceCount1 := 0;
    SpaceCount2 := 0;

    { Hard tabbing? }
    if AHardTab then
    begin
      Spaces := #9;
      NewCaretX := fCaretX + 1;
    end

    { No, run various checks }
    else begin
      SpaceCount1 := 0;
      SpaceCount2 := GetLeadingExpandedLength(Ln, fTabWidth);
      AllWhiteUpToCaret := IsAllWhiteUpToCaret(Ln);

      iTabs := 0;
      iSpaces := 0;
      iWhitespace := 0;
      GetLineInfo(iTabs, iSpaces, iWhitespace);

      { Check if can do smart tabs }
      if (eoSmartTabs in fOptions) and (AllWhiteUpToCaret) then
      begin

        { Find constraint }
        if fTopLine > fLinesInWindow then
          iTopLine := RowToLine(fTopLine - fLinesInWindow)
        else
          iTopLine := RowToLine(fTopLine);

        { Loop to find more visual whitespace }
        Dec(Counter);
        repeat
          { Blank? }
          if ExpandLines.AccessStringLength(Counter) = 0 then
          begin
            Dec(Counter);
            if Counter < Len then
              Break;
            Continue;
          end;

          { More? }
          Len := GetLeadingExpandedLength(ExpandLines[Counter], fTabWidth);
          if SpaceCount2 < Len then
          begin
            SpaceCount1 := Len - SpaceCount2;
            vIgnoreSmartTabs := False;
            Break;
          end

          { Avoid looking above less indented lines }
          else if not ExpandLines.GetIsLineWhitespaceOnly(Counter) then
            Break;

          Dec(Counter);
        until
          Counter < iTopLine;
      end;
    end;

    if not AHardTab then
    begin
      if eoTabsToSpaces in fOptions then
      begin
        { Smart tabs had produced results }
        if not vIgnoreSmartTabs then
          Spaces := UnicodeString(StringOfChar(#32, SpaceCount1))

        { Otherwise, handle single tab insertion.
          Insert whitespace amount visually equal to single tab }
        else begin
          if AllWhiteUpToCaret then
            SpaceCount1 := fTabWidth - (GetLeadingExpandedLength(Ln, fTabWidth) mod fTabWidth)
          else
            SpaceCount1 := fTabWidth - (GetExpandedLengthUpTo(Ln, fTabWidth, StartOfBlock.Char - 1) mod fTabWidth);
          Spaces := UnicodeString(StringOfChar(#32, SpaceCount1));
        end;
        NewCaretX := StartOfBlock.Char + SpaceCount1;
      end
      else begin
        if not vIgnoreSmartTabs then
        begin
          Spaces := UnicodeString(StringOfChar(#9, SpaceCount1 div fTabWidth) +
            StringOfChar(#32, SpaceCount1 mod fTabWidth));
          SpaceCount1 := Length(Spaces); // Now instead of visual length we have
                                         // real
        end
        else
          { If all previous chars are white }
          if AllWhiteUpToCaret then

            { We can insert tab only if all previous white chars up to the caret
              are tabs too }
            if (fCaretX = 1) or (iSpaces = 0) then
            begin
              SpaceCount1 := 1;
              Spaces := #9;
            end
            else begin
              SpaceCount1 := fTabWidth - (SpaceCount2 mod fTabWidth);
              Spaces := UnicodeString(StringOfChar(#32, SpaceCount1));
            end

          { Tabbing inside actual text }
          else begin
            SpaceCount1 := fTabWidth - (GetExpandedLengthUpTo(Ln, fTabWidth, StartOfBlock.Char - 1) mod fTabWidth);
            Spaces := UnicodeString(StringOfChar(#32, SpaceCount1))
          end;
        NewCaretX := StartOfBlock.Char + SpaceCount1;
      end;

      { We need to insert spaces (#32) *after* tabs to do actual indenting }
      if (iTabs > 0) and ((eoTabsToSpaces in Options) or (PosEx(#32, Spaces) > 0)) and
        (fCaretX < iWhitespace) then
      begin
        fCaretX := iWhitespace; // Sets caret before first printable
        StartOfBlock.Char := iWhitespace; // Required for correct undo
        NewCaretX := iWhitespace + Length(Spaces);
      end;
    end;

    { Do insert needed amount of whitespace, restrict trimming not to
      corrupt undo flow (also, user probably wants to see immediate
      display of what whitespace has been putted) }
    SetSelTextPrimitiveEx(fActiveSelectionMode, PChar(Spaces), True, False, False);

    { Undo is already handled in SetSelText when SelectionMode is Column }
    if fActiveSelectionMode <> smColumn then
      fUndoList.AddChange(crInsert, StartOfBlock, CaretXY, CurrentSnippetCaret, SelText,
        fActiveSelectionMode, ExpandLines.GetLineStates(CaretXY.Line - 1, CaretXY.Line - 1));

    { Update line state }
    ExpandLines.ModifiedLines[CaretXY.Line - 1] := True;

    { The flag below used to add a group break if next command will be ecChar }
    Include(fStateFlags, sfJustIndented);

  finally
    fUndoList.EndBlock;
  end;

  { Set new caret pos }
  InternalCaretX := NewCaretX;
  EnsureCursorPosVisible;
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.DoShiftTabKey;
var
  NewX: Integer;
  Line: UnicodeString;
  LineLen: Integer;
  DestX: Integer;

  MaxLen, iLine: Integer;
  PrevLine, OldSelText: UnicodeString;
  p: PWideChar;
  OldCaretXY: TBufferCoord;
begin
  { Provide Visual Studio like block indenting }
  if (eoTabIndent in Options) and ((SelTabBlock) or (SelTabLine)) then
  begin
    DoBlockUnIndent;
    Exit;
  end;

  NewX := CaretX;

  if (NewX <> 1) and (eoSmartTabs in fOptions) then
  begin
    iLine := CaretY - 1;
    if (iLine > 0) and (iLine < Lines.Count) then
    begin
      Dec(iLine);
      MaxLen := CaretX - 1;
      repeat
        PrevLine := Lines[iLine];
        if (Length(PrevLine) >= MaxLen) then
        begin
          p := @PrevLine[MaxLen];
          // scan over whitespaces
          repeat
            if p^ <> #32 then break;
            Dec(NewX);
            Dec(p);
          until NewX = 1;
          // scan over non-whitespaces
          if NewX <> 1 then
            repeat
              if p^ = #32 then break;
              Dec(NewX);
              Dec(p);
            until NewX = 1;
          break;
        end;
        Dec(iLine);
      until iLine < 0;
    end;
  end;

  if NewX = CaretX then
  begin
    Line := LineText;
    LineLen := Length(Line);

    // find real un-tab position

    DestX := ((CaretX - 2) div TabWidth) * TabWidth + 1;
    if NewX > LineLen then
      NewX := DestX
    else if (NewX > DestX) and (Line[NewX - 1] = #9) then
      dec(NewX)
    else begin
      while (NewX > DestX) and ((NewX - 1 > LineLen) or (Line[NewX - 1] = #32)) do
        dec(NewX);
    end;
  end;

  // perform un-tab
  if (NewX <> CaretX) then
  begin
    SetBlockBegin(BufferCoord(NewX, CaretY));
    SetBlockEnd(CaretXY);
    OldCaretXY := CaretXY;

    OldSelText := SelText;
    SetSelTextPrimitive('');

    fUndoList.AddChange(crSilentDelete, BufferCoord(NewX, CaretY),
      OldCaretXY, CurrentSnippetCaret, OldSelText, smNormal);

    InternalCaretX := NewX;
  end;
end;

procedure TCustomSynEdit.DoHomeKey(Selection: Boolean);

  function LastCharInRow: Integer;
  var
    vPos: TDisplayCoord;
  begin
    if fLines.Count = 0 then
      Result := 1
    else
    begin
      vPos := DisplayXY;
      vPos.Column := Min(CharsInWindow, fWordWrapPlugin.GetRowLength(vPos.Row, CaretXY.Line) + 1); // § Garnet WW
      Result := DisplayToBufferPos(vPos).Char;
    end;
  end;

var
  newX: Integer;
  first_nonblank: Integer;
  s: UnicodeString;
  vNewPos: TDisplayCoord;
  vMaxX: Integer;
begin
  // home key enhancement
  if (eoEnhanceHomeKey in fOptions) and (LineToRow(CaretY) = DisplayY) then
  begin
    s := fLines[CaretXY.Line - 1];

    first_nonblank := 1;
    if WordWrap then
      vMaxX := LastCharInRow() -1
    else
      vMaxX := Length(s);
    while (first_nonblank <= vMaxX) and
      CharInSet(s[first_nonblank], [#32, #9])
    do
      inc(first_nonblank);
    dec(first_nonblank);

    newX := CaretXY.Char - 1;

    if (newX > first_nonblank) or (newX = 0) then
      newX := first_nonblank + 1
    else
      newX := 1;
  end
  else
    newX := 1;

  if WordWrap then
  begin
    vNewPos.Row := DisplayY;
    vNewPos.Column := BufferToDisplayPos(BufferCoord(newX, CaretY)).Column;
    MoveCaretAndSelection(CaretXY, DisplayToBufferPos(vNewPos), Selection);
  end
  else
    MoveCaretAndSelection(CaretXY, BufferCoord(newX, CaretY), Selection);
end;

procedure TCustomSynEdit.DoEndKey(Selection: Boolean);

  function CaretInLastRow: Boolean;
  var
    vLastRow: Integer;
  begin
    if not WordWrap then
      Result := True
    else
    begin
      vLastRow := LineToRow(CaretY + 1) - 1;
      // This check allows good behaviour with empty rows (this can be useful in a diff app ;-)
      while (vLastRow > 1)
        and (fWordWrapPlugin.GetRowLength(vLastRow, CaretY) = 0) // § Garnet WW
        and (RowToLine(vLastRow) = CaretY) do
      begin
        Dec(vLastRow);
      end;
      Result := DisplayY = vLastRow;
    end;
  end;

  function FirstCharInRow: Integer;
  var
    vPos: TDisplayCoord;
  begin
    vPos.Row := DisplayY;
    vPos.Column := 1;
    Result := DisplayToBufferPos(vPos).Char;
  end;

var
  vText: UnicodeString;
  vLastNonBlank: Integer;
  vNewX: Integer;
  vNewCaret: TDisplayCoord;
  vMinX: Integer;
  vEnhance: Boolean;
begin
  if (eoEnhanceEndKey in fOptions) and CaretInLastRow then
  begin
    vEnhance := True;
    vText := LineText;
    vLastNonBlank := Length(vText);
    if WordWrap then
      vMinX := FirstCharInRow() - 1
    else
      vMinX := 0;
    while (vLastNonBlank > vMinX) and CharInSet(vText[vLastNonBlank], [#32, #9]) do
      Dec(vLastNonBlank);

    vNewX := CaretX - 1;
    if vNewX = vLastNonBlank then
      vNewX := Length(LineText) + 1
    else
      vNewX := vLastNonBlank + 1;
  end
  else
  begin
    vNewX := Length(LineText) + 1;
    vEnhance := False;
  end;

  if WordWrap then
  begin
    vNewCaret.Row := DisplayY;
    if vEnhance then
      vNewCaret.Column := BufferToDisplayPos(BufferCoord(vNewX, CaretY)).Column
    else
      vNewCaret.Column := fWordWrapPlugin.GetRowLength(vNewCaret.Row, CaretY) + 1; // § Garnet WW
    vNewCaret.Column := Min(CharsInWindow + 1, vNewCaret.Column);
    MoveCaretAndSelection(CaretXY, DisplayToBufferPos(vNewCaret), Selection);

    { Updates fCaretAtEOL flag }
    SetInternalDisplayXY(vNewCaret);
  end
  else
    MoveCaretAndSelection(CaretXY,
      BufferCoord(vNewX, CaretY), Selection);
end;

procedure TCustomSynEdit.CreateWnd;
begin
  inherited;

  if (eoDropFiles in fOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, True);

  UpdateScrollBars;
end;

procedure TCustomSynEdit.DestroyWnd;
begin
  if (eoDropFiles in fOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, False);

  inherited;
end;

procedure TCustomSynEdit.InvalidateRect(const aRect: TRect; aErase: Boolean);
begin
  Windows.InvalidateRect(Handle, @aRect, aErase);
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.DoBlockIndent;
var
  OrgCaretPos: TBufferCoord;
  OrgSelectionMode: TSynSelectionMode;
  BB, BE: TBufferCoord;
  I, J, Run, BX, EX, E: Integer;
  StrToInsert, Spaces: UnicodeString;
  InsertionPos: TBufferCoord;
  StrA: PBArray; bSpaces: Boolean;
begin
  { Remember caret pos and selection }
  OrgCaretPos := CaretXY;
  OrgSelectionMode := fActiveSelectionMode;

  { Shut up Delphi compiler }
  BX := 1; EX := 1;
  bSpaces := False;

  try
    { Keep current selection details }
    BB := BlockBegin;
    BE := BlockEnd;

    { Obtain correct last line index }
    if (BE.Char = 1) then
    begin
      BX := 1;
      E := BE.Line - 1;
    end
    else
      E := BE.Line;

    { Build insertion string }
    Run := 1;
    SetLength(StrToInsert, 32);
    for I := BB.Line to E do
    begin

      { Initialize string }
      SetLength(Spaces, 0);

      { Find correct indent when in column mode looking at the first (base) line }
      if I = BB.Line then
        if (OrgSelectionMode = smColumn) then

          { Substitute tab with correct amount of whitespace }
          if not IsAllTabsUpToCaret(fLines.fList^[Pred(I)].fString, BB.Char) then
          begin
            StrA := fLines.Analyzis[Pred(I)];
            J := fTabWidth - ExpandedPos(1, BB.Char, StrA) mod fTabWidth;
            bSpaces := True;
          end

          { Tab length is correct here }
          else
            J := fTabWidth;

      { Do not insert anything in completely blank strings }
      if Length(fLines.fList^[Pred(I)].fString) > 0 then

        { Should simply insert spaces? }
        if (eoTabsToSpaces in Options) then

          { Calculate amount of spaces to insert }
          if OrgSelectionMode = smColumn then
            Spaces := StringOfChar(#32, J)
          else
            Spaces := StringOfChar(#32, fTabWidth)

        { Can insert tabs }
        else

          { Check if can insert tab }
          if OrgSelectionMode = smColumn then

            { No actual chars before aret? Can put tab }
            if not bSpaces then
              Spaces := #9

            { Substitute tab with correct amount of spaces }
            else
              Spaces := StringOfChar(#32, J)

          { Can insert tab }
          else
            Spaces := #9;

      { Find caret delta for block begin on first iteration }
      if I = BB.Line then
        if BE.Char > 1 then
          if Spaces[1] = #9 then
            BX := 1
          else
            BX := Length(Spaces);

      { Find caret delta for block begin on last iteration }
      if I = E then
        if BE.Char > 1 then
          if Spaces[1] = #9 then
            EX := 1
          else
            EX := Length(Spaces);

      { Avoid new line on last line }
      if I < E then
        Spaces := Spaces + WideCRLF;

      { Copy build space buffer string in real buffer }
      if Length(StrToInsert) < Run + Length(Spaces) then
        SetLength(StrToInsert, Run * 2 + Length(Spaces));
      Move(Spaces[1], StrToInsert[Run], Length(Spaces) * SizeOf(Char));
      Inc(Run, Length(Spaces));
    end;

    { Fix built space buffer }
    SetLength(StrToInsert, Pred(Run));

    { Insert it, add undo items }
    fUndoList.BeginBlock;
    try

      { This undo item is required to restore selection as it was before
        insertion }
      fUndoList.AddChange(crSelection, BB, BE, CurrentSnippetCaret, '', OrgSelectionMode);

      { Build insertion point }
      InsertionPos.Line := BB.Line;

      { Ensure correct x coord in column mode }
      if fActiveSelectionMode = smColumn then
        InsertionPos.Char := Min(BB.Char, BE.Char)
      else
        InsertionPos.Char := 1;

      { Do insert }
      InsertBlock(InsertionPos, InsertionPos, PChar(StrToInsert), True);
    finally
      fUndoList.EndBlock;
    end;

    { Adjust the x position of orgcaretpos appropriately }
    Inc(OrgCaretPos.Char, BX);
    Inc(BB.Char, BX);
    Inc(BE.Char, EX);
  finally
    SetCaretAndSelection(OrgCaretPos, BB, BE);
    ActiveSelectionMode := OrgSelectionMode;
  end;
end;

procedure TCustomSynEdit.DoBlockUnindent;
var
  OrgCaretPos: TBufferCoord;
  OrgSelectionMode: TSynSelectionMode;
  BB, BE: TBufferCoord;
  StrToDelete: UnicodeString;
  I, X, StrToDeleteLen, Caret, DisplayCaret, BX, EX, E: Integer;
  SpaceCount1, SpaceCount2, DelLen: Integer;
  Ln: String;
  Len: Integer;
  StrA: PBArray;
  SomethingToDelete: Boolean;

  procedure PutEol;
  begin
    if I < E then
    begin
      if Length(StrToDelete) < DelLen + Length(WideCRLF) then
        SetLength(StrToDelete, DelLen * 2 + Length(WideCRLF));
      Move(WideCRLF[1], StrToDelete[DelLen], Length(WideCRLF) * SizeOf(Char));
      Inc(DelLen, Length(WideCRLF));
    end;
  end;

begin
  { Iniialize }
  SomethingToDelete := False;

  { Remember caret and selection }
  OrgCaretPos := CaretXY;
  OrgSelectionMode := fActiveSelectionMode;
  BB := BlockBegin;
  BE := BlockEnd;

  { Find place to do unindent }
  if fActiveSelectionMode <> smColumn then
    Caret := 1
  else if BB.Char > 1 then
  begin
    Caret := BB.Char;
    DisplayCaret := BufferToDisplayPos(BB).Column;
  end

  { Nowhere to unindent }
  else
    Exit;

  { Find correct end line }
  if BE.Char = 1 then
    E := BE.Line - 1
  else
    E := BE.Line;

  { Check if we can unindent }
  SetLength(StrToDelete, 32);
  DelLen := 1;
  for I := BB.Line to E do
  begin
    { Fetch a line }
    Ln := fLines.fList^[Pred(I)].fString;
    Len := Length(Ln);
    StrA := fLines.Analyzis[Pred(I)];

    { smNormal, smLine unindenting in the beginning of lines }
    if Caret = 1 then
    begin
      { Get visual length of whitespace on this line up to the tab width }
      SpaceCount1 := 0;
      SpaceCount2 := 0;
      while (SpaceCount2 < fTabWidth) and (SpaceCount1 < Length(StrA^))
        and (Ln[Succ(SpaceCount1)] < #33) do
      begin
        Inc(SpaceCount2, StrA^[SpaceCount1]);
        Inc(SpaceCount1);
      end;

      { Skip blank lines and lines where there's no room to unindent }
      if (SpaceCount2 > fTabWidth) or (SpaceCount1 = 0) then
      begin

        { Put Eol if not last line }
        PutEol;

        { Next line }
        Continue;
      end;

      { Something to be deleted 100% now }
      SomethingToDelete := True;

      { Copy that into string to delete for undo }
      if Length(StrToDelete) < DelLen + SpaceCount1 then
        SetLength(StrToDelete, DelLen * 2 + SpaceCount1);
      Move(Ln[1], StrToDelete[DelLen], SpaceCount1 * SizeOf(Char));
      Inc(DelLen, SpaceCount1);

      { Put Eol if not last line }
      PutEol;

      { Delete from line (modified state updated lately) }
      Delete(Ln, 1, SpaceCount1);
      fLines[Pred(I)] := Ln;
    end

    { smColumn unindention }
    else begin

      { Do nothing with lines which are before selection }
      if Len < Caret then
      begin

        { Put Eol if not last line }
        PutEol;

        { Next line }
        Continue;
      end;

      { Find amount of whitespace left from selection start }
      SpaceCount1 := 0;
      SpaceCount2 := 0;
      X := Pred(DisplayToBufferPos(DisplayCoord(DisplayCaret, LineToRow(I))).Char);
      while SpaceCount2 < fTabWidth do
      begin

        { No room? }
        if (Len < X) or (X < 1) then
          Break;

        { Space }
        if Ln[X] = #32 then
          Inc(SpaceCount2, 1)

        { Tab }
        else if Ln[X] = #9 then
          Inc(SpaceCount2, fTabWidth - (GetExpandedLengthUpTo(Ln, fTabWidth, Pred(X)) mod fTabWidth))

        { Found non-whitespace. Stop }
        else
          Break;

        { Next char }
        Inc(SpaceCount1);
        Dec(X);
      end;

      { Found a line where deletable whitespace overlaps TabWidth.
        We should cancel. Otherwise, unindent on this line will
        be visually bigger than on others }
      if (SpaceCount2 > fTabWidth) or (SpaceCount1 = 0) then
      begin

        { Put Eol if not last line }
        PutEol;

        { Next line }
        Continue;
      end;

      { Update beginning of undo caret }
      if I = BB.Line then
        Dec(BB.Char, SpaceCount1);

      { Update ending of undo caret }
      if I = E then
        Dec(BE.Char, SpaceCount1);

      { Update original caret pos }
      if I = OrgCaretPos.Line then
        Dec(OrgCaretPos.Char, SpaceCount1);

      { At least one deletion will now happen }
      SomethingToDelete := True;

      { Copy that into string to delete for undo }
      if Length(StrToDelete) < DelLen + SpaceCount1 then
        SetLength(StrToDelete, DelLen * 2 + SpaceCount1);
      Move(Ln[Succ(X)], StrToDelete[DelLen], SpaceCount1 * SizeOf(Char));
      Inc(DelLen, SpaceCount1);

      { Put Eol if not last line }
      PutEol;

      { Delete from line }
      Delete(Ln, Succ(X), SpaceCount1);
      ExpandLines[Pred(I)] := Ln;
    end;
  end;

  { Correct buffer bounds }
  SetLength(StrToDelete, Pred(DelLen));

  { If StrToDelete doesn't consist only of EOLs, then we did actually
    deleted something and can add to undo, reset selection bounds }
  if SomethingToDelete then
  begin

    { Add to undo }
    fUndoList.AddChange(crUnindent, BB, BE, CurrentSnippetCaret, PChar(StrToDelete), OrgSelectionMode,
      ExpandLines.GetLineStates(BB.Line - 1, E - 1));

    { Change line states }
    for I := BB.Line to E do fLines.ModifiedLines[Pred(I)] := True;

    { Set new caret selection }
    SetCaretAndSelection(OrgCaretPos, BB, BE);
  end;
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.UpdateWordWrapHiddenOffsets;
begin
  if WordWrap then
    fWordWrapPlugin.LinesFolded(-1, -1);
end;

procedure TCustomSynEdit.SetModified(Value: Boolean);
begin
  if Value <> fModified then
  begin
    fModified := Value;
    if (eoGroupUndo in Options) and (not Value) and UndoList.CanUndo then
      UndoList.AddGroupBreak;
    UndoList.InitialState := not Value;

    StatusChanged([scModified]);

    Invalidate;
  end;
end;

function TCustomSynEdit.DoOnSpecialLineColors(Line: Integer; var Foreground,
  Background: TColor): Boolean;
begin
  Result := False;
  if Assigned(fOnSpecialLineColors) then
    fOnSpecialLineColors(Self, Line, Result, Foreground, Background);
end;

procedure TCustomSynEdit.InvalidateLine(ALine: Integer);
var
  rcInval: TRect;
  cRow: Integer;
begin
  if (not HandleAllocated) or (ALine < 1) or (ALine > Lines.Count) or
    (not Visible)
  then
    Exit;

  { In word wrap mode even single line should be painted with whole cycle
    because it can consist of multiple rows }
  if WordWrap then
  begin
    InvalidateLines(ALine, ALine);
    Exit;
  end;

  { Compute line rect and invalidate text area of this line }
  cRow := LineToRow(ALine);
  if (cRow >= fTopLine) and (cRow <= fTopLine + fLinesInWindow) then
  begin
    rcInval := Rect(fGutter.Width, fTextHeight*(cRow - fTopLine), ClientWidth, 0);
    with rcInval do
      Bottom := Top + fTextHeight;

    if sfLinesChanging in fStateFlags then
      UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
    else
      InvalidateRect(rcInval, False);
  end;
end;

function TCustomSynEdit.GetReadOnly: Boolean;
begin
  Result := fReadOnly;
end;

procedure TCustomSynEdit.SetReadOnly(Value: Boolean);
begin
  if fReadOnly <> Value then
  begin
    fReadOnly := Value;
    StatusChanged([scReadOnly]);
  end;
end;

// § Garnet
function TCustomSynEdit.GetHighlighterAttriAtRowCol(const XY: TBufferCoord;
  var Attri: TSynHighlighterAttributes): Boolean;
var
  TmpToken: String;
  TmpType, TmpStart: Integer;
begin
  Result := GetHighlighterAttriAtRowColEx(XY, TmpToken, TmpType, TmpStart, Attri);
end;

function TCustomSynEdit.GetHighlighterAttriAtRowCol(const XY: TBufferCoord;
  var Token: UnicodeString; var Attri: TSynHighlighterAttributes): Boolean;
var
  TmpType, TmpStart: Integer;
begin
  Result := GetHighlighterAttriAtRowColEx(XY, Token, TmpType, TmpStart, Attri);
end;
// § Garnet

function TCustomSynEdit.GetHighlighterAttriAtRowColEx(const XY: TBufferCoord;
  var Token: UnicodeString; var TokenType, Start: Integer;
  var Attri: TSynHighlighterAttributes): boolean;
var
  PosX, PosY: Integer;
  Line: UnicodeString;
begin
  PosY := XY.Line - 1;
  if Assigned(Highlighter) and (PosY >= 0) and (PosY < Lines.Count) then
  begin
    Line := Lines[PosY];
    if PosY = 0 then
      Highlighter.ResetRange
    else
      Highlighter.SetRange(TSynEditStringList(Lines).Ranges[PosY - 1]);
    Highlighter.SetLine(Line, PosY);
    PosX := XY.Char;
    if (PosX > 0) and (PosX <= Length(Line)) then
      while not Highlighter.GetEol do
      begin
        Start := Highlighter.GetTokenPos + 1;
        Token := Highlighter.GetToken;
        if (PosX >= Start) and (PosX < Start + Length(Token)) then
        begin
          Attri := Highlighter.GetTokenAttribute;
          TokenType := Highlighter.GetTokenKind;
          Result := True;
          exit;
        end;
        Highlighter.Next;
      end;
  end;
  Token := '';
  Attri := nil;
  Result := False;
end;

function TCustomSynEdit.FindHookedCmdEvent(AHandlerProc: THookedCommandEvent): Integer;
var
  Entry: THookedCommandHandlerEntry;
begin
  Result := GetHookedCommandHandlersCount - 1;
  while Result >= 0 do
  begin
    Entry := THookedCommandHandlerEntry(fHookedCommandHandlers[Result]);
    if Entry.Equals(AHandlerProc) then
      break;
    Dec(Result);
  end;
end;

function TCustomSynEdit.GetHookedCommandHandlersCount: Integer;
begin
  if Assigned(fHookedCommandHandlers) then
    Result := fHookedCommandHandlers.Count
  else
    Result := 0;
end;

procedure TCustomSynEdit.RegisterCommandHandler(
  const AHandlerProc: THookedCommandEvent; AHandlerData: pointer);
begin
  if not Assigned(AHandlerProc) then
  begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.Create('Event handler is NIL in RegisterCommandHandler');
{$ENDIF}
    exit;
  end;
  if not Assigned(fHookedCommandHandlers) then
    fHookedCommandHandlers := TObjectList.Create;
  if FindHookedCmdEvent(AHandlerProc) = -1 then
    fHookedCommandHandlers.Add(THookedCommandHandlerEntry.Create(
      AHandlerProc, AHandlerData))
  else
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.CreateFmt('Event handler (%p, %p) already registered',
      [TMethod(AHandlerProc).Data, TMethod(AHandlerProc).Code]);
{$ENDIF}
end;

procedure TCustomSynEdit.UnregisterCommandHandler(AHandlerProc:
  THookedCommandEvent);
var
  i: Integer;
begin
  if not Assigned(AHandlerProc) then
  begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.Create('Event handler is NIL in UnregisterCommandHandler');
{$ENDIF}
    exit;
  end;
  i := FindHookedCmdEvent(AHandlerProc);
  if i > -1 then
    fHookedCommandHandlers.Delete(i)
  else
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.CreateFmt('Event handler (%p, %p) is not registered',
      [TMethod(AHandlerProc).Data, TMethod(AHandlerProc).Code]);
{$ENDIF}
end;

procedure TCustomSynEdit.NotifyHookedCommandHandlers(AfterProcessing: Boolean;
  var Command: TSynEditorCommand; var AChar: WideChar; Data: pointer);
var
  Handled: Boolean;
  i: Integer;
  Entry: THookedCommandHandlerEntry;
begin
  Handled := False;
  for i := 0 to GetHookedCommandHandlersCount - 1 do
  begin
    Entry := THookedCommandHandlerEntry(fHookedCommandHandlers[i]);
    // NOTE: Command should NOT be set to ecNone, because this might interfere
    // with other handlers.  Set Handled to False instead (and check its value
    // to not process the command twice).
    Entry.fEvent(Self, AfterProcessing, Handled, Command, AChar, Data,
      Entry.fData);
  end;
  if Handled then
    Command := ecNone;
end;

procedure TCustomSynEdit.DoOnClearBookmark(var Mark: TSynEditMark);
begin
  if Assigned(fOnClearMark) then
    fOnClearMark(Self, Mark);
end;

procedure TCustomSynEdit.DoOnPaintTransientEx(TransientType: TTransientType; Lock: Boolean);
var DoTransient: Boolean;
begin
  DoTransient:=(FPaintTransientLock=0);
  if Lock then
    begin
    if (TransientType=ttBefore) then inc(FPaintTransientLock)
    else
      begin
      dec(FPaintTransientLock);
      DoTransient:=(FPaintTransientLock=0);
      end;
    end;

  if DoTransient and Assigned(fOnPaintTransient) then
  begin
    Canvas.Font.Assign(Font);
    Canvas.Brush.Color := Color;
    HideCaret;
    try
      fOnPaintTransient(Self, Canvas, TransientType);
    finally
      ShowCaret;
    end;
  end;
end;

procedure TCustomSynEdit.DoOnPaintTransient(TransientType: TTransientType);
begin
  DoOnPaintTransientEx(TransientType, False);
end;

procedure TCustomSynEdit.DoOnPaint;
begin
  if Assigned(fOnPaint) then
  begin
    Canvas.Font.Assign(Font);
    Canvas.Brush.Color := Color;
    fOnPaint(Self, Canvas);
  end;
end;

procedure TCustomSynEdit.DoOnPlaceMark(var Mark: TSynEditMark);
begin
  if Assigned(fOnPlaceMark) then
    fOnPlaceMark(Self, Mark);
end;

function TCustomSynEdit.DoOnReplaceText(const ASearch, AReplace: UnicodeString;
  Line, Column: Integer): TSynReplaceAction;
begin
  Result := raCancel;
  if Assigned(fOnReplaceText) then
    fOnReplaceText(Self, ASearch, AReplace, Line, Column, Result);
end;

procedure TCustomSynEdit.DoOnStatusChange(Changes: TSynStatusChanges);
begin
  if Assigned(fOnStatusChange) then
  begin
    fOnStatusChange(Self, fStatusChanges);
    fStatusChanges := [];
  end;
end;

procedure TCustomSynEdit.UpdateModifiedStatus;
begin
  Modified := not UndoList.InitialState;
end;

procedure TCustomSynEdit.UpdateLongestLine;
begin
  ExpandLines.GetLengthOfLongestLine(RowToLine(fTopLine),
    RowToLine(fTopLine + fLinesInWindow));
end;

procedure TCustomSynEdit.UndoRedoAdded(Sender: TObject);
begin
  UpdateModifiedStatus;

  { We have to clear the redo information, since adding undo info removes
    the necessary context to undo earlier edit actions }
  if (Sender = fUndoList) and not fUndoList.InsideRedo and
     (fUndoList.PeekItem<>nil) and (fUndoList.PeekItem.ChangeReason<>crGroupBreak)
  then
    fRedoList.Clear;
  if TSynEditUndoList(Sender).BlockCount = 0 then
    DoChange;
end;

// -----------------------------------------------------------------------------
// § Garnet
function TCustomSynEdit.GetWordAtRowCol(XY: TBufferCoord): UnicodeString;
begin
  Result := EmptyStr;
  GetTokenKind(Self, XY);
  if fHighlighter.GetTokenNature in [tknnIdent, tknnNumber] then
    Result := fHighlighter.GetToken;
end;

function TCustomSynEdit.GetCharAtRowCol(XY: TBufferCoord): Char;
begin
  Result := #0;
  if fLines.Count >= XY.Line then
    if fLines.AccessStringLength(Pred(XY.Line)) >= XY.Char then
      Result := fLines.fList^[Pred(XY.Line)].fString[XY.Char];
end;

procedure TCustomSynEdit.SetWordAtRowCol(XY: TBufferCoord;
  const Value: UnicodeString);
var
  CurrWord: UnicodeString;
  BB, BE: TBufferCoord;
begin
  CurrWord := GetWordAtRowCol(XY);
  if CurrWord <> EmptyStr then
  begin
    BB := WordStartEx(XY);
    BE := WordEndEx(XY);
    SetCaretAndSelection(BB, BB, BE);
    SelText := Value;
  end;
end;

procedure TCustomSynEdit.SetCharAtRowCol(XY: TBufferCoord; Value: Char);
begin
  if GetCharAtRowCol(XY) <> #0 then
  begin
    SetCaretAndSelection(XY, XY, BufferCoord(Succ(XY.Char), XY.Line));
    SelText := Value;
  end;
end;

// -----------------------------------------------------------------------------
// § Garnet
// BufferToDisplayPos takes a position in the text and transforms it into
// the row and column it appears to be on the editor display
function TCustomSynEdit.BufferToDisplayPos(const P: TBufferCoord): TDisplayCoord;
var
  I, J: Integer;
  StrA: PBArray;
begin
  { Fallback }
  Result := TDisplayCoord(P);

  { Get row }
  if not WordWrap then
    Result.Row := GetUnRealLineNumber(Result.Row);

  { Get column }
  if P.Line - 1 < fLines.Count then
  begin
    Result.Column := 1;
    StrA := fLines.Analyzis[Pred(P.Line)];
    J := Length(StrA^);
    for I := 0 to P.Char - 2 do
      if I < J then
        Inc(Result.Column, StrA^[I])
      else
        Inc(Result.Column);
  end;

  { Get display coord in word wrap mode }
  if WordWrap then
    Result := fWordWrapPlugin.BufferToDisplayPos(TBufferCoord(Result));
end;

// -----------------------------------------------------------------------------
// Does the same, but treats hidden rows as if they were shown
function TCustomSynEdit.BufferToRealDisplayPos(const P: TBufferCoord): TDisplayCoord;
var
  I, J: Integer;
  StrA: PBArray;
begin
  { Fallback }
  Result := TDisplayCoord(P);

  { Get row }
  if not WordWrap then
    Result.Row := P.Line;

  { Get column }
  if P.Line - 1 < fLines.Count then
  begin
    Result.Column := 1;
    StrA := fLines.Analyzis[Pred(P.Line)];
    J := Length(StrA^);
    for I := 0 to P.Char - 2 do
      if I < J then
        Inc(Result.Column, StrA^[I])
      else
        Inc(Result.Column);
  end;

  { Get display coord in word wrap mode }
  if WordWrap then
    Result := fWordWrapPlugin.BufferToRealDisplayPos(TBufferCoord(Result));
end;

// -----------------------------------------------------------------------------
// DisplayToBufferPos takes a position on editor canvas and transfrom it
// into real position in text
function TCustomSynEdit.DisplayToBufferPos(const P: TDisplayCoord): TBufferCoord;
var
  I, J, K: Integer;
  StrA: PBArray;
begin
  { Get buffer coord in word wrap mode }
  if WordWrap then
    Result := fWordWrapPlugin.DisplayToBufferPos(P)

  { In normal mode }
  else begin
    Result := TBufferCoord(P);
    Result.Line := GetRealLineNumber(P.Row);
  end;

  { Get char }
  if Result.Line <= lines.Count then
  begin
    I := 0; K := 0;
    StrA := fLines.Analyzis[Pred(Result.Line)];
    J := Length(StrA^);
    while K < Result.Char do
    begin
      if I < J then
        Inc(K, StrA^[I])
      else
        Inc(K);
      Inc(I);
    end;
    Result.Char := I;
  end;
end;

// -----------------------------------------------------------------------------
// Does the same but doesn't take folding into account
function TCustomSynEdit.RealDisplayToBufferPos(const P: TDisplayCoord): TBufferCoord;
var
  I, J, K: Integer;
  StrA: PBArray;
begin
  { Get buffer coord in word wrap mode }
  if WordWrap then
    Result := fWordWrapPlugin.RealDisplayToBufferPos(P)

  { In normal mode }
  else begin
    Result := TBufferCoord(P);
    Result.Line := P.Row;
  end;

  { Get char }
  if Result.Line <= lines.Count then
  begin
    I := 0; K := 0;
    StrA := fLines.Analyzis[Pred(Result.Line)];
    J := Length(StrA^);
    while K < Result.Char do
    begin
      if I < J then
        Inc(K, StrA^[I])
      else
        Inc(K);
      Inc(I);
    end;
    Result.Char := I;
  end;
end;

procedure TCustomSynEdit.DoLinesDeleted(FirstLine, Count: Integer);
var
  I: Integer;
begin
  { Gutter marks }
  for I := 0 to Marks.Count - 1 do
    if Marks[I].Line >= FirstLine + Count then
      Marks[I].Line := Marks[I].Line - Count
    else if Marks[I].Line > FirstLine then
      Marks[I].Line := FirstLine;

  { Plugins }
  if fPlugins <> nil then
    for I := 0 to fPlugins.Count - 1 do
      TSynEditPlugin(fPlugins[I]).LinesDeleted(FirstLine, Count);

  { External handler }
  if Assigned(fOnLinesDeleted) then
    fOnLinesDeleted(Self, FirstLine, Count);
end;

procedure TCustomSynEdit.DoLinesInserted(FirstLine, Count: Integer);
var
  I: Integer;
begin
  { Gutter marks }
  for I := 0 to Marks.Count - 1 do
    if Marks[I].Line >= FirstLine then
      Marks[I].Line := Marks[I].Line + Count;

  { Plugins }
  if fPlugins <> nil then
    for I := 0 to fPlugins.Count - 1 do
      TSynEditPlugin(fPlugins[I]).LinesInserted(FirstLine, Count);
end;

procedure TCustomSynEdit.PluginsAfterPaint(ACanvas: TCanvas; const AClip: TRect;
  FirstLine, LastLine: Integer);
var
  i: Integer;
begin
  if fPlugins <> nil then
    for i := 0 to fPlugins.Count - 1 do
      TSynEditPlugin(fPlugins[i]).AfterPaint(ACanvas, AClip, FirstLine, LastLine);
end;

procedure TCustomSynEdit.AddKeyUpHandler(aHandler: TKeyEvent);
begin
  fKbdHandler.AddKeyUpHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveKeyUpHandler(aHandler: TKeyEvent);
begin
  fKbdHandler.RemoveKeyUpHandler(aHandler);
end;

procedure TCustomSynEdit.AddKeyDownHandler(aHandler: TSynKeyEvent);
begin
  fKbdHandler.AddKeyDownHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveKeyDownHandler(aHandler: TSynKeyEvent);
begin
  fKbdHandler.RemoveKeyDownHandler(aHandler);
end;

procedure TCustomSynEdit.AddKeyPressHandler(aHandler: TKeyPressWEvent);
begin
  fKbdHandler.AddKeyPressHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveKeyPressHandler(aHandler: TKeyPressWEvent);
begin
  fKbdHandler.RemoveKeyPressHandler(aHandler);
end;

procedure TCustomSynEdit.AddFocusControl(aControl: TWinControl);
begin
  fFocusList.Add(aControl);
end;

procedure TCustomSynEdit.RemoveFocusControl(aControl: TWinControl);
begin
  fFocusList.Remove(aControl);
end;

// § Garnet
// -----------------------------------------------------------------------------
function TCustomSynEdit.IsIdentChar(AChar: Char): Boolean;
begin
  if Assigned(Highlighter) then
  begin
    GetTokenKind(Self, CaretXY);
    Result := Highlighter.IsIdentChar(AChar);
  end
  else
    Result := not IsWordBreakChar(AChar);
end;

function TCustomSynEdit.IsWordBreakChar(AChar: Char): Boolean;
begin
  if Assigned(fHighlighter) then
  begin
    GetTokenKind(Self, CaretXY);
    Result := fHighlighter.IsWordBreakChar(AChar);
  end
  else
    Result := SynEditTypes.IsWordBreakChar(AChar);
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.SetSearchEngine(Value: TSynEditSearchCustom);
begin
  if (fSearchEngine <> Value) then
  begin
    fSearchEngine := Value;
    if Assigned(fSearchEngine) then
      fSearchEngine.FreeNotification(Self);
  end;
end;

function TCustomSynEdit.NextWordPos: TBufferCoord;
begin
  Result := NextWordPosEx(CaretXY);
end;

function TCustomSynEdit.WordStart: TBufferCoord;
begin
  Result := WordStartEx(CaretXY);
end;

function TCustomSynEdit.WordEnd: TBufferCoord;
begin
  Result := WordEndEx(CaretXY);
end;

function TCustomSynEdit.PrevWordPos: TBufferCoord;
begin
  Result := PrevWordPosEx(CaretXY);
end;

function TCustomSynEdit.GetPositionOfMouse(out aPos: TBufferCoord): Boolean;
// Get XY caret position of mouse. Returns False if point is outside the
// region of the SynEdit control.
var
  Point: TPoint;
begin
  GetCursorPos(Point);                    // mouse position (on screen)
  Point := Self.ScreenToClient(Point);    // convert to SynEdit coordinates
  { Make sure it fits within the SynEdit bounds }
  if (Point.X < 0) or (Point.Y < 0) or (Point.X > Self.Width) or (Point.Y> Self.Height) then
  begin
    Result := False;
    Exit;
  end;

  { inside the editor, get the word under the mouse pointer }
  aPos := DisplayToBufferPos(PixelsToRowColumn(Point.X, Point.Y));
  Result := True;
end;

function TCustomSynEdit.GetWordAtMouse: UnicodeString;
var
  Point: TBufferCoord;
begin
  { Return the word under the mouse }
  if GetPositionOfMouse(Point) then        // if point is valid
    Result := Self.GetWordAtRowCol(Point); // return the point at the mouse position
end;

function TCustomSynEdit.CharIndexToRowCol(Index: Integer): TBufferCoord;
{ Index is 0-based; Result.x and Result.y are 1-based }
var
  x, y, Chars: Integer;
begin
  x := 0;
  y := 0;
  Chars := 0;
  while y < Lines.Count do
  begin
    x := Length(Lines[y]);
    if Chars + x + 2 > Index then
    begin
      x := Index - Chars;
      break;
    end;
    Inc(Chars, x + 2);
    x := 0;
    Inc(y);
  end;
  Result.Char := x + 1;
  Result.Line := y + 1;
end;

function TCustomSynEdit.RowColToCharIndex(RowCol: TBufferCoord): Integer;
{ Row and Col are 1-based; Result is 0-based }
var
  i: Integer;
begin
  Result := 0;
  RowCol.Line := Min(Lines.Count, RowCol.Line) - 1;
  for i := 0 to RowCol.Line - 1 do
    Result := Result + Length(Lines[i]) + 2;
  Result := Result + (RowCol.Char -1);
end;

procedure TCustomSynEdit.Clear;
{ just to attain interface compatibility with TMemo }
begin
  ClearAll;
end;

function TCustomSynEdit.GetSelLength: Integer;
begin
  if SelAvail then
    Result := RowColToCharIndex(BlockEnd) - RowColToCharIndex(BlockBegin)
  else
    Result := 0;
end;

procedure TCustomSynEdit.SetSelLength(const Value: Integer);
var
  iNewCharIndex: Integer;
  iNewBegin: TBufferCoord;
  iNewEnd: TBufferCoord;
begin
  iNewCharIndex := RowColToCharIndex(BlockBegin) + Value;
  if (Value >= 0) or (iNewCharIndex < 0) then
  begin
    if iNewCharIndex < 0 then
    begin
      iNewEnd.Char := Length(Lines[Lines.Count - 1]) + 1;
      iNewEnd.Line := Lines.Count;
    end
    else
      iNewEnd := CharIndexToRowCol(iNewCharIndex);
    SetCaretAndSelection(iNewEnd, BlockBegin, iNewEnd);
  end
  else begin
    iNewBegin := CharIndexToRowCol(iNewCharIndex);
    SetCaretAndSelection(iNewBegin, iNewBegin, BlockBegin);
  end;
end;

procedure TCustomSynEdit.DefineProperties(Filer: TFiler);

  function CollectionsEqual(C1, C2: TCollection): Boolean;
  begin
    Result := Classes.CollectionsEqual(C1, C2, nil, nil);
  end;

  function HasKeyData: Boolean;
  var
    iDefKeys: TSynEditKeyStrokes;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := not CollectionsEqual(Keystrokes,
        TCustomSynEdit(Filer.Ancestor).Keystrokes);
    end
    else begin
      iDefKeys := TSynEditKeyStrokes.Create(nil);
      try
        iDefKeys.ResetDefaults;
        Result := not CollectionsEqual(Keystrokes, iDefKeys);
      finally
        iDefKeys.Free;
      end;
    end;
  end;

var
  iSaveKeyData: Boolean;
begin
  inherited;
{$IFNDEF UNICODE}
  UnicodeDefineProperties(Filer, Self);
{$ENDIF}
  iSaveKeyData := HasKeyData;
  Filer.DefineProperty('RemovedKeystrokes', ReadRemovedKeystrokes,
    WriteRemovedKeystrokes, iSaveKeyData);
  Filer.DefineProperty('AddedKeystrokes', ReadAddedKeystrokes, WriteAddedKeystrokes,
    iSaveKeyData);
end;

procedure TCustomSynEdit.DoChange;
begin
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

procedure TCustomSynEdit.ReadAddedKeystrokes(Reader: TReader);
var
  iAddKeys: TSynEditKeyStrokes;
  cKey: Integer;
begin
  if Reader.NextValue = vaCollection then
    Reader.ReadValue
  else
    Exit;
  iAddKeys := TSynEditKeyStrokes.Create(Self);
  try
    Reader.ReadCollection(iAddKeys);
    for cKey := 0 to iAddKeys.Count -1 do
      Keystrokes.Add.Assign(iAddKeys[cKey]);
  finally
    iAddKeys.Free;
  end;
end;

procedure TCustomSynEdit.ReadRemovedKeystrokes(Reader: TReader);
var
  iDelKeys: TSynEditKeyStrokes;
  cKey: Integer;
  iKey: TSynEditKeyStroke;
  iToDelete: Integer;
begin
  if Reader.NextValue = vaCollection then
    Reader.ReadValue
  else
    Exit;
  iDelKeys := TSynEditKeyStrokes.Create(nil);
  try
    Reader.ReadCollection(iDelKeys);
    for cKey := 0 to iDelKeys.Count -1 do
    begin
      iKey := iDelKeys[cKey];
      iToDelete := Keystrokes.FindShortcut2(iKey.ShortCut, iKey.ShortCut2);
      if (iToDelete >= 0) and (Keystrokes[iToDelete].Command = iKey.Command) then
        Keystrokes[iToDelete].Free;
    end;
  finally
    iDelKeys.Free;
  end;
end;

procedure TCustomSynEdit.WriteAddedKeystrokes(Writer: TWriter);
var
  iDefaultKeys: TSynEditKeyStrokes;
  iAddedKeys: TSynEditKeyStrokes;
  cKey: Integer;
  iKey: TSynEditKeyStroke;
  iDelIndex: Integer;
begin
  iDefaultKeys := TSynEditKeyStrokes.Create(nil);
  try
    if Writer.Ancestor <> nil then
      iDefaultKeys.Assign(TSynEdit(Writer.Ancestor).Keystrokes)
    else
      iDefaultKeys.ResetDefaults;
    iAddedKeys := TSynEditKeyStrokes.Create(nil);
    try
      for cKey := 0 to Keystrokes.Count -1 do
      begin
        iKey := Keystrokes[cKey];
        iDelIndex := iDefaultKeys.FindShortcut2(iKey.ShortCut, iKey.ShortCut2);
        //if it's not a default keystroke, add it
        if (iDelIndex < 0) or (iDefaultKeys[iDelIndex].Command <> iKey.Command) then
          iAddedKeys.Add.Assign(iKey);
      end;
      Writer.WriteCollection(iAddedKeys);
    finally
      iAddedKeys.Free;
    end;
  finally
    iDefaultKeys.Free;
  end;
end;

procedure TCustomSynEdit.WriteRemovedKeystrokes(Writer: TWriter);
var
  iRemovedKeys: TSynEditKeyStrokes;
  cKey: Integer;
  iKey: TSynEditKeyStroke;
  iFoundAt: Integer;
begin
  iRemovedKeys := TSynEditKeyStrokes.Create(nil);
  try
    if Writer.Ancestor <> nil then
      iRemovedKeys.Assign(TSynEdit(Writer.Ancestor).Keystrokes)
    else
      iRemovedKeys.ResetDefaults;
    cKey := 0;
    while cKey < iRemovedKeys.Count do
    begin
      iKey := iRemovedKeys[cKey];
      iFoundAt := Keystrokes.FindShortcut2(iKey.ShortCut, iKey.ShortCut2);
      if (iFoundAt >= 0) and (Keystrokes[iFoundAt].Command = iKey.Command) then
        iKey.Free //if exists in Keystrokes, then shouldn't be in "removed" list
      else
        Inc(cKey);
    end;
    Writer.WriteCollection(iRemovedKeys);
  finally
    iRemovedKeys.Free;
  end;
end;

procedure TCustomSynEdit.SetSpellCheck(Value: Boolean);
begin
  if fSpellCheck <> Value then
  begin
    fSpellCheck := Value;
    Invalidate;
  end;
end;

procedure TCustomSynEdit.UpdateCharsInWindow;
begin
  if HandleAllocated then
  begin
    fCharsInWindow := Max(ClientWidth - Gutter.Width - 2, 0) div fCharWidth;
    if WordWrap then
      fWordWrapPlugin.DisplayChanged;
    UpdateScrollBars;
    Invalidate;
  end;
end;

procedure TCustomSynEdit.AddMouseDownHandler(aHandler: TMouseEvent);
begin
  fKbdHandler.AddMouseDownHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveMouseDownHandler(aHandler: TMouseEvent);
begin
  fKbdHandler.RemoveMouseDownHandler(aHandler);
end;

procedure TCustomSynEdit.AddMouseUpHandler(aHandler: TMouseEvent);
begin
  fKbdHandler.AddMouseUpHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveMouseUpHandler(aHandler: TMouseEvent);
begin
  fKbdHandler.RemoveMouseUpHandler(aHandler);
end;

procedure TCustomSynEdit.AddMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  fKbdHandler.AddMouseCursorHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  fKbdHandler.RemoveMouseCursorHandler(aHandler);
end;

function TCustomSynEdit.GetWordWrap: Boolean;
begin
  Result := fWordWrapPlugin <> nil;
end;

function TCustomSynEdit.GetCurrentScope: UnicodeString;
var
  Caret: TBufferCoord;
  Tkn: TSynHighlighterAttributes;
  Rule: TSynRule;
begin
  { Initialize }
  Result := EmptyStr;
  if fHighlighter = nil then Exit;
  if fLines.Count = 0 then Exit;

  { Get currect caret }
  Caret := CaretXY;
  Dec(Caret.Line);
  Dec(Caret.Char);
  with fHighlighter do
  begin

    { Scan to the caret position }
    if Caret.Line = 0 then
      ResetRange
    else
      SetRange(fLines.Ranges[Pred(Caret.Line)]);
    SetLine(fLines.fList^[Caret.Line].fString, Caret.Line);
    while not GetEol and (Caret.Char >= GetTokenPos + GetTokenLen) do
      Next;

    { Get token }
    Tkn := fHighlighter.GetTokenAttribute;
    if Assigned(Tkn) and (Tkn.Rule is TSynSpecialRule) then
    begin
      Result := UTF8ToUnicodeString(Tkn.Name + WideLF + Tkn.FriendlyName);
      Rule := Tkn.Rule as TSynRule;
    end
    else begin
      if Caret.Char = GetTokenPos then
        Rule := TSynRange((fHighlighter as TSynUniSyn).GetPrevRange)
      else
        Rule := TSynRange((fHighlighter as TSynUniSyn).GetRange);
      if (Rule is TSynRange) and ((Rule as TSynRange).Linked) then
        Rule := (Rule as TSynRange).LinkSource;
    end;

    { Prepend parent scopes }
    while Rule <> nil do
    begin
      Result := UTF8ToUnicodeString(Rule.Name + WideLF + Rule.Style) + WideLF
        + Result;
      Rule := Rule.Parent;
    end;

    { Cut off last eol }
    if Result[Length(Result)] = WideLF then
      SetLength(Result, Pred(Length(Result)));
  end;
end;

function TCustomSynEdit.GetCaretByIndex(const Index: Integer;
  const SearchMirrors: Boolean = False): TBufferCoord;
var
  I: Integer;
begin
  Result := BufferCoord(0, 0);
  for I := 0 to High(fCarets) do
    if (not fCarets[I].bMirror or SearchMirrors) and (fCarets[I].nIndex = Index) then
    begin
      Result := fCarets[I].bcCaret;
      Exit;
    end;
end;

function TCustomSynEdit.GetOriginalSelectionIndex: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(fCarets) do
    if not fCarets[I].bMirror and (fCarets[I].nIndex = 1) then
    begin
      Result := I;
      Exit;
    end;
end;

function TCustomSynEdit.GetAtLeastOneBlockOfSelection: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(fCarets) do
    if not CaretsEqual(fCarets[I].bcStart, fCarets[I].bcEnd) then
    begin
      {$IFDEF DEBUG}
      WriteLn('Not equal: ', fCarets[I].bcStart.Char, ' != ', fCarets[I].bcEnd.Char);
      {$ENDIF}
      Result := True;
      Break;
    end;
end;

procedure TCustomSynEdit.SetWordWrap(const Value: Boolean);
var
  vTempBlockBegin, vTempBlockEnd : TBufferCoord;
  vOldTopLine: Integer;
  vShowCaret: Boolean;
begin
  if WordWrap <> Value then
  begin
    Invalidate; // Better Invalidate before changing LeftChar and TopLine
    vShowCaret := CaretInView;
    vOldTopLine := RowToLine(TopLine);
    if Value then
    begin
      fWordWrapPlugin := TSynWordWrapPlugin.Create(Self);
      LeftChar := 1;
      if eoWrapAgainstMargin in fOptions then
        fRightEdgeShow := True;
    end
    else
      fWordWrapPlugin := nil;

    fTopLine := LineToRow(vOldTopLine);
    UpdateScrollBars;

    // constrain caret position to MaxScrollWidth if eoScrollPastEol is enabled
    if (eoScrollPastEol in Options) then
    begin
      InternalCaretXY := CaretXY;
      vTempBlockBegin := BlockBegin;
      vTempBlockEnd := BlockEnd;
      SetBlockBegin(vTempBlockBegin);
      SetBlockEnd(vTempBlockEnd);
    end;
    if vShowCaret then
      EnsureCursorPosVisible;
  end;
end;

// § Garnet
function TCustomSynEdit.GetDisplayLineCount: Integer;

  function GetTotalCollapsedLines: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to fAllFoldRanges.AllCount - 1 do
      if not fAllFoldRanges[I].ParentCollapsed then
        Inc(Result, fAllFoldRanges[I].LinesCollapsed);
  end;

begin
  if fWordWrapPlugin = nil then
    Result := Lines.Count - GetTotalCollapsedLines
  else if Lines.Count = 0 then
    Result := 0
  else begin
    Result := fWordWrapPlugin.GetRowCount;
  end;
end;

// § Garnet
function TCustomSynEdit.LineToRow(ALine: Integer): Integer;
begin
  if not WordWrap then
    Result := GetUnRealLineNumber(aLine)
  else
    Result := fWordWrapPlugin.LineToRow(ALine);
end;

function TCustomSynEdit.RowToLine(ARow: Integer): Integer;
begin
  if not WordWrap then
    Result := GetRealLineNumber(ARow)
  else
    Result := fWordWrapPlugin.RowToLine(ARow);
end;
// § Garnet

procedure TCustomSynEdit.SetInternalDisplayXY(const aPos: TDisplayCoord);
begin
  IncPaintLock;
  InternalCaretXY := DisplayToBufferPos(aPos);
  fCaretAtEOL := WordWrap and (aPos.Row <= fWordWrapPlugin.GetRowCount) and
    (aPos.Column > fWordWrapPlugin.GetRowLength(aPos.Row, fCaretY)) and // § Garnet WW
    (DisplayY <> aPos.Row);
  DecPaintLock;
  UpdateLastCaretX;
end;

procedure TCustomSynEdit.SetWantReturns(Value: Boolean);
begin
  fWantReturns := Value;
end;

procedure TCustomSynEdit.SetWantTabs(Value: Boolean);
begin
  fWantTabs := Value;
end;

procedure TCustomSynEdit.ExecuteSnippet(const S, Directory: UnicodeString);
var
  I, nX, nY, nR, nI, NestLevel: Integer;
  R: UnicodeString;
  Found: Boolean;
  MirrorCheck: array of Integer;

  { Add mirror index to check it later }
  procedure AddMirrorCheck(Value: Integer);
  var
    I: Integer;
  begin
    for I := 0 to High(MirrorCheck) do
      if MirrorCheck[I] = Value then
        Exit;
    SetLength(MirrorCheck, Succ(Length(MirrorCheck)));
    MirrorCheck[High(MirrorCheck)] := Value;
  end;

  { Terminates on string end or found closing bracket is subexpression }
  function ProcessSnippet(Subexpression: Boolean; Silent: Boolean): Boolean;

    { Returns -1 on invalid statement }
    function GetDigit: Integer;
    var
      J: Integer;
    begin
      { Prepare }
      Result := -1;

      { Look }
      J := I;
      while (J <= Length(S)) and (S[J] in ['0'..'9']) do
        Inc(J);

      { Get }
      if J > I then
        Result := StrToIntDef(Copy(S, I, J - I), -1);

      { Success? }
      if Result > -1 then
        I := J;
    end;

    { Returns a valid variable }
    function GetVariable: Integer;
    const
      KnownVars: array [0..2] of UnicodeString = (
        'LETTERPRESS_WORD',
        'LETTERPRESS_LINE',
        'LETTERPRESS_SELECTION'
      );
    var
      J: Integer;
      V: UnicodeString;
    begin
      { Prepare }
      Result := -1;
      V := EmptyStr;

      { Look }
      J := I;
      while (J <= Length(S)) and (S[J] in ['A'..'Z', '_']) do
        Inc(J);

      { Get }
      if J > I then
        V := Copy(S, I, J - I);

      { Validate }
      if V <> EmptyStr then
        for J := Low(KnownVars) to High(KnownVars) do
          if KnownVars[J] = V then
          begin
            Result := J;
            Inc(I, Length(V));
            Exit;
          end;
    end;

    { Returns true on successfull expansion }
    function ExpandVariable(const V: Integer; Complex: Boolean = True): Boolean;
    const
      LETTERPRESS_WORD = 0;
      LETTERPRESS_LINE = 1;
      LETTERPRESS_SELECTION = 2;
    var
      J: Integer;
      E: UnicodeString;
      Search, Replace: UTF8String;
      Options: UnicodeString;
      NewOptions: TRegexOptions;
    begin
      { Prepare }
      Result := False;
      if V = -1 then
        Exit;
      Result := True;

      { Validate }
      E := EmptyStr;
      if not Silent then
        case V of
          LETTERPRESS_WORD: E := GetWordAtCursor;
          LETTERPRESS_LINE: E := GetLineText;
          LETTERPRESS_SELECTION: E := GetSelText;
        end;

      { See if substitution provided }
      if Complex then
      begin

        { See if default value provided }
        if S[I] = ':' then
        begin
          Inc(I);
          if E = EmptyStr then
            if not ProcessSnippet(True, Silent) then
            begin
              Result := False;
              Exit;
            end
            else
          else
            if not ProcessSnippet(True, True) then
            begin
              Result := False;
              Exit;
            end;
        end

        { See if replacement provided }
        else if S[I] = '/' then
        begin
          { Initialize }
          Search := EmptyAnsiStr;
          Replace := EmptyAnsiStr;

          { Read search pattern }
          Inc(I); // Skip '/'
          J := I;
          while I <= Length(S) do
          begin
            if (S[I] = '/') and (S[Pred(I)] <> '\') then Break;
            Inc(I);
          end;
          if not Silent then
            Search := UnicodeStringToUTF8(Copy(S, J, I - J));

          if I < Length(S) then
          begin
            { Read replace pattern }
            Inc(I); // Skip '/'
            J := I;
            while I <= Length(S) do
            begin
              if (S[I] = '/') and (S[Pred(I)] <> '\') then Break;
              Inc(I);
            end;
            if not Silent then
              Replace := UnicodeStringToUTF8(StringReplace(Copy(S, J, I - J), '\/', '/', [rfReplaceAll]));

            { Read options }
            if I < Length(S) then
            begin
              Inc(I); // Skip '/'
              J := I;
              while I <= Length(S) do
              begin
                if S[I] = '}' then Break;
                Inc(I);
              end;
              if not Silent then
                Options := Copy(S, J, I - J);
            end;
          end;

          { Need to process variable? }
          if (Search <> EmptyAnsiStr) and (Replace <> EmptyAnsiStr) then
          begin
            NewOptions := StringToRegexOptions(Options);
            E := TRegex.Replace(UnicodeStringToUTF8(E), Search, Replace, NewOptions);
          end;
        end;
      end;

      { Insert variable if it was provided }
      if E <> EmptyStr then
      begin

        { Count lines in variable being inserted }
        J := 1;
        while J <= Length(E) do
        begin
          if E[J] = #13 then
          begin
            Inc(nY);
            nX := 1 + nI;
            if (J <= Length(E)) and (E[Succ(J)] = #10) then
              Inc(J);
          end
          else if E[J] = #10 then
          begin
            Inc(nY);
            nX := 1 + nI;
          end;
          Inc(J);
          Inc(nX);
        end;

        { Put variable in buffer }
        if Length(R) < nR + Length(E) then
          SetLength(R, (nR + Length(E)) * 2);
        Move(E[1], R[Succ(nR)], Length(E) * SizeOf(Char));
        Inc(nR, Length(E));
      end;

      { Skip bracket }
      if Complex then
        Inc(I); // Skip '}'
    end;

    { Parses mirror or placeholders. Returns false on fail }
    function ExpandMirror(const M: Integer; Complex: Boolean = True): Boolean;
    var
      J, N: Integer;
      sPatternOptions: UTF8String;
    begin
      Result := True;
      if not Silent then
      begin
        SetLength(fCarets, Succ(Length(fCarets)));
        N := High(fCarets);
        with fCarets[N] do
        begin
          { Set index }
          nIndex := M;

          { Set next level }
          nNestLevel := NestLevel;

          { Decide if mirror }
          if M = 0 then
            bMirror := False
          else begin
            bMirror := True;
            AddMirrorCheck(M);
          end;

          { Reset patters }
          sSearchPattern := EmptyAnsiStr;
          sReplacePattern := EmptyAnsiStr;
          ePatternOptions := [roSingleLine];

          { Set start }
          bcStart := BufferCoord(nX, nY);
          bcEnd := bcStart;
        end;
      end;

      { Read additional things }
      if Complex and (M > 0) then
      begin

        { See if subpattern provided (or simple placeholder) }
        if S[I] = ':' then
        begin
          Inc(I); // Skip ':'

          { Process subpattern }
          Inc(NestLevel);
          if not ProcessSnippet(True, False) then
          begin
            Dec(NestLevel);
            Result := False;
            Exit;
          end;
          Dec(NestLevel);

          { Set end }
          if not Silent then
          begin
            fCarets[N].bMirror := False;
            fCarets[N].bcEnd := BufferCoord(Max(nX, 1), nY);
          end;
        end

        { See if search / replacement patterns provided }
        else if S[I] = '/' then
        begin

          { Read search pattern }
          Inc(I); // Skip '/'
          J := I;
          while I <= Length(S) do
          begin
            if (S[I] = '/') and (S[Pred(I)] <> '\') then Break;
            Inc(I);
          end;
          if not Silent then
            fCarets[N].sSearchPattern := UnicodeStringToUTF8(Copy(S, J, I - J));

          { Can search for replace pattern }
          if (I < Length(S)) then
          begin
            { Read replace pattern }
            Inc(I); // Skip '/'
            J := I;
            while I <= Length(S) do
            begin
              if (S[I] = '/') and (S[Pred(I)] <> '\') then Break;
              Inc(I);
            end;
            if not Silent then
              fCarets[N].sReplacePattern := UnicodeStringToUTF8(Copy(S, J, I - J));

            { Can read pattern options }
            if (I < Length(S)) then
            begin
              { Read pattern options }
              Inc(I); // Skip '/'
              J := I;
              while S[I] <> '}' do
              begin
                Inc(I);
                if I > Length(S) then begin Result := False; Break; end;
              end;
              if not Silent then
              begin
                sPatternOptions := Copy(S, J, I - J);

                { Parse options string }
                fCarets[N].ePatternOptions := StringToRegexOptions(UTF8ToUnicodeString(sPatternOptions));
              end;
            end;
          end;
        end

        { See if shell command provided }
        else if S[I] = '`' then
        begin

          { Read everything until next apostroph }
          Inc(I); // Skip '`'
          J := I;
          while I <= Length(S) do
          begin
            if (S[I] = '`') and (S[Pred(I)] <> '\') then Break;
            Inc(I);
          end;
          if not Silent then
            fCarets[N].sShellCommand := UnicodeStringToUTF8(Copy(S, J, I - J));

          { Skip last '`' to the closing bracket }
          Inc(I);
        end;

        { Skip bracket }
        Result := S[I] = '}';
        if Result then
          Inc(I); // Skip '}'
      end

      { Zero tab stops can't have any additinal syntax }
      else if (M = 0) and Complex then
        Result := False;
    end;

    { Writes current character into buffer }
    procedure PutCurrentChar;
    begin
      if Silent then Exit;
      if Length(R) < nR + 1 then
        SetLength(R, (nR + 1) * 2);
      Move(S[I], R[Succ(nR)], 1 * SizeOf(Char));
      Inc(nR, 1);
    end;

  var
    M: Integer;
  begin
    { Initialize }
    Result := True;

    { Place carets }
    while I <= Length(S) do
    begin
      { Stop? }
      if Subexpression and (S[I] = '}') and (S[Pred(I)] <> '\') then
        Exit

      { Mirror found? }
      else if (S[I] = '$') and ((I = 1) or (S[Pred(I)] <> '\')) then
      begin
        { Skip '$' }
        Inc(I);

        { Nothing follows next }
        if I > Length(S) then
        begin
          Result := False;
          Exit;
        end;

        { Advanced mirror, subexpression or constant expression }
        if S[I] = '{' then
        begin
          { Skip bracket }
          Inc(I);

          { Mirror? }
          M := GetDigit;
          if M > -1 then

            { We must exit on incorrect mirror syntax }
            if not ExpandMirror(M) then begin Result := False; Exit; end
            else

          { We must exit if no mirror index or variable provided }
          else if not ExpandVariable(GetVariable) then
          begin
            Result := False;
            Exit;
          end;
        end

        { Simple constant expression or simple mirror }
        else begin

          { Mirror? }
          M := GetDigit;
          if M > -1 then

            { We must exit on incorrect mirror syntax }
            if not ExpandMirror(M, False) then begin Result := False; Exit; end
            else

          { We must exit if no mirror index or variable provided }
          else if not ExpandVariable(GetVariable, False) then
          begin
            Result := False;
            Exit;
          end;
        end;
      end

      { See if standalone shell command provided }
      else if (S[I] = '`') and ((I = 1) or (S[Pred(I)] <> '\')) then
      begin
        if not Silent then
        begin
          SetLength(fCarets, Succ(Length(fCarets)));

          { Standalone shell commands are placed in carets too but with
            invalid (-1) index so that they can be easiliy distinguished }
          fCarets[High(fCarets)].nIndex := -1;
          fCarets[High(fCarets)].bcStart.Char := nX;
          fCarets[High(fCarets)].bcStart.Line := nY;
          fCarets[High(fCarets)].bcEnd := fCarets[High(fCarets)].bcStart;
        end;

        { Read everything until next apostroph }
        Inc(I); // Skip '`'
        M := I;
        while I <= Length(S) do
        begin
          if (S[I] = '`') and (S[Pred(I)] <> '\') then Break;
          Inc(I);
        end;
        if not Silent then
          fCarets[High(fCarets)].sShellCommand := UnicodeStringToUTF8(Copy(S, M, I - M));

        { Skip last '`' to the closing bracket }
        Inc(I);
      end

      { EOLs }
      else if S[I] = #13 then
      begin
        PutCurrentChar;
        Inc(nY); Inc(I);
        nX := 1 + nI;
        if (I <= Length(S)) and (S[I] = #10) then
        begin
          PutCurrentChar;
          Inc(I);
        end;
      end
      else if S[I] = #10 then
      begin
        PutCurrentChar;
        Inc(nY); Inc(I);
        nX := 1 + nI;
      end

      { Simply put char in buffer }
      else begin

        { Skip if it's an escape char }
        if S[I] = '\' then
          if (I < Length(S)) and ((S[Succ(I)] = '$') or (S[Succ(I)] = '`') or
            (Subexpression and (S[Succ(I)] = '}'))) then
          begin
            Inc(I);
            Continue;
          end;

        { Put }
        PutCurrentChar;
        Inc(nX); Inc(I);
      end;
    end;
  end;

var
  J: Integer;
  D: UnicodeString;
begin
  { Prepare }
  I := 1;
  nX := fCaretX;
  nY := fCaretY;
  nI := GetLineLeadingWhite(Self, nY);
  NestLevel := 0;
  SetLength(fCarets, 0);
  SetLength(MirrorCheck, 0);

  { Allocate buffer }
  nR := 0;
  SetLength(R, Length(S));

  { Process }
  if not ProcessSnippet(False, False) then Exit;
  fSnippetDir := Directory;

  { Fix }
  SetLength(R, nR);

  { Enusre last placeholder ($0) exists }
  Found := False;
  for I := 0 to High(fCarets) do
    if fCarets[I].nIndex = 0 then
    begin
      Found := True;
      Break;
    end;
  if not Found then
  begin
    SetLength(fCarets, Succ(Length(fCarets)));
    with fCarets[High(fCarets)] do
    begin
      nIndex := 0;
      bMirror := False;
      bcStart := BufferCoord(nX, nY);
      bcEnd := bcStart;
    end;
  end;

  { 1. Check placeholders. Each placeholder index set must have at least one
    not-a-mirror placeholder }
  for I := 0 to High(MirrorCheck) do
  begin
    Found := False;
    for J := 0 to High(fCarets) do
      if (fCarets[J].nIndex = MirrorCheck[I]) and not fCarets[J].bMirror then
      begin
        Found := True;
        Break;
      end;

    { Make the first one not-a-mirror }
    if not Found then
      for J := 0 to High(fCarets) do
        if fCarets[J].nIndex = MirrorCheck[I] then
        begin
          fCarets[J].bMirror := False;
          Break;
        end;
  end;

  { Everything is fine and we can enter snippet mode }
  fUndoList.BeginBlock;
  SelText := R;
  fSnippet := True;
  fSmartCaretsUpdating := True;

  { 2. Expand placeholder mirrors }
  for I := 0 to High(MirrorCheck) do
    for J := 0 to High(fCarets) do
      if (fCarets[J].nIndex = MirrorCheck[I]) and not fCarets[J].bMirror and
        not CaretsEqual(fCarets[J].bcStart, fCarets[J].bcEnd) then
      begin
        SetCaretAndSelection(fCarets[J].bcStart, fCarets[J].bcStart, fCarets[J].bcEnd);
        D := GetSelText;
        SetCaretAndSelection(fCarets[J].bcStart, fCarets[J].bcStart, fCarets[J].bcStart);
        MirrorCommand(ecNone, #0, nil, fCarets[J].bcStart, fCarets[J].bcStart, fCarets[J].bcStart,
          PChar(D));
        Break;
      end;

  { Finalize mirror data }
  SetLength(MirrorCheck, 0);

  { 3. Expand standalone shell commands }
  for J := 0 to High(fCarets) do
    if (fCarets[J].nIndex = -1) and (fCarets[J].sShellCommand <> EmptyAnsiStr) then
    begin
      SetCaretAndSelection(fCarets[J].bcStart, fCarets[J].bcStart, fCarets[J].bcStart);
      D := RunSnippetShellCode(fCarets[J].sShellCommand, False);
      SetSelTextPrimitive(D);
    end;

  { Go to the first placeholder }
  fUndoList.EndBlock;
  fUndoList.AddGroupBreak;
  NextSnippetVar(True);
  fSmartCaretsUpdating := False;

  { Debug }
  {WriteLn('Carets:');
  for I := 0 to High(fCarets) do
    with fCarets[I] do
      WriteLn(nIndex, ' => ', bcStart.Char, ' : ', bcStart.Line, '; ', bcEnd.Char, ' : ', bcEnd.Line, '; ',
        sSearchPattern, ' : ', sReplacePattern, '; ', sShellCommand, '. ', bMirror, ', ', nNestLevel);
  WriteLn('');}
end;

procedure TCustomSynEdit.EditEachLineInSelection;
var
  I, J: Integer;
  BB, BE: TBufferCoord;
begin
  BB := BlockBegin;
  BE := BlockEnd;
  J := MaxInt;
  for I := BB.Line to BE.Line do
    if fLines.AccessStringLength(Pred(I)) < J then
      J := fLines.AccessStringLength(Pred(I));
  Dec(J);
  SetLength(fCarets, Succ(BE.Line - BB.Line));
  for I := 0 to High(fCarets) do
    with fCarets[I] do
    begin
      nIndex := 1;
      if I > 0 then
        bMirror := True
      else
        bMirror := False;
      bcStart := BufferCoord(fLines.AccessStringLength(Pred(BB.Line + I)) - J, BB.Line + I);
      bcEnd := BufferCoord(fLines.AccessStringLength(Pred(BB.Line + I)) + 1, BB.Line + I);
    end;
  fSnippet := True;
  fColumn := True;
  fSmartCaretsUpdating := True;
  SetCaretAndSelection(fCarets[0].bcEnd, fCarets[0].bcEnd, fCarets[0].bcEnd);
  fSmartCaretsUpdating := False;
end;

procedure TCustomSynEdit.SyncEditLines;
type
  TRecognizedWord = record
    sWord: UnicodeString;
    bcStart, bcEnd: TBufferCoord;
    bSealed: Boolean;
  end;
var
  I, N: Integer;
  C, OldC, BB, BE: TBufferCoord;
  RecognizedWords: array of TRecognizedWord;
  S: UnicodeString;
  bFound: Boolean;
begin
  BB := BlockBegin;
  BE := BlockEnd;

  SetLength(fCarets, 1);
  fCarets[High(fCarets)].nIndex := 0;
  fCarets[High(fCarets)].bcStart := BE;
  fCarets[High(fCarets)].bcEnd := BE;

  SetLength(RecognizedWords, 0);
  BB.Char := Min(BB.Char - 1, 1);
  SetCaretAndSelection(BB, BB, BB);

  C := NextWordPos;
  while True do
  begin
    if C.Line = BE.Line then
      if C.Char >= BE.Char then
        Break
      else
    else if C.Line > BE.Line then
      Break;
    S := GetWordAtRowCol(C);
    bFound := False;
    for I := 0 to High(RecognizedWords) do
      if RecognizedWords[I].sWord = S then
      begin
        N := I;
        bFound := True;
        Break;
      end;

    if bFound then
    begin
      if not RecognizedWords[N].bSealed then
      begin
        SetLength(fCarets, Succ(Length(fCarets)));
        with fCarets[High(fCarets)] do
        begin
          nIndex := Succ(N);
          bMirror := False;
          bcStart := RecognizedWords[N].bcStart;
          bcEnd := RecognizedWords[N].bcEnd;
          RecognizedWords[N].bSealed := True;
        end;
      end;
      SetLength(fCarets, Succ(Length(fCarets)));
      with fCarets[High(fCarets)] do
      begin
        nIndex := Succ(N);
        bMirror := True;
        bcStart := WordStartEx(C);
        bcEnd := WordEndEx(C);
      end;
    end
    else begin
      SetLength(RecognizedWords, Succ(Length(RecognizedWords)));
      with RecognizedWords[High(RecognizedWords)] do
      begin
        sWord := S;
        bSealed := False;
        bcStart := WordStartEx(C);
        bcEnd := WordEndEx(C);
      end;
    end;

    OldC := C;
    C := NextWordPosEx(C);
    if CaretsEqual(C, OldC) then
      Break;
  end;
  fSnippet := True;
  fUndoList.AddGroupBreak;
  NextSnippetVar(True);
end;

function TCustomSynEdit.RunSnippetShellCode(const Command: UnicodeString;
  FromMirror: Boolean; const CurrentVar: UnicodeString = ''): UnicodeString;
var
  Runner: TOutputCommandThread;
  ChildStdRd, ChildStdWr, ChildStdMon1, ChildStdMon2, ChildProc: THandle;

  function BuildSnippetEnvironment: UnicodeString;
  var
    I: Integer;
    Caret, BB, BE: TBufferCoord;
  begin
    Result := EmptyStr;
    Caret := CaretXY;
    BB := BlockBegin;
    BE := BlockEnd;
    if FromMirror then
      Result := 'LETTERPRESS_SNIPPET_VAR=' + CurrentVar + #26;
    for I := 0 to High(fCarets) do
      if not fCarets[I].bMirror and (fCarets[I].nIndex > 0) then
      with fCarets[I] do
      begin
        SetCaretAndSelection(bcStart, bcStart, bcEnd);
        Result := Result + 'LETTERPRESS_PLACEHOLDER_' + IntToStr(fCarets[I].nIndex) +
          '=' + GetSelText + #26;
      end;
    SetCaretAndSelection(Caret, BB, BE);
  end;

begin
  { Initialize }
  Result := EmptyStr;

  { Try to launch given shell command }
  if not LaunchPipedProcess(ChildStdRd, ChildStdWr, ChildStdMon1, ChildStdMon2,
    ChildProc, Command, fSnippetDir, fSnippetDir, BuildSnippetEnvironment)
  then
    Exit;

  { Lock editing }
  fReadOnly := True;

  { Run }
  Runner := TOutputCommandThread.Create(True, True, True, ChildStdRd, ChildProc);
  Runner.OnOutputCompleted := RoutineCommandCompleted;
  Runner.Start;

  { Wait for output }
  fRoutineCompleted := False;
  fRoutineOutput := EmptyStr;
  while not fRoutineCompleted do
  begin
    { Hard reset requested }
    if fRoutineTerminated then
    begin
      { Stop }
      fReadOnly := False;
      Runner.Terminate;
      Exit;
    end;
    Application.ProcessMessages;
  end;

  { Successfull execution }
  Result := fRoutineOutput;
end;

procedure TCustomSynEdit.RoutineCommandCompleted(Sender: TObject;
  const Data: UTF8String);
begin
  fReadOnly := False;
  fRoutineOutput := UTF8ToUnicodeString(Data);
  fRoutineCompleted := True;
end;

function TCustomSynEdit.CurrentSnippetCaret(Char: Integer = 0;
  Line: Integer = 0; ForceCheck: Boolean = False): Integer;

// Since placeholders can be nested into each other we must look for the most
// deep one. Our placeholder array is sorted and it's easily achieved by
// searching it backwards. (Sub-placeholders appear after their parent.)

var
  I: Integer;
begin
  { Fallback to error }
  Result := -1;
  if not fSnippet then
    Exit;
  if (fCurrCaret > -1) and (fCurrCaret < Length(fCarets)) and not ForceCheck then
  begin
    Result := fCurrCaret;
    Exit;
  end;
  if Char = 0 then Char := fCaretX;
  if Line = 0 then Line := fCaretY;

  { Look for caret }
  for I := High(fCarets) downto 0 do
    with fCarets[I] do
    begin
      { Do not look into standalone shell commands and zero placeholder }
      if fCarets[I].nIndex < 1 then
        Continue;

      { See if we fall in range }
      if InRange(Line, bcStart.Line, bcEnd.Line) then
        if bcStart.Line <> bcEnd.Line then
          if (Line = bcStart.Line) and (Char >= bcStart.Char) then
            Result := I
          else if (Line = bcEnd.Line) and (Char <= Succ(bcEnd.Char)) then
            Result := I
          else if (Line > bcStart.Line) and (Line < bcEnd.Line) then
            Result := I
          else
        else if InRange(Char, bcStart.Char, Succ(bcEnd.Char)) then
          Result := I;

      { Found }
      if Result > -1 then
      begin

        { We can't edit mirrors }
        if not fInsertingMirrors and not fUndoRedo and bMirror then
        begin
          Result := -1;
          Continue;
        end;

        { Done }
        Break;
      end;
    end;
end;

procedure TCustomSynEdit.NextSnippetVar(First: Boolean = False);
var
  I, C: Integer;
begin
  { Get current snippet caret }
  if First then
    C := 0
  else begin
    C := CurrentSnippetCaret;
    if C = -1 then
    begin
      CancelSnippet;
      Exit;
    end;
    C := fCarets[C].nIndex;
  end;

  { Search for the closest caret with greater index }
  for I := 0 to High(fCarets) do
    if (fCarets[I].nIndex > C) and not fCarets[I].bMirror then
    begin
      SetCaretAndSelection(fCarets[I].bcStart, fCarets[I].bcStart, fCarets[I].bcEnd);
      Exit;
    end;

  { If no next placeholder, then jump on zero placeholder (exit snippet) }
  for I := 0 to High(fCarets) do
    if fCarets[I].nIndex = 0 then
    begin
      SetCaretAndSelection(fCarets[I].bcStart, fCarets[I].bcStart, fCarets[I].bcEnd);
      CancelSnippet;
      Exit;
    end;

  { No next snippet or zero placeholder found. Halt }
  CancelSnippet;
end;

procedure TCustomSynEdit.PrevSnippetVar;
var
  I, C: Integer;
begin
  { Get current snippet caret }
  C := CurrentSnippetCaret;
  if C = -1 then
  begin
    CancelSnippet;
    Exit;
  end;

  { Search for the closest caret with greater index }
  for I := High(fCarets) downto 0 do
    if (fCarets[I].nIndex < fCarets[C].nIndex) and (fCarets[I].nIndex > 0) and
      not fCarets[I].bMirror then
    begin
      SetCaretAndSelection(fCarets[I].bcStart, fCarets[I].bcStart, fCarets[I].bcEnd);
      Break;
    end;
end;

procedure TCustomSynEdit.CancelSnippet(Check: Boolean = False);
begin
  if not fSnippet then
    Exit;
  if Check then
    if CurrentSnippetCaret(0, 0, True) > -1 then
      Exit;
  SetLength(fCarets, 0);
  fSnippet := False;
  fColumn := False;
  if fSelections then
  begin
    fSelections := False;
    InitializeCaret(True);
  end;
  if not (csDestroying in ComponentState) then
    if fPaintLock = 0 then
      Invalidate;
end;

procedure TCustomSynEdit.InsertedInSnippet(const X, Y, DX, DY: Integer);
var
  I, C, OldStartCaret, OldEndCaret: Integer;
  bSpecialCase: Boolean;

  { Deletes nested placeholders if parent placeholder
    text no longer can hold them }
  procedure CheckNestedPlaceholders;
  var
    J: Integer;
  begin
    { Need to check only on deletion }
    if (DX >= 0) and (DY >= 0) then
      Exit;

    { Look for nested placeholders }
    J := C;
    while J < Length(fCarets) do
    begin

      { Skip invalid }
      if fCarets[J].nIndex < 0 then Continue;

      { Check if still in range }
      if (fCarets[J].bcStart.Line > fCarets[C].bcEnd.Line) or
        ((fCarets[J].bcStart.Line = fCarets[C].bcEnd.Line) and
         (fCarets[J].bcStart.Char > fCarets[C].bcEnd.Char))
      then
        Break;

      { Check if in range of text being deleted }
      if DY < 0 then
        if J > C then
          fCarets[J].nIndex := -2
        else

      { Mutiline deletion alays finishes all nested placeholders.
        In single line we can analyze }
      else begin

        { Check start caret }
        if (X + DX < fCarets[J].bcStart.Char) and (X > fCarets[J].bcStart.Char) then
          fCarets[J].nIndex := -1

        { Check end caret }
        else if (X + DX < fCarets[J].bcEnd.Char) and (X > fCarets[J].bcEnd.Char) then
          fCarets[J].nIndex := -1;
      end;

      { Next placeholder }
      Inc(J);
    end;

    { Check if deleting at start. I.e. need to finalize placeholder }
    if CaretsEqual(fCarets[C].bcStart, BufferCoord(X, Y)) then
      fCarets[C].nIndex := -1;
  end;

begin
  { Check }
  if not fSnippet then Exit;

  { Get current caret where insertion / deletion took place }
  C := CurrentSnippetCaret(X, Y);

  { Cancel when last operation overflew bounds }
  if C = -1 then Exit;
  CheckNestedPlaceholders;

  { Fix bounds }
  for I := High(fCarets) downto 0 do
  begin

    { Skip invalid placeholders }
    if fCarets[I].nIndex < 0 then
      Continue;

    { Skip placeholder on the following condition. I'll try to explain.
      If the caret of change is right at the end of the placeholder AND
      there's another placeholder right after it which starts on the same
      position where current ends AND it's level of nest is the same as
      current, then we must skip current and increase / decrease the next one }
    if CaretsEqual(fCarets[I].bcStart, BufferCoord(X, Y)) and (I > 0) and
      CaretsEqual(fCarets[Pred(I)].bcEnd, BufferCoord(X, Y)) and
      (fCarets[Pred(I)].nIndex > -1) and
      (fCarets[Pred(I)].nNestLevel = fCarets[I].nNestLevel)
    then
      bSpecialCase := True
    else
      bSpecialCase := False;

    { Fix all carets on the line where multiple strings were inserted }
    if DY <> 0 then
    begin

      { Increase start x coord if it's ahead insertion place }
      OldStartCaret := fCarets[I].bcStart.Char;
      if (fCarets[I].bcStart.Line = Y) and ((fCarets[I].bcStart.Char > X) or bSpecialCase) then
        fCarets[I].bcStart.Char := fCarets[I].bcStart.Char - X + DX + 1;

      { Increase end x coord if it's ahead insertion place }
      OldEndCaret := fCarets[I].bcEnd.Char;
      if (fCarets[I].bcEnd.Line = Y) and (fCarets[I].bcEnd.Char > X) then
        fCarets[I].bcEnd.Char := fCarets[I].bcEnd.Char - X + DX + 1;

      { Increase start line if it is ahead of insertion point }
      if (fCarets[I].bcStart.Line > Y) or ((fCarets[I].bcStart.Line = Y)
        and ((OldStartCaret > X) or bSpecialCase))
      then
        Inc(fCarets[I].bcStart.Line, DY);

      { Increase end line if ahead of insertion point }
      if (fCarets[I].bcEnd.Line > Y) or ((fCarets[I].bcEnd.Line = Y)
        and (OldEndCaret > X))
      then
        Inc(fCarets[I].bcEnd.Line, DY);
    end

    { Only chars inserted. Fix carets just on this line }
    else if DX <> 0 then
    begin

      { Incrase start x coord if it's behind insertion }
      if (fCarets[I].bcStart.Line = Y) and ((fCarets[I].bcStart.Char > X) or bSpecialCase) then
        Inc(fCarets[I].bcStart.Char, DX);

      { Incrase end x coord if it's behind insertion }
      if (fCarets[I].bcEnd.Line = Y) and (fCarets[I].bcEnd.Char >= X) then
        Inc(fCarets[I].bcEnd.Char, DX);
    end;

    { Seize selection block to caret }
    if fSelections and (C = I) then
    begin
      fCarets[I].bcStart := fCarets[I].bcEnd;
      fCarets[I].bcCaret := fCarets[I].bcStart;
    end;
  end;

  { Debug }
  {WriteLn('Carets:');
  for I := 0 to High(fCarets) do
    with fCarets[I] do
      WriteLn(nIndex, ' => ', bcStart.Char, ' : ', bcStart.Line, '; ', bcEnd.Char, ' : ', bcEnd.Line, '; ',
        sSearchPattern, ' : ', sReplacePattern, '; ', sShellCommand, '.', bMirror);
  WriteLn('');}
end;

procedure TCustomSynEdit.MirrorCommand(Command: Integer; AChar: Char;
  AData: Pointer; CommandPos, CommandBegin, CommandEnd: TBufferCoord;
  AText: PChar);
var
  I, J, C, DX, DY, DXB, DYB, DXE, DYE, NX, NY, NXB, NYB, NXE, NYE: Integer;
  Mirrors: array of Integer;
  bAfterEol: Boolean;
  Caret, BB, BE: TBufferCoord;
  S: UnicodeString;
  P: PChar;
begin
  { Innitialize }
  if not fSnippet or fInsertingMirrors then
    Exit;

  { Check }
  C := CurrentSnippetCaret;
  if C = -1 then Exit;

  { Get offsets }
  if not fSelections then
  begin
    DY := CommandPos.Line - fCarets[C].bcStart.Line;
    if DY > 0 then
      DX := CommandPos.Char - GetLineLeadingWhite(Self, CommandPos.Line)
    else
      DX := CommandPos.Char - fCarets[C].bcStart.Char;
    DYB := CommandBegin.Line - fCarets[C].bcStart.Line;
    if DYB > 0 then
      DXB := CommandBegin.Char - GetLineLeadingWhite(Self, CommandPos.Line)
    else
      DXB := CommandBegin.Char - fCarets[C].bcStart.Char;
    DYE := CommandEnd.Line - fCarets[C].bcStart.Line;
    if DYE > 0 then
      DXE := CommandEnd.Char - GetLineLeadingWhite(Self, CommandPos.Line)
    else
      DXE := CommandEnd.Char - fCarets[C].bcStart.Char;
  end;

  { Need to fix caret in placeholder }
  if Command = ecGotoXY then
  begin

    { Get real placeholder pos }
    if fCarets[C].bMirror then
    for I := 0 to High(fCarets) do
      if (I <> C) and (fCarets[I].nIndex = fCarets[C].nIndex) and not fCarets[I].bMirror then
      with fCarets[I] do
      begin

        { Set selection bounds }
        if DY > 0 then
        begin
          NX := GetLineLeadingWhite(Self, bcStart.Line + DY) + DX;
          NY := bcStart.Line + DY;
        end
        else begin
          NX := bcStart.Char + DX;
          NY := bcStart.Line + DY;
        end;
        if DYB > 0 then
        begin
          NXB := GetLineLeadingWhite(Self, bcStart.Line + DYB) + DXB;
          NYB := bcStart.Line + DYB;
        end
        else begin
          NXB := bcStart.Char + DXB;
          NYB := bcStart.Line + DYB;
        end;
        if DYE > 0 then
        begin
          NXE := GetLineLeadingWhite(Self, bcStart.Line + DYE) + DXE;
          NYE := bcStart.Line + DYE;
        end
        else begin
          NXE := bcStart.Char + DXE;
          NYE := bcStart.Line + DYE;
        end;

        { Fix if ahead of placeholder bounds }
        if (NY > bcEnd.Line) or ((NY = bcEnd.Line) and (NX > Succ(bcEnd.Char))) then
        begin
          NX := bcStart.Char;
          NY := bcStart.Line;
        end;
        if (NYB > bcEnd.Line) or ((NYB = bcEnd.Line) and (NXB > Succ(bcEnd.Char))) then
        begin
          NXB := bcStart.Char;
          NYB := bcStart.Line;
        end;
        if (NYE > bcEnd.Line) or ((NYE = bcEnd.Line) and (NXE > Succ(bcEnd.Char))) then
        begin
          NXE := bcEnd.Char;
          NYE := bcEnd.Line;
        end;

        { Set caret on this mirror }
        SetCaretAndSelection(BufferCoord(NX, NY), BufferCoord(NXB, NYB),
          BufferCoord(NXE, NYE));

        { Done }
        Break;
      end;

    { ecGotoXY isn't a snippet command and we are all right to use it like that }
    Exit;
  end;

  { Get list of mirros }
  SetLength(Mirrors, 0);
  for I := 0 to High(fCarets) do
    if (I <> C) and (fCarets[I].nIndex = fCarets[C].nIndex) and (fCarets[I].bMirror or fSelections) then
    begin
      SetLength(Mirrors, Succ(Length(Mirrors)));
      Mirrors[High(Mirrors)] := I;
    end;
  if Length(Mirrors) = 0 then Exit;

  { Remember current caret }
  Caret := CaretXY;
  BB := BlockBegin;
  BE := BlockEnd;

  { Mirror command }
  {$IFDEF DEBUG}
  WriteLn('Mirroring');
  {$ENDIF}
  fInsertingMirrors := True;
  IncPaintLock;
  bAfterEol := not (eoScrollPastEol in fOptions);
  if bAfterEol then
    Include(fOptions, eoScrollPastEol);
  try
    for I := 0 to High(Mirrors) do
      with fCarets[Mirrors[I]] do
      begin
        fCurrCaret := Mirrors[I];
        if fSelections and ((Command = ecDeleteLastChar) or
          (Command = ecDeleteChar)) and GetAtLeastOneBlockOfSelection
        then begin
          Command := ecNone;
          {$IFDEF DEBUG}
          WriteLn('Command now ecNone');
          {$ENDIF}
        end;

        { SetSelText() routine mirroring }
        if (Command = ecNone) or (fSelections and GetAtLeastOneBlockOfSelection) then
        begin

          { Set selection bounds }
          if fSelections then
          begin
            NX := bcStart.Char;
            NY := bcStart.Line;
            NXB := bcStart.Char;
            NYB := bcStart.Line;
            NXE := bcEnd.Char;
            NYE := bcEnd.Line;
          end
          else begin
            if DY > 0 then
            begin
              NX := GetLineLeadingWhite(Self, bcStart.Line + DY) + DX;
              NY := bcStart.Line + DY;
            end
            else begin
              NX := bcStart.Char + DX;
              NY := bcStart.Line + DY;
            end;
            if DYB > 0 then
            begin
              NXB := GetLineLeadingWhite(Self, bcStart.Line + DYB) + DXB;
              NYB := bcStart.Line + DYB;
            end
            else begin
              NXB := bcStart.Char + DXB;
              NYB := bcStart.Line + DYB;
            end;
            if DYE > 0 then
            begin
              NXE := GetLineLeadingWhite(Self, bcStart.Line + DYE) + DXE;
              NYE := bcStart.Line + DYE;
            end
            else begin
              NXE := bcStart.Char + DXE;
              NYE := bcStart.Line + DYE;
            end;
          end;

          { Retreat if ahead mirror bounds }
          if (NY > bcEnd.Line) or ((NY = bcEnd.Line) and (NX > Succ(bcEnd.Char))) then
            Continue;
          if (NYB > bcEnd.Line) or ((NYB = bcEnd.Line) and (NXB > Succ(bcEnd.Char))) then
            Continue;
          if (NYE > bcEnd.Line) or ((NYE = bcEnd.Line) and (NXE > Succ(bcEnd.Char))) then
            Continue;

          { Set caret on this mirror }
          SetCaretAndSelection(BufferCoord(NX, NY), BufferCoord(NXB, NYB),
            BufferCoord(NXE, NYE));

          { See if text being mirrored must undergo regex replacement }
          if fSelections then
            P := AText
          else
            if (sSearchPattern <> EmptyAnsiStr) and (sReplacePattern <> EmptyAnsiStr) then
            begin
              S := AText;
              S := UTF8ToUnicodeString(TRegex.Replace(UnicodeStringToUTF8(S), sSearchPattern, sReplacePattern, ePatternOptions));
              P := PChar(S);
            end

            { See if text being mirrored must be interpoalted via shell command }
            else if sShellCommand <> EmptyAnsiStr then
            begin
              S := AText;
              S := RunSnippetShellCode(sShellCommand, True, S);
              P := PChar(S);
            end

            { Just put it as it is }
            else
              P := AText;

          { Insert }
          SetSelectedTextEmpty(P);
        end;

        { Mirroring command execution }
        if Command <> ecNone then
        begin

          { See if text being mirrored must undergo regex replacement }
          if (sSearchPattern <> EmptyAnsiStr) and (sReplacePattern <> EmptyAnsiStr)
            and (Command = ecChar) then
          begin
            SetCaretAndSelection(bcStart, bcStart, bcEnd);
            S := GetSelText + AChar;
            S := UTF8ToUnicodeString(TRegex.Replace(UnicodeStringToUTF8(S),
              sSearchPattern, sReplacePattern, ePatternOptions));

            { Insert }
            P := PChar(S);
            SetSelectedTextEmpty(P);
          end

          { See if text being mirrored must be interpoalted via shell command }
          else if (sShellCommand <> EmptyAnsiStr) and (Command = ecChar) then
          begin
            SetCaretAndSelection(bcStart, bcStart, bcEnd);
            S := GetSelText + AChar;
            S := RunSnippetShellCode(sShellCommand, True, S);

            { Insert }
            P := PChar(S);
            SetSelectedTextEmpty(P);
          end

          { Simply put as it is }
          else begin

            { Set caret bounds }
            if fSelections then
            begin
              NX := fCarets[C].bcCaret.Char;
              NY := fCarets[C].bcCaret.Line;
            end
            else
              if DY > 0 then
              begin
                NX := GetLineLeadingWhite(Self, bcStart.Line + DY) + DX;
                NY := bcStart.Line + DY;
              end
              else begin
                NX := bcStart.Char + DX;
                NY := bcStart.Line + DY;
              end;

            { Retreat if ahead mirror bounds }
            if (NY > bcEnd.Line) or ((NY = bcEnd.Line) and (NX > Succ(bcEnd.Char))) then
              Continue;

            { Set caret on this mirror }
            InternalSetCaretXY(BufferCoord(NX, NY));

            { Re-execute the command here }
            ExecuteCommand(Command, AChar, AData);
          end;
        end;
      end;
  finally

    { Cleanup }
    if bAfterEol then
      Exclude(fOptions, eoScrollPastEol);
    fCurrCaret := C;
    SetCaretAndSelection(Caret, BB, BE);
    DecPaintLock;

    { Allow more precise undo by putting a break after each action chain }
    fUndoList.AddGroupBreak;

    { Done! }
    SetLength(Mirrors, 0);
    fInsertingMirrors := False;
  end;

  { Debug }
  {WriteLn('After:');
  for J := 0 to High(fCarets) do
    with fCarets[J] do
      WriteLn(nIndex, ' => ', bcStart.Char, ' : ', bcStart.Line, '; ', bcEnd.Char, ' : ', bcEnd.Line);
  WriteLn('');}
end;

{ Code folding functions }

// -----------------------------------------------------------------------------

function TCustomSynEdit.FoldRangeForLine(Line: Integer): TSynEditFoldRange;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to fAllFoldRanges.AllCount - 1 do
    with fAllFoldRanges[I] do
      if FromLine = Line then
      begin
        Result := fAllFoldRanges[I];
        Break;
      end;
end;

// -----------------------------------------------------------------------------
// Expands range on line when it's affected
procedure TCustomSynEdit.ExpandCollapsedLine(const ALine: Integer);
var
  FoldRange: TSynEditFoldRange;
begin
  FoldRange := FoldRangeForLine(ALine);
  if FoldRange = nil then
    FoldRange := FoldRangeForLineTo(ALine);
  if Assigned(FoldRange) then
    with fAllFoldRanges do
    begin
      Delete(FoldRange);
      UpdateFoldRanges;
      UpdateWordWrapHiddenOffsets;
    end;
  end;

// -----------------------------------------------------------------------------
// Expand all ranges which are in range of affected lines
procedure TCustomSynEdit.ExpandCollapsedLines(const AFirst, ALast: Integer);
var
  I: Integer;
  FoldRange: TSynEditFoldRange;
begin
  for I := fAllFoldRanges.AllCount - 1 downto 0 do
  begin
    FoldRange := fAllFoldRanges[I];
    if InRange(FoldRange.FromLine, AFirst, ALast) or InRange(FoldRange.ToLine,
      AFirst, ALast)
    then
      fAllFoldRanges.Delete(I);
  end;
  fAllFoldRanges.UpdateFoldRanges;
  UpdateWordWrapHiddenOffsets;
end;

// -----------------------------------------------------------------------------

function TCustomSynEdit.DoTrimTrailingSpaces(const S: UnicodeString;
  ATrim: Boolean): UnicodeString;
var
  I: Integer;
begin
  Result := S;
  if (not (eoTrimTrailingSpaces in fOptions)) or (not ATrim) then
    Exit;
  I := Length(S);
  while (I > 0) and (S[I] < #33) do
    Dec(I);
  if I < Length(S) then
    Result := Copy(S, 1, I);
end;

function TCustomSynEdit.DoTrimTrailingSpaces(ALine: Integer): Integer;
var
  Ln: UnicodeString;
  I, Len: Integer;
begin
  Result := 0;
  if not (eoTrimTrailingSpaces in fOptions) then
    Exit;
  Ln := ExpandLines[ALine-1];
  Len := Length(Ln);
  I := 0;
  while (Len > 0) and (Ln[Len] < #33) do
  begin
    Dec(Len);
    Inc(I);
  end;
  if I = 0 then
    Exit;

  { Actually trim line }
  ExpandLines[ALine-1] := Copy(ExpandLines[ALine-1], 1, Len);
  Result := I;
end;

procedure TCustomSynEdit.UncollapseAll;
begin
  with fAllFoldRanges do
  begin
    ClearAll;
    UpdateFoldRanges;
  end;
  UpdateWordWrapHiddenOffsets;
end;

procedure TCustomSynEdit.InitCodeFolding;
begin
  UncollapseAll;
  InvalidateGutter;
end;

procedure TCustomSynEdit.CollapseAll;
begin
  UncollapseAll;
  { Mark all fold ranges }
end;

function TCustomSynEdit.FoldRangeForLineTo(Line: Integer): TSynEditFoldRange;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to fAllFoldRanges.AllCount - 1 do
    with fAllFoldRanges[I] do
      if ToLine = Line then
      begin
        Result := fAllFoldRanges[I];
        Break;
      end;
end;

// -----------------------------------------------------------------------------
// Both line and row required to reduce conversions. Since it's mostly called
// during paint, we got them all there
function TCustomSynEdit.GetCollapseMarkRect(FoldRange: TSynEditFoldRange;
  Row: Integer; Line: Integer; Indent: Integer = 0): TRect;
begin

  { Prepare rect }
  with Result do
  begin
    Top := (Row - fTopLine) * fTextHeight + 1;
    Bottom := Top + fTextHeight;
  end;

  { In word wrap we will need to find display coords of last char }
  if WordWrap then
  begin
    Result.Left := fTextOffset + fWordWrapPlugin.GetRowLength(Row, Line) * fCharWidth;
    if (eoAlignedWrap in fOptions) and (Indent = 0) then
    begin
      if fWordWrapPlugin.LineToRow(Line) < Row then
        Inc(Result.Left, GetLeadingExpandedLength(fLines.fList^[Pred(Line)].fString, fTabWidth) * fCharWidth);
    end;
  end

  { Can avoid additional operations }
  else
    Result.Left := fTextOffset +
      ExpandLines.ExpandedStringLengths[Line-1] * fCharWidth;

  { Fix rect }
  if eoShowSpecialChars in fOptions then
    Inc(Result.Left, 9);
  With Result do
  begin
    if FoldRange.FoldRegion.Name <> '' then
      Right := Left + (Length(FoldRange.FoldRegion.Name) + 2) * fCharWidth
    else
      Right := Left + fCharWidth * 5;
    Inc(Right, 1);
    Dec(Bottom, 2);
  end;
  InflateRect(Result, -fCharWidth, 0);
end;

procedure TCustomSynEdit.SetCodeFolding(Value: TSynCodeFolding);
begin
  fCodeFolding.Assign(value);
end;

procedure TCustomSynEdit.CodeFoldingOnChange(Event: TSynCodeFoldingChanges);
begin
  if Event = fcEnabled then
  begin
    if fCodeFolding.Enabled then
    begin
      fGutter.RightOffset := 14 // § Garnet
    end
    else begin
      fGutter.RightOffset := 2; // § Garnet
      UncollapseAll;
    end;
  end
  else if Event = fcRescan then
    InitCodeFolding;

  Invalidate;
end;

function TCustomSynEdit.GetRealLineNumber(ALine: Integer): Integer;
var
  Diff, I: Integer;
  FoldRange: TSynEditFoldRange;
begin
  Diff := 0;
  FoldRange := nil;
  Result := aLine;

  for I := 0 to fAllFoldRanges.AllCount - 1 do
    with fAllFoldRanges[I] do
      if (fAllFoldRanges[I] <> FoldRange) and not ParentCollapsed then
        if GetUnRealLineNumber(FromLine) < ALine then
          Inc(Diff, LinesCollapsed)
        else
          Break;

  Inc(Result, Diff);
end;
                        // :-) Temporary )
function TCustomSynEdit.GetUnRealLineNumber(aLine: Integer): Integer;
var
  Diff, I: Integer;
  FoldRange: TSynEditFoldRange;
begin
  Diff := 0;
  FoldRange := FoldRangeForLine(aLine);
  Result := aLine;

  for I := 0 to fAllFoldRanges.AllCount - 1 do
    with fAllFoldRanges[I] do
      if (fAllFoldRanges[I] <> FoldRange) and not ParentCollapsed
      then
        if (ToLine <= aLine) then
          Inc(Diff, LinesCollapsed) // Garnet CF
        else
          Break;

  Dec(Result, Diff);
end;

// -----------------------------------------------------------------------------
// Checks if ALine is folded (resides in one of collapsed blocks)
function TCustomSynEdit.IsLineVisible(ALine: Integer): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to fAllFoldRanges.AllCount - 1 do
    with fAllFoldRanges[I] do
      if (ALine > FromLine) and (ALine < ToLine) then
      begin
        Result := False;
        Break;
      end;
end;

// -----------------------------------------------------------------------------

function TCustomSynEdit.FindMatchingPair(var Open, Close: TBufferCoord;
  FoldingOnly: Boolean = False): Boolean;
var
  nLineStart, nLineEnd, nCharStart, nCharEnd, nLastCheck, nLastCheckStartPos, nLastCheckEndPos, nCheck: Integer;
  nOpenLevel, nCloseLevel: Integer;
  rMatch: TSynTokenMatched;
begin
  { Initialize }
  Result := False;
  nOpenLevel := 1;
  nCloseLevel := 1;
  nCharStart := -1;
  nCharEnd := -1;
  nLineStart := fCaretY;
  nLineEnd := fCaretY;
  nLastCheckStartPos := -1;
  nLastCheckEndPos := -1;

  with fHighlighter do
  begin

    while (nLineStart > 0) and (nLineEnd < fLines.Count) do
    begin

      { Look behind }
      if nOpenLevel > 0 then
      begin
        while True do
        begin
          nLastCheck := 0;

          if nLineStart = 0 then
            Break;

          if nLineStart = 1 then
            ResetRange
          else
            SetRange(fLines.Ranges[Pred(Pred(nLineStart))]);
          SetLine(fLines.fList^[Pred(nLineStart)].fString, Pred(nLineStart));

          while not GetEol do
          begin
            if (GetTokenPos = nLastCheckStartPos) then
              Break;
            if (nLineStart = fCaretY) and (GetTokenPos >= Pred(fCaretX)) then
              Break;
            nCheck := SynTokenMatch.SynEditGetMatchingPartKind(Self, Pred(nLineStart), MatchTokens, FoldingOnly);
            if nCheck <> 0 then
            begin
              nCharStart := GetTokenPos;
              nLastCheck := nCheck;
            end;
            Next;
          end;

          if nLastCheck <> 0 then
          begin
            nLastCheckStartPos := nCharStart;
            if nLastCheck = 1 then
              Dec(nOpenLevel)
            else
              Inc(nOpenLevel);
            Break;
          end;

          nLastCheckStartPos := -1;
          Dec(nLineStart);
        end;
      end;

      if nOpenLevel = 0 then nCloseLevel := 0;

      { Look ahead }
      if nCloseLevel > 0 then
      begin
        while True do
        begin
          nLastCheck := 0;
          if nLineEnd = fLines.Count then
            Break;

          SetRange(fLines.Ranges[Pred(Pred(nLineEnd))]);
          SetLine(fLines.fList^[Pred(nLineEnd)].fString, Pred(nLineEnd));

          if nLineEnd = fCaretY then
            while not GetEol do
            begin
              if GetTokenPos + GetTokenLen > Pred(fCaretX) then
                Break;
              Next;
            end;

          if nLastCheckEndPos > -1 then
            while not GetEol do
            begin
              if GetTokenPos > nLastCheckEndPos then
                Break;
              Next;
            end;

          while not GetEol do
          begin
            nCheck := SynTokenMatch.SynEditGetMatchingPartKind(Self, Pred(nLineEnd), MatchTokens, FoldingOnly);
            if nCheck <> 0 then
            begin
              nCharEnd := GetTokenPos;
              nLastCheck := nCheck;
              Break;
            end;
            Next;
          end;

          if nLastCheck <> 0 then
          begin
            nLastCheckEndPos := nCharEnd;
            if nLastCheck = -1 then
              Dec(nCloseLevel)
            else
              Inc(nCloseLevel);
            Break;
          end;

          nLastCheckEndPos := -1;
          Inc(nLineEnd);
        end;
      end;

      if nOpenLevel = 0 then
        SynEditGetMatchingTokenEx(Self, BufferCoord(Succ(nCharStart), nLineStart),
          fHighlighter.MatchTokens, rMatch, True, FoldingOnly)
      else if nCloseLevel = 0 then
        SynEditGetMatchingTokenEx(Self, BufferCoord(Succ(nCharEnd), nLineEnd),
          fHighlighter.MatchTokens, rMatch, True, FoldingOnly);

      if (nOpenLevel = 0) or (nCloseLevel = 0) then
      begin
        Result := True;
        Open := rMatch.OpenTokenPos;
        Close := rMatch.CloseTokenPos;
        Break;
      end;

    end;
  end;
end;

procedure TCustomSynEdit.SelectCurrentScope;

  procedure SelectRange(Range: TSynRange);
  var
    bFound, bInside: Boolean;
    nCharStart, nLineStart: Integer;
    nCharEnd, nLineEnd: Integer;
  begin
    with fHighlighter as TSynUniSyn do
    begin

      { Look ahead }
      nLineEnd := fCaretY;
      bFound := False;
      while True do
      begin

        { Look until EOL }
        while not GetEol do
        begin
          if not HasInParents(GetRule, Range) then
          begin
            bFound := True;
            Break;
          end;
          Next;
        end;
        if not HasInParents(GetRule, Range) then
          begin
            bFound := True;
            Break;
          end;

        { Found? }
        if bFound then
          Break;

        { Go to next line and repeat }
        if not bFound then
        begin
          Inc(nLineEnd);
          if nLineEnd >= fLines.Count then
            Break;
          SetRange(fLines.Ranges[Pred(Pred(nLineEnd))]);
          SetLine(fLines.fList^[Pred(nLineEnd)].fString, Pred(nLineEnd));
        end;
      end;

      { Remember end position }
      if bFound then
        nCharEnd := GetTokenPos
      else
        nCharEnd := GetTokenPos + GetTokenLen;

      { Look behind }
      nLineStart := fCaretY;
      bFound := False;
      while True do
      begin

        { Done? }
        if nLineStart = 0 then
          Break;

        { Check this line }
        if nLineStart = 1 then
          ResetRange
        else
          SetRange(fLines.Ranges[Pred(Pred(nLineStart))]);
        SetLine(fLines.fList^[Pred(nLineStart)].fString, Pred(nLineStart));

        { Still on scope? }
        if HasInParents(GetRule, Range) then
        begin
          Dec(nLineStart);
          Continue;
        end;

        { Look until EOL or caret for the most far occurence }
        bFound := False;
        bInside := False;
        nCharStart := 0;
        while not GetEol do
        begin
          if HasInParents(GetRule, Range) then
          begin
            bFound := True;
            if (nLineStart = fCaretY) and (GetTokenPos + GetTokenLen > Pred(fCaretX)) then
            begin
              if nCharStart = 0 then
                nCharStart := GetTokenPos;
              Break;
            end;
            if not bInside then
              nCharStart := GetTokenPos;
            bInside := True;
          end
          else begin
            bFound := False;
            bInside := False;
          end;
          Next;
        end;

        { Revert to previous line }
        if not bFound then
        begin
          Inc(nLineStart);
          SetLine(fLines.fList^[Pred(nLineStart)].fString, Pred(nLineStart));
          nCharStart := GetTokenPos;
        end;

        { Done }
        Break;
      end;

      { Do selection }
      SetCaretAndSelection(BufferCoord(Succ(nCharStart), nLineStart),
        BufferCoord(Succ(nCharStart), nLineStart),
        BufferCoord(Succ(nCharEnd), nLineEnd));
    end;
  end;

  procedure SelectSpecialRuleOrToken(Rule: TSynSpecialRule;
    Token: TSynHighlighterAttributes = nil);
  var
    nCharStart, nCharEnd: Integer;
  begin
    with fHighlighter as TSynUniSyn do
    begin

      { Remember start in case we are already on it }
      nCharStart := GetTokenPos;

      { Look ahead }
      while not GetEol do
      begin
        nCharEnd := GetTokenPos;
        if Token <> nil then
          if TSynHighlighterAttributes(GetTokenKind) <> Token then
            Break
          else
        else if GetRule <> Rule then
          Break;
        Next;
      end;

      { Reset line }
      if fCaretY = 1 then
        ResetRange
      else
        SetRange(fLines.Ranges[Pred(Pred(fCaretY))]);
      SetLine(fLines.fList^[Pred(fCaretY)].fString, Pred(fCaretY));

      { Look behind }
      while GetTokenPos + GetTokenLen < Pred(fCaretX) do
      begin
        if Token <> nil then
          if TSynHighlighterAttributes(GetTokenKind) = Token then
            nCharStart := GetTokenPos
          else
        else if GetRule = Rule then
          nCharStart := GetTokenPos;
        Next;
      end;

      { Do selection }
      SetCaretAndSelection(BufferCoord(Succ(nCharStart), fCaretY),
        BufferCoord(Succ(nCharStart), fCaretY),
        BufferCoord(Succ(nCharEnd), fCaretY));
    end;
  end;

var
  currToken: Integer;
  currRule: TSynRule;
begin
  currRule := nil;
  currToken := GetTokenKind(Self, CaretXY);
  if currToken <> 0 then
    currRule := (fHighlighter as TSynUniSyn).GetRule
  else
    currRule := fHighlighter.GetRange;

  if currRule is TSynRange then
    SelectRange(currRule as TSynRange)
  else if (currRule is TSynSpecialRule) and ((currRule as TSynSpecialRule).Style <> EmptyAnsiStr) then
    SelectSpecialRuleOrToken(currRule as TSynSpecialRule)
  else if (currRule is TSynKeywords) or (currRule is TSynSet) then
    SelectRange(currRule.Parent)
  else
    SelectSpecialRuleOrToken(nil, TSynHighlighterAttributes(currToken));
end;

// -----------------------------------------------------------------------------

procedure TCustomSynEdit.SaveLines;
var
  I: Integer;
begin
  for I := 0 to fLines.Count - 1 do
    if ExpandLines.GetLineFlag(I, STRING_REC_MODIFIED) then
    begin
      ExpandLines.SetLineFlag(I, STRING_REC_MODIFIED, False);
      ExpandLines.SetLineFlag(I, STRING_REC_SAVED, True);
    end;
  fUndoList.ClearLineStates;
end;

// -----------------------------------------------------------------------------

{ TSynEditPlugin }

// -----------------------------------------------------------------------------

constructor TSynEditPlugin.Create(AOwner: TCustomSynEdit);
begin
  inherited Create;
  if AOwner <> nil then
  begin
    fOwner := AOwner;
    if fOwner.fPlugins = nil then
      fOwner.fPlugins := TObjectList.Create;
    fOwner.fPlugins.Add(Self);
  end;
end;

// -----------------------------------------------------------------------------

destructor TSynEditPlugin.Destroy;
begin
  if fOwner <> nil then
    fOwner.fPlugins.Extract(Self); // We are being destroyed,
                                   // fOwner should not free us
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

{ TSynEditCodeFoldingPlugin }

// -----------------------------------------------------------------------------
// The functions below are to track changes in lines of collapsed ranges
procedure TSynEditCodeFoldingPlugin.AfterPaint(ACanvas: TCanvas;
  const AClip: TRect; FirstLine, LastLine: Integer);
begin
  { Do nothing }
  inherited;
end;

// -----------------------------------------------------------------------------
// Delete all fold ranges which were affected by deletion. Move up others
procedure TSynEditCodeFoldingPlugin.LinesDeleted(FirstLine, Count: Integer);
var
  I: Integer;
  FoldRange: TSynEditFoldRange;
begin
  for I := fOwner.AllFoldRanges.AllCount - 1 downto 0 do
  begin
    FoldRange := fOwner.AllFoldRanges[I];

    { Inside range? Overlaps? }
    if ((FoldRange.FromLine < FirstLine) and (FirstLine + Count - Ord(Count > 0) < FoldRange.ToLine))
      or InRange(FoldRange.FromLine, FirstLine, FirstLine + Count - Ord(Count > 0))
      or InRange(FoldRange.ToLine, FirstLine, FirstLine + Count - Ord(Count > 0))
    then
      { Collapse }
      fOwner.AllFoldRanges.Delete(I)

    { Before range }
    else if (Count > 0) and (FoldRange.FromLine > FirstLine) then
      { Move }
      FoldRange.MoveBy(-Count);
  end;
  fOwner.AllFoldRanges.UpdateFoldRanges;
  fOwner.UpdateWordWrapHiddenOffsets;
  inherited;
end;

// -----------------------------------------------------------------------------
// Delete all fold ranges which were in scope of insertion, move others down
procedure TSynEditCodeFoldingPlugin.LinesInserted(FirstLine,
  Count: integer);
var
  I: Integer;
  FoldRange: TSynEditFoldRange;
begin
  for I := 0 to fOwner.AllFoldRanges.AllCount - 1 do
  begin
    FoldRange := fOwner.AllFoldRanges[I];

    { Inside range? }
    if (FoldRange.FromLine < FirstLine) and (FirstLine <= FoldRange.ToLine)
    then
      { Collapse }
      fOwner.AllFoldRanges.Delete(FoldRange)

    { Before range }
    else if (FoldRange.FromLine >= FirstLine) then
      { Move }
      FoldRange.MoveBy(Count);
  end;
  fOwner.AllFoldRanges.UpdateFoldRanges;
  fOwner.UpdateWordWrapHiddenOffsets;
  inherited;
end;

end.
