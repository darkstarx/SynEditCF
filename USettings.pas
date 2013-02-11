(*

  Letterpress

  Settings storage.

  Copyright 2009-2010, Garnet

*)

unit USettings;

interface

uses
  Classes;

type
  TSetGeneral = record
    { Recents }
    SetMRU,
    SetSession: TStringList;
    SetLastProject: UnicodeString;

    { Theme }
    SetScheme: UnicodeString;

    { Default grammar settings }
    SetDefaultEncoding: Integer;
    SetDefaultEndings: Integer;
    SetDefaultGrammar: UnicodeString;
    SetDefaultTabSize: Integer;
    SetDefaultTabSoft,
    SetDefaultTabSmart,
    SetDefaultTabSmartIndent: Boolean;
  end;

const
  sSetGeneral = 'General';

  sSetLeft = 'Left';
  sSetTop = 'Top';
  sSetWidth = 'Width';
  sSetHeight = 'Height';
  sSetState = 'State';
  sSetDefaultWidth = 640;
  sSetDefaultHeight = 480;
  sSetDefaultState = 0;

  sSetMenu = 'Menu';
  sSetTabs = 'Tabs';
  sSetStatus = 'Status';

  sSetSession = 'Session';
  sSetMRU = 'MRU';
  sSetLastProject = 'LastProject';

  sSetStylesheet = 'ColorScheme';
  sSetDefaultStylesheet = 'Alpine';

  sSetEncoding = 'Encoding';
  sSetEndings = 'Endings';
  sSetGrammar = 'Grammar';
  sSetDefaultTabSize = 'TabSize';
  sSetDefaultTabSoft = 'TabSoft';
  sSetDefaultTabSmart = 'TabSmart';
  sSetDefaultTabSmartIndent = 'TabSmartIndent';

type
  TSetEnvironment = record
    SetGlobal: TStringList;
  end;

const
  sSetEnvironment = 'Environment';

type
  TSetFormat = record
    { Word wrap }
    SetWordWrap,
    SetWrapAligned,
    SetWrapBreakWhitespace,
    SetWrapAtMargin,
    SetWrapCentered: Boolean;

    { Whitespace }
    SetWhitespace,
    SetWhitespaceInSelection,
    SetWhitespaceEndings: Boolean;

    { Move }
    SetPairCharacters,
    SetTrimWhitespace,
    SetFreehandEditing,
    SetOverwrite: Boolean;

    { Spelling }
    SetSpellCheck: Boolean;

    { Font }
    SetFontName: UnicodeString;
    SetFontSize: Byte;
    SetFontWeight: Byte;
  end;

const
  sSetFormat = 'Format';

  sSetWordWrap = 'WordWrap';
  sSetWrapAligned = 'WrapAligned';
  sSetWrapBreakWhitespace = 'WrapBreakWhitespace';
  sSetWrapAtMargin = 'WrapAtMargin';
  sSetWrapCentered = 'WrapCentered';

  sSetWhitespace = 'Whitespace';
  sSetWhitespaceInSelection = 'WhitespaceInSelection';
  sSetWhitespaceEndings = 'WhitespaceEndings';

  sSetPairCharacters = 'PairCharacters';
  sSetTrimWhitespace = 'TrimWhitespace';
  sSetFreehandEditing = 'FreehandEditing';
  sSetOverwrite = 'Overwrite';

  sSetSpellCheck = 'SpellCheck';

  sSetFontName = 'FontName';
  sSetFontSize = 'FontSize';
  sSetFontWeight = 'FontWeight';
  sSetDefaultFont = 'Consolas';
  sSetDefaultFontSize = 9;

type
  TSetView = record
    { Gutter }
    SetGutterVisible,
    SetGutterNumbering,
    SetGutterOutlining,
    SetGutterLifebar: Boolean;

    { Margin }
    SetMargin: Integer;
    SetMarginVisible: Boolean;
    SetMarginHighlight: Boolean;
  end;

const
  sSetView = 'View';

  sSetGutterVisible = 'GutterVisible';
  sSetGutterNumbering = 'GutterNumbering';
  sSetGutterOutlining = 'GutterOutlining';
  sSetGutterLifebar = 'GutterLifebar';

  sSetMargin = 'Margin';
  sSetMarginVisible = 'MarginVisible';
  sSetMarginHighlight = 'MarginHighlight';
  sSetDefaultMargin = 80;

type
  TSetSearch = record
    { Text }
    SetSearchText,
    SetReplaceText: UnicodeString;

    { Options }
    SetRegEx,
    SetCaseSensitive,
    SetWholeWords: Boolean;
  end;

const
  sSetSearch = 'Search';

  sSetSearchText = 'Find';
  sSetReplaceText = 'Replace';

  sSetRegEx = 'RegEx';
  sSetCaseSensitive = 'CaseSensitive';
  sSetWholeWords = 'WholeWords';

type
  TSetPage = record
    { General }
    SetUnits: Byte;
    SetMirror: Boolean;

    { Margins }
    SetLeft: Extended;
    SetRight,
    SetTop,
    SetBottom,
    SetHeader,
    SetFooter,
    SetHLI,
    SetFRI,
    SetIM,
    SetGutter: Double;

    { Options }
    SetColor,
    SetPreserveBackground,
    SetSyntax,
    SetLineNumbers,
    SetInMargins: Boolean;

    { Header }
    SetHLeft,
    SetHCenter,
    SetHRight: UnicodeString;
    SetHSeparator: Boolean;
    SetHSepColor: Cardinal;
    SetHShadow: Boolean;
    SetHShadowColor: Cardinal;
    SetHBorders,
    SetHMirror,
    SetHRoman: Boolean;

    { Footer }
    SetFLeft,
    SetFCenter,
    SetFRight: UnicodeString;
    SetFSeparator: Boolean;
    SetFSepColor: Cardinal;
    SetFShadow: Boolean;
    SetFShadowColor: Cardinal;
    SetFBorders,
    SetFMirror,
    SetFRoman: Boolean;
  end;

const
  sSetPrint = 'Print';

type
  TSetSystem = record
    SetMaxRecentFiles: Byte;
    SetAlwaysPresentInputPrompt: Boolean;
  end;

const
  sSetSystem = 'System';

  sSetMaxRecentFiles = 'MaxRecentFiles';
  sSetAlwaysPresentInputPrompt = 'AlwaysPresentInputPrompt';

var
  SetGeneral: TSetGeneral;
  SetEnvironment: TSetEnvironment;
  SetSearch: TSetSearch;
  SetFormat: TSetFormat;
  SetView: TSetView;
  SetPage: TSetPage;
  SetSystem: TSetSystem;

const
  sSetPinned = 'Pinned';
  sSetPreferred = 'Preferred';

implementation

end.
