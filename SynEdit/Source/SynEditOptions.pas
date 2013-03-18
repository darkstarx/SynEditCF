unit SynEditOptions;

interface

uses
  Classes;

type
  TSynEditPaintOption = (
    eoAdaptiveSpecialLine,     // Active line color is choosed by darkening (or lightening) current editor background
    eoSpecialLineDefaultFg,    // Disables the foreground text color override when using the OnSpecialLineColor event
    eoAdaptiveSelectionSpecialLine, // Draw active line on selection
    eoHighlightMargin,         // Additional background highlight after margin
    eoShowSpecialChars,        // Shows the special Characters
    eoShowSpecialCharsInSelection, // Shows invisibles in selection only
    eoShowEolSpecialChar       // Includes end of line character in specials
  );
  TSynEditPaintOptions = set of TSynEditPaintOption;

  TSynEditCaretOption = (
    eoAltSetsColumnMode,       // Holding down the Alt Key will put the selection mode into columnar format
    eoKeepCaretX,              // When moving through lines w/o Cursor Past EOL, keeps the X position of the cursor
    eoNoCaret                  // Makes it so the caret is never visible
  );
  TSynEditCaretOptions = set of TSynEditCaretOption;

  TSynEditScrollOption = (
    eoAutoSizeMaxScrollWidth,  // Automatically resizes the MaxScrollWidth property when inserting text
    eoDisableScrollArrows,     // Disables the scroll bar arrow buttons when you can't scroll in that direction any more
    eoHalfPageScroll,          // When scrolling with page-up and page-down commands, only scroll a half page at a time
    eoHideShowScrollbars,      // If enabled, then the scrollbars will only show when necessary. If you have ScrollPastEOL, then it the horizontal bar will always be there (it uses MaxLength instead)
    eoScrollByOneLess,         // Forces scrolling to be one less
    eoScrollHintFollows,       // The scroll hint follows the mouse when scrolling vertically
    eoScrollPastEof,           // Allows the cursor to go past the end of file marker
    eoScrollPastEol,           // Allows the cursor to go past the last character into the white space at the end of a line
    eoShowScrollHint           // Shows a hint of the visible line numbers when scrolling vertically
  );
  TSynEditScrollOptions = set of TSynEditScrollOption;

  TSynEditDragNDropOption = (
    eoDragDropEditing,         // Allows you to select a block of text and drag it within the document to another location
    eoDropFiles                // Allows the editor accept OLE file drops
  );
  TSynEditDragNDropOptions = set of TSynEditDragNDropOption;

  TSynEditBehaviourOption = (
    eoAlignedWrap,             // Indented word wrap
    eoAutoIndent,              // Will indent the caret on new lines with the same amount of leading white space as the preceding line
    eoTabIndent,               // When active <Tab> and <Shift><Tab> act as block indent, unindent when text is selected
    eoEnhanceHomeKey,          // enhances home key positioning, similar to visual studio
    eoEnhanceEndKey,           // enhances End key positioning, similar to JDeveloper
    eoGroupUndo,               // When undoing/redoing actions, handle all continous changes of the same kind in one call instead undoing/redoing each command separately
    eoRightMouseMovesCursor,   // When clicking with the right mouse for a popup menu, move the cursor to that location
    eoSmartTabs,               // When tabbing, the cursor will go to the next non-white space character of the previous line
    eoSmartTabDelete,          // Similar to Smart Tabs, but when you delete characters
    eoTabsToSpaces,            // Converts a tab character to a specified number of space characters
    eoTrimTrailingSpaces,      // Spaces at the end of lines will be trimmed and not saved
    eoWrapAgainstMargin        // Use FRightMargin instead of client width as word wrap boundary
  );
  TSynEditBehaviourOptions = set of TSynEditBehaviourOption;

  TSynEditSelectionOption = (
    eoNoSelection,             // Disables selecting text
    eoAdaptiveSelection        // Selection background (except active line) is choosen by darkening or lightening editor background
  );
  TSynEditSelectionOptions = set of TSynEditSelectionOption;


const
  SYNEDIT_DEFAULT_PAINT_OPTIONS =
    [];
  SYNEDIT_DEFAULT_CARET_OPTIONS =
    [];
  SYNEDIT_DEFAULT_SCROLL_OPTIONS =
    [eoScrollPastEol, eoShowScrollHint];
  SYNEDIT_DEFAULT_DRAGNDROP_OPTIONS =
    [eoDragDropEditing];
  SYNEDIT_DEFAULT_BEHAVIOUR_OPTIONS =
    [eoAutoIndent, eoEnhanceEndKey, eoSmartTabs, eoTabsToSpaces, eoSmartTabDelete, eoGroupUndo];
  SYNEDIT_DEFAULT_SELECTION_OPTIONS =
    [];

type
  TSynEditorOptions = class
  private
    FPaintOptions: TSynEditPaintOptions;
    FCaretOptions: TSynEditCaretOptions;
    FScrollOptions: TSynEditScrollOptions;
    FDragNDropOptions: TSynEditDragNDropOptions;
    FBehaviourOptions: TSynEditBehaviourOptions;
    FSelectionOptions: TSynEditSelectionOptions;
    FOnChange: TNotifyEvent;

    procedure SetPaintOptions(Value: TSynEditPaintOptions);
    procedure SetCaretOptions(Value: TSynEditCaretOptions);
    procedure SetScrollOptions(Value: TSynEditScrollOptions);
    procedure SetDragNDropOptions(Value: TSynEditDragNDropOptions);
    procedure SetBehaviourOptions(Value: TSynEditBehaviourOptions);
    procedure SetSelectionOptions(Value: TSynEditSelectionOptions);

    function GetPaintOptions(): TSynEditPaintOptions;
    function GetCaretOptions(): TSynEditCaretOptions;
    function GetScrollOptions(): TSynEditScrollOptions;
    function GetDragNDropOptions(): TSynEditDragNDropOptions;
    function GetBehaviourOptions(): TSynEditBehaviourOptions;
    function GetSelectionOptions(): TSynEditSelectionOptions;

    procedure Changed();
  published
    property PaintOptions: TSynEditPaintOptions read GetPaintOptions write SetPaintOptions
      default SYNEDIT_DEFAULT_PAINT_OPTIONS;
    property CaretOptions: TSynEditCaretOptions read GetCaretOptions write SetCaretOptions
      default SYNEDIT_DEFAULT_CARET_OPTIONS;
    property ScrollOptions: TSynEditScrollOptions read GetScrollOptions write SetScrollOptions
      default SYNEDIT_DEFAULT_SCROLL_OPTIONS;
    property DragNDropOptions: TSynEditDragNDropOptions read GetDragNDropOptions write SetDragNDropOptions
      default SYNEDIT_DEFAULT_DRAGNDROP_OPTIONS;
    property BehaviourOptions: TSynEditBehaviourOptions read GetBehaviourOptions write SetBehaviourOptions
      default SYNEDIT_DEFAULT_BEHAVIOUR_OPTIONS;
    property SelectionOptions: TSynEditSelectionOptions read GetSelectionOptions write SetSelectionOptions
      default SYNEDIT_DEFAULT_SELECTION_OPTIONS;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TSynEditorOptions }

procedure TSynEditorOptions.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TSynEditorOptions.GetBehaviourOptions: TSynEditBehaviourOptions;
begin
  Result := FBehaviourOptions;
end;

function TSynEditorOptions.GetCaretOptions: TSynEditCaretOptions;
begin
  Result := FCaretOptions;
end;

function TSynEditorOptions.GetDragNDropOptions: TSynEditDragNDropOptions;
begin
  Result := FDragNDropOptions;
end;

function TSynEditorOptions.GetPaintOptions: TSynEditPaintOptions;
begin
  Result := FPaintOptions;
end;

function TSynEditorOptions.GetScrollOptions: TSynEditScrollOptions;
begin
  Result := FScrollOptions;
end;

function TSynEditorOptions.GetSelectionOptions: TSynEditSelectionOptions;
begin
  Result := SelectionOptions;
end;

procedure TSynEditorOptions.SetBehaviourOptions(Value: TSynEditBehaviourOptions);
begin
  if Value <> FBehaviourOptions then
  begin
    FBehaviourOptions := Value;
    Changed;
  end;
end;

procedure TSynEditorOptions.SetCaretOptions(Value: TSynEditCaretOptions);
begin
  if Value <> FCaretOptions then
  begin
    FCaretOptions := Value;
    Changed;
  end;
end;

procedure TSynEditorOptions.SetDragNDropOptions(Value: TSynEditDragNDropOptions);
begin
  if Value <> FDragNDropOptions then
  begin
    FDragNDropOptions := Value;
    Changed;
  end;
end;

procedure TSynEditorOptions.SetPaintOptions(Value: TSynEditPaintOptions);
begin
  if Value <> FPaintOptions then
  begin
    FPaintOptions := Value;
    Changed;
  end;
end;

procedure TSynEditorOptions.SetScrollOptions(Value: TSynEditScrollOptions);
begin
  if Value <> FScrollOptions then
  begin
    FScrollOptions := Value;
    Changed;
  end;
end;

procedure TSynEditorOptions.SetSelectionOptions(
  Value: TSynEditSelectionOptions);
begin
  if Value <> FSelectionOptions then
  begin
    FSelectionOptions := Value;
    Changed;
  end;
end;

end.
