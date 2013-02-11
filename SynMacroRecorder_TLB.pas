unit SynMacroRecorder_TLB;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Classes, SysUtils, ActiveX, ComObj, StdVcl, binary_TLB, UMacroInput;

type

  { Process editor related commands and routines }
  TSynMacroHandler = class(TAutoObject, ISynMacroHandler)
  private
    fMacroRecorder, fScriptControl, fEditor: TObject;
    fUpdateCallback: UnicodeString;
  protected
    procedure ExecuteCommand(const ACommand, AData: WideString; ACount: Integer); safecall;
    procedure Prompt(const ATitle, APrompt, AUpdateCallback, AConfirmCallback: WideString); safecall;
    function Get_CaretX: Integer; safecall;
    function Get_CaretY: Integer; safecall;
    function Get_CurrText: WideString; safecall;
    procedure Set_CaretX(Value: Integer); safecall;
    procedure Set_CaretY(Value: Integer); safecall;
    procedure Set_CurrText(const Value: WideString); safecall;
    procedure SetCaretAndSelection(ACaretX, ACaretY, AStartX, AStartY, AEndX, AEndY: Integer); safecall;
    function Get_MacroDir: WideString; safecall;
    function Get_CurrChar: WideString; safecall;
    function Get_CurrWord: WideString; safecall;
    function Get_HasSelection: WordBool; safecall;
    procedure Set_CurrChar(const Value: WideString); safecall;
    procedure Set_CurrWord(const Value: WideString); safecall;
    function Get_BlockBeginX: Integer; safecall;
    function Get_BlockBeginY: Integer; safecall;
    function Get_BlockEndX: Integer; safecall;
    function Get_BlockEndY: Integer; safecall;
    procedure Set_BlockBeginX(Value: Integer); safecall;
    procedure Set_BlockBeginY(Value: Integer); safecall;
    procedure Set_BlockEndX(Value: Integer); safecall;
    procedure Set_BlockEndY(Value: Integer); safecall;
    function GetLine(ALine: Integer): WideString; safecall;
    function Get_TabSize: Integer; safecall;
    function Get_WordBeginX: Integer; safecall;
    function Get_WordEndX: Integer; safecall;
    procedure Set_TabSize(Value: Integer); safecall;
    function Get_LineCount: Integer; safecall;
  public
    function PromptUpdated(const Response: WideString): WideString;
    property MacroRecorder: TObject read fMacroRecorder write fMacroRecorder;
    property ScriptControl: TObject read fScriptControl write fScriptControl;
    property Editor: TObject read fEditor write fEditor;
  end;

implementation

uses
  ComServ, Controls, Variants, aw_SCtrl, SynMacroRecorder, SynEditTypes,
  SynEditKeyCmds, SynEdit;

{ TSynMacroHandler }

// -----------------------------------------------------------------------------

procedure TSynMacroHandler.ExecuteCommand(const ACommand, AData: WideString;
  ACount: Integer);
var
  I, J, Command: Integer;
  NewBufferCoord: TBufferCoord;
begin
  Command := SynEditKeyCmds.ConvertCodeStringToCommand(ACommand);
  for J := 0 to Pred(ACount) do
  begin
    if Command >= ecUserCommandFirst then
      (fMacroRecorder as TSynMacroRecorder).ProcessUserCommand(Command, AData)
    else case Command of
      ecGotoXY, ecSelGotoXY, ecOffsetCaret, ecSelOffsetCaret:
        begin
          I := Pos(',', AData);
          if I > 0 then
          begin
            with NewBufferCoord do
            begin
              Char := StrToIntDef(Trim(Copy(AData, 1, Pred(I))), 0);
              Line := StrToIntDef(Trim(Copy(AData, Succ(I), MaxInt)), 0);
            end;
            (fEditor as TSynEdit).CommandProcessor(Command, #0, @NewBufferCoord);
          end
          else
            (fEditor as TSynEdit).CommandProcessor(ecNone, #0, nil);
        end;
      ecChar:
        if Length(AData) > 0 then
          (fEditor as TSynEdit).CommandProcessor(Command, AData[1], nil)
        else
          (fEditor as TSynEdit).CommandProcessor(ecNone, #0, nil);
      ecString:
        for I := 1 to Length(AData) do
          (fEditor as TSynEdit).CommandProcessor(ecChar, AData[I], nil);
      else
        (fEditor as TSynEdit).CommandProcessor(Command, #0, nil);
    end;
  end;
end;

procedure TSynMacroHandler.Prompt(const ATitle, APrompt, AUpdateCallback, AConfirmCallback: WideString);
var
  fPrompt: TFrmMacroInput;
begin
  fPrompt := TFrmMacroInput.Create({MainForm}nil);
  try
    with fPrompt do
    begin
      Caption := ATitle;
      lblPrompt.Caption := APrompt;
      MacroHandler := Self;
      fUpdateCallback := AUpdateCallback;
      ShowModal;
      if ModalResult = mrOk then
        (fScriptControl as TawScriptControl).CallFunction(AConfirmCallback,
          [memoResponse.Text]);
    end;
  finally
    FreeAndnil(fPrompt);
  end;
end;

function TSynMacroHandler.PromptUpdated(const Response: WideString): WideString;
begin
  try
    Result := VarToWideStrDef((fScriptControl as TawScriptControl).CallFunction(fUpdateCallback,
      [Response]), EmptyStr);
  finally
  end;
end;

function TSynMacroHandler.Get_CaretX: Integer;
begin
  Result := (fEditor as TSynEdit).CaretX;
end;

function TSynMacroHandler.Get_CaretY: Integer;
begin
  Result := (fEditor as TSynEdit).CaretY;
end;

function TSynMacroHandler.Get_CurrText: WideString;
begin
  Result := (fEditor as TSynEdit).SelText;
end;

procedure TSynMacroHandler.Set_CaretX(Value: Integer);
begin
  (fEditor as TSynEdit).CaretX := Value;
end;

procedure TSynMacroHandler.Set_CaretY(Value: Integer);
begin
  (fEditor as TSynEdit).CaretY := Value;
end;

procedure TSynMacroHandler.Set_CurrText(const Value: WideString);
begin
  (fEditor as TSynEdit).SelText := Value;
end;

procedure TSynMacroHandler.SetCaretAndSelection(ACaretX, ACaretY, AStartX,
  AStartY, AEndX, AEndY: Integer);
begin
  (fEditor as TSynEdit).SetCaretAndSelection(BufferCoord(ACaretX, ACaretY),
    BufferCoord(AStartX, AStartY), BufferCoord(AEndX, AEndY));
end;

function TSynMacroHandler.Get_MacroDir: WideString;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath((fMacroRecorder as
    TCustomSynMacroRecorder).FileName));
end;

function TSynMacroHandler.Get_CurrChar: WideString;
var
  Editor: TSynEdit;
begin
  Editor := (fEditor as TSynEdit);
  Result := Editor.GetCharAtRowCol(Editor.CaretXY);
end;

function TSynMacroHandler.Get_CurrWord: WideString;
var
  Editor: TSynEdit;
begin
  Editor := (fEditor as TSynEdit);
  Result := Editor.GetWordAtRowCol(Editor.CaretXY);
end;

function TSynMacroHandler.Get_HasSelection: WordBool;
var
  Editor: TSynEdit;
begin
  Editor := (fEditor as TSynEdit);
  Result := Editor.SelAvail;
end;

procedure TSynMacroHandler.Set_CurrChar(const Value: WideString);
var
  Editor: TSynEdit;
begin
  Editor := (fEditor as TSynEdit);
  Editor.SetCharAtRowCol(Editor.CaretXY, Value[1]);
end;

procedure TSynMacroHandler.Set_CurrWord(const Value: WideString);
var
  Editor: TSynEdit;
begin
  Editor := (fEditor as TSynEdit);
  Editor.SetWordAtRowCol(Editor.CaretXY, Value);
end;

function TSynMacroHandler.Get_BlockBeginX: Integer;
var
  Editor: TSynEdit;
begin
  Editor := (fEditor as TSynEdit);
  Result := Editor.BlockBegin.Char;
end;

function TSynMacroHandler.Get_BlockBeginY: Integer;
var
  Editor: TSynEdit;
begin
  Editor := (fEditor as TSynEdit);
  Result := Editor.BlockBegin.Line;
end;

function TSynMacroHandler.Get_BlockEndX: Integer;
var
  Editor: TSynEdit;
begin
  Editor := (fEditor as TSynEdit);
  Result := Editor.BlockEnd.Char;
end;

function TSynMacroHandler.Get_BlockEndY: Integer;
var
  Editor: TSynEdit;
begin
  Editor := (fEditor as TSynEdit);
  Result := Editor.BlockEnd.Line;
end;

procedure TSynMacroHandler.Set_BlockBeginX(Value: Integer);
var
  Editor: TSynEdit;
begin
  Editor := (fEditor as TSynEdit);
  Editor.BlockBegin := BufferCoord(Value, Editor.BlockBegin.Line);
end;

procedure TSynMacroHandler.Set_BlockBeginY(Value: Integer);
var
  Editor: TSynEdit;
begin
  Editor := (fEditor as TSynEdit);
  Editor.BlockBegin := BufferCoord(Editor.BlockBegin.Char, Value);
end;

procedure TSynMacroHandler.Set_BlockEndX(Value: Integer);
var
  Editor: TSynEdit;
begin
  Editor := (fEditor as TSynEdit);
  Editor.BlockEnd := BufferCoord(Value, Editor.BlockEnd.Line);
end;

procedure TSynMacroHandler.Set_BlockEndY(Value: Integer);
var
  Editor: TSynEdit;
begin
  Editor := (fEditor as TSynEdit);
  Editor.BlockEnd := BufferCoord(Editor.BlockEnd.Char, Value);
end;

function TSynMacroHandler.GetLine(ALine: Integer): WideString;
var
  Editor: TSynEdit;
begin
{
  Editor := (fEditor as TSynEdit);
  Result := EmptyStr;
  if (ALine > 0) and (ALine <= Editor.Lines.Count) then
    Result := Editor.Lines[Pred(ALine)];
}
end;

function TSynMacroHandler.Get_TabSize: Integer;
var
  Editor: TSynEdit;
begin
  Editor := (fEditor as TSynEdit);
  Result := Editor.TabWidth;
end;

function TSynMacroHandler.Get_WordBeginX: Integer;
var
  Editor: TSynEdit;
begin
  Editor := (fEditor as TSynEdit);
  Result := Editor.WordStart.Char;
end;

function TSynMacroHandler.Get_WordEndX: Integer;
var
  Editor: TSynEdit;
begin
  Editor := (fEditor as TSynEdit);
  Result := Editor.WordEnd.Char;
end;

procedure TSynMacroHandler.Set_TabSize(Value: Integer);
var
  Editor: TSynEdit;
begin
  Editor := (fEditor as TSynEdit);
  Editor.TabWidth := Value;
end;

function TSynMacroHandler.Get_LineCount: Integer;
var
  Editor: TSynEdit;
begin
  Editor := (fEditor as TSynEdit);
  Result := Editor.Lines.Count;
end;

//initialization
//  TAutoObjectFactory.Create(ComServer, TSynMacroHandler, Class_SynMacroHandler,
//    ciMultiInstance, tmApartment);

end.
