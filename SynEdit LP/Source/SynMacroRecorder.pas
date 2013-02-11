{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynMacroRecorder.pas, released 2001-10-17.

Author of this file is Flávio Etrusco.
Portions created by Flávio Etrusco are Copyright 2001 Flávio Etrusco.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynMacroRecorder.pas,v 1.31.2.3 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynMacroRecorder;

{$I SynEdit.inc}

interface

uses
  StdCtrls, Controls, Windows, Messages, Graphics, Menus, WideStrUtils, Classes,

  { SynEdit }
  SynEdit, SynEditKeyCmds, SynEditPlugins, SynEditTypes, SynUnicode,
  SynMacroRecorder_TLB, AW_SCtrl;

const
  sMacroUntitled = 'New Macro.macro';
  sCannotRecord = 'Cannot record macro: already recording or playing.';
  sCannotPlay = 'Cannot playback macro: already playing or recording.';
  sCannotPause = 'Can only pause when recording.';
  sCannotResume = 'Can only resume when paused.';

type
  TSynMacroState = (msStopped, msRecording, msPlaying, msPaused);

  TSynMacroCommand = (mcRecord, mcPlayback);

  TCustomSynMacroRecorder = class;

  TSynUserCommandEvent = procedure (Sender: TCustomSynMacroRecorder;
    ACommand: Integer; const AData: UnicodeString) of object;

  { TCustomSynMacroRecorder
    OnStateChange:
      occurs right after start playing, recording, pausing or stopping
    SaveMarkerPos:
      if true, Bookmark position is recorded in the macro. Otherwise, the Bookmark
      is created in the position the Caret is at the time of playback. }
  TCustomSynMacroRecorder = class(TAbstractSynHookerPlugin)
  private
    fShortCuts: array [TSynMacroCommand] of TShortCut;
    fOnStateChange: TNotifyEvent;
    fOnUserCommand: TSynUserCommandEvent;
    fSaveMarkerPos: Boolean;
    fModified: Boolean;
    fFileName: UnicodeString;
    fMacroStack: array of TawScriptControl;
  protected
    fCurrentEditor: TCustomSynEdit;
    fState: TSynMacroState;
    fEvents: TList;
    fCommandIDs: array [TSynMacroCommand] of TSynEditorCommand;

    function AddMacro: TawScriptControl;
    procedure RemoveMacro;
    function GetScriptControl: TawScriptControl;

    function GetIsEmpty: Boolean;
    procedure SetShortCut(const Index: Integer; const Value: TShortCut);
    procedure StateChanged;
    procedure DoAddEditor(AEditor: TCustomSynEdit); override;
    procedure DoRemoveEditor(AEditor: TCustomSynEdit); override;
    procedure OnCommand(Sender: TObject; AfterProcessing: Boolean;
      var Handled: boolean; var Command: TSynEditorCommand; var AChar: WideChar;
      Data: Pointer; HandlerData: Pointer); override;
    procedure OnError(Sender: TObject; Error: TawScriptError);
  protected
    property RecordCommandID: TSynEditorCommand read fCommandIDs[mcRecord];
    property PlaybackCommandID: TSynEditorCommand read fCommandIDs[mcPlayback];
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure Stop;
    procedure Pause;
    procedure Resume;

    procedure RecordMacro(AEditor: TCustomSynEdit);
    procedure PlaybackMacro(AEditor: TCustomSynEdit);

    procedure AddUserCommand(ACommand: Integer; const AData: UnicodeString = '';
      ACount: Integer = 1);
    procedure ProcessUserCommand(ACommand: Integer; const AData: UnicodeString);

    procedure LoadFromStream(ASrc: TStream);
    procedure SaveToStream(ADest: TStream);
    procedure LoadFromFile(AFileName: UnicodeString);
    procedure SaveToFile(AFileName: UnicodeString);

    property IsEmpty: Boolean read GetIsEmpty;
    property State: TSynMacroState read fState;
    property Modified: Boolean read fModified write fModified;
    property FileName: UnicodeString read fFileName;
    property RecordShortCut: TShortCut index Ord(mcRecord)
      read fShortCuts[mcRecord] write SetShortCut;
    property PlaybackShortCut: TShortCut index Ord(mcPlayback)
      read fShortCuts[mcPlayback] write SetShortCut;
    property SaveMarkerPos: boolean read fSaveMarkerPos
      write fSaveMarkerPos default False;

    property OnStateChange: TNotifyEvent read fOnStateChange write fOnStateChange;
    property OnUserCommand: TSynUserCommandEvent read fOnUserCommand
      write fOnUserCommand;
  end;

  TSynMacroRecorder = class(TCustomSynMacroRecorder)
  published
    property SaveMarkerPos;
    property RecordShortCut;
    property PlaybackShortCut;
    property OnStateChange;
    property OnUserCommand;
  end;

implementation

uses
  Forms, SysUtils, SynEditTextBuffer, SynEditMiscProcs;

{ TCustomSynMacroRecorder }

procedure TCustomSynMacroRecorder.Clear;
var
  I: Integer;
begin
  fModified := False;
  fFileName := sMacroUntitled;
  for I := 0 to High(fMacroStack) do
    FreeAndNil(fMacroStack[I]);
  SetLength(fMacroStack, 0);
end;

constructor TCustomSynMacroRecorder.Create(AOwner: TComponent);
begin
  inherited;

  { Initialize }
  fCommandIDs[mcRecord] := NewPluginCommand;
  fCommandIDs[mcPlayback] := NewPluginCommand;
  fShortCuts[mcRecord] := 0;
  fShortCuts[mcPlayback] := 0;

  { Reset everything }
  Clear;
end;

destructor TCustomSynMacroRecorder.Destroy;
begin
  Clear;
  inherited;
  ReleasePluginCommand(PlaybackCommandID);
  ReleasePluginCommand(RecordCommandID);
end;

procedure TCustomSynMacroRecorder.DoAddEditor(AEditor: TCustomSynEdit);
begin
  HookEditor(AEditor, RecordCommandID, 0, RecordShortCut);
  HookEditor(AEditor, PlaybackCommandID, 0, PlaybackShortCut);
end;

procedure TCustomSynMacroRecorder.DoRemoveEditor(AEditor: TCustomSynEdit);
begin
  UnHookEditor(AEditor, RecordCommandID, RecordShortCut);
  UnHookEditor(AEditor, PlaybackCommandID, PlaybackShortCut);
end;

function TCustomSynMacroRecorder.GetIsEmpty: Boolean;
var
  ScriptCode: UnicodeString;
begin
  { Initialize }
  Result := True;
  if GetScriptControl = nil then Exit;

  { Get script source without whitespace }
  ScriptCode := StringReplace(GetScriptControl.Code.Text, #32, EmptyStr, [rfReplaceAll]);
  ScriptCode := StringReplace(ScriptCode, #13, EmptyStr, [rfReplaceAll]);
  ScriptCode := StringReplace(ScriptCode, #10, EmptyStr, [rfReplaceAll]);
  ScriptCode := StringReplace(ScriptCode, #9, EmptyStr, [rfReplaceAll]);

  { See if main() metod exists at all }
  Result := Pos('functionmain(){', ScriptCode) < 1;

  { See if it's empty }
  if not Result then
    Result := Pos('functionmain(){}', ScriptCode) > 0;
end;

procedure TCustomSynMacroRecorder.LoadFromStream(ASrc: TStream);
var
  ScriptControl: TawScriptControl;
begin
  try
    ScriptControl := AddMacro;
    if ScriptControl <> nil then
    begin
      ScriptControl.Code.LoadFromStream(ASrc, TEncoding.UTF8);
      if Length(fMacroStack) = 1 then
      begin
        fFileName := EmptyStr;
        fModified := False;
      end;
    end;
  finally
  end;
end;

procedure TCustomSynMacroRecorder.OnCommand(Sender: TObject;
  AfterProcessing: Boolean; var Handled: Boolean;
  var Command: TSynEditorCommand; var AChar: WideChar; Data,
  HandlerData: Pointer);
var
  ScriptControl: TawScriptControl;
  sData: UnicodeString;
begin
  if AfterProcessing then
  begin
    ScriptControl := GetScriptControl;
    if (Sender = fCurrentEditor) and (State = msRecording) and not Handled and
      Assigned(ScriptControl) then
    begin
      fModified := True;
      case Command of
        ecGotoXY, ecSelGotoXY, ecOffsetCaret, ecSelOffsetCaret:
          sData := IntToStr(PBufferCoord(Data)^.Char) + ', ' + IntToStr(PBufferCoord(Data)^.Line);
        ecString: sData := PChar(Data);
        else if AChar <> #0 then sData := AChar
        else sData := EmptyStr;
      end;
      if sData = EmptyStr then
        ScriptControl.Code.Add(#9'letterpress.executeCommand("' + EditorCommandToCodeString(Command) + '");')
      else
        ScriptControl.Code.Add(#9'letterpress.executeCommand("' + EditorCommandToCodeString(Command) + '", "' + sData + '");');
    end;
  end
  else begin
    case State of
      msStopped:
        if Command = RecordCommandID then
        begin
          RecordMacro(TCustomSynEdit(Sender));
          Handled := True;
        end
        else if Command = PlaybackCommandID then
        begin
          PlaybackMacro(TCustomSynEdit(Sender));
          Handled := True;
        end;
      msPlaying:
        { Do nothing };
      msPaused:
        if Command = PlaybackCommandID then
        begin
          Resume;
          Handled := True;
        end;
      msRecording:
        if Command = PlaybackCommandID then
        begin
          Pause;
          Handled := True;
        end
        else if Command = RecordCommandID then
        begin
          Stop;
          Handled := True;
        end;
    end;
  end;
end;

procedure TCustomSynMacroRecorder.OnError(Sender: TObject;
  Error: TawScriptError);
begin
  MessageBox(0, PChar(Error.Source), 'Macro', MB_OK);
end;

procedure TCustomSynMacroRecorder.Pause;
begin
  if State <> msRecording then
    Exit;
  fState := msPaused;
  StateChanged;
end;

procedure TCustomSynMacroRecorder.PlaybackMacro(AEditor: TCustomSynEdit);
var
  CurrScriptControl: TawScriptControl;
begin
  if GetIsEmpty then Exit;
  if (Length(fMacroStack) = 1) and (State <> msStopped) then Exit;
  if Length(fMacroStack) = 1 then
  begin
    fState := msPlaying;
    if AEditor.UndoList.ItemCount > 0 then
      AEditor.UndoList.AddGroupBreak;
    AEditor.BeginUndoBlock;
  end;
  try
    if Length(fMacroStack) = 1 then
      StateChanged;
    CurrScriptControl := fMacroStack[High(fMacroStack)];
    with TSynMacroHandler(CurrScriptControl.AutoObjects[0].AutoObject) do
    begin
      MacroRecorder := Self;
      ScriptControl := CurrScriptControl;
      Editor := AEditor;
    end;
    CurrScriptControl.CallFunction('main', []);
  finally
    if Length(fMacroStack) = 1 then
    begin
      AEditor.EndUndoBlock;
      if State = msPlaying then
      begin
        fState := msStopped;
        StateChanged;
      end;
    end
    else
      RemoveMacro;
  end;
end;

procedure TCustomSynMacroRecorder.AddUserCommand(ACommand: Integer;
  const AData: UnicodeString = ''; ACount: Integer = 1);
var
  ScriptControl: TawScriptControl;
  S: UnicodeString;
begin
  if fState <> msRecording then Exit;
  S := #9'letterpress.executeCommand("' + EditorCommandToCodeString(ACommand) + '"';
  if AData <> EmptyStr then
    S := S + ', "' + AData + '"';
  if ACount > 1 then
    S := S + ', "' + IntToStr(ACount) + '"';
  S := S + ');';
  ScriptControl := GetScriptControl;
  if Assigned(ScriptControl) then
    ScriptControl.Code.Add(S);
end;

procedure TCustomSynMacroRecorder.ProcessUserCommand(ACommand: Integer;
  const AData: UnicodeString);
begin
  if Assigned(fOnUserCommand) then
    fOnUserCommand(Self, ACommand, AData);
end;

procedure TCustomSynMacroRecorder.RecordMacro(AEditor: TCustomSynEdit);
var
  CurrScriptControl: TawScriptControl;
begin
  if fState <> msStopped then
    Exit;
  Clear;
  CurrScriptControl := AddMacro;
  if Assigned(CurrScriptControl) then
  begin
    with CurrScriptControl.Code do
    begin
      Clear;
      BeginUpdate;
      Add('function main()');
      Add('{');
    end;
    fState := msRecording;
    fCurrentEditor := AEditor;
    StateChanged;
  end;
end;

procedure TCustomSynMacroRecorder.Resume;
begin
  if fState <> msPaused then Exit;
  fState := msRecording;
  StateChanged;
end;

function TCustomSynMacroRecorder.AddMacro: TawScriptControl;
begin
  SetLength(fMacroStack, Succ(Length(fMacroStack)));
  fMacroStack[High(fMacroStack)] := TawScriptControl.Create(Self);
  Result := fMacroStack[High(fMacroStack)];
  with Result do
  begin
    OnError := OnError;
    AutoObjects.BeginUpdate;
  end;
  with Result.AutoObjects.Add do
  begin
    AutoObject := TSynMacroHandler.Create;
    with (AutoObject as TSynMacroHandler) do
    begin
      MacroRecorder := nil;
      ScriptControl := nil;
      Editor := nil;
    end;
    AutoObjectName := 'letterpress';
  end;
  Result.AutoObjects.EndUpdate;
end;

procedure TCustomSynMacroRecorder.RemoveMacro;
begin
  if Length(fMacroStack) = 0 then Exit;
  FreeAndNil(fMacroStack[High(fMacroStack)]);
  SetLength(fMacroStack, High(fMacroStack));
end;

function TCustomSynMacroRecorder.GetScriptControl: TawScriptControl;
begin
  if Length(fMacroStack) > 0 then
    Result := fMacroStack[0]
  else
    Result := nil;
end;

procedure TCustomSynMacroRecorder.SaveToStream(ADest: TStream);
begin
  try
    GetScriptControl.Code.SaveToStream(ADest, TEncoding.UTF8);
    fModified := False;
  finally
  end;
end;

procedure TCustomSynMacroRecorder.SetShortCut(const Index: Integer;
  const Value: TShortCut);
var
  cEditor: integer;
begin
  if fShortCuts[TSynMacroCommand(Index)] <> Value then
  begin
    if Assigned(fEditors) then
      if Value <> 0 then
      begin
        for cEditor := 0 to fEditors.Count -1 do
          HookEditor(Editors[cEditor], fCommandIDs[TSynMacroCommand(Index)],
            fShortCuts[TSynMacroCommand(Index)], Value);
      end else
      begin
        for cEditor := 0 to fEditors.Count -1 do
          UnHookEditor(Editors[cEditor], fCommandIDs[TSynMacroCommand(Index)],
            fShortCuts[TSynMacroCommand(Index)]);
      end;
    fShortCuts[TSynMacroCommand(Index)] := Value;
  end;
end;

procedure TCustomSynMacroRecorder.StateChanged;
begin
  if Assigned(OnStateChange) then
    OnStateChange(Self);
end;

procedure TCustomSynMacroRecorder.Stop;
begin
  if fState = msStopped then Exit;
  fState := msStopped;
  fCurrentEditor := nil;
  with GetScriptControl.Code do
  begin
    Add('}');
    EndUpdate;
  end;
  StateChanged;
end;

procedure TCustomSynMacroRecorder.LoadFromFile(AFileName: UnicodeString);
var
  ScriptControl: TawScriptControl;
begin
  try
    ScriptControl := AddMacro;
    if ScriptControl <> nil then
    begin
      ScriptControl.Code.LoadFromFile(AFileName, TEncoding.UTF8);
      if Length(fMacroStack) = 1 then
      begin
        fFileName := AFileName;
        fModified := False;
      end;
    end;
  finally
  end;
end;

procedure TCustomSynMacroRecorder.SaveToFile(AFileName: UnicodeString);
begin
  try
    GetScriptControl.Code.SaveToFile(AFileName, TEncoding.UTF8);
    fModified := False;
  finally
  end;
end;

end.
