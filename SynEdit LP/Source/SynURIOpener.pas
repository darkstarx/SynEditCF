(*
  Letterpress project unit.

  Provides support for navigation on highlighted by SynURIHighlighter URIs by
  Ctrl+Clicking.

  Initial author: Maël Hörz.
  Miscellanious correction: Kristian Williams.

  Copyright 2003-2010, Letterpress project team.
*)

{ Below starts old original license text and initial author commentary }

{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynURIOpener.pas, released 2003-09-25.
The Initial Author of this file is Maël Hörz.
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

You may retrieve the latest version of SynEdit from the SynEdit home page,
located at http://SynEdit.SourceForge.net
-------------------------------------------------------------------------------}
{
@abstract(Plugin for SynEdit to make links (URIs) clickable)
@author(Maël Hörz)
@created(2003)
@lastmod(2004-03-19)

The SynURIOpener unit extends SynEdit to make links highlighted by SynURISyn
clickable.

http://www.mh-net.de.vu
}

unit SynURIOpener;

{$I SynEdit.inc}
              
interface

uses
  Windows, Controls, Classes,

  { SynEdit }
  SynEdit, SynEditTypes, SynEditHighlighter, SynUnicode, SynEditKeyCmds;

type
  TSynURIOpener = class(TComponent)
  private
    FControlDown: Boolean;
    FCtrlActivatesLinks: Boolean;
    FEditor: TCustomSynEdit;
    FMouseDownX: Integer;
    FMouseDownY: Integer;
    FURIHighlighter: TSynCustomHighlighter;
    FVisitedURIs: TStringList;
    procedure OpenLink(URI: String; LinkType: Integer);
    function MouseInSynEdit: Boolean;
  protected
    procedure NewKeyDown(Sender: TObject; var Key: Word; Shift: TSynShiftState);
    procedure NewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure NewMouseCursor(Sender: TObject; const aLineCharPos: TBufferCoord;
      var aCursor: TCursor);
    procedure NewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    procedure SetEditor(const Value: TCustomSynEdit);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property CtrlActivatesLinks: Boolean read FCtrlActivatesLinks
      write FCtrlActivatesLinks default True;
    property Editor: TCustomSynEdit read FEditor write SetEditor;
    property URIHighlighter: TSynCustomHighlighter read FURIHighlighter
      write FURIHighlighter;
  end;

implementation

uses
  Forms, SysUtils, ShellAPI,

  { SynEdit }
  SynEditKeyConst;

type
  TAccessCustomSynEdit = class(TCustomSynEdit);

{ TSynURIOpener }

constructor TSynURIOpener.Create(AOwner: TComponent);
begin
  inherited;
  FCtrlActivatesLinks := True;
  FVisitedURIs := TStringList.Create;
  FVisitedURIs.Sorted := True;
end;

destructor TSynURIOpener.Destroy;
begin
  FVisitedURIs.Free;
  inherited;
end;

function TSynURIOpener.MouseInSynEdit: Boolean;
begin
  Result := PtInRect(FEditor.ClientRect,
    FEditor.ScreenToClient(Mouse.CursorPos));
end;

procedure TSynURIOpener.NewKeyDown(Sender: TObject; var Key: Word;
  Shift: TSynShiftState);
begin
  if (Key = SYNEDIT_CONTROL) and not FControlDown and MouseInSynEdit then
  begin
    FControlDown := True;
    TAccessCustomSynEdit(FEditor).UpdateMouseCursor;
  end;
end;

procedure TSynURIOpener.NewKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = SYNEDIT_CONTROL) and FControlDown then
  begin
    FControlDown := False;
    TAccessCustomSynEdit(FEditor).UpdateMouseCursor;
  end;
end;

function IsControlPressed: Boolean;
begin
  Result := GetAsyncKeyState(VK_CONTROL) <> 0;
end;

procedure TSynURIOpener.NewMouseCursor(Sender: TObject;
  const aLineCharPos: TBufferCoord; var aCursor: TCursor);
var
  TokenType, Start: Integer;
  Token: UnicodeString;
  Attri: TSynHighlighterAttributes;
begin
  if (csDesigning in FEditor.ComponentState) then
    Exit;

  FControlDown := IsControlPressed;
  if FCtrlActivatesLinks and FEditor.Focused and FControlDown
  then
    with FEditor do
    begin
      GetHighlighterAttriAtRowColEx(aLineCharPos, Token, TokenType, Start,
        Attri);
      if Assigned(URIHighlighter) and Assigned(Attri) and (Pos('.href', Attri.Name) > 0) and
        not ((eoDragDropEditing in Options) and IsPointInSelection(aLineCharPos))
      then
        aCursor := crHandPoint;
    end;
end;

procedure TSynURIOpener.NewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and not(FCtrlActivatesLinks) or FControlDown then
  begin
    FMouseDownX := X;
    FMouseDownY := Y;
  end
end;

procedure TSynURIOpener.NewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ptLineCol: TBufferCoord;
  TokenType, Start: Integer;
  Token: UnicodeString;
  Attri: TSynHighlighterAttributes;
begin
  if (Button <> mbLeft) or (FCtrlActivatesLinks and not FControlDown) or
    (Abs(FMouseDownX - X) > 4) or (Abs(FMouseDownY - Y) > 4) then exit;

  with TAccessCustomSynEdit(FEditor) do
  begin
    if (eoDragDropEditing in Options) and IsPointInSelection(ptLineCol) then
      exit;

    if X >= Gutter.Width then
    begin
      ptLineCol := DisplayToBufferPos(PixelsToRowColumn(X,Y));
      GetHighlighterAttriAtRowColEx(ptLineCol, Token, TokenType, Start, Attri);
      if Assigned(URIHighlighter) and Assigned(Attri) and (Pos('.href', Attri.Name) > 0) and
        not((eoDragDropEditing in Options) and
          IsPointInSelection(ptLineCol)) then
      begin
        OpenLink(Token, TokenType);
        InvalidateLine(ptLineCol.Line);
      end;
    end
  end;
end;

procedure TSynURIOpener.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and Assigned(Editor) and (AComponent = Editor) then
    Editor := nil;
  if (Operation = opRemove) and Assigned(URIHighlighter) and
    (AComponent = URIHighlighter)
  then
    URIHighlighter := nil;
end;

procedure TSynURIOpener.OpenLink(URI: string; LinkType: Integer);
begin
  FVisitedURIs.Add(URI);

  ShellExecute(0, nil, PChar(URI), nil, nil, 1);
end;

procedure TSynURIOpener.SetEditor(const Value: TCustomSynEdit);
begin
  if Editor <> Value then
  begin
    if not(csDesigning in ComponentState) and Assigned(FEditor) then
    begin
      with FEditor do
      begin
        RemoveKeyDownHandler(NewKeyDown);
        RemoveKeyUpHandler(NewKeyUp);
        RemoveMouseCursorHandler(NewMouseCursor);
        RemoveMouseDownHandler(NewMouseDown);
        RemoveMouseUpHandler(NewMouseUp);
      end;
    end;

    FEditor := Value;

    if not(csDesigning in ComponentState) and Assigned(FEditor) then
    begin
      with FEditor do
      begin
        AddKeyDownHandler(NewKeyDown);
        AddKeyUpHandler(NewKeyUp);
        AddMouseCursorHandler(NewMouseCursor);
        AddMouseDownHandler(NewMouseDown);
        AddMouseUpHandler(NewMouseUp);
      end;
    end;
  end;
end;

const
  IDC_LINK = MakeIntResource(32649);

var
  CursorHandle: THandle;

initialization
  CursorHandle := LoadCursor(0, IDC_LINK);
  if CursorHandle <> 0 then
    Screen.Cursors[crHandPoint] := CursorHandle;

end.