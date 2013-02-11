unit aw_SEditor;

{
	The contents of this file are subject to the Mozilla Public License Version 1.0 (the
	"License"); you may not use this file except in compliance with the License. You may
	obtain a copy of the License at http://www.mozilla.org/MPL/

	Software distributed under the License is distributed on an "AS IS" basis, WITHOUT
	WARRANTY OF ANY KIND, either express or implied. See the License for the
	specific language governing rights and limitations under the License.

	The Original Code is the ActiveX Scripting Components.

	The Initial Developer of the Original Code is Alexander Wingrove <awingrove@bigfoot.com>.
	Portions created by Alexander Wingrove are Copyright (C) 1999 Alexander Wingrove.
	All Rights Reserved.

	Contributor(s): <none>
}

{
	unit: aw_SEditor
	version: 1.01, 17/06/99
	components:
		TawScriptEditorForm
}

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, ExtCtrls, ComCtrls, Menus;

type

{ TawScriptEditorForm }

	TawScriptEditorForm = class(TForm)
		Memo: TMemo;
		StatusPanel: TPanel;
		ScriptStatusBar: TStatusBar;
		Panel2: TPanel;
		LanguageEdit: TEdit;
		DummyMenu: TMainMenu;
		EditMenu: TMenuItem;
		MI_SelectAll: TMenuItem;
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
		procedure FormCreate(Sender: TObject);
		procedure SetChange(Sender: TObject);
		procedure MemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure MemoClick(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MI_SelectAllClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
	private
		{ Private declarations }
		FResult: integer;
		Row, Col: cardinal;
		procedure UpdateCharPos;
	protected
		{ Protected declarations }
		procedure Save;
	public
		{ Public declarations }
		procedure Open(const Script, Language: string);
	end;

var
	awScriptEditorForm: TawScriptEditorForm;

implementation

{$R *.DFM}

uses
	aw_SCtrl;

{ TawScriptEditorForm }

procedure TawScriptEditorForm.FormCreate(Sender: TObject);
var
	l_tstop: cardinal;
begin
	// set memo's tabstop
	l_tstop := 16; // 4 chars
	SendMessage(Memo.Handle, EM_SETTABSTOPS, 1, cardinal(@l_tstop));
	
	FResult := mrCancel;
end;

procedure TawScriptEditorForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
	l_res: Word;
begin
	if (Memo.Modified) or (LanguageEdit.Modified) then
		l_res := MessageDlg('Save changes?', mtConfirmation, mbYesNoCancel, 0)
	else
		// nothing modified, so just exit
		l_res := mrNo;

	// save if they said yes
	if l_res = mrYes then
	begin
		Save;
		FResult := mrOK;
	end;

	// let them out unless they picked cancel
	CanClose := (l_res <> mrCancel);
end;

procedure TawScriptEditorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Action := caFree;
	ModalResult := FResult;
end;


{
********************************************************************************
* Util procedures
********************************************************************************
}

procedure TawScriptEditorForm.Open(const Script, Language: string);
begin
	LanguageEdit.Text := Language;
	Memo.Text := Script;

	Memo.SelStart := 0;

	UpdateCharPos;
	ScriptStatusBar.Panels[1].Text := '';
end;

procedure TawScriptEditorForm.Save;
begin
	with TawScriptEditor(Owner) do
	begin
		Code := Memo.Lines;
		Language := LanguageEdit.Text;
		if Assigned(OnChange) then
			OnChange(Owner);
	end;
end;


{
********************************************************************************
* UI procedures
********************************************************************************
}

procedure TawScriptEditorForm.UpdateCharPos;
begin
	// send Windows messages to retreive row and col info
	Row := SendMessage(Memo.Handle, EM_LINEFROMCHAR, -1, 0);
	Col := Memo.SelStart - SendMessage(Memo.Handle, EM_LINEINDEX, Row, 0);

	ScriptStatusBar.Panels[0].Text := 'Ln '+IntToStr(Row+1)+', Char '+IntToStr(Col+1);
end;

// called by both memo and lang. edit
procedure TawScriptEditorForm.SetChange(Sender: TObject);
begin
	ScriptStatusBar.Panels[1].Text := 'Modified';
end;

procedure TawScriptEditorForm.MemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	UpdateCharPos;
end;

procedure TawScriptEditorForm.MemoClick(Sender: TObject);
begin
	UpdateCharPos;
end;

procedure TawScriptEditorForm.MI_SelectAllClick(Sender: TObject);
begin
	Memo.SelStart := 0;
	Memo.SelLength := Length(Memo.Text)+1;
	UpdateCharPos;
end;

procedure TawScriptEditorForm.FormKeyUp(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	if Key = VK_ESCAPE then Close;
end;

end.
