unit aw_SCtrl;

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

	Contributor(s):
		Martijn van der Kooij <MKGal@geocities.com>
		Winston R.S <winstone@centrin.net.id>
}

{
	unit: aw_SCtrl
	version: 1.04, 27/8/99
	components:
		TawScriptControl
		TawScriptEditor
		TawScriptErrorDlg
}

interface

uses
	Classes, Graphics, SysUtils, ComObj, ActiveX, AW_MSSC_TLB;

const
	CRLF = #13#10;

type
	TawScriptError = class(TObject)
	private
		FScriptError: IScriptError;
		function Get_Number: Integer;
		function Get_Source: WideString;
		function Get_Description: WideString;
		function Get_HelpFile: WideString;
		function Get_HelpContext: Integer;
		function Get_Text: WideString;
		function Get_Line: Integer;
		function Get_Column: Integer;
	public
		property Number: Integer read Get_Number;
		property Source: WideString read Get_Source;
		property Description: WideString read Get_Description;
		property HelpFile: WideString read Get_HelpFile;
		property HelpContext: Integer read Get_HelpContext;
		property Text: WideString read Get_Text;
		property Line: Integer read Get_Line;
		property Column: Integer read Get_Column;
	end;

{ TawScriptControl }

	TawScriptControl = class;

	TawAutoObject = class(TCollectionItem)
	private
		FAutoObjectName: string;
		FAutoObject: IDispatch;
		procedure SetAutoObject(const Value: IDispatch);
		procedure SetAutoObjectName(const Value: string);
	public
		constructor Create(Collection: TCollection); override;
		procedure Assign(Source: TPersistent); override;

		property AutoObject: IDispatch read FAutoObject write SetAutoObject;
	published
		property AutoObjectName: string read FAutoObjectName write SetAutoObjectName;
	end;

	TawAutoObjects = class(TCollection)
	private
		FScriptControl: TawScriptControl;
		function GetItem(Index: Integer): TawAutoObject;
		procedure SetItem(Index: Integer; Value: TawAutoObject);
	protected
		function GetOwner: TPersistent; override;
		procedure Update(Item: TCollectionItem); override;
	public
		constructor Create(ScriptControl: TawScriptControl);
		function Add: TawAutoObject;
		property Items[Index: Integer]: TawAutoObject read GetItem write SetItem; default;
	end;

	TawScriptControlErrorEvent = procedure (Sender: TObject;
		Error: TawScriptError) of object;
	TawScriptControlCallEvent = procedure (Sender: TObject;
		const FunctionName: string; const Params: array of Variant) of object;

	TawScriptControl = class(TComponent)
	private
		FAllowUI: Boolean;
		FAutoObjects: TawAutoObjects;
		FCode: TStrings;
		FLanguage: string;
		FOnError: TawScriptControlErrorEvent;
		FOnCallFunction: TawScriptControlCallEvent;
		FOnTimeout: TNotifyEvent;
		FScriptControl: TScriptControl;
		FTimeout: integer;
		function GetAllowUI: boolean;
		function GetTimeout: integer;
		procedure SetAllowUI(const Value: boolean);
		procedure SetCode(const Value: TStrings);
		procedure SetLanguage(const Value: string);
		procedure SetTimeout(const Value: integer);
		procedure SetAutoObjects(const Value: TawAutoObjects);
	protected
		procedure SCtrlOnError(Sender: TObject); virtual;
		procedure SCtrlOnTimeout(Sender: TObject); virtual;
		procedure CodeOnChange(Sender: TObject); virtual;
		// 27/8/99 ACW - made UpdateAutoObjects a virtual proc
		procedure UpdateAutoObjects; virtual;
		procedure Loaded; override;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		// 27/8/99 Winston R.S - this should return an OleVariant
		function CallFunction(const FunctionName: string;
			const Params: array of Variant): OleVariant;
		procedure Clear;	// 27/8/99 ACW - Added clear method to clear code and objects
		// 27/8/99 ACW - Added run-time property to allow access directly
		// to the activex control.
		property ActiveXScriptControl: TScriptControl read FScriptControl;
	published
		property AllowUI: boolean read GetAllowUI write SetAllowUI default True;
		property Language: string read FLanguage write SetLanguage;
		property AutoObjects: TawAutoObjects read FAutoObjects write SetAutoObjects;
		property Code: TStrings read FCode write SetCode;
		property Timeout: integer read GetTimeout write SetTimeout default 10000;

		property OnError: TawScriptControlErrorEvent read FOnError write FOnError;
		property OnTimeout: TNotifyEvent read FOnTimeout write FOnTimeout;
		// 27/8/99 ACW - Added new OnCall event
		property OnCallFunction: TawScriptControlCallEvent
			read FOnCallFunction write FOnCallFunction;
	end;

{ TawScriptEditor }

	TawScriptEditor = class(TComponent)
	private
		FCaption: string;
		FCode: TStrings;
		FFont: TFont;
		FEditorFont: TFont;
		FHeight: integer;
		FIsOpen: boolean;
		FLanguage: string;
		FWidth: integer;
		FOnChange: TNotifyEvent;
		procedure SetCode(Value: TStrings);
		procedure SetFont(Value: TFont);
		procedure SetEditorFont(Value: TFont);
	protected
		procedure CreateEditWin;
		procedure EditorFormDestroy(Sender: TObject);
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure Show;
		function ShowModal: integer; 
		property IsOpen: boolean read FIsOpen;
	published
		property Caption: string read FCaption write FCaption;
		property Code: TStrings read FCode write SetCode;
		property EditorFont: TFont read FEditorFont write SetEditorFont;
		property Font: TFont read FFont write SetFont;
		property Height: integer read FHeight write FHeight default 0;
		property Language: string read FLanguage write FLanguage;
		property Width: integer read FWidth write FWidth default 0;
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
	end;

{ TawScriptErrorDlg }

	TawScriptErrorIcon = (eiInfo, eiError);

	TawScriptErrorDlg = class(TComponent)
	private
		FCaption: string;
		FErrorFont: TFont;
		FFont: TFont;
		FHeight: integer;
		FWidth: integer;
		procedure SetErrorFont(const Value: TFont);
		procedure SetFont(const Value: TFont);
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure ShowModalMsg(Msg: string; Icon: TawScriptErrorIcon);
		procedure ShowModalError(Error: TawScriptError);
	published
		property Caption: string read FCaption write FCaption;
		property ErrorFont: TFont read FErrorFont write SetErrorFont;
		property Font: TFont read FFont write SetFont;
		property Height: integer read FHeight write FHeight default 150;
		property Width: integer read FWidth write FWidth default 500;
	end;

procedure Register;

implementation

// 17/06/99 - ACW - Added OleCtrls
uses
	Forms, aw_SEditor, aw_SError, OleCtrls, Variants;

{ TawScriptError }

function TawScriptError.Get_Column: Integer;
begin
	Result := FScriptError.Column;
end;

function TawScriptError.Get_Description: WideString;
begin
	Result := FScriptError.Description;
end;

function TawScriptError.Get_HelpContext: Integer;
begin
	Result := FScriptError.HelpContext;
end;

function TawScriptError.Get_HelpFile: WideString;
begin
	Result := FScriptError.HelpFile;
end;

function TawScriptError.Get_Line: Integer;
begin
	Result := FScriptError.Line;
end;

function TawScriptError.Get_Number: Integer;
begin
	Result := FScriptError.Number;
end;

function TawScriptError.Get_Source: WideString;
begin
	Result := FScriptError.Source;
end;

function TawScriptError.Get_Text: WideString;
begin
	Result := FScriptError.Text;
end;


{ TawAutoObject }

constructor TawAutoObject.Create(Collection: TCollection);
begin
	FAutoObjectName := '';
	FAutoObject := nil;
	inherited Create(Collection);
end;

procedure TawAutoObject.Assign(Source: TPersistent);
begin
	if Source is TawAutoObject then
	begin
		AutoObjectName := TawAutoObject(Source).AutoObjectName;
		AutoObject := TawAutoObject(Source).AutoObject;
		Changed(False);
	end
	else
		inherited Assign(Source);
end;

procedure TawAutoObject.SetAutoObject(const Value: IDispatch);
begin
	FAutoObject := Value;
	Changed(False);
end;

procedure TawAutoObject.SetAutoObjectName(const Value: string);
begin
	if FAutoObjectName <> Value then
	begin
		FAutoObjectName := Value;
		Changed(False);
	end;
end;


{ TawAutoObjects }

constructor TawAutoObjects.Create(ScriptControl: TawScriptControl);
begin
	inherited Create(TawAutoObject);
	FScriptControl := ScriptControl;
end;

function TawAutoObjects.Add: TawAutoObject;
begin
	Result := TawAutoObject(inherited Add);
end;

function TawAutoObjects.GetItem(Index: Integer): TawAutoObject;
begin
	Result := TawAutoObject(inherited GetItem(Index));
end;

function TawAutoObjects.GetOwner: TPersistent;
begin
	Result := FScriptControl;
end;

procedure TawAutoObjects.SetItem(Index: Integer; Value: TawAutoObject);
begin
	inherited SetItem(Index, Value);
end;

procedure TawAutoObjects.Update(Item: TCollectionItem);
begin
	FScriptControl.UpdateAutoObjects;
end;


{ TawScriptControl }

{
	Create / Destroy
}

constructor TawScriptControl.Create(AOwner: TComponent);
begin
	inherited;

	// 17/06/99 - Martijn van der Kooij, ACW
	// Do class create bits first, then make sure that if
	// TScriptControl.Create fails, it is handled.
	FLanguage := 'JScript';
	FAllowUI := True;
	FTimeout := 10000;
	FAutoObjects := TawAutoObjects.Create(self);

	FCode := TStringList.Create;
	TStringList(FCode).OnChange := CodeOnChange;

	FScriptControl := nil;
	if not (csDesigning in ComponentState) then
	begin
		try
			FScriptControl := TScriptControl.Create(self);
			FScriptControl.Language := 'JScript';
			FScriptControl.OnError := SCtrlOnError;
			FScriptControl.OnTimeout := SCtrlOnTimeout;
			FScriptControl.AllowUI := True;
			FScriptControl.Timeout := 10000;
		except
			// if create fails, make sure FScriptControl is nil and raise an exception
			FScriptControl := nil;
			raise EOleCtrlError.Create('Microsoft ActiveX Script Control not installed.');
		end;
	end;
end;

destructor TawScriptControl.Destroy;
begin
	if fScriptControl <> nil then
		if fScriptControl.Language <> EmptyStr then
			fScriptControl.Reset;
	fCode.Free;
	inherited;
end;

{
	Functions
}

// call with standard Delphi array
function TawScriptControl.CallFunction(const FunctionName: string;
	const Params: array of Variant): OleVariant;
var
	l_args_arr: Variant;
	l_args: PSafeArray;
	l_high, i: integer;
begin
	if FScriptControl = nil then Exit;

	// 27/8/99 ACW - fire the OnCall event
	if Assigned(FOnCallFunction) then FOnCallFunction(Self, FunctionName, Params);

	// build array of values
	l_high := High(Params);
	l_args_arr := VarArrayCreate([0, l_high], varVariant);
	for i := 0 to l_high do
		l_args_arr[i] := Params[i];

	// convert to PSafeArray
	l_args := PSafeArray(TVarData(l_args_arr).VArray);

	// call the function
	Result := FScriptControl.Run(FunctionName, l_args);
end;

procedure TawScriptControl.Clear;
begin
	// clear the code and objects, then update the script control 
	FCode.Clear;
	FAutoObjects.Clear;
	UpdateAutoObjects;
end;

procedure TawScriptControl.SCtrlOnError(Sender: TObject);
var
	l_error: TawScriptError;
begin
	if assigned(FOnError) then
	begin
		l_error := TawScriptError.Create;
		l_error.FScriptError := FScriptControl.Error;

		FOnError(self, l_error);

		l_error.Free;
	end;
end;

procedure TawScriptControl.SCtrlOnTimeout(Sender: TObject);
begin
	if assigned(FOnTimeout) then
		FOnTimeout(self);
end;

procedure TawScriptControl.CodeOnChange(Sender: TObject);
begin
	// ACW 22/7/99 check that component is not loading before setting code
	if (FScriptControl <> nil) and not (csLoading in ComponentState) then
		FScriptControl.AddCode(Code.Text);
end;

procedure TawScriptControl.UpdateAutoObjects;
var
	i: integer;
begin
	if FScriptControl <> nil then
	begin
		if FScriptControl.Language <> '' then
		begin
			FScriptControl.Reset;
			FScriptControl.Language := FLanguage;
		end;

		for i := 0 to AutoObjects.Count-1 do
		begin
			// if we have a valid object, add it in
			if (AutoObjects[i].AutoObject <> nil) and
				(AutoObjects[i].AutoObjectName <> '') then
			begin
				FScriptControl.AddObject(AutoObjects[i].AutoObjectName, AutoObjects[i].AutoObject, TRUE);
			end;
		end;

		FScriptControl.AddCode(Code.Text);
	end;
end;

procedure TawScriptControl.Loaded;
begin
	// ACW 26/7/99 forgot to call inherited
	inherited;

	// ACW 22/7/99 Only load code into control once loaded
	if (Code.Text <> '') and (FScriptControl <> nil) then
	begin
		FScriptControl.AddCode(Code.Text);
	end;
end;


{
	Properties
}

function TawScriptControl.GetAllowUI: boolean;
begin
	Result := FAllowUI;
end;

function TawScriptControl.GetTimeout: integer;
begin
	Result := FTimeout;
end;

procedure TawScriptControl.SetAllowUI(const Value: boolean);
begin
	FAllowUI := Value;
	if FScriptControl <> nil then
		FScriptControl.AllowUI := Value;
end;

procedure TawScriptControl.SetAutoObjects(const Value: TawAutoObjects);
begin
	FAutoObjects.Assign(Value);
end;

procedure TawScriptControl.SetCode(const Value: TStrings);
begin
	FCode.Assign(Value);
	// ACW 22/7/99 check that component is not loading before setting code
	if (FScriptControl <> nil) and not (csLoading in ComponentState) then
		FScriptControl.AddCode(Code.Text);
end;

procedure TawScriptControl.SetLanguage(const Value: string);
begin
	if FLanguage <> Value then
	begin
		FLanguage := Value;
		// ACW 22/7/99 only clear the code if we are not loading
		if not (csLoading in ComponentState) then Code.Clear;

		if FScriptControl <> nil then
		begin
			FScriptControl.Language := FLanguage;
			UpdateAutoObjects;
			FScriptControl.AddCode(Code.Text);
		end;
	end;
end;

procedure TawScriptControl.SetTimeout(const Value: integer);
begin
	if FTimeout <> Value then
	begin
		FTimeout := Value;
		if FScriptControl <> nil then
			FScriptControl.Timeout := Value;
	end;
end;


{ TawScriptEditor }

constructor TawScriptEditor.Create(AOwner: TComponent);
begin
	inherited;

	FCaption := 'Script Editor';
	FHeight := 0;
	FWidth := 0;
	FEditorFont := TFont.Create;
	FFont := TFont.Create;
	FCode := TStringList.Create;
	FIsOpen := False;

	if Owner is TForm then
		FFont.Assign(TForm(Owner).Font);

	FEditorFont.Name := 'Courier New';
	FEditorFont.Size := 8;
end;

destructor TawScriptEditor.Destroy;
begin
	FEditorFont.Free;
	FFont.Free;
	FCode.Free;
	inherited;
end;

procedure TawScriptEditor.CreateEditWin;
begin
	FIsOpen := True;

	awScriptEditorForm := TawScriptEditorForm.Create(self);

	if (FWidth > 0) and (FHeight > 0) then
	begin
		awScriptEditorForm.Width := FWidth;
		awScriptEditorForm.Height := FHeight;
		awScriptEditorForm.Position := poDesigned;
	end;

	awScriptEditorForm.Caption := FCaption;
	awScriptEditorForm.Font := FFont;
	awScriptEditorForm.Memo.Font := FEditorFont;

	awScriptEditorForm.OnDestroy := EditorFormDestroy;

	awScriptEditorForm.Open(FCode.Text, FLanguage);
end;

procedure TawScriptEditor.Show;
begin
	CreateEditWin;
	awScriptEditorForm.Show;
end;

function TawScriptEditor.ShowModal: integer;
begin
	CreateEditWin;
	Result := awScriptEditorForm.ShowModal;
end;

procedure TawScriptEditor.SetCode(Value: TStrings);
begin
	FCode.Assign(Value);
end;

procedure TawScriptEditor.SetEditorFont(Value: TFont);
begin
	FEditorFont.Assign(Value);
end;

procedure TawScriptEditor.SetFont(Value: TFont);
begin
	FFont.Assign(Value);
end;

procedure TawScriptEditor.EditorFormDestroy(Sender: TObject);
begin
	FIsOpen := False;
end;

{ TawScriptErrorDlg }

constructor TawScriptErrorDlg.Create(AOwner: TComponent);
begin
	inherited;

	FCaption := 'Script Error';
	FWidth := 500;
	FHeight := 150;
	FFont := TFont.Create;
	FErrorFont := TFont.Create;

	if Owner is TForm then
		FFont.Assign(TForm(Owner).Font);

	FErrorFont.Name := 'Courier New';
	FErrorFont.Size := 8;
end;

destructor TawScriptErrorDlg.Destroy;
begin
	FFont.Free;
	inherited;
end;

procedure TawScriptErrorDlg.SetErrorFont(const Value: TFont);
begin
	FErrorFont.Assign(Value);
end;

procedure TawScriptErrorDlg.SetFont(const Value: TFont);
begin
	FFont.Assign(Value);
end;

procedure TawScriptErrorDlg.ShowModalMsg(Msg: string; Icon: TawScriptErrorIcon);
begin
	awScriptErrorForm := TawScriptErrorForm.Create(Application);
	try
		awScriptErrorForm.Width := FWidth;
		awScriptErrorForm.Height := FHeight;
		awScriptErrorForm.Font := FFont;
		awScriptErrorForm.TextEdit.Font := FErrorFont;
		awScriptErrorForm.Caption := FCaption;

		if Icon = eiInfo then
		begin
			awScriptErrorForm.InfoImage.Visible := True;
			awScriptErrorForm.ErrorImage.Visible := False;
		end
		else begin
			awScriptErrorForm.InfoImage.Visible := False;
			awScriptErrorForm.ErrorImage.Visible := True;
		end;

		awScriptErrorForm.TextEdit.Text := Msg;
		awScriptErrorForm.ShowModal;
	finally
		awScriptErrorForm.Free;
	end;
end;

procedure TawScriptErrorDlg.ShowModalError(Error: TawScriptError);
var
	l_msg: string;
begin
	// 27/8/99 Winston R.S, ACW - Added error number to message
	l_msg := Format(
		'[%d] %s:' + CRLF +
		'%s at line:%d, char:%d' + CRLF +
		'near:%s',
		[Error.Number, Error.Source, Error.Description, Error.Line,
			Error.Column, Error.Text]);
	ShowModalMsg(l_msg, eiError);
end;


{
	Register
}

procedure Register;
begin
	RegisterComponents('AW Controls',
		[TawScriptControl, TawScriptEditor, TawScriptErrorDlg]);
end;

end.
