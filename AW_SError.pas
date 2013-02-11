unit aw_SError;

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
	unit: aw_SError
	version: 1.01, 17/06/99
	components:
		TawScriptErrorForm
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
	TawScriptErrorForm = class(TForm)
    IconPanel: TPanel;
		ErrorImage: TImage;
		InfoImage: TImage;
    ButtonPanel: TPanel;
    OKButton: TButton;
    MemoPanel: TPanel;
    TextEdit: TMemo;
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
		{ Public declarations }
  end;

var
	awScriptErrorForm: TawScriptErrorForm;

implementation

{$R *.DFM}

procedure TawScriptErrorForm.FormResize(Sender: TObject);
begin
	// put OK button in the center
	OKButton.Left := Round(ButtonPanel.Width / 2) - Round(OKButton.Width / 2) + 2;
end;

end.
 