unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,

  SynEdit, SynEditHighlighter, SynUniHighlighter, StdCtrls;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  myEditor: TSynEdit;
  myHighlighter: TSynUniSyn;
  myTheme: TSynHighlighterTheme;

implementation

{$R *.dfm}

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(myHighlighter);
  FreeAndNil(myTheme);
  FreeAndNil(myEditor);
end;

procedure TForm1.FormClick(Sender: TObject);
begin
  myEditor.Gutter.Visible := true;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  myEditor := TSynEdit.Create(self);

  myEditor.Parent := self;
  myEditor.Width := 600;
  myEditor.Height := Self.ClientHeight;
  myEditor.Anchors := [akLeft, akTop, akRight, akBottom];
  myEditor.BorderStyle := bsNone;
  myEditor.Visible := true;
//  myEditor.WordWrap := true;  // << слова делятся на части при переносе
  myEditor.DoubleBuffered := true;
  myEditor.Gutter.Visible := true;

  myEditor.Gutter.ShowLineNumbers := true;   // << здесь вроде всё норм
  myEditor.Gutter.ShowLineStates := true;    // << эта фигня не работает
  myEditor.Gutter.ShowCodeFolding := true;   // << эта фигня тоже не работает

  myEditor.Gutter.DigitCount := 2;
  myEditor.Gutter.AutoSize := true;

  // SpellChecker тоже нихрена не работает, его пока вообще нет в SynEdit-е

  // Если попытаться вставить текст этого модуля, появятся иероглифы и другая дрянь

  myHighlighter := TSynUniSyn.Create(myEditor);
  myHighlighter.LoadGrammar('..\..\Delphi.package\delphi.grammar',
    true, nil, 'Delphi', [suloExternalGrammars], '');

  myEditor.Highlighter := myHighlighter;

  myTheme := TSynHighlighterTheme.Create('..\..\themes\Alpine.colors');
  myHighlighter.LoadFromFile(myTheme);

  myEditor.Text := 'select * from myTable' + #13#10 + 'where Name like ''%world!'';';
end;

end.
