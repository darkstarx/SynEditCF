program TestControl;

uses
  Forms,
  SynUniClasses in 'SynUni\Source\SynUniClasses.pas',
  SynUniHighlighter in 'SynUni\Source\SynUniHighlighter.pas',
  SynUniRules in 'SynUni\Source\SynUniRules.pas',
  SynAutoCorrect in 'SynEdit\Source\SynAutoCorrect.pas',
  SynAutoCorrectEditor in 'SynEdit\Source\SynAutoCorrectEditor.pas' {frmAutoCorrectEditor},
  SynCompletionProposal in 'SynEdit\Source\SynCompletionProposal.pas',
  SynDBEdit in 'SynEdit\Source\SynDBEdit.pas',
  SynEdit in 'SynEdit\Source\SynEdit.pas',
  SynEditAutoComplete in 'SynEdit\Source\SynEditAutoComplete.pas',
  SynEditExport in 'SynEdit\Source\SynEditExport.pas',
  SynEditHighlighter in 'SynEdit\Source\SynEditHighlighter.pas',
  SynEditKbdHandler in 'SynEdit\Source\SynEditKbdHandler.pas',
  SynEditKeyCmdEditor in 'SynEdit\Source\SynEditKeyCmdEditor.pas' {SynEditKeystrokeEditorForm},
  SynEditKeyCmds in 'SynEdit\Source\SynEditKeyCmds.pas',
  SynEditKeyCmdsEditor in 'SynEdit\Source\SynEditKeyCmdsEditor.pas' {SynEditKeystrokesEditorForm},
  SynEditKeyConst in 'SynEdit\Source\SynEditKeyConst.pas',
  SynEditMiscClasses in 'SynEdit\Source\SynEditMiscClasses.pas',
  SynEditMiscProcs in 'SynEdit\Source\SynEditMiscProcs.pas',
  SynEditOptionsDialog in 'SynEdit\Source\SynEditOptionsDialog.pas' {fmEditorOptionsDialog},
  SynEditPlugins in 'SynEdit\Source\SynEditPlugins.pas',
  SynEditPrint in 'SynEdit\Source\SynEditPrint.pas',
  SynEditPrinterInfo in 'SynEdit\Source\SynEditPrinterInfo.pas',
  SynEditPrintHeaderFooter in 'SynEdit\Source\SynEditPrintHeaderFooter.pas',
  SynEditPrintMargins in 'SynEdit\Source\SynEditPrintMargins.pas',
  SynEditPrintMarginsDialog in 'SynEdit\Source\SynEditPrintMarginsDialog.pas' {SynEditPrintMarginsDlg},
  SynEditPrintPreview in 'SynEdit\Source\SynEditPrintPreview.pas',
  SynEditPrintTypes in 'SynEdit\Source\SynEditPrintTypes.pas',
  SynEditPythonBehaviour in 'SynEdit\Source\SynEditPythonBehaviour.pas',
  SynEditRegexSearch in 'SynEdit\Source\SynEditRegexSearch.pas',
  SynEditSearch in 'SynEdit\Source\SynEditSearch.pas',
  SynEditStrConst in 'SynEdit\Source\SynEditStrConst.pas',
  SynEditTextBuffer in 'SynEdit\Source\SynEditTextBuffer.pas',
  SynEditTypes in 'SynEdit\Source\SynEditTypes.pas',
  SynEditWildcardSearch in 'SynEdit\Source\SynEditWildcardSearch.pas',
  SynEditWordWrap in 'SynEdit\Source\SynEditWordWrap.pas',
  SynExportHTML in 'SynEdit\Source\SynExportHTML.pas',
  SynExportRTF in 'SynEdit\Source\SynExportRTF.pas',
  SynExportTeX in 'SynEdit\Source\SynExportTeX.pas',
  SynMacroRecorder in 'SynEdit\Source\SynMacroRecorder.pas',
  SynMemo in 'SynEdit\Source\SynMemo.pas',
  SynRegExpr in 'SynEdit\Source\SynRegExpr.pas',
  SynTextDrawer in 'SynEdit\Source\SynTextDrawer.pas',
  SynUnicode in 'SynEdit\Source\SynUnicode.pas',
  SynUsp10 in 'SynEdit\Source\SynUsp10.pas',
  SynTokenMatch in 'SynEdit\Source\SynTokenMatch.pas',
  HTMLColors in 'ColorLib\HTMLColors.pas',
  pcre in 'regex\pcre.pas',
  RegularExpressions in 'regex\RegularExpressions.pas',
  SynEditCodeFolding in 'SynEdit\Source\SynEditCodeFolding.pas',
  SynHighlighterMulti in 'SynEdit\Source\SynHighlighterMulti.pas',
  Unit1 in 'Unit1.pas' {Form1},
  TBXUtils in 'TBX\TBXUtils.pas',
  SynEditOptions in 'SynEdit\Source\SynEditOptions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
