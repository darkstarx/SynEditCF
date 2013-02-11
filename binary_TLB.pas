unit binary_TLB;

// ************************************************************************ //
// WARNING
// -------
// The types declared in this file were generated from data read from a
// Type Library. If this type library is explicitly or indirectly (via
// another type library referring to this type library) re-imported, or the
// 'Refresh' command of the Type Library Editor activated while editing the
// Type Library, the contents of this file will be regenerated and all
// manual modifications will be lost.
// ************************************************************************ //

// $Rev: 17244 $
// File generated on 10/2/2010 1:34:31 AM from Type Library described below.

// ************************************************************************  //
// Type Lib: D:\University\Letterpress\source\letterpress\binary (1)
// LIBID: {B57D334C-993E-44FE-BC1B-9402CDA7A5F1}
// LCID: 0
// Helpfile:
// HelpString:
// DepndLst:
//   (1) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, StdVCL, Variants;


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries     : LIBID_xxxx
//   CoClasses          : CLASS_xxxx
//   DISPInterfaces     : DIID_xxxx
//   Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  binaryMajorVersion = 1;
  binaryMinorVersion = 0;

  LIBID_binary: TGUID = '{B57D334C-993E-44FE-BC1B-9402CDA7A5F1}';

  IID_ISynMacroHandler: TGUID = '{CE51F21A-8AFE-4CC8-AFB9-40BD1219691E}';
  CLASS_SynMacroHandler: TGUID = '{496281E4-A345-490B-9423-471EA1BDAA8B}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary
// *********************************************************************//
  ISynMacroHandler = interface;
  ISynMacroHandlerDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library
// (NOTE: Here we map each CoClass to its Default Interface)
// *********************************************************************//
  SynMacroHandler = ISynMacroHandler;


// *********************************************************************//
// Interface: ISynMacroHandler
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CE51F21A-8AFE-4CC8-AFB9-40BD1219691E}
// *********************************************************************//
  ISynMacroHandler = interface(IDispatch)
    ['{CE51F21A-8AFE-4CC8-AFB9-40BD1219691E}']
    procedure ExecuteCommand(const ACommand: WideString; const AData: WideString; ACount: Integer); safecall;
    procedure Prompt(const ATitle: WideString; const APrompt: WideString;
                     const AUpdateCallback: WideString; const AConfirmCallback: WideString); safecall;
    procedure SetCaretAndSelection(ACaretX: Integer; ACaretY: Integer; AStartX: Integer;
                                   AStartY: Integer; AEndX: Integer; AEndY: Integer); safecall;
    function Get_CurrText: WideString; safecall;
    procedure Set_CurrText(const Value: WideString); safecall;
    function Get_CaretX: Integer; safecall;
    procedure Set_CaretX(Value: Integer); safecall;
    function Get_CaretY: Integer; safecall;
    procedure Set_CaretY(Value: Integer); safecall;
    function Get_MacroDir: WideString; safecall;
    function Get_CurrWord: WideString; safecall;
    procedure Set_CurrWord(const Value: WideString); safecall;
    function Get_CurrChar: WideString; safecall;
    procedure Set_CurrChar(const Value: WideString); safecall;
    function Get_HasSelection: WordBool; safecall;
    function Get_BlockBeginX: Integer; safecall;
    procedure Set_BlockBeginX(Value: Integer); safecall;
    function Get_BlockBeginY: Integer; safecall;
    procedure Set_BlockBeginY(Value: Integer); safecall;
    function Get_BlockEndX: Integer; safecall;
    procedure Set_BlockEndX(Value: Integer); safecall;
    function Get_BlockEndY: Integer; safecall;
    procedure Set_BlockEndY(Value: Integer); safecall;
    function GetLine(ALine: Integer): WideString; safecall;
    function Get_WordBeginX: Integer; safecall;
    function Get_WordEndX: Integer; safecall;
    function Get_TabSize: Integer; safecall;
    procedure Set_TabSize(Value: Integer); safecall;
    function Get_LineCount: Integer; safecall;
    property CurrText: WideString read Get_CurrText write Set_CurrText;
    property CaretX: Integer read Get_CaretX write Set_CaretX;
    property CaretY: Integer read Get_CaretY write Set_CaretY;
    property MacroDir: WideString read Get_MacroDir;
    property CurrWord: WideString read Get_CurrWord write Set_CurrWord;
    property CurrChar: WideString read Get_CurrChar write Set_CurrChar;
    property HasSelection: WordBool read Get_HasSelection;
    property BlockBeginX: Integer read Get_BlockBeginX write Set_BlockBeginX;
    property BlockBeginY: Integer read Get_BlockBeginY write Set_BlockBeginY;
    property BlockEndX: Integer read Get_BlockEndX write Set_BlockEndX;
    property BlockEndY: Integer read Get_BlockEndY write Set_BlockEndY;
    property WordBeginX: Integer read Get_WordBeginX;
    property WordEndX: Integer read Get_WordEndX;
    property TabSize: Integer read Get_TabSize write Set_TabSize;
    property LineCount: Integer read Get_LineCount;
  end;

// *********************************************************************//
// DispIntf:  ISynMacroHandlerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CE51F21A-8AFE-4CC8-AFB9-40BD1219691E}
// *********************************************************************//
  ISynMacroHandlerDisp = dispinterface
    ['{CE51F21A-8AFE-4CC8-AFB9-40BD1219691E}']
    procedure ExecuteCommand(const ACommand: WideString; const AData: WideString; ACount: Integer); dispid 101;
    procedure Prompt(const ATitle: WideString; const APrompt: WideString;
                     const AUpdateCallback: WideString; const AConfirmCallback: WideString); dispid 201;
    procedure SetCaretAndSelection(ACaretX: Integer; ACaretY: Integer; AStartX: Integer;
                                   AStartY: Integer; AEndX: Integer; AEndY: Integer); dispid 202;
    property CurrText: WideString dispid 203;
    property CaretX: Integer dispid 204;
    property CaretY: Integer dispid 205;
    property MacroDir: WideString readonly dispid 206;
    property CurrWord: WideString dispid 207;
    property CurrChar: WideString dispid 208;
    property HasSelection: WordBool readonly dispid 209;
    property BlockBeginX: Integer dispid 210;
    property BlockBeginY: Integer dispid 211;
    property BlockEndX: Integer dispid 212;
    property BlockEndY: Integer dispid 213;
    function GetLine(ALine: Integer): WideString; dispid 214;
    property WordBeginX: Integer readonly dispid 216;
    property WordEndX: Integer readonly dispid 217;
    property TabSize: Integer dispid 215;
    property LineCount: Integer readonly dispid 219;
  end;

// *********************************************************************//
// The Class CoSynMacroHandler provides a Create and CreateRemote method to
// create instances of the default interface ISynMacroHandler exposed by
// the CoClass SynMacroHandler. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoSynMacroHandler = class
    class function Create: ISynMacroHandler;
    class function CreateRemote(const MachineName: string): ISynMacroHandler;
  end;

implementation

uses ComObj;

class function CoSynMacroHandler.Create: ISynMacroHandler;
begin
  Result := CreateComObject(CLASS_SynMacroHandler) as ISynMacroHandler;
end;

class function CoSynMacroHandler.CreateRemote(const MachineName: string): ISynMacroHandler;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SynMacroHandler) as ISynMacroHandler;
end;

end.

