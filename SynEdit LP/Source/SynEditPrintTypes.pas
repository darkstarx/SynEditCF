(*
  Letterpress

  Copyright 2000=2010, initial developers and Garnet
*)

{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/mpl/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPrintTypes.pas, released 2000-06-01.

The Initial Author of the Original Code is Morten J. Skovrup.
Portions written by Morten J. Skovrup are copyright 2000 Morten J. Skovrup.
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

$Id: SynEditPrintTypes.pas,v 1.4.2.3 2008/09/14 16:24:59 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://synedit.sourceforge.net
-------------------------------------------------------------------------------}

unit SynEditPrintTypes;

{$M+}
{$I SynEdit.inc }

interface

uses
  Graphics, Classes, SysUtils,

  { SynEdit }
  SynUnicode, SynEditPrinterInfo, SynEditMiscProcs;

const
  DefLeft = 25;   // Default left margin (mm)
  DefRight = 15;  // Default right margin (mm)
  DefTop = 25;    // Default top margin (mm)
  DefBottom = 25; // Default bottom margin (mm)
  DefHeader = 15; // Default margin from top of paper to bottom of header (mm)
  DefFooter = 15; // Default margin from top of footer to bottom of paper (mm)
  DefLeftHFTextIndent = 2; // Default Header / Footer indent from left margin (mm)
  DefRightHFTextIndent = 2; // Default Header / Footer indent from right margin (mm)
  DefHFInternalMargin = 0.5; // Default Internal margin between Header / Footer text and lines (mm)
  DefGutter = 0; // Default binding gutter - added to left or right margin (mm)

  mmPrInch = 25.4;
  mmPrCm = 10;

type
  { Frame around Header / Footer }
  TFrameType = (ftLine, ftBox, ftShaded);
  TFrameTypes = set of TFrameType;

  { Margin units (internally is allways used (mm)) }
  TUnitSystem = (usMM, usCM, usInch, muThousandthsOfInches);

  { Print status events }
  TSynPrintStatus = (psBegin, psNewPage, psEnd);
  TPrintStatusEvent = procedure(Sender: TObject; Status: TSynPrintStatus;
    PageNumber: Integer; var Abort: Boolean) of object;

  { Event raised when a line is printed (can be used to generate Table of Contents) }
  TPrintLineEvent = procedure(Sender: TObject; LineNumber, PageNumber: Integer) of object;

  { Margins class - sorting out dimensions of printable area }
  TSynEditPrintMargins = class(TPersistent)
  private
    FLeft,                      // Distance from left edge of paper to text
    FRight,                     // Distance from right edge of paper to text
    FTop,                       // Distance from top edge of paper to top of text
    FBottom: Double;            // Distance from bottom edge of paper to bottom of text
    FHeader,                    // Distance from top edge of paper to line below header
    FFooter: Double;            // Distance from bottom edge of paper to line above footer
    FLeftHFTextIndent: Double;  // Distance from left margin to first left-aligned character
                                // in header or footer
    FRightHFTextIndent: Double; // Distance from right margin to last right-aligned character
                                // in header or footer
    FHFInternalMargin: Double;  // Internal margin between top-line and text in header and
                                // footer AND between bottom-line and text in header and
                                // footer
    FGutter: Double;            // Binding gutter - added to right margin (or left if 2-sided)
    FMirrorMargins: Boolean;    // Set if margins should be mirrored (i.e. when printing
                                // 2-sided)
    FUnitSystem: TUnitSystem;   // The units used to specify sizes in.
                                // Internally is allways used mm
    function ConvertTo(Value: Double): Double;
    function ConvertFrom(Value: Double): Double;
    function GetBottom: Double;
    function GetFooter: Double;
    function GetGutter: Double;
    function GetHeader: Double;
    function GetLeft: Double;
    function GetRight: Double;
    function GetTop: Double;
    function GetLeftHFTextIndent: Double;
    function GetRightHFTextIndent: Double;
    function GetHFInternalMargin: Double;
    procedure SetBottom(const Value: Double);
    procedure SetFooter(const Value: Double);
    procedure SetGutter(const Value: Double);
    procedure SetHeader(const Value: Double);
    procedure SetLeft(const Value: Double);
    procedure SetRight(const Value: Double);
    procedure SetTop(const Value: Double);
    procedure SetLeftHFTextIndent(const Value: Double);
    procedure SetRightHFTextIndent(const Value: Double);
    procedure SetHFInternalMargin(const Value: Double);
  public
    { When initpage has been called, the following values will reflect the
      margins in paper units. Note that all values are calculated from
      left or top of paper (i.e. PRight is distance from left margin) }

    PLeft,  // Left position of text in device units (pixels) - this is the left
            // margin minus the left unprintable distance (+ gutter)
    PRight, // Right position of text in device units (pixels) - calculated form
            // left
    PTop,   // Top position of text in device units (pixels) - this is the top
            // margin minus the top unprintable distance
    PBottom: Integer; // Bottom position of text in device units (pixels) -
                      // calculated form top
    PHeader,          // Header in device units (pixels)
    PFooter: Integer; // Footer in device units (pixels) - calculated from top
    PLeftHFTextIndent: Integer;  // Left position of text in header and footer in device
                                 // units (pixels). Calculated as Left margin + LeftHFTextIndent
    PRightHFTextIndent: Integer; // Right position of text in header and footer in device
                                 // units (pixels). Calculated from left
    PHFInternalMargin: Integer;  // Internal margin in device units (pixels)
    PGutter: Integer; // Binding gutter in device units (pixels)
    constructor Create;
    procedure InitPage(ACanvas: TCanvas; PageNum: Integer;
      PrinterInfo: TSynEditPrinterInfo; LineNumbers,
      LineNumbersInMargin: Boolean; MaxLineNum: Integer);
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  published
    property UnitSystem: TUnitSystem read FUnitSystem write FUnitSystem
      default usMM;
    property Left: Double read GetLeft write SetLeft;
    property Right: Double read GetRight write SetRight;
    property Top: Double read GetTop write SetTop;
    property Bottom: Double read GetBottom write SetBottom;
    property Header: Double read GetHeader write SetHeader;
    property Footer: Double read GetFooter write SetFooter;
    property LeftHFTextIndent: Double read GetLeftHFTextIndent
      write SetLeftHFTextIndent;
    property RightHFTextIndent: Double read GetRightHFTextIndent
      write SetRightHFTextIndent;
    property HFInternalMargin: Double read GetHFInternalMargin
      write SetHFInternalMargin;
    property Gutter: Double read GetGutter write SetGutter;
    property MirrorMargins: Boolean read FMirrorMargins write FMirrorMargins;
  end;

implementation

{ TSynEditPrintMargins }

// -----------------------------------------------------------------------------

constructor TSynEditPrintMargins.Create;
begin
  inherited;
  FUnitSystem := usMM;
  FLeft := DefLeft;
  FRight := DefRight;
  FTop := DefTop;
  FBottom := DefBottom;
  FHeader := DefHeader;
  FFooter := DefFooter;
  FLeftHFTextIndent := DefLeftHFTextIndent;
  FRightHFTextIndent := DefRightHFTextIndent;
  FHFInternalMargin := DefHFInternalMargin;
  FGutter := DefGutter;
  FMirrorMargins := False;
end;

// -----------------------------------------------------------------------------
// Convert Value to mm
function TSynEditPrintMargins.ConvertTo(Value: Double): Double;
begin
  case FUnitSystem of
    usCM: Result := Value * mmPrCm;
    usInch: Result := Value * mmPrInch;
    muThousandthsOfInches: Result := mmPrInch * Value / 1000;
  else
    Result := Value;
  end;
end;

// -----------------------------------------------------------------------------
// Convert from mm to selected UnitSystem
function TSynEditPrintMargins.ConvertFrom(Value: Double): Double;
begin
  case FUnitSystem of
    usCM: Result := Value / mmPrCm;
    usInch: Result := Value / mmPrInch;
    muThousandthsOfInches: Result := 1000 * Value / mmPrInch;
  else
    Result := Value;
  end;
end;

function TSynEditPrintMargins.GetBottom: Double;
begin
  Result := ConvertFrom(FBottom);
end;

function TSynEditPrintMargins.GetFooter: Double;
begin
  Result := ConvertFrom(FFooter);
end;

function TSynEditPrintMargins.GetGutter: Double;
begin
  Result := ConvertFrom(FGutter);
end;

function TSynEditPrintMargins.GetHeader: Double;
begin
  Result := ConvertFrom(FHeader);
end;

function TSynEditPrintMargins.GetLeft: Double;
begin
  Result := ConvertFrom(FLeft);
end;

function TSynEditPrintMargins.GetRight: Double;
begin
  Result := ConvertFrom(FRight);
end;

function TSynEditPrintMargins.GetTop: Double;
begin
  Result := ConvertFrom(FTop);
end;

function TSynEditPrintMargins.GetLeftHFTextIndent: Double;
begin
  Result := ConvertFrom(FLeftHFTextIndent);
end;

function TSynEditPrintMargins.GetRightHFTextIndent: Double;
begin
  Result := ConvertFrom(FRightHFTextIndent);
end;

function TSynEditPrintMargins.GetHFInternalMargin: Double;
begin
  Result := ConvertFrom(FHFInternalMargin);
end;

procedure TSynEditPrintMargins.SetBottom(const Value: Double);
begin
  FBottom := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetFooter(const Value: Double);
begin
  FFooter := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetGutter(const Value: Double);
begin
  FGutter := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetHeader(const Value: Double);
begin
  FHeader := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetLeft(const Value: Double);
begin
  FLeft := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetRight(const Value: Double);
begin
  FRight := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetTop(const Value: Double);
begin
  FTop := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetLeftHFTextIndent(const Value: Double);
begin
  FLeftHFTextIndent := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetRightHFTextIndent(const Value: Double);
begin
  FRightHFTextIndent := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetHFInternalMargin(const Value: Double);
begin
  FHFInternalMargin := ConvertTo(Value);
end;

// -----------------------------------------------------------------------------
// Called by TSynEditPrint class to initialize margins
procedure TSynEditPrintMargins.InitPage(ACanvas: TCanvas; PageNum: Integer;
  PrinterInfo: TSynEditPrinterInfo; LineNumbers, LineNumbersInMargin: Boolean;
  MaxLineNum: Integer);
begin
  if FMirrorMargins and ((PageNum mod 2) = 0) then
  begin
    PLeft := PrinterInfo.PixFromLeft(FRight);
    PRight := PrinterInfo.PrintableWidth - PrinterInfo.PixFromRight(FLeft + FGutter);
  end
  else begin
    PLeft := PrinterInfo.PixFromLeft(FLeft + FGutter);
    PRight := PrinterInfo.PrintableWidth - PrinterInfo.PixFromRight(FRight);
  end;
  if LineNumbers and (not LineNumbersInMargin) then
    PLeft := PLeft + TextWidth(ACanvas, IntToStr(MaxLineNum) + ': ');
  PTop := PrinterInfo.PixFromTop(FTop);
  PBottom := PrinterInfo.PrintableHeight - PrinterInfo.PixFromBottom(FBottom);
  PHeader := PrinterInfo.PixFromTop(FHeader);
  PFooter := PrinterInfo.PrintableHeight - PrinterInfo.PixFromBottom(FFooter);
  PHFInternalMargin := Round(PrinterInfo.YPixPrmm * FHFInternalMargin);
  PGutter := Round(PrinterInfo.XPixPrmm * FGutter);
  PRightHFTextIndent := PRight - Round(PrinterInfo.XPixPrmm * FRightHFTextIndent);
  PLeftHFTextIndent := PLeft + Round(PrinterInfo.XPixPrmm * FLeftHFTextIndent);
end;

// -----------------------------------------------------------------------------
// Assign values from another TSynEditPrintMargins object
procedure TSynEditPrintMargins.Assign(Source: TPersistent);
var
  Src: TSynEditPrintMargins;
begin
  if (Source <> nil) and (Source is TSynEditPrintMargins) then begin
    Src := TSynEditPrintMargins(Source);
    FLeft := Src.FLeft;
    FRight := Src.FRight;
    FTop := Src.FTop;
    FBottom := Src.FBottom;
    FHeader := Src.FHeader;
    FFooter := Src.FFooter;
    FLeftHFTextIndent := Src.FLeftHFTextIndent;
    FRightHFTextIndent := Src.FRightHFTextIndent;
    FHFInternalMargin := Src.FHFInternalMargin;
    FGutter := Src.FGutter;
    FMirrorMargins := Src.FMirrorMargins;
    FUnitSystem := Src.FUnitSystem;
  end else
    inherited;
end;

procedure TSynEditPrintMargins.LoadFromStream(AStream: TStream);
begin
  with AStream do
  begin
    Read(FUnitSystem, SizeOf(FUnitSystem));
    Read(FLeft, SizeOf(FLeft));
    Read(FRight, SizeOf(FRight));
    Read(FTop, SizeOf(FTop));
    Read(FBottom, SizeOf(FBottom));
    Read(FHeader, SizeOf(FHeader));
    Read(FFooter, SizeOf(FFooter));
    Read(FLeftHFTextIndent, SizeOf(FLeftHFTextIndent));
    Read(FRightHFTextIndent, SizeOf(FRightHFTextIndent));
    Read(FHFInternalMargin, SizeOf(FHFInternalMargin));
    Read(FGutter, SizeOf(FGutter));
    Read(FMirrorMargins, SizeOf(FMirrorMargins));
  end;
end;

procedure TSynEditPrintMargins.SaveToStream(AStream: TStream);
begin
  with AStream do
  begin
    Write(FUnitSystem, SizeOf(FUnitSystem));
    Write(FLeft, SizeOf(FLeft));
    Write(FRight, SizeOf(FRight));
    Write(FTop, SizeOf(FTop));
    Write(FBottom, SizeOf(FBottom));
    Write(FHeader, SizeOf(FHeader));
    Write(FFooter, SizeOf(FFooter));
    Write(FLeftHFTextIndent, SizeOf(FLeftHFTextIndent));
    Write(FRightHFTextIndent, SizeOf(FRightHFTextIndent));
    Write(FHFInternalMargin, SizeOf(FHFInternalMargin));
    Write(FGutter, SizeOf(FGutter));
    Write(FMirrorMargins, SizeOf(FMirrorMargins));
  end;
end;

end.
