unit TBXUtils;

// TBX Package
// Copyright 2001-2004 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// $Id: TBXUtils.pas 132 2005-11-07 20:50:26Z Alex $

interface

uses
  Types, Windows, Messages, Classes, SysUtils, Graphics, Controls, Forms, ImgList,
  CommCtrl;

const
  WeightR: single = 0.764706;
  WeightG: single = 1.52941;
  WeightB: single = 0.254902;

type
{ TILBlendData }
{ TILBlendData is used in ImageList_Blend function for rendering image lists }
  PRGBQuad = ^TRGBQuad;
  TRGBQuad = Cardinal;
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array [Byte] of TRGBQuad;
  TRGBQuadDynArray = array of TRGBQuad;
  TILBlendData = record
    Opacity: Byte;         // $00 - transparent; $FF - opaque
    Desaturate: Byte;      // $00 - normal; $FF - grayscale
    EnhanceContrast: Byte; // $00 - none; $01 - invert some colors
    BlendAmount: Byte;     // $00 - normal; $FF - replace all colors with BlendColor
    BlendColor: TColorRef; // in $00bbggrr format
  end;

{ Text rotation flags }
const
  DTR_0    = 0;
  DTR_90   = $01000000;
  DTR_270  = $02000000;
  DTR_MASK = $0F000000;

function ColorDistance(C1, C2: Integer): Single;

function  MixColors(C1, C2: TColor; W1: Integer): TColor;
function  SameColors(C1, C2: TColor): Boolean;
function  Lighten(C: TColor; Amount: Integer): TColor;
function  NearestLighten(C: TColor; Amount: Integer): TColor;
function  NearestMixedColor(C1, C2: TColor; W1: Integer): TColor;
function  ColorIntensity(C: TColor): Integer;
function  IsDarkColor(C: TColor; Threshold: Integer = 100): Boolean;
function  Blend(C1, C2: TColor; W1: Integer): TColor;
procedure SetContrast(var Color: TColor; BkgndColor: TColor; Threshold: Integer);
function  GetBGR(C: TColorRef): Cardinal;

{ Gradients }
type
  TGradientKind = (gkHorz, gkVert);

procedure GradFill(DC: HDC; ARect: TRect; ClrTopLeft, ClrBottomRight: TColor; Kind: TGradientKind);
procedure BrushedFill(DC: HDC; Origin: PPoint; ARect: TRect; Color: TColor; Roughness: Integer);
procedure ResetBrushedFillCache;

{ alternatives to fillchar and move routines what work with 32-bit aligned memory blocks }
procedure FillLongword(var Dst; Count: Integer; Value: Longword);
procedure MoveLongword(const Src; var Dst; Count: Integer);
procedure BidiMoveLongword(const Src; var Dst; Count: Integer);

{ An additional declaration for D4 compiler }
type
  PColor = ^TColor;

{ Stock Objects }
var
  StockBitmap1, StockBitmap2: TBitmap;
  StockMonoBitmap, StockCompatibleBitmap: TBitmap;
  SmCaptionFont: TFont;

const
  ROP_DSPDxax = $00E20746;
  {$NODEFINE ILD_SCALE}
  ILD_SCALE = $2000;

type
  PBlendFunction = ^TBlendFunction;
  TBlendFunction = packed record
    BlendOp: Byte;
    BlendFlags: Byte;
    SourceConstantAlpha: Byte;
    AlphaFormat: Byte;
  end;

  TUpdateLayeredWindow = function(hWnd : hWnd; hdcDst : hDC; pptDst : PPoint;
    psize : PSize; hdcSrc : hDC; pptSrc : PPoint; crKey : TColorRef;
    pblend : PBlendFunction; dwFlags : Integer): Integer; stdcall;

  TAlphaBlend = function(hdcDest: HDC; nXOriginDest, nYOriginDest,
    nWidthDest, nHeightDest: Integer; hdcSrc: HDC;
    nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer;
    blendFunction: TBlendFunction): BOOL; stdcall;

  TGradientFill = function(Handle: HDC; pVertex: Pointer; dwNumVertex: DWORD;
    pMesh: Pointer; dwNumMesh: DWORD; dwMode: DWORD): DWORD; stdcall;

var
  UpdateLayeredWindow: TUpdateLayeredWindow = nil;
  AlphaBlend: TAlphaBlend = nil;
  GradientFill: TGradientFill = nil;

implementation

{$R-}{$Q-}

uses Math;

function EnumFontsProc(const lplf: TLogFont; const lptm: TTextMetric;
  dwType: DWORD; lpData: LPARAM): Integer; stdcall;
begin
  Boolean(Pointer(lpData)^) := True;
  Result := 0;
end;

function CreateFontR(DC: HDC; Flags: Cardinal): HFONT;
var
  LogFont: TLogFont;
  VerticalFontName: array[0..LF_FACESIZE-1] of Char;
  VerticalFontExists: Boolean;
  TM: TTextMetric;
begin
  Result := 0;
  if GetObject(GetCurrentObject(DC, OBJ_FONT), SizeOf(LogFont), @LogFont) = 0 then
    Exit;

  case Flags and DTR_MASK of
    DTR_0: LogFont.lfEscapement := 0;
    DTR_90: LogFont.lfEscapement := 900;
    DTR_270: LogFont.lfEscapement := 2700;
  else
    Exit;
  end;
  LogFont.lfOrientation := LogFont.lfEscapement;
  LogFont.lfOutPrecision := OUT_TT_ONLY_PRECIS;  { needed for Win9x }

  if LogFont.lfEscapement <> 0 then
  begin
    { Don't let a random TrueType font be substituted when MS Sans Serif or
      Microsoft Sans Serif are used. Hard-code Arial. }
    if (StrIComp(LogFont.lfFaceName, 'MS Sans Serif') = 0) or
       (StrIComp(LogFont.lfFaceName, 'Microsoft Sans Serif') = 0) then
    begin
      StrPCopy(LogFont.lfFaceName, 'Arial');
      if GetTextMetrics(DC, TM) then
      begin
        { If the original height was negative, keep it negative }
        if LogFont.lfHeight <= 0 then LogFont.lfHeight := -(TM.tmHeight - TM.tmInternalLeading)
        else LogFont.lfHeight := TM.tmHeight;
      end;
    end;

    { Use a vertical font if available so that Asian characters aren't drawn
      sideways }
    if StrLen(LogFont.lfFaceName) < SizeOf(VerticalFontName) - 1 then
    begin
      VerticalFontName[0] := '@';
      StrCopy(@VerticalFontName[1], LogFont.lfFaceName);
      VerticalFontExists := False;
      EnumFonts(DC, VerticalFontName, @EnumFontsProc, @VerticalFontExists);
      if VerticalFontExists then StrCopy(LogFont.lfFaceName, VerticalFontName);
    end;
  end;

  Result := CreateFontIndirect(LogFont);
end;

function MixColors(C1, C2: TColor; W1: Integer): TColor;
var
  W2: Cardinal;
begin
  Assert(W1 in [0..255]);
  W2 := W1 xor 255;
  if Integer(C1) < 0 then C1 := GetSysColor(C1 and $000000FF);
  if Integer(C2) < 0 then C2 := GetSysColor(C2 and $000000FF);
  Result := Integer(
    ((Cardinal(C1) and $FF00FF) * Cardinal(W1) +
    (Cardinal(C2) and $FF00FF) * W2) and $FF00FF00 +
    ((Cardinal(C1) and $00FF00) * Cardinal(W1) +
    (Cardinal(C2) and $00FF00) * W2) and $00FF0000) shr 8;
end;

function SameColors(C1, C2: TColor): Boolean;
begin
  if C1 < 0 then C1 := GetSysColor(C1 and $000000FF);
  if C2 < 0 then C2 := GetSysColor(C2 and $000000FF);
  Result := C1 = C2;
end;

function Lighten(C: TColor; Amount: Integer): TColor;
var
  R, G, B: Integer;
begin
  if C < 0 then C := GetSysColor(C and $000000FF);
  R := C and $FF + Amount;
  G := C shr 8 and $FF + Amount;
  B := C shr 16 and $FF + Amount;
  if R < 0 then R := 0 else if R > 255 then R := 255;
  if G < 0 then G := 0 else if G > 255 then G := 255;
  if B < 0 then B := 0 else if B > 255 then B := 255;
  Result := R or (G shl 8) or (B shl 16);
end;

function NearestLighten(C: TColor; Amount: Integer): TColor;
begin
  Result := GetNearestColor(StockCompatibleBitmap.Canvas.Handle, Lighten(C, Amount));
end;

function NearestMixedColor(C1, C2: TColor; W1: Integer): TColor;
begin
  Result := MixColors(C1, C2, W1);
  Result := GetNearestColor(StockCompatibleBitmap.Canvas.Handle, Result);
end;

function ColorIntensity(C: TColor): Integer;
begin
  if C < 0 then C := GetSysColor(C and $FF);
  Result := ((C shr 16 and $FF) * 30 + (C shr 8 and $FF) * 150 + (C and $FF) * 76) shr 8;
end;

function IsDarkColor(C: TColor; Threshold: Integer = 100): Boolean;
begin
  if C < 0 then C := GetSysColor(C and $FF);
  Threshold := Threshold shl 8;
  Result := ((C and $FF) * 76 + (C shr 8 and $FF) * 150 + (C shr 16 and $FF) * 30 ) < Threshold;
end;

function Blend(C1, C2: TColor; W1: Integer): TColor;
var
  W2, A1, A2, D, F, G: Integer;
begin
  if C1 < 0 then C1 := GetSysColor(C1 and $FF);
  if C2 < 0 then C2 := GetSysColor(C2 and $FF);

  if W1 >= 100 then D := 1000
  else D := 100;

  W2 := D - W1;
  F := D div 2;

  A2 := C2 shr 16 * W2;
  A1 := C1 shr 16 * W1;
  G := (A1 + A2 + F) div D and $FF;
  Result := G shl 16;

  A2 := (C2 shr 8 and $FF) * W2;
  A1 := (C1 shr 8 and $FF) * W1;
  G := (A1 + A2 + F) div D and $FF;
  Result := Result or G shl 8;

  A2 := (C2 and $FF) * W2;
  A1 := (C1 and $FF) * W1;
  G := (A1 + A2 + F) div D and $FF;
  Result := Result or G;
end;

function ColorDistance(C1, C2: Integer): Single;
var
  DR, DG, DB: Integer;
begin
  DR := (C1 and $FF) - (C2 and $FF);
  Result := Sqr(DR * WeightR);
  DG := (C1 shr 8 and $FF) - (C2 shr 8 and $FF);
  Result := Result + Sqr(DG * WeightG);
  DB := (C1 shr 16) - (C2 shr 16);
  Result := Result + Sqr(DB * WeightB);
  Result := SqRt(Result);
end;

function GetAdjustedThreshold(BkgndIntensity, Threshold: Single): Single;
begin
  if BkgndIntensity < 220 then Result := (2 - BkgndIntensity / 220) * Threshold
  else Result := Threshold;
end;

function IsContrastEnough(AColor, ABkgndColor: Integer;
  DoAdjustThreshold: Boolean; Threshold: Single): Boolean;
begin
  if DoAdjustThreshold then
    Threshold := GetAdjustedThreshold(ColorDistance(ABkgndColor, $000000), Threshold);
  Result := ColorDistance(ABkgndColor, AColor) > Threshold;
end;

procedure AdjustContrast(var AColor: Integer; ABkgndColor: Integer; Threshold: Single);
var
  x, y, z: Single;
  r, g, b: Single;
  RR, GG, BB: Integer;
  i1, i2, s, q, w: Single;
  DoInvert: Boolean;
begin
  i1 := ColorDistance(AColor, $000000);
  i2 := ColorDistance(ABkgndColor, $000000);
  Threshold := GetAdjustedThreshold(i2, Threshold);

  if i1 > i2 then DoInvert := i2 < 442 - Threshold
  else DoInvert := i2 < Threshold;  

  x := (ABkgndColor and $FF) * WeightR;
  y := (ABkgndColor shr 8 and $FF) * WeightG;
  z := (ABkgndColor shr 16) * WeightB;

  r := (AColor and $FF) * WeightR;
  g := (AColor shr 8 and $FF) * WeightG;
  b := (AColor shr  16) * WeightB;

  if DoInvert then
  begin
    r := 195 - r;
    g := 390 - g;
    b := 65 - b;
    x := 195 - x;
    y := 390 - y;
    z := 65 - z;
  end;

  s := Sqrt(Sqr(b) + Sqr(g) + Sqr(r));
  if s < 0.01 then s := 0.01;

  q := (r * x + g * y + b * z) / S;

  x := Q / S * r - x;
  y := Q / S * g - y;
  z := Q / S * b - z;

  w :=  Sqrt(Sqr(Threshold) - Sqr(x) - Sqr(y) - Sqr(z));

  r := (q - w) * r / s;
  g := (q - w) * g / s;
  b := (q - w) * b / s;

  if DoInvert then
  begin
    r := 195 - r;
    g := 390 - g;
    b :=  65 - b;
  end;

  if r < 0 then r := 0 else if r > 195 then r := 195;
  if g < 0 then g := 0 else if g > 390 then g := 390;
  if b < 0 then b := 0 else if b >  65 then b :=  65;

  RR := Trunc(r * (1 / WeightR) + 0.5);
  GG := Trunc(g * (1 / WeightG) + 0.5);
  BB := Trunc(b * (1 / WeightB) + 0.5);

  if RR > $FF then RR := $FF else if RR < 0 then RR := 0;
  if GG > $FF then GG := $FF else if GG < 0 then GG := 0;
  if BB > $FF then BB := $FF else if BB < 0 then BB := 0;

  AColor := (BB and $FF) shl 16 or (GG and $FF) shl 8 or (RR and $FF);
end;

procedure SetContrast(var Color: TColor; BkgndColor: TColor; Threshold: Integer);
var
  t: Single;
begin
  if Color < 0 then Color := GetSysColor(Color and $FF);
  if BkgndColor < 0 then BkgndColor := GetSysColor(BkgndColor and $FF);
  t := Threshold;
  if not IsContrastEnough(Color, BkgndColor, True, t) then
    AdjustContrast(Integer(Color), BkgndColor, t);
end;


function GetBGR(C: TColorRef): Cardinal;
asm
        MOV     ECX,EAX         // this function swaps R and B bytes in ABGR
        SHR     EAX,16
        XCHG    AL,CL
        MOV     AH,$FF          // and writes $FF into A component
        SHL     EAX,16
        MOV     AX,CX
end;


{ Drawing routines }


{ Misc. routines }

procedure FillLongword(var Dst; Count: Integer; Value: Longword);
asm
{ eax = Dst;  edx = Count; ecx = Value }
    push edi
    mov edi,eax
    mov eax,ecx
    mov ecx,edx
    test ecx,ecx
    js @1
    rep stosd
@1: pop edi
end;

procedure MoveLongword(const Src; var Dst; Count: Integer);
asm
// eax = Src; edx = Dst; ecx = Count
    push esi
    push edi
    mov esi,eax
    mov edi,edx
    mov eax,ecx
    cmp edi,esi
    je @1
    rep movsd
@1: pop edi
    pop esi
end;

procedure BidiMoveLongword(const Src; var Dst; Count: Integer);
asm
// eax = Src; edx = Dst; ecx = Count
    push esi
    push edi
    mov esi,eax
    mov edi,edx
    mov eax,ecx
    cmp edi,esi
    ja @1
    je @2
    rep movsd
    jmp @2
@1: lea esi,[esi+ecx*4-4]
    lea edi,[edi+ecx*4-4]
    std
    rep movsd
    cld
@2: pop edi
    pop esi
end;


{ ImageList functions }

//----------------------------------------------------------------------------//

{ Gradients }

const
  GRADIENT_CACHE_SIZE = 16;

var
  GradientCache: array [0..GRADIENT_CACHE_SIZE] of array of TRGBQuad;
  NextCacheIndex: Integer = 0;

function FindGradient(Size: Integer; CL, CR: TRGBQuad): Integer;
begin
  Assert(Size > 0);
  Result := GRADIENT_CACHE_SIZE - 1;
  while Result >= 0 do
  begin
    if (Length(GradientCache[Result]) = Size) and
      (GradientCache[Result][0] = CL) and
      (GradientCache[Result][Length(GradientCache[Result]) - 1] = CR) then Exit;
    Dec(Result);
  end;
end;

function MakeGradient(Size: Integer; CL, CR: TRGBQuad): Integer;
var
  R1, G1, B1: Integer;
  R2, G2, B2: Integer;
  R, G, B: Integer;
  I: Integer;
  Bias: Integer;
begin
  Assert(Size > 0);
  Result := NextCacheIndex;
  Inc(NextCacheIndex);
  if NextCacheIndex >= GRADIENT_CACHE_SIZE then NextCacheIndex := 0;
  R1 := CL and $FF;
  G1 := CL shr 8 and $FF;
  B1 := CL shr 16 and $FF;
  R2 := Integer(CR and $FF) - R1;
  G2 := Integer(CR shr 8 and $FF) - G1;
  B2 := Integer(CR shr 16 and $FF) - B1;
  SetLength(GradientCache[Result], Size);
  Dec(Size);
  Bias := Size div 2;
  if Size > 0 then
    for I := 0 to Size do
    begin
      R := R1 + (R2 * I + Bias) div Size;
      G := G1 + (G2 * I + Bias) div Size;
      B := B1 + (B2 * I + Bias) div Size;
      GradientCache[Result][I] := R + G shl 8 + B shl 16;
    end
  else
  begin
    R := R1 + R2 div 2;
    G := G1 + G2 div 2;
    B := B1 + B2 div 2;
    GradientCache[Result][0] := R + G shl 8 + B shl 16;
  end;
end;

function GetGradient(Size: Integer; CL, CR: TRGBQuad): Integer;
begin
  Result := FindGradient(Size, CL, CR);
  if Result < 0 then Result := MakeGradient(Size, CL, CR);
end;

{ GradFill function }

procedure GradFill(DC: HDC; ARect: TRect; ClrTopLeft, ClrBottomRight: TColor; Kind: TGradientKind);
const
  GRAD_MODE: array [TGradientKind] of DWORD = (GRADIENT_FILL_RECT_H, GRADIENT_FILL_RECT_V);
  W: array [TGradientKind] of Integer = (2, 1);
  H: array [TGradientKind] of Integer = (1, 2);
type
  TriVertex = packed record
    X, Y: Longint;
    R, G, B, A: Word;
  end;
var
  V: array [0..1] of TriVertex;
  GR: GRADIENT_RECT;
  Size, I, Start, Finish: Integer;
  GradIndex: Integer;
  R, CR: TRect;
  Brush: HBRUSH;
begin
  if not RectVisible(DC, ARect) then Exit;
  
  ClrTopLeft := ColorToRGB(ClrTopLeft);
  ClrBottomRight := ColorToRGB(ClrBottomRight);
  if @GradientFill <> nil then
  begin
    { Use msimg32.dll }
    with V[0] do
    begin
      X := ARect.Left;
      Y := ARect.Top;
      R := ClrTopLeft shl 8 and $FF00;
      G := ClrTopLeft and $FF00;
      B := ClrTopLeft shr 8 and $FF00;
      A := 0;
    end;
    with V[1] do
    begin
      X := ARect.Right;
      Y := ARect.Bottom;
      R := ClrBottomRight shl 8 and $FF00;
      G := ClrBottomRight and $FF00;
      B := ClrBottomRight shr 8 and $FF00;
      A := 0;
    end;
    GR.UpperLeft := 0; GR.LowerRight := 1;
    GradientFill(DC, @V, 2, @GR, 1, GRAD_MODE[Kind]);
  end
  else
  begin
    { Have to do it manually if msimg32.dll is not available }
    GetClipBox(DC, CR);

    if Kind = gkHorz then
    begin
      Size := ARect.Right - ARect.Left;
      if Size <= 0 then Exit;
      Start := 0; Finish := Size - 1;
      if CR.Left > ARect.Left then Inc(Start, CR.Left - ARect.Left);
      if CR.Right < ARect.Right then Dec(Finish, ARect.Right - CR.Right);
      R := ARect; Inc(R.Left, Start); R.Right := R.Left + 1;
    end
    else
    begin
      Size := ARect.Bottom - ARect.Top;
      if Size <= 0 then Exit;
      Start := 0; Finish := Size - 1;
      if CR.Top > ARect.Top then Inc(Start, CR.Top - ARect.Top);
      if CR.Bottom < ARect.Bottom then Dec(Finish, ARect.Bottom - CR.Bottom);
      R := ARect; Inc(R.Top, Start); R.Bottom := R.Top + 1;
    end;
    GradIndex := GetGradient(Size, ClrTopLeft, ClrBottomRight);
    for I := Start to Finish do
    begin
      Brush := CreateSolidBrush(GradientCache[GradIndex][I]);
      Windows.FillRect(DC, R, Brush);
      OffsetRect(R, Integer(Kind = gkHorz), Integer(Kind = gkVert));
      DeleteObject(Brush);
    end;
  end;
end;

{ Brushed Fill } ///////////////////////////////////////////////////////////////

{ Templates }

const
  NUM_TEMPLATES = 8;
  MIN_TEMPLATE_SIZE = 100;
  MAX_TEMPLATE_SIZE = 200;

var
  ThreadTemplates: array [0..NUM_TEMPLATES - 1] of array of Integer;
  RandThreadIndex: array [0..1023] of Integer;
  RandThreadPositions: array [0..1023] of Integer;

procedure InitializeBrushedFill;
const
  Pi = 3.14159265358987;
var
  TemplateIndex, Size, I, V, V1, V2: Integer;
  T, R12, R13, R14, R21, R22, R23, R24: Single;
begin
  { Make thread templates }
  for TemplateIndex := 0 to NUM_TEMPLATES - 1 do
  begin
    Size := (MIN_TEMPLATE_SIZE + Random(MAX_TEMPLATE_SIZE - MIN_TEMPLATE_SIZE + 1)) div 2;
    SetLength(ThreadTemplates[TemplateIndex], Size * 2);
    R12 := Random * 2 * Pi;
    R13 := Random * 2 * Pi;
    R14 := Random * 2 * Pi;
    R21 := Random * 2 * Pi;
    R22 := Random * 2 * Pi;
    R23 := Random * 2 * Pi;
    R24 := Random * 2 * Pi;
    for I := 0 to Size - 1 do
    begin
      T := 2 * Pi * I / Size;
      V1 := Round(150 * Sin(T) + 100 * Sin(2 * T + R12) + 50 * Sin(3 * T + R13) + 20 * Sin(4 * T + R14));
      if V1 > 255 then V1 := 255;
      if V1 < -255 then V1 := -255;

      V2 := Round(150 * Sin(T + R21) + 100 * Sin(2 * T + R22) + 50 * Sin(3 * T + R23) + 20 * Sin(4 * T + R24));
      if V2 > 255 then V2 := 255;
      if V2 < -255 then V2 := -255;

      if Abs(V2 - V1) > 300 then
      begin
        V := (V1 + V2) div 2;
        V1 := V - 150;
        V2 := V + 150;
      end;

      ThreadTemplates[TemplateIndex][I * 2] := Min(V1, V2);
      ThreadTemplates[TemplateIndex][I * 2 + 1] := Max(V1, V2);
    end;
  end;

  { Initialize Rand arrays }
  for I := 0 to 1023 do
  begin
    RandThreadIndex[I] := Random(NUM_TEMPLATES);
    V1 := Random(Length(ThreadTemplates[RandThreadIndex[I]])) and not $1;
    if Odd(I) then Inc(V1);
    RandThreadPositions[I] := V1;
  end;
end;

{ Cache }

const
  THREAD_CACHE_SIZE = 16;

type
  TThreadCacheItem = record
    BaseColor: TColorRef;
    Roughness: Integer;
    Bitmaps: array [0..NUM_TEMPLATES - 1] of HBITMAP;
  end;

var
  ThreadCache: array [0..THREAD_CACHE_SIZE] of TThreadCacheItem;
  NextCacheEntry: Integer = 0;

procedure ClearCacheItem(var CacheItem: TThreadCacheItem);
var
  I: Integer;
begin
  for I := NUM_TEMPLATES - 1 downto 0 do
  begin
    CacheItem.BaseColor := $FFFFFFFF;
    CacheItem.Roughness := -1;
    if CacheItem.Bitmaps[I] <> 0 then DeleteObject(CacheItem.Bitmaps[I]);
    CacheItem.Bitmaps[I] := 0;
  end;
end;

procedure ResetBrushedFillCache;
var
  I: Integer;
begin
  { Should be called each time the screen parameters change }
  for I := THREAD_CACHE_SIZE - 1 downto 0 do ClearCacheItem(ThreadCache[I]);
end;

procedure FinalizeBrushedFill;
begin
  ResetBrushedFillCache;
end;

procedure MakeCacheItem(var CacheItem: TThreadCacheItem; Color: TColorRef; Roughness: Integer);
var
  TemplateIndex, Size, I, V: Integer;
  CR, CG, CB: Integer;
  R, G, B: Integer;
  ScreenDC: HDC;
  BMI: TBitmapInfo;
  Bits: PRGBQuadArray;
  DIBSection: HBITMAP;
  DIBDC, CacheDC: HDC;
begin
  ScreenDC := GetDC(0);
  FillChar(BMI, SizeOf(TBitmapInfo), 0);
  with BMI.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biPlanes := 1;
    biCompression := BI_RGB;
    biWidth := MAX_TEMPLATE_SIZE;
    biHeight := -1;
    biBitCount := 32;
  end;
  DIBSection := CreateDIBSection(0, BMI, DIB_RGB_COLORS, Pointer(Bits), 0, 0);
  DIBDC := CreateCompatibleDC(0);
  SelectObject(DIBDC, DIBSection);
  CacheDC := CreateCompatibleDC(0);

  CR := Color shl 8 and $FF00;
  CG := Color and $FF00;
  CB := Color shr 8 and $FF00;

  try
  for TemplateIndex := 0 to NUM_TEMPLATES - 1 do
  begin
    CacheItem.BaseColor := Color;
    CacheItem.Roughness := Roughness;
    Size := Length(ThreadTemplates[TemplateIndex]);

      if CacheItem.Bitmaps[TemplateIndex] = 0 then
        CacheItem.Bitmaps[TemplateIndex] := CreateCompatibleBitmap(ScreenDC, Size, 1);
      SelectObject(CacheDC, CacheItem.Bitmaps[TemplateIndex]);

    for I := 0 to Size - 1 do
    begin
      V := ThreadTemplates[TemplateIndex][I];
      R := CR + V * Roughness;
      G := CG + V * Roughness;
      B := CB + V * Roughness;
      if R < 0 then R := 0;
      if G < 0 then G := 0;
      if B < 0 then B := 0;
      if R > $EF00 then R := $EF00;
      if G > $EF00 then G := $EF00;
      if B > $EF00 then B := $EF00;
      Bits^[I] := (R and $FF00 + (G and $FF00) shl 8 + (B and $FF00) shl 16) shr 8;
    end;

      BitBlt(CacheDC, 0, 0, Size, 1, DIBDC, 0, 0, SRCCOPY);
    end;

  finally
    DeleteDC(CacheDC);
    DeleteDC(DIBDC);
    DeleteObject(DIBSection);
    ReleaseDC(0, ScreenDC);
  end;
end;

function FindCacheItem(Color: TColorRef; Roughness: Integer): Integer;
begin
  Result := THREAD_CACHE_SIZE - 1;
  while Result >= 0 do
    if (ThreadCache[Result].BaseColor = Color) and (ThreadCache[Result].Roughness = Roughness) then Exit
    else Dec(Result);
end;

function GetCacheItem(Color: TColorRef; Roughness: Integer): Integer;
begin
  Result := FindCacheItem(Color, Roughness);
  if Result >= 0 then Exit
  else
  begin
    Result := NextCacheEntry;
    MakeCacheItem(ThreadCache[Result], Color, Roughness);
    NextCacheEntry := (NextCacheEntry + 1) mod THREAD_CACHE_SIZE;
  end;
end;

procedure BrushedFill(DC: HDC; Origin: PPoint; ARect: TRect; Color: TColor; Roughness: Integer);
const
  ZeroOrigin: TPoint = (X: 0; Y: 0);
var
  CR: TColorRef;
  X, Y: Integer;
  CacheIndex: Integer;
  TemplateIndex: Integer;
  CacheDC: HDC;
  Size: Integer;
  BoxR: TRect;
begin
  if (Color = clNone) or not RectVisible(DC, ARect) then Exit;
  CR := GetBGR(ColorToRGB(Color));
  if Origin = nil then Origin := @ZeroOrigin;
  CacheIndex := GetCacheItem(CR, Roughness);
  GetClipBox(DC, BoxR);
  IntersectRect(ARect, ARect, BoxR);
  SaveDC(DC);
  with ARect do IntersectClipRect(DC, Left, Top, Right, Bottom);

  CacheDC := CreateCompatibleDC(0);
  for Y := ARect.Top to ARect.Bottom - 1 do
  begin
    TemplateIndex := RandThreadIndex[(65536 + Y - Origin.Y) mod 1024];
    Size := Length(ThreadTemplates[TemplateIndex]);
    X := -RandThreadPositions[(65536 + Y - Origin.Y) mod 1024] + Origin.X;
    SelectObject(CacheDC, ThreadCache[CacheIndex].Bitmaps[TemplateIndex]);
    while X < ARect.Right do
    begin
      if X + Size >= ARect.Left then BitBlt(DC, X, Y, Size, 1, CacheDC, 0, 0, SRCCOPY);
      Inc(X, Size);
    end;
  end;
  DeleteDC(CacheDC);

  RestoreDC(DC, -1);
end;

var
  hUser, hMSImg: HModule;

initialization

hUser := LoadLibrary('user32.dll');
hMSImg := LoadLibrary('msimg32.dll');
@UpdateLayeredWindow := GetProcAddress(hUser, 'UpdateLayeredWindow');
@AlphaBlend := GetProcAddress(hMSImg, 'AlphaBlend');
@GradientFill := GetProcAddress(hMSImg, 'GradientFill');

InitializeBrushedFill;
ResetBrushedFillCache;

finalization

FinalizeBrushedFill;

FreeLibrary(hMSImg);
FreeLibrary(hUser);

end.
