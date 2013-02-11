(*
  Letterpress

  Copyright 2009-2010, Garnet
*)

{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is SynUnicode.pas by Maël Hörz, released 2004-05-30.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynUnicode.pas,v 1.1.2.43 2008/10/03 18:50:12 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net
-------------------------------------------------------------------------------}

unit SynUnicode;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, Controls, Forms, Graphics, Clipbrd, Types, Classes,
  SysUtils;

const
  SLineBreak = #13#10;
  SAnsiLineBreak: UTF8String = #13#10;
  UTF8BOM: array[0..2] of Byte = ($EF, $BB, $BF);
  UTF16BOMLE: array[0..1] of Byte = ($FF, $FE);
  UTF16BOMBE: array[0..1] of Byte = ($FE, $FF);
  UTF32BOMLE: array[0..3] of Byte = ($FF, $FE, $00, $00);
  UTF32BOMBE: array[0..3] of Byte = ($00, $00, $FE, $FF);

const
  WideNull = #0;
  WideTabulator = #9;
  WideSpace = #32;

  WideLF = #10;
  WideLineFeed = #10;
  WideVerticalTab = #11;
  WideFormFeed = #12;
  WideCR = #13;
  WideCarriageReturn = #13;
  WideCRLF = #13#10;
  WideLineSeparator = #$2028;
  WideParagraphSeparator = #$2029;
  WideNbsp = #$00A0;

  BOM_LSB_FIRST = WideChar($FEFF);
  BOM_MSB_FIRST = WideChar($FFFE);

type
  TSaveFormat = (sfUTF16LSB, sfUTF16MSB, sfUTF8, sfAnsi);

const
  sfUnicodeLSB = sfUTF16LSB;
  sfUnicodeMSB = sfUTF16MSB;

type
  TFontCharSet = 0..255;

{ Functions from JCLUnicode.pas }
function CharSetFromLocale(Language: LCID): TFontCharSet;
function CodePageFromLocale(Language: LCID): Integer;
function KeyboardCodePage: Word;
function KeyUnicode(C: AnsiChar): WideChar;
function StringToUnicodeStringEx(const S: AnsiString; CodePage: Word): UnicodeString;
function UnicodeStringToStringEx(const WS: UnicodeString; CodePage: Word): AnsiString;

{ functions providing same behavior on Win9x and WinNT based systems}
function GetTextSize(DC: HDC; Str: PWideChar; Count: Integer): TSize;

{ Unicode versions of TCanvas-methods }
function TextExtent(ACanvas: TCanvas; const Text: UnicodeString): TSize;
function TextWidth(ACanvas: TCanvas; const Text: UnicodeString): Integer;
function TextHeight(ACanvas: TCanvas; const Text: UnicodeString): Integer;
procedure TextOut(ACanvas: TCanvas; X, Y: Integer; const Text: UnicodeString);
procedure TextRect(ACanvas: TCanvas; Rect: TRect; X, Y: Integer;
  const Text: UnicodeString);

{ Unicode streaming-support }
type
  TSynEncoding = (seUTF8, seUTF16LE, seUTF16BE, seAnsi);
  TSynEncodings = set of TSynEncoding;

function IsAnsiOnly(const S: UnicodeString): Boolean;
function IsUTF16LE(Stream: TStream): Boolean;
function IsUTF16BE(Stream: TStream): Boolean;
function IsUTF8(Stream: TStream; out WithBOM: Boolean): Boolean; overload;
function IsUTF8(const FileName: UnicodeString; out WithBOM: Boolean): Boolean; overload;

function UnicodeStringToUTF8(const S: UnicodeString): UTF8String;

function ClipboardProvidesText: Boolean;
function GetClipboardText: UnicodeString;
procedure SetClipboardText(const Text: UnicodeString);

function IsWideCharMappableToAnsi(const C: Char): Boolean;
function IsUnicodeStringMappableToAnsi(const S: UnicodeString): Boolean;

var
  Win32PlatformIsUnicode: Boolean;

implementation

uses
  SynEditTextBuffer,
    {$IFDEF SYN_UNISCRIBE}
    SynUsp10,
    {$ENDIF}
  Math;

function WStrLen(const Str: PWideChar): Cardinal;
asm
        MOV     EDX,EDI
        MOV     EDI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        MOV     EAX,0FFFFFFFEH
        SUB     EAX,ECX
        MOV     EDI,EDX
end;

function WStrEnd(const Str: PWideChar): PWideChar;
asm
        MOV     EDX,EDI
        MOV     EDI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        LEA     EAX,[EDI-2]
        MOV     EDI,EDX
end;

function WStrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        NOT     ECX
        MOV     EDI,ESI
        MOV     ESI,EDX
        MOV     EDX,ECX
        MOV     EAX,EDI
        SHR     ECX,1
        REP     MOVSD
        MOV     ECX,EDX
        AND     ECX,1
        REP     MOVSW
        POP     ESI
        POP     EDI
end;

function WStrLCopy(Dest: PWideChar; const Source: PWideChar; MaxLen: Cardinal): PWideChar;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EBX,ECX
        XOR     AX,AX
        TEST    ECX,ECX
        JZ      @@1
        REPNE   SCASW
        JNE     @@1
        INC     ECX
@@1:    SUB     EBX,ECX
        MOV     EDI,ESI
        MOV     ESI,EDX
        MOV     EDX,EDI
        MOV     ECX,EBX
        SHR     ECX,1
        REP     MOVSD
        MOV     ECX,EBX
        AND     ECX,1
        REP     MOVSW
        STOSW
        MOV     EAX,EDX
        POP     EBX
        POP     ESI
        POP     EDI
end;

function WStrCat(Dest: PWideChar; const Source: PWideChar): PWideChar;
begin
  WStrCopy(WStrEnd(Dest), Source);
  Result := Dest;
end;

function WStrScan(const Str: PWideChar; Chr: WideChar): PWideChar;
begin
  Result := Str;
  while Result^ <> Chr do
  begin
    if Result^ = #0 then
    begin
      Result := nil;
      Exit;
    end;
    Inc(Result);
  end;
end;

const
  // data used to bring UTF-16 coded strings into correct UTF-32 order for correct comparation
  UTF16Fixup: array[0..31] of Word = (
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    $2000, $F800, $F800, $F800, $F800
  );

// Binary comparation of Str1 and Str2 with surrogate fix-up.
// Returns < 0 if Str1 is smaller in binary order than Str2, = 0 if both strings are
// equal and > 0 if Str1 is larger than Str2.
//
// This code is based on an idea of Markus W. Scherer (IBM).
// Note: The surrogate fix-up is necessary because some single value code points have
//       larger values than surrogates which are in UTF-32 actually larger.
function WStrComp(Str1, Str2: PWideChar): Integer;
var
  C1, C2: Word;
  Run1, Run2: PWideChar;
begin
  Run1 := Str1;
  Run2 := Str2;
  repeat
    C1 := Word(Run1^);
    C1 := Word(C1 + UTF16Fixup[C1 shr 11]);
    C2 := Word(Run2^);
    C2 := Word(C2 + UTF16Fixup[C2 shr 11]);

    // now C1 and C2 are in UTF-32-compatible order
    Result := Integer(C1) - Integer(C2);
    if(Result <> 0) or (C1 = 0) or (C2 = 0) then
      Break;
    Inc(Run1);
    Inc(Run2);
  until
    False;

  // If the strings have different lengths but the comparation returned equity so far
  // then adjust the result so that the longer string is marked as the larger one.
  if Result = 0 then
    Result := (Run1 - Str1) - (Run2 - Str2);
end;

// compares strings up to MaxLen code points
// see also StrCompW
function WStrLComp(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
var
  C1, C2: Word;
begin
  if MaxLen > 0 then
  begin
    repeat
      C1 := Word(Str1^);
      C1 := Word(C1 + UTF16Fixup[C1 shr 11]);
      C2 := Word(Str2^);
      C2 := Word(C2 + UTF16Fixup[C2 shr 11]);

      // now C1 and C2 are in UTF-32-compatible order
      { TODO : surrogates take up 2 words and are counted twice here, count them only once }
      Result := Integer(C1) - Integer(C2);
      Dec(MaxLen);
      if(Result <> 0) or (C1 = 0) or (C2 = 0) or (MaxLen = 0) then
        Break;
      Inc(Str1);
      Inc(Str2);
    until False;
  end
  else
    Result := 0;
end;

// exchanges in each character of the given string the low order and high order
// byte to go from LSB to MSB and vice versa.
// EAX contains address of string
procedure StrSwapByteOrder(Str: PWideChar);
asm
       PUSH    ESI
       PUSH    EDI
       MOV     ESI, EAX
       MOV     EDI, ESI
       XOR     EAX, EAX // clear high order byte to be able to use 32bit operand below
@@1:
       LODSW
       OR      EAX, EAX
       JZ      @@2
       XCHG    AL, AH
       STOSW
       JMP     @@1


@@2:
       POP     EDI
       POP     ESI
end;

function TranslateCharsetInfoEx(lpSrc: PDWORD; var lpCs: TCharsetInfo; dwFlags: DWORD): BOOL; stdcall;
  external 'gdi32.dll' name 'TranslateCharsetInfo';

// determines the code page for a given locale
function CodePageFromLocale(Language: LCID): Integer;
var
  Buf: array[0..6] of Char;
begin
  GetLocaleInfo(Language, LOCALE_IDefaultAnsiCodePage, Buf, 6);
  Result := StrToIntDef(Buf, GetACP);
end;

function CharSetFromLocale(Language: LCID): TFontCharSet;
var
  CP: Cardinal;
  CSI: TCharsetInfo;
begin
  CP:= CodePageFromLocale(Language);
  TranslateCharsetInfoEx(Pointer(CP), CSI, TCI_SRCCODEPAGE);
  Result:= CSI.ciCharset;
end;

function KeyboardCodePage: Word;
begin
  Result := CodePageFromLocale(GetKeyboardLayout(0) and $FFFF);
end;

// converts the given character (as it comes with a WM_CHAR message) into its
// corresponding Unicode character depending on the active keyboard layout
function KeyUnicode(C: AnsiChar): WideChar;
begin
  MultiByteToWideChar(KeyboardCodePage, MB_USEGLYPHCHARS, @C, 1, @Result, 1);
end;

function StringToUnicodeStringEx(const S: AnsiString; CodePage: Word): UnicodeString;
var
  InputLength,
  OutputLength: Integer;
begin
  InputLength := Length(S);
  OutputLength := MultiByteToWideChar(CodePage, 0, PAnsiChar(S), InputLength,
    nil, 0);
  SetLength(Result, OutputLength);
  MultiByteToWideChar(CodePage, 0, PAnsiChar(S), InputLength, PWideChar(Result),
    OutputLength);
end;

function UnicodeStringToStringEx(const WS: UnicodeString; CodePage: Word): AnsiString;
var
  InputLength,
  OutputLength: Integer;
begin
  InputLength := Length(WS);
  OutputLength := WideCharToMultiByte(CodePage, 0, PWideChar(WS), InputLength,
    nil, 0, nil, nil);
  SetLength(Result, OutputLength);
  WideCharToMultiByte(CodePage, 0, PWideChar(WS), InputLength, PAnsiChar(Result),
    OutputLength, nil, nil);
end;

function GetTextSize(DC: HDC; Str: PWideChar; Count: Integer): TSize;
{$IFDEF SYN_UNISCRIBE}
const
  SSAnalyseFlags = SSA_GLYPHS or SSA_FALLBACK or SSA_LINK;
{$ENDIF}
var
  tm: TTextMetricA;
  {$IFDEF SYN_UNISCRIBE}
  GlyphBufferSize: Integer;
  saa: TScriptStringAnalysis;
  lpSize: PSize;
  {$ENDIF}
begin
  Result.cx := 0;
  Result.cy := 0;

{$IFDEF SYN_UNISCRIBE}
  if Usp10IsInstalled then
  begin
    if Count <= 0 then Exit;

    // According to the MS Windows SDK (1.5 * Count + 16) is the recommended
    // value for GlyphBufferSize (see documentation of cGlyphs parameter of
    // ScriptStringAnalyse function)
    GlyphBufferSize := (3 * Count) div 2 + 16;
    
    if Succeeded(ScriptStringAnalyse(DC, Str, Count, GlyphBufferSize, -1,
      SSAnalyseFlags, 0, nil, nil, nil, nil, nil, @saa)) then
    begin
      lpSize := ScriptString_pSize(saa);
      if lpSize <> nil then
      begin
        Result := lpSize^;
        if Result.cx = 0 then
        begin
          GetTextMetricsA(DC, tm);
          Result.cx := tm.tmAveCharWidth;
        end;
      end;
      ScriptStringFree(@saa);
    end;
  end
  else
{$ENDIF}
  begin
    GetTextExtentPoint32W(DC, Str, Count, Result);
    if not Win32PlatformIsUnicode then
    begin
      GetTextMetricsA(DC, tm);
      if tm.tmPitchAndFamily and TMPF_TRUETYPE <> 0 then
        Result.cx := Result.cx - tm.tmOverhang
      else
        Result.cx := tm.tmAveCharWidth * Count;
    end;
  end;
end;

type
  TAccessCanvas = class(TCanvas)
  end;

function TextExtent(ACanvas: TCanvas; const Text: UnicodeString): TSize;
begin
  with TAccessCanvas(ACanvas) do
  begin
    RequiredState([csHandleValid, csFontValid]);
    Result := GetTextSize(Handle, PWideChar(Text), Length(Text));
  end;
end;

function TextWidth(ACanvas: TCanvas; const Text: UnicodeString): Integer;
begin
  Result := TextExtent(ACanvas, Text).cX;
end;

function TextHeight(ACanvas: TCanvas; const Text: UnicodeString): Integer;
begin
  Result := TextExtent(ACanvas, Text).cY;
end;

procedure TextOut(ACanvas: TCanvas; X, Y: Integer; const Text: UnicodeString);
begin
  with TAccessCanvas(ACanvas) do
  begin
    Changing;
    RequiredState([csHandleValid, csFontValid, csBrushValid]);
    if CanvasOrientation = coRightToLeft then
      Inc(X, SynUnicode.TextWidth(ACanvas, Text) + 1);
    Windows.ExtTextOutW(Handle, X, Y, TextFlags, nil, PWideChar(Text),
     Length(Text), nil);
    MoveTo(X + SynUnicode.TextWidth(ACanvas, Text), Y);
    Changed;
  end;
end;

procedure TextRect(ACanvas: TCanvas; Rect: TRect; X, Y: Integer;
  const Text: UnicodeString);
var
  Options: Longint;
begin
  with TAccessCanvas(ACanvas) do
  begin
    Changing;
    RequiredState([csHandleValid, csFontValid, csBrushValid]);
    Options := ETO_CLIPPED or TextFlags;
    if Brush.Style <> bsClear then
      Options := Options or ETO_OPAQUE;
    if ((TextFlags and ETO_RTLREADING) <> 0) and
       (CanvasOrientation = coRightToLeft)
    then
      Inc(X, SynUnicode.TextWidth(ACanvas, Text) + 1);
    Windows.ExtTextOutW(Handle, X, Y, Options, @Rect, PWideChar(Text),
      Length(Text), nil);
    Changed;
  end;
end;

function IsAnsiOnly(const S: UnicodeString): Boolean;
begin
  Result := IsUnicodeStringMappableToAnsi(S);
end;

function IsUTF16LE(Stream: TStream): Boolean;
var
  Buff: array of Byte;
begin
  { Initialize }
  Result := Stream.Size >= Length(UTF16BOMLE);
  if not Result then Exit;

  { Fill buffer }
  SetLength(Buff, Length(UTF16BOMLE));
  with Stream do
  begin
    ReadBuffer(Buff[0], Length(UTF16BOMLE));
    Seek(-Length(UTF16BOMLE), soFromCurrent);
  end;

  { Check }
  Result := CompareMem(@Buff[0], @UTF16BOMLE[0], Length(UTF16BOMLE));
end;

function IsUTF16BE(Stream: TStream): Boolean;
var
  Buff: array of Byte;
begin
  { Initialize }
  Result := Stream.Size >= Length(UTF16BOMBE);
  if not Result then Exit;

  { Fill buffer }
  SetLength(Buff, Length(UTF16BOMBE));
  with Stream do
  begin
    ReadBuffer(Buff[0], Length(UTF16BOMBE));
    Seek(-Length(UTF16BOMLE), soFromCurrent);
  end;

  { Check }
  Result := CompareMem(@Buff[0], @UTF16BOMBE[0], Length(UTF16BOMBE));
end;

function IsUTF8(const FileName: UnicodeString; out WithBOM: Boolean): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := IsUTF8(Stream, WithBOM);
  finally
    Stream.Free;
  end;
end;

{ Checks for a BOM in UTF-8 format or searches the first 4096 bytes for
  typical UTF-8 octet sequences }
function IsUTF8(Stream: TStream; out WithBOM: Boolean): Boolean;
const
  MinimumCountOfUTF8Strings = 1;
  MaxBufferSize = $4000;
var
  Buffer: array of Byte;
  BufferSize, I, FoundUTF8Strings: Integer;

  { 3 trailing bytes are the maximum in valid UTF-8 streams,
    so a count of 4 trailing bytes is enough to detect invalid UTF-8 streams }
  function CountOfTrailingBytes: Integer;
  begin
    Result := 0;
    inc(I);
    while (I < BufferSize) and (Result < 4) do
    begin
      if Buffer[I] in [$80..$BF] then
        inc(Result)
      else
        Break;
      inc(I);
    end;
  end;

begin
  { Start analysis at actual Stream.Position }
  BufferSize := Min(MaxBufferSize, Stream.Size - Stream.Position);

  { if no special characteristics are found it is not UTF-8 }
  Result := False;
  WithBOM := False;

  { Empty buffer check }
  if BufferSize > 0 then
  begin
    SetLength(Buffer, BufferSize);
    Stream.ReadBuffer(Buffer[0], BufferSize);
    Stream.Seek(-BufferSize, soFromCurrent);

    { First search for BOM }
    if (BufferSize >= Length(UTF8BOM)) and
      CompareMem(@Buffer[0], @UTF8BOM[0], Length(UTF8BOM)) then
    begin
      WithBOM := True;
      Result := True;
      Exit;
    end;

    { If no BOM was found, check for leading/trailing byte sequences,
      which are uncommon in usual non UTF-8 encoded text.

      Note: There is no 100% save way to detect UTF-8 streams. The bigger
            MinimumCountOfUTF8Strings, the lower is the probability of
            a false positive. On the other hand, a big MinimumCountOfUTF8Strings
            makes it unlikely to detect files with only little usage of non
            US-ASCII chars, like usual in European languages }
    FoundUTF8Strings := 0;
    I := 0;
    while I < BufferSize do
    begin
      case Buffer[I] of
        $00..$7F:;// Skip US-ASCII characters as they could belong to various charsets
        $C2..$DF:
          if CountOfTrailingBytes = 1 then
            Inc(FoundUTF8Strings)
          else
            Break;
        $E0:
          begin
            inc(I);
            if (I < BufferSize) and (Buffer[I] in [$A0..$BF]) and (CountOfTrailingBytes = 1) then
              Inc(FoundUTF8Strings)
            else
              Break;
          end;
        $E1..$EC, $EE..$EF:
          if CountOfTrailingBytes = 2 then
            Inc(FoundUTF8Strings)
          else
            Break;
        $ED:
          begin
            inc(I);
            if (I < BufferSize) and (Buffer[I] in [$80..$9F]) and (CountOfTrailingBytes = 1) then
              Inc(FoundUTF8Strings)
            else
              Break;
          end;
        $F0:
          begin
            Inc(I);
            if (I < BufferSize) and (Buffer[I] in [$90..$BF]) and (CountOfTrailingBytes = 2) then
              Inc(FoundUTF8Strings)
            else
              Break;
          end;
        $F1..$F3:
          if CountOfTrailingBytes = 3 then
            Inc(FoundUTF8Strings)
          else
            Break;
        $F4:
          begin
            Inc(i);
            if (I < BufferSize) and (Buffer[I] in [$80..$8F]) and (CountOfTrailingBytes = 2) then
              Inc(FoundUTF8Strings)
            else
              Break;
          end;
        $C0, $C1, $F5..$FF: // Invalid UTF-8 bytes
          Break;
        $80..$BF: // Trailing bytes are consumed when handling leading bytes,
                  // any occurence of "orphaned" trailing bytes is invalid UTF-8
          Break;
      end;

      if FoundUTF8Strings = MinimumCountOfUTF8Strings then
      begin
        Result := True;
        Break;
      end;

      Inc(I);
    end;
  end;
end;

function UnicodeStringToUTF8(const S: UnicodeString): UTF8String;
begin
  SetLength(Result, Length(S) * 3);
  SetLength(Result, UnicodeToUtf8(PAnsiChar(Result), Length(Result),
    PChar(S), Length(S){DELPHI2010 - 1}));
end;

function ClipboardProvidesText: Boolean;
begin
  Result := IsClipboardFormatAvailable(CF_TEXT) or
    IsClipboardFormatAvailable(CF_UNICODETEXT);
end;

function GetClipboardText: UnicodeString;
var
  Mem: HGLOBAL;
  LocaleID: LCID;
  P: PByte;
begin
  Result := '';
  Clipboard.Open;
  try
    if Clipboard.HasFormat(CF_UNICODETEXT) then
    begin
      Mem := Clipboard.GetAsHandle(CF_UNICODETEXT);
        try
          if Mem <> 0 then
            Result := PWideChar(GlobalLock(Mem));
        finally
          if Mem <> 0 then GlobalUnlock(Mem);
        end;
    end
    else begin
      LocaleID := 0;
      Mem := Clipboard.GetAsHandle(CF_LOCALE);
      try
        if Mem <> 0 then LocaleID := PInteger(GlobalLock(Mem))^;
      finally
        if Mem <> 0 then GlobalUnlock(Mem);
      end;

      Mem := Clipboard.GetAsHandle(CF_TEXT);
      try
        if Mem <> 0 then
        begin
          P := GlobalLock(Mem);
          Result := StringToUnicodeStringEx(PAnsiChar(P), CodePageFromLocale(LocaleID));
        end
      finally
        if Mem <> 0 then GlobalUnlock(Mem);
      end;
    end;
  finally
    Clipboard.Close;
  end;
end;

procedure SetClipboardText(const Text: UnicodeString);
var
  Mem: HGLOBAL;
  P: PByte;
  SLen: Integer;
begin
  if Length(Text) = 0 then
    Exit;
  SLen := Length(Text);
  Clipboard.Open;
  try
    Clipboard.Clear;

    { Set unicode text, this also works on Win9X, even if the clipboard-viewer
     can't show it, Word 2000+ can paste it including the unicode only characters }
    Mem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE,
      (SLen + 1) * sizeof(WideChar));
    if Mem <> 0 then
    begin
      P := GlobalLock(Mem);
      try
        if P <> nil then
        begin
          Move(PWideChar(Text)^, P^, (SLen + 1) * sizeof(WideChar));
          Clipboard.SetAsHandle(CF_UNICODETEXT, Mem);
        end;
      finally
        GlobalUnlock(Mem);
      end;
    end;
    { Don't free Mem!  It belongs to the clipboard now, and it will free it
      when it is done with it }
  finally
    Clipboard.Close;
  end;
end;

function IsWideCharMappableToAnsi(const C: Char): Boolean;
begin
  Result := IsUnicodeStringMappableToAnsi(C);
end;

function IsUnicodeStringMappableToAnsi(const S: UnicodeString): Boolean;
var
  UsedDefaultChar: BOOL;
begin
  WideCharToMultiByte(DefaultSystemCodePage, 0, PWideChar(S), Length(S), nil, 0,
    nil, @UsedDefaultChar);
  Result := not UsedDefaultChar;
end;

initialization
  Win32PlatformIsUnicode := (Win32Platform = VER_PLATFORM_WIN32_NT);

end.
