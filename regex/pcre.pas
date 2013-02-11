unit pcre;

{ .Net-style Regular Expression Library for use with Delphi 2009 or later.
  Copyright (C) 2009 by Erik van Bilsen.
  Email: erik@bilsen.com
  Website: www.bilsen.com/regularexpressions

License in plain English:

1. I don't promise that this software works. (But if you find any bugs,
   please let me know!)
2. You can use this software for whatever you want. You don't have to pay me.
3. You may not pretend that you wrote this software. If you use it in a program,
   you must acknowledge somewhere in your documentation that you've used this
   code.

In legalese:

The author makes NO WARRANTY or representation, either express or implied,
with respect to this software, its quality, accuracy, merchantability, or
fitness for a particular purpose.  This software is provided "AS IS", and you,
its user, assume the entire risk as to its quality and accuracy.

Permission is hereby granted to use, copy, modify, and distribute this
software (or portions thereof) for any purpose, without fee, subject to these
conditions:
(1) If any part of the source code for this software is distributed, then the
License.txt file must be included, with this copyright and no-warranty notice
unaltered; and any additions, deletions, or changes to the original files
must be clearly indicated in accompanying documentation.
(2) If only executable code is distributed, then the accompanying
documentation must state that "this software is based in part on the Regular
Expression Library by Erik van Bilsen".
(3) Permission for use of this software is granted only if the user accepts
full responsibility for any undesirable consequences; the author accepts
NO LIABILITY for damages of any kind. }

interface

{ Imports for the PCRE object files.
  To build the object files yourself, see the Readme.txt file in the
  "C++ Builder Projects" directory. }

type
  PPCRE = Pointer;
  PPCREExtra = Pointer;
  PPPAnsiChar = ^PPAnsiChar;

const { Options }
  PCRE_CASELESS          = $00000001;
  PCRE_MULTILINE         = $00000002;
  PCRE_DOTALL            = $00000004;
  PCRE_EXTENDED          = $00000008;
  PCRE_ANCHORED          = $00000010;
  PCRE_DOLLAR_ENDONLY    = $00000020;
  PCRE_EXTRA             = $00000040;
  PCRE_NOTBOL            = $00000080;
  PCRE_NOTEOL            = $00000100;
  PCRE_UNGREEDY          = $00000200;
  PCRE_NOTEMPTY          = $00000400;
  PCRE_UTF8              = $00000800;
  PCRE_NO_AUTO_CAPTURE   = $00001000;
  PCRE_NO_UTF8_CHECK     = $00002000;
  PCRE_AUTO_CALLOUT      = $00004000;
  PCRE_PARTIAL           = $00008000;
  PCRE_DFA_SHORTEST      = $00010000;
  PCRE_DFA_RESTART       = $00020000;
  PCRE_FIRSTLINE         = $00040000;
  PCRE_DUPNAMES          = $00080000;
  PCRE_NEWLINE_CR        = $00100000;
  PCRE_NEWLINE_LF        = $00200000;
  PCRE_NEWLINE_CRLF      = $00300000;
  PCRE_NEWLINE_ANY       = $00400000;
  PCRE_NEWLINE_ANYCRLF   = $00500000;
  PCRE_BSR_ANYCRLF       = $00800000;
  PCRE_BSR_UNICODE       = $01000000;
  PCRE_JAVASCRIPT_COMPAT = $02000000;
  PCRE_NO_START_OPTIMIZE = $04000000;
  PCRE_NO_START_OPTIMISE = $04000000;

const { Error codes }
  PCRE_ERROR_NOMATCH        = -1;
  PCRE_ERROR_NULL           = -2;
  PCRE_ERROR_BADOPTION      = -3;
  PCRE_ERROR_BADMAGIC       = -4;
  PCRE_ERROR_UNKNOWN_NODE   = -5;
  PCRE_ERROR_NOMEMORY       = -6;
  PCRE_ERROR_NOSUBSTRING    = -7;
  PCRE_ERROR_MATCHLIMIT     = -8;
  PCRE_ERROR_CALLOUT        = -9;
  PCRE_ERROR_BADUTF8        = -10;
  PCRE_ERROR_BADUTF8_OFFSET = -11;
  PCRE_ERROR_PARTIAL        = -12;
  PCRE_ERROR_BADPARTIAL     = -13;
  PCRE_ERROR_INTERNAL       = -14;
  PCRE_ERROR_BADCOUNT       = -15;
  PCRE_ERROR_DFA_UITEM      = -16;
  PCRE_ERROR_DFA_UCOND      = -17;
  PCRE_ERROR_DFA_UMLIMIT    = -18;
  PCRE_ERROR_DFA_WSSIZE     = -19;
  PCRE_ERROR_DFA_RECURSE    = -20;
  PCRE_ERROR_RECURSIONLIMIT = -21;
  PCRE_ERROR_NULLWSLIMIT    = -22;
  PCRE_ERROR_BADNEWLINE     = -23;

const { Flags for pcre_fullinfo() }
  PCRE_INFO_OPTIONS        = 0;
  PCRE_INFO_SIZE           = 1;
  PCRE_INFO_CAPTURECOUNT   = 2;
  PCRE_INFO_BACKREFMAX     = 3;
  PCRE_INFO_FIRSTCHAR      = 4;
  PCRE_INFO_FIRSTTABLE     = 5;
  PCRE_INFO_LASTLITERAL    = 6;
  PCRE_INFO_NAMEENTRYSIZE  = 7;
  PCRE_INFO_NAMECOUNT      = 8;
  PCRE_INFO_NAMETABLE      = 9;
  PCRE_INFO_STUDYSIZE      = 10;
  PCRE_INFO_DEFAULT_TABLES = 11;
  PCRE_INFO_OKPARTIAL      = 12;
  PCRE_INFO_JCHANGED       = 13;
  PCRE_INFO_HASCRORLF      = 14;

const { Flags for pcre_config }
  PCRE_CONFIG_UTF8                   = 0;
  PCRE_CONFIG_NEWLINE                = 1;
  PCRE_CONFIG_LINK_SIZE              = 2;
  PCRE_CONFIG_POSIX_MALLOC_THRESHOLD = 3;
  PCRE_CONFIG_MATCH_LIMIT            = 4;
  PCRE_CONFIG_STACKRECURSE           = 5;
  PCRE_CONFIG_UNICODE_PROPERTIES     = 6;
  PCRE_CONFIG_MATCH_LIMIT_RECURSION  = 7;
  PCRE_CONFIG_BSR                    = 8;

const { Flags for the pcre_extra structure }
  PCRE_EXTRA_STUDY_DATA            = $0001;
  PCRE_EXTRA_MATCH_LIMIT           = $0002;
  PCRE_EXTRA_CALLOUT_DATA          = $0004;
  PCRE_EXTRA_TABLES                = $0008;
  PCRE_EXTRA_MATCH_LIMIT_RECURSION = $0010;

function pcre_compile(const Pattern: PAnsiChar; Options: Integer;
  out ErrPtr: PAnsiChar; out ErrOffset: Integer;
  const TablePtr: PAnsiChar): PPCRE; external;

function pcre_exec(const Code: PPCRE; const Extra: PPCREExtra;
  const Subject: PAnsiChar; Length, StartOffset, Options: Integer;
  Ovector: PInteger; OvecSize: Integer): Integer; external;

function pcre_fullinfo(const Code: PPCRE; const Extra: PPCREExtra;
  What: Integer; Where: Pointer): Integer; external;

function pcre_get_stringnumber(const Code: PPCRE;
  const StringName: PAnsiChar): Integer; external;

function pcre_study(const Code: PPCRE; Options: Integer;
  out ErrPtr: PAnsiChar): PPCREExtra; external;

procedure pcre_dispose(Pattern, Hints, CharTable: Pointer);

{ Other PCRE APIs currently not used by the Delphi wrapper: }

//function pcre_compile2(const Pattern: PAnsiChar; Options: Integer;
//  const ErrorCodePtr: PInteger; const ErrorPtr: PPAnsiChar;
//  ErrorOffset: PInteger; const Tables: PAnsiChar): PPCRE; external;

//function pcre_config(What: Integer; Where: Pointer): Integer; external;

//function pcre_copy_named_substring(const Code: PPCRE; const Subject: PAnsiChar;
//  Ovector: PInteger; StringCount: Integer; const StringName: PAnsiChar;
//  Buffer: PAnsiChar; Size: Integer): Integer; external;

//function pcre_copy_substring(const Subject: PAnsiChar; Ovector: PInteger;
//  StringCount, StringNumber: Integer; Buffer: PAnsiChar;
//  BufferSize: Integer): Integer; external;

//function pcre_dfa_exec(const ArgumentRe: PPCRE; const ExtraData: PPCREExtra;
//  const Subject: PAnsiChar; Length: Integer; StartOffset: Integer;
//  Options: Integer; Offsets: PInteger; OffsetCount: Integer;
//  Workspace: PInteger; WSCount: Integer): Integer; external;

//procedure pcre_free_substring(StringPtr: PAnsiChar); external;

//procedure pcre_free_substring_list(StringListPtr: PPAnsiChar); external;

//function pcre_get_named_substring(const Code: PPCRE; const Subject: PAnsiChar;
//  Ovector: PInteger; StringCount: Integer; const StringName: PAnsiChar;
//  const StringPtr: PPAnsiChar): Integer; external;

//function pcre_get_stringtable_entries(const Code: PPCRE;
//  const StringName: PAnsiChar; FirstPtr, LastPtr: PPAnsiChar): Integer; external;

//function pcre_get_substring(const Subject: PAnsiChar; Ovector: PInteger;
//  StringCount, StringNumber: Integer;
//  const StringPtr: PPAnsiChar): Integer; external;

//function pcre_get_substring_list(const Subject: PAnsiChar; Ovector: PInteger;
//  StringCount: Integer; ListPtr: PPPAnsiChar): Integer; external;

//function pcre_info(const Code: PPCRE; OptPtr,
//  FirstCharPtr: PInteger): Integer; external;

//function pcre_maketables: PAnsiChar; external;

//function pcre_refcount(ArgumentRe: PPCRE; Adjust: Integer): Integer; external;

//function pcre_version: PAnsiChar; external;

implementation

uses
  Windows,
  SysUtils;

{$LINK pcre_compile.obj}
{$LINK pcre_exec.obj}
{$LINK pcre_fullinfo.obj}
{$LINK pcre_get.obj}
{$LINK pcre_globals.obj}
{$LINK pcre_newline.obj}
{$LINK pcre_ord2utf8.obj}
{$LINK pcre_study.obj}
{$LINK pcre_tables.obj}
{$LINK pcre_try_flipped.obj}
{$LINK pcre_ucd.obj}
{$LINK pcre_valid_utf8.obj}
{$LINK pcre_xclass.obj}
{$LINK pcre_default_tables.obj}

{ Object files currently not used by the Delphi wrapper: }

//{$LINK pcre_config.obj}
//{$LINK pcre_dfa_exec.obj}
//{$LINK pcre_info.obj}
//{$LINK pcre_maketables.obj}
//{$LINK pcre_refcount.obj}
//{$LINK pcre_version.obj}

{ Partial C run-time library.
  Needed because the PCRE object files use these }

function strncmp(const String1, String2: PAnsiChar; Count: Integer): Integer; cdecl;
begin
  Result := SysUtils.StrLComp(String1, String2, Count)
end;

function memmove(Dest: Pointer; const Src: Pointer; Count: Integer): Pointer; cdecl;
begin
  Move(Src^, Dest^, Count);
  Result := Dest;
end;

function memset(Dest: Pointer; C, Count: Integer): Pointer; cdecl;
begin
  FillChar(Dest^, Count, C);
  Result := Dest;
end;

function memcpy(Dest: Pointer; const Src: Pointer; Count: Integer): Pointer; cdecl;
begin
  Move(Src^, Dest^, Count);
  Result := Dest;
end;

function memcmp(const Buf1, Buf2: Pointer; Count: Integer): Integer; cdecl;
var
  P1, P2: PByte;
begin
  P1 := Buf1;
  P2 := Buf2;
  while (Count > 0) do
  begin
    if (P1^ < P2^) then
    begin
      Result := -1;
      Exit;
    end
    else
    if (P1^ > P2^) then
    begin
      Result := 1;
      Exit;
    end;
    Inc(P1);
    Inc(P2);
    Dec(Count);
  end;
  Result := 0;
end;

function malloc(Size: Integer): Pointer;
begin
  GetMem(Result, Size);
end;

procedure free(MemBlock: Pointer);
begin
  FreeMem(MemBlock);
end;

procedure pcre_dispose(Pattern, Hints, CharTable: Pointer);
begin
  FreeMem(Pattern);
  FreeMem(Hints);
  FreeMem(CharTable);
end;

end.
