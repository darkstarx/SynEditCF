(*

  Letterpress

  File format reading routines.

  Copyright 2009-2010, Garnet

*)

{ Skips whitespace in read line }
procedure SkipWhitespace;
begin
  while P^ < #33 do Inc(P);
end;

{ Reads UTF8 string }
procedure RdLn;
begin
  ReadLn(F, S);
  P := PAnsiChar(S);
  SkipWhitespace;
end;

{ Reads UTF8 string and returns it as Unicode }
function RdULn: UnicodeString;
begin
  Result := UTF8ToUnicodeString(S);
end;

{ Tests read line for a specified tag }
function IsTagRead(const Tag: UTF8String): Boolean;
begin
  Result := CompareMem(P, PAnsiChar(Tag), Length(Tag));
end;

{ Tests new line for a specified tag }
function IsTag(const Tag: UTF8String): Boolean;
begin
  RdLn; Result := IsTagRead(Tag);
end;

{ Retrieves a tag name }
function GetTag: UTF8String;
begin
  SetLength(Result, Length(S) - (P - PAnsiChar(S)) - 2);
  Move(P^, Result[1], Length(Result));
end;

{ Retrieves current param name }
function GetName: UTF8String;
var
  Runner: PAnsiChar;
begin
  Runner := P; while Runner^ <> #32 do Inc(Runner);
  SetLength(Result, Runner - P);
  Move(P^, Result[1], Length(Result));
end;

{ Retrieves value from a specified param }
function GetValue(const Tag: UTF8String): UTF8String;
begin
  SetLength(Result, Length(S) - (P - PAnsiChar(S)) - Length(Tag) - Length(sTail));
  Inc(P, Length(Tag));
  Move(P^, Result[1], Length(Result));
end;