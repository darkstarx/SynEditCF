(*

  Letterpress

  File format writing routines.

  Copyright 2009-2010, Garnet

*)

{ Returns identation level }
function GetIndent: UTF8String;
begin
  Result := UTF8Encode(StringOfChar(#9, I));
end;

{ Writes UTF8 line }
procedure WrLn(const S: UTF8String; NoIndent: Boolean = False);
begin
  if NoIndent then
    WriteLn(F, S)
  else
    WriteLn(F, GetIndent + S);
end;

{ Writes UTF8 line converting from Unicode }
procedure WrULn(const S: UnicodeString; NoIndent: Boolean = False);
var
  U: UTF8String;
begin
  SetLength(U, Length(S) * 3);
  SetLength(U, UnicodeToUtf8(@U[1], Length(U), @S[1], Length(S)) - 1);
  if NoIndent then
    WriteLn(F, U)
  else
    WriteLn(F, GetIndent + U);
end;