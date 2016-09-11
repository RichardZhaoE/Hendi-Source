unit Auth;

interface

uses Classes, IdHTTP, StrUtils;

function GetCookieFromNexon(const Username, Password: string): string;

implementation

function GetCookieFromNexon(const Username, Password: string): string;
var
  HTTP: TIdHTTP;
  S: TStringList;
  C: string;
  p: Integer;
begin
  HTTP := TIdHTTP.Create;
  S := TStringList.Create;
  try
    S.Text := 'tbID=' + Username + '&tbPass=' + Password;
    HTTP.Request.UserAgent := 'Mozilla/4.0 (compatible)';
    HTTP.HTTPOptions := [];
    HTTP.Post('http://maplestory.nexon.net/controls/passport.aspx', S);
    C := HTTP.Response.RawHeaders.Text;
    p := Pos('NPP=', C) + 4;
    if p > 0 then
      Result := Copy(C, p, PosEx(';', C, p) - p)
    else
      Result := '';
  finally
    HTTP.Free;
    S.Free;
  end;
end;

end.
