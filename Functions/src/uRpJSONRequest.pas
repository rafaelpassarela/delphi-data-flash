unit uRpJSONRequest;

{$I ..\..\Common\src\RpInc.inc}

interface

uses
  IdHTTP, SysUtils, Classes;

type
  TRpRequestInfoEncode = (icUTF_8, icISO_8859_1);

  TRpJsonRequest = class
  private
    FHttp: TIdHTTP;
    FServerUrl: string;
    FEncode: TRpRequestInfoEncode;
    procedure SetServerUrl(const Value: string);
    function GetEncoding : string;
  public
    constructor Create;
    destructor Destroy; override;
    property ServerUrl : string read FServerUrl write SetServerUrl;
    property Encode : TRpRequestInfoEncode read FEncode write FEncode default icUTF_8;

    procedure AddHeader(const AHeader : string; const AValue : string);
    function Get(const AUrl : string; out ABody : string) : integer;
    function Post(const AUrl : string; out ABody : string;
      const AParameters : string = '') : Integer;
  end;

implementation

{ TJsonRequest }

procedure TRpJsonRequest.AddHeader(const AHeader, AValue: string);
var
  lIndex : integer;
begin
  FHttp.Request.CustomHeaders.FoldLines := false;

  lIndex := FHttp.Request.CustomHeaders.IndexOfName(AHeader);
  if lIndex > -1 then
    FHttp.Request.CustomHeaders.Delete(lIndex);

  FHttp.Request.CustomHeaders.Values[AHeader] := AValue;
end;

constructor TRpJsonRequest.Create;
begin
  FHttp := TIdHTTP.Create(nil);
  FHttp.HandleRedirects := True;
  FHttp.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2342.2 Safari/537.36';
  FHttp.Request.AcceptEncoding := 'gzip, deflate, ';
  FHttp.Request.AcceptLanguage := 'pt-BR, pt; q=0.8, en-US; q=0.6, en; q=0.4';
  FHttp.Request.Accept := '*/*';

  FHttp.Request.ContentType := 'application/json';
  FHttp.Request.CustomHeaders.Add('CSP: active');
  FHttp.Request.CustomHeaders.Add('Cache-Control: no-cache');
  FHttp.Request.ContentEncoding := GetEncoding;
end;

destructor TRpJsonRequest.Destroy;
begin
  FreeAndNil(FHttp);
  inherited;
end;

function TRpJsonRequest.Get(const AUrl: string; out ABody: string): integer;
begin
  try
    FHttp.Request.ContentEncoding := GetEncoding;
    ABody := FHttp.Get(FServerUrl + AUrl);
    Result := FHttp.ResponseCode;
    FHttp.Disconnect;
  except
    on E: EIdHTTPProtocolException do
       Result := FHttp.ResponseCode;
  end;
end;

function TRpJsonRequest.GetEncoding: string;
begin
  if FEncode = icISO_8859_1 then
    Result := 'iso-8859-1'
  else
    Result := 'utf-8';
end;

function TRpJsonRequest.Post(const AUrl: string; out ABody: string;
  const AParameters: string): Integer;
var
  lSource : TStringList;
begin
  lSource := TStringList.Create;
  try
    FHttp.Request.ContentEncoding := GetEncoding;
//    if FEncode = icISO_8859_1 then
    lSource.Text := AParameters;
//    else
//      lSource.Text := AnsiToUtf(AParameters);
    try
      try
        ABody := FHttp.Post(FServerUrl + AURL, lSource);
//        FReturn := UtfToAnsi(FReturn);
        Result := FHttp.ResponseCode;
      finally
        if Assigned(FHttp) then
          FHttp.Disconnect;
      end;
    except
      on E: EIdHTTPProtocolException do
        Result := FHttp.ResponseCode;
      on E:Exception do
        Result := -1;
    end;
  finally
    FreeAndNil(lSource);
  end;
end;

procedure TRpJsonRequest.SetServerUrl(const Value: string);
begin
  FServerUrl := Value;

  if (FServerUrl <> '') and (FServerUrl[Length(FServerUrl)] <> '/') then
    FServerUrl := FServerUrl + '/';
end;

end.
