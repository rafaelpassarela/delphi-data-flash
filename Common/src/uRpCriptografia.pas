unit uRpCriptografia;

interface

uses
  IdHashMessageDigest, IdCoderMIME, SysUtils, Classes, uRpAlgoritmos;

type

  TRpCriptografia = class;
  TRpCriptografiaClass = class of TRpCriptografia;

  TRpCriptografia = class(TPersistent)
  private const
    IDENTIFICADOR_CRIPTO = '@*CRYPTO';
  public
    constructor Create; virtual;
    function GetMarcacaoCripoPadrao : string;
    function Criptografar(const pValor : string) : string; virtual; abstract;
    function Descriptografar(const pValor : string) : string; virtual; abstract;

    class function GetIdentificadorPadrao : String;
  end;

  TRpCriptografiaBase64 = class(TRpCriptografia)
  public
    function Criptografar(const pValor: string): string; override;
    function Descriptografar(const pValor: string): string; override;
  end;

  TRpCriptografiaBase64Compressed = class(TRpCriptografia)
  public
    function Criptografar(const pValor: string): string; override;
    function Descriptografar(const pValor: string): string; override;
  end;


  TRpCriptografiaMD5 = class
  private const
    CHAVE_PRIVADA = 'Rp_SWORDFISH';
  public
    class function Gerar(const pValor: string; const pQuantidadeDigitos : Integer = -1): string; // -1 = Todos
    class function GerarF(const pArquivo: string): string;
    class function Validar(const pValor, pValorMD5: string): Boolean;
  end;

  TRpCriptografiaCifrada = class(TRpCriptografia)
  private const
    CHAVE_PRIVADA = '12345';
  private
    FEncoder : TIdEncoderMIME;
    FDecoder : TIdDecoderMIME;
    function GetMD5(const pValor : string) : string;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Criptografar(const pValor: string): string; override;
    function Descriptografar(const pValor: string): string; override;


    class function Criptografa(const pValor: string): string; overload;
    class function Descriptografa(const pValor: string): string; overload;

    class function Criptografa(const pValor: string; out pMD5 : string): string; overload;
    class function Descriptografa(const pValor, pMD5 : string): string; overload;
  end;

  TMinhaCprito = class(TRpCriptografia)
  public
    class function Hex2Dec(const S: string): Longint;
    class function HexToString(const pValor: string): string;
    class function StringToHex(const pValor: string): string;
    class function HexaToBin(const pValor : String) : String;
    class function BinToHexa(const pValor : string) : String;
    class function Criptografa(const pValor: string): string;
    class function Descriptografa(const pValor : string): string;
  end;

implementation

{ TRpCriptografiaCifrada }

constructor TRpCriptografiaCifrada.Create;
begin
  inherited Create;
  FEncoder := TIdEncoderMIME.Create;
  FDecoder := TIdDecoderMIME.Create;
end;

class function TRpCriptografiaCifrada.Criptografa(const pValor: string): string;
var
  lCrypto: TRpCriptografiaCifrada;
begin
  lCrypto := TRpCriptografiaCifrada.Create;
  try
    Result := lCrypto.Criptografar(pValor);
  finally
    lCrypto.Free;
  end;
end;

class function TRpCriptografiaCifrada.Criptografa(const pValor: string; out pMD5: string): string;
var
  lCrypto: TRpCriptografiaCifrada;
begin
  lCrypto := TRpCriptografiaCifrada.Create;
  try
    Result := lCrypto.Criptografar(pValor);
    pMD5 := lCrypto.GetMD5(Result);
  finally
    lCrypto.Free;
  end;
end;

function TRpCriptografiaCifrada.Criptografar(const pValor: string): string;
var
  lStreamValor : TStringStream;
begin
  Result := EmptyStr;
  lStreamValor := TStringStream.Create(pValor);
  try
    Result := FEncoder.Encode(lStreamValor);
  finally
    lStreamValor.Free;
  end;
end;

class function TRpCriptografiaCifrada.Descriptografa(const pValor: string): string;
var
  lCrypto: TRpCriptografiaCifrada;
begin
  lCrypto := TRpCriptografiaCifrada.Create;
  try
    Result := lCrypto.Descriptografar(pValor);
  finally
    lCrypto.Free;
  end;
end;

class function TRpCriptografiaCifrada.Descriptografa(const pValor, pMD5: string): string;
var
  lCrypto: TRpCriptografiaCifrada;
  lMD5Descripto: string;
begin
  lCrypto := TRpCriptografiaCifrada.Create;
  try
    lMD5Descripto := lCrypto.GetMD5(pValor);
    Result := lCrypto.Descriptografar(pValor);
    if lMD5Descripto <> pMD5 then
    begin
      Result := EmptyStr;
    end;
  finally
    lCrypto.Free;
  end;
end;

function TRpCriptografiaCifrada.Descriptografar(const pValor: string): string;
var
  lStreamValor : TStringStream;
begin
  Result := EmptyStr;
  lStreamValor := TStringStream.Create(EmptyStr);
  try
    FDecoder.DecodeBegin(lStreamValor);
    FDecoder.Decode(pValor);
    FDecoder.DecodeEnd;
    Result := lStreamValor.DataString;
  finally
    lStreamValor.Free;
  end;
end;

destructor TRpCriptografiaCifrada.Destroy;
begin
  FEncoder.Free;
  FDecoder.Free;
  inherited;
end;

function TRpCriptografiaCifrada.GetMD5(const pValor : string): string;
begin
  Result := TRpCriptografiaMD5.Gerar(pValor);
end;

{ TRpCriptografia }

constructor TRpCriptografia.Create;
begin
  //
end;


class function TRpCriptografia.GetIdentificadorPadrao: String;
begin
  Result := IDENTIFICADOR_CRIPTO;
end;

function TRpCriptografia.GetMarcacaoCripoPadrao: string;
begin
  Result := IDENTIFICADOR_CRIPTO;
end;

{ TRpCriptografiaMD5 }

class function TRpCriptografiaMD5.Gerar(const pValor: string; const pQuantidadeDigitos : Integer): string;
var
  lDegestMd5: TIdHashMessageDigest5;
  lValorDigest: string;
  lStream: TStringStream;
begin
  lStream := TStringStream.Create('');
  lDegestMd5 := TIdHashMessageDigest5.Create;
  try
    lValorDigest := pValor + CHAVE_PRIVADA;
    lStream.WriteString(lValorDigest);
    {$IFDEF UNICODE}
      Result := lDegestMd5.HashStringAsHex(lValorDigest);
    {$ELSE}
      Result := lDegestMd5.AsHex(lDegestMd5.HashValue(lValorDigest));
    {$ENDIF}
    if pQuantidadeDigitos <> -1 then
    begin
      Result := Copy(Result, 1 , pQuantidadeDigitos);
    end;

  finally
    lStream.Size := 0;
    lStream.Free;
    lDegestMd5.Free;
  end;
end;

class function TRpCriptografiaMD5.GerarF(const pArquivo: string): string;
var
  lDegestMd5: TIdHashMessageDigest5;
  lStream: TFileStream;
  lValoresRandom : array[1..2048] of char;
begin
  Randomize;
  Result := Gerar(lValoresRandom[Random(Length(lValoresRandom))]);

  if FileExists(pArquivo) then
  begin
    lStream := TFileStream.Create(pArquivo, fmOpenRead);
    try
      lDegestMd5 := TIdHashMessageDigest5.Create;
      try
        {$IFDEF UNICODE}
          Result := lDegestMd5.HashStreamAsHex(lStream);
        {$ELSE}
          Result := lDegestMd5.AsHex(lDegestMd5.HashValue(lStream));
        {$ENDIF}
      finally
        lDegestMd5.Free;
      end;
    finally
      lStream.Free;
    end;
  end;
end;

class function TRpCriptografiaMD5.Validar(const pValor, pValorMD5: string): Boolean;
var
  lMd5Local : string;
begin
  lMd5Local := TRpCriptografiaMD5.Gerar(pValor);
  Result := CompareStr(lMd5Local,pValorMD5) = 0;
end;

{ TMinhaCprito }

class function TMinhaCprito.BinToHexa(const pValor: string): String;
var
  LStr1, LStr2: WideString;
begin
  {$IFDEF UNICODE}
    { Store the text in the memo to a String variable. }
    LStr1 := pValor;
    LStr2 := pValor;
    { Set the length of the String to hold the conversion. }
    SetLength(LStr2, Length(LStr1) * 4);
    { Call the binary to hexadecimal conversion procedure. }
  //  BinToHex(PChar(LStr1[1]), PChar(LStr2), Length(LStr1) * SizeOf(Char));
    BinToHex(LStr1[1], PWideChar(LStr2), Length(LStr1) * SizeOf(Char));
    { Put the results in Memo2. }
    Result := LStr2;
  {$ELSE}
    LStr1 := pValor;
    LStr2 := pValor;
    { Set the length of the String to hold the conversion. }
    SetLength(LStr2, Length(LStr1));
    { Call the binary to hexadecimal conversion procedure. }
//    HexToBin(@LStr1, PChar(LStr2), Length(LStr1) * SizeOf(LStr2) - 1);
    HexToBin(@LStr1, @LStr2, Length(LStr1) * SizeOf(Char));
    { Put the results in Memo2. }
    Result := LStr2;
  {$ENDIF}



//    LStr1 := pValor;
//    LStr2 := pValor;
//    { Set the length of the String to hold the conversion. }
//    SetLength(LStr2, Length(LStr1) * 4);
//    { Call the binary to hexadecimal conversion procedure. }
//    HexToBin(pChar(LStr1), @LStr2, SizeOf(LStr2) - 1);
//    { Put the results in Memo2. }
//    Result := LStr2;

end;

class function TMinhaCprito.Criptografa(const pValor: string): string;
begin

//    {$IFDEF VER220}
//      Result := lDegestMd5.HashStringAsHex(lValorDigest);
//    {$ELSE}
//      Result := lDegestMd5.AsHex(lDegestMd5.HashValue(lValorDigest));
//    {$ENDIF}

  Result := HexaToBin(StringToHex(TRpCriptografiaCifrada.Criptografa(pValor)));
end;

class function TMinhaCprito.Descriptografa(const pValor : String): string;
begin
  Result := TRpCriptografiaCifrada.Descriptografa(HexToString(BinToHexa(pvalor)));

//  Result := TRpCriptografiaCifrada.Descriptografa(HexToString(BinToHexa(pvalor)));
end;

class function TMinhaCprito.Hex2Dec(const S: string): Longint;
var
  HexStr: string;
begin
  if Pos('$', S) = 0 then
		HexStr := '$' + S
  else
		HexStr := S;
  Result := StrToIntDef(HexStr, 0);
end;

class function TMinhaCprito.HexaToBin(const pValor: String): String;
var
  LStr1, LStr2: WideString;
begin

  {$IFDEF UNICODE}
    { Store the text in the memo to a String variable. }
    LStr1 := pValor;
    LStr2 := pValor;
    { Set the length of the String to hold the conversion. }
    SetLength(LStr2, Length(LStr1) div 4);
    { Call the hexadecimal to binary conversion procedure. }
    HexToBin(PWideChar(LStr1), LStr2[1], Length(LStr1) div SizeOf(Char));
  //  HexToBin(PChar(LStr1), PChar(LStr2[1]), Length(LStr1) div SizeOf(Char));
    { Output the results to Memo1. }
    Result := LStr2;
  {$ELSE}
    LStr1 := pValor;
    LStr2 := pValor;
    { Set the length of the String to hold the conversion. }
    SetLength(LStr2, Length(LStr1));
    { Call the hexadecimal to binary conversion procedure. }
//    HexToBin(pChar(LStr1), @LStr2, Length(LStr1) * SizeOf(LStr1));
    HexToBin(@LStr1, @LStr2, Length(LStr1) div SizeOf(Char));
//    LStr1[SizeOf(LStr1) - 1] := #0;
    { Output the results to Memo1. }
    Result := LStr2;
  {$ENDIF}

//    LStr1 := pValor;
//    LStr2 := pValor;
//    { Set the length of the String to hold the conversion. }
//    SetLength(LStr2, Length(LStr1) div 4);
//    { Call the hexadecimal to binary conversion procedure. }
//    HexToBin(pChar(LStr1), @LStr2, SizeOf(LStr1));
//    LStr1[SizeOf(LStr1) - 1] := #0;
//    { Output the results to Memo1. }
//    Result := LStr2;

end;

class function TMinhaCprito.HexToString(const pValor: string): string;
const
	HEX_STR = '0123456789ABCDEF';

var
	I: Integer;
	bt, cStr: string;
  lValor: string;
begin

	cStr := '';
	lValor := UpperCase(pValor);

	// Limpa string
	for I := 1 to Length(lValor) do begin
		bt := Copy(lValor, I, 1);
		if Pos(bt , HEX_STR) > 0 then
			cStr := cStr + bt;
	end;

	if (Length(cStr) mod 4) = 1 then
		cStr := '0'+cStr;

	I := 1;
	Result := '';
	while I < Length(cStr) do begin
		Result := Result + Chr(Hex2Dec(copy(cStr,I,4)));
		I := I + 4;
	end;
end;

class function TMinhaCprito.StringToHex(const pValor: string): string;
var
  I: Integer;
  lArray : Array of Integer;
begin
  SetLength(lArray, Length(pValor));
  for I := 0 to Length(pValor) - 1 do
  begin
    lArray[I] := Ord(pValor[I + 1]);
  end;

  for I := 0 to Length(lArray) - 1 do
  begin
    Result := Result + IntToHex(lArray[I], 4);
  end;
end;

{ TRpCriptografiaBase64 }

function TRpCriptografiaBase64.Criptografar(const pValor: string): string;
begin
  Algoritimos.Base64Encode(pValor, Result);
end;

function TRpCriptografiaBase64.Descriptografar(const pValor: string): string;
begin
  Algoritimos.Base64Decode(pValor, Result);
end;

{ TRpCriptografiaBase64Compressed }

function TRpCriptografiaBase64Compressed.Criptografar(
  const pValor: string): string;
begin
  Result := Algoritimos.Base64CompressedString(pValor);
end;

function TRpCriptografiaBase64Compressed.Descriptografar(
  const pValor: string): string;
begin
  Result := Algoritimos.Base64DecompressedString(pValor);
end;

end.
