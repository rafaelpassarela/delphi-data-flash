unit uRpEncryption;

interface

uses
  IdHashMessageDigest, IdCoderMIME, SysUtils, Classes, uRpAlgorithms;

type
  TRpEncryption = class;
  TRpEncryptionClass = class of TRpEncryption;

  TRpEncryption = class(TPersistent)
  private const
    ENC_IDENTIFIER = '@*CRYPTO';
  public
    constructor Create; virtual;
    function GetDefaultEncryptionIdentifier : string;
    function Encrypt(const AValue : string) : string; virtual; abstract;
    function Decrypt(const AValue : string) : string; virtual; abstract;

    class function GetDefaultIdentifier : String;
  end;

  TRpEncryptionBase64 = class(TRpEncryption)
  public
    function Encrypt(const AValue: string): string; override;
    function Decrypt(const AValue: string): string; override;
  end;

  TRpEncryptionBase64Compressed = class(TRpEncryption)
  public
    function Encrypt(const AValue: string): string; override;
    function Decrypt(const AValue: string): string; override;
  end;

  TRpEncryptionMD5 = class
  private const
    PRIVATE_KEY = 'Rp_SWORDFISH_MD5';
  public
    class function Gerar(const AValue: string; const AMaxLength : Integer = -1): string; // -1 = All
    class function GerarF(const pArquivo: string): string;
    class function Validar(const AValue, AValueMD5: string): Boolean;
  end;

  TRpEncryptionCiffer = class(TRpEncryption)
  private const
    PRIVATE_KEY = 'Rp_SWORDFISH_CIFFER';
  private
    FEncoder : TIdEncoderMIME;
    FDecoder : TIdDecoderMIME;
    function GetMD5(const AValue : string) : string;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Encrypt(const AValue: string): string; override;
    function Decrypt(const AValue: string): string; override;

    class function Encrypting(const AValue: string): string; overload;
    class function Decrypting(const AValue: string): string; overload;

    class function Encrypting(const AValue: string; out pMD5 : string): string; overload;
    class function Decrypting(const AValue, pMD5 : string): string; overload;
  end;

  TMyEncryption = class(TRpEncryption)
  public
    class function Hex2Dec(const S: string): Longint;
    class function HexToString(const AValue: string): string;
    class function StringToHex(const AValue: string): string;
    class function HexToBin(const AValue : String) : String;
    class function BinToHex(const AValue : string) : String;
    class function Encrypting(const AValue: string): string;
    class function Decrypting(const AValue : string): string;
  end;

implementation

{ TRpEncryptionCiffer }

constructor TRpEncryptionCiffer.Create;
begin
  inherited Create;
  FEncoder := TIdEncoderMIME.Create;
  FDecoder := TIdDecoderMIME.Create;
end;

class function TRpEncryptionCiffer.Encrypting(const AValue: string): string;
var
  lCrypto: TRpEncryptionCiffer;
begin
  lCrypto := TRpEncryptionCiffer.Create;
  try
    Result := lCrypto.Encrypt(AValue);
  finally
    lCrypto.Free;
  end;
end;

class function TRpEncryptionCiffer.Encrypting(const AValue: string; out pMD5: string): string;
var
  lCrypto: TRpEncryptionCiffer;
begin
  lCrypto := TRpEncryptionCiffer.Create;
  try
    Result := lCrypto.Encrypt(AValue);
    pMD5 := lCrypto.GetMD5(Result);
  finally
    lCrypto.Free;
  end;
end;

function TRpEncryptionCiffer.Encrypt(const AValue: string): string;
var
  lStreamValor : TStringStream;
begin
  Result := EmptyStr;
  lStreamValor := TStringStream.Create(AValue);
  try
    Result := FEncoder.Encode(lStreamValor);
  finally
    lStreamValor.Free;
  end;
end;

class function TRpEncryptionCiffer.Decrypting(const AValue: string): string;
var
  lCrypto: TRpEncryptionCiffer;
begin
  lCrypto := TRpEncryptionCiffer.Create;
  try
    Result := lCrypto.Decrypt(AValue);
  finally
    lCrypto.Free;
  end;
end;

class function TRpEncryptionCiffer.Decrypting(const AValue, pMD5: string): string;
var
  lCrypto: TRpEncryptionCiffer;
  lMD5Descripto: string;
begin
  lCrypto := TRpEncryptionCiffer.Create;
  try
    lMD5Descripto := lCrypto.GetMD5(AValue);
    Result := lCrypto.Decrypt(AValue);
    if lMD5Descripto <> pMD5 then
    begin
      Result := EmptyStr;
    end;
  finally
    lCrypto.Free;
  end;
end;

function TRpEncryptionCiffer.Decrypt(const AValue: string): string;
var
  lStreamValor : TStringStream;
begin
  Result := EmptyStr;
  lStreamValor := TStringStream.Create(EmptyStr);
  try
    FDecoder.DecodeBegin(lStreamValor);
    FDecoder.Decode(AValue);
    FDecoder.DecodeEnd;
    Result := lStreamValor.DataString;
  finally
    lStreamValor.Free;
  end;
end;

destructor TRpEncryptionCiffer.Destroy;
begin
  FEncoder.Free;
  FDecoder.Free;
  inherited;
end;

function TRpEncryptionCiffer.GetMD5(const AValue : string): string;
begin
  Result := TRpEncryptionMD5.Gerar(AValue);
end;

{ TRpEncryption }

constructor TRpEncryption.Create;
begin
  //
end;

class function TRpEncryption.GetDefaultIdentifier: String;
begin
  Result := ENC_IDENTIFIER;
end;

function TRpEncryption.GetDefaultEncryptionIdentifier: string;
begin
  Result := ENC_IDENTIFIER;
end;

{ TRpEncryptionMD5 }

class function TRpEncryptionMD5.Gerar(const AValue: string; const AMaxLength : Integer): string;
var
  lDegestMd5: TIdHashMessageDigest5;
  lValorDigest: string;
  lStream: TStringStream;
begin
  lStream := TStringStream.Create('');
  lDegestMd5 := TIdHashMessageDigest5.Create;
  try
    lValorDigest := AValue + PRIVATE_KEY;
    lStream.WriteString(lValorDigest);
    {$IFDEF UNICODE}
      Result := lDegestMd5.HashStringAsHex(lValorDigest);
    {$ELSE}
      Result := lDegestMd5.AsHex(lDegestMd5.HashValue(lValorDigest));
    {$ENDIF}
    if AMaxLength <> -1 then
      Result := Copy(Result, 1 , AMaxLength);
  finally
    lStream.Size := 0;
    lStream.Free;
    lDegestMd5.Free;
  end;
end;

class function TRpEncryptionMD5.GerarF(const pArquivo: string): string;
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

class function TRpEncryptionMD5.Validar(const AValue, AValueMD5: string): Boolean;
var
  lMd5Local : string;
begin
  lMd5Local := TRpEncryptionMD5.Gerar(AValue);
  Result := CompareStr(lMd5Local,AValueMD5) = 0;
end;

{ TMyEncryption }

class function TMyEncryption.BinToHex(const AValue: string): String;
var
  LStr1, LStr2: WideString;
begin
  {$IFDEF UNICODE}
    { Store the text in the memo to a String variable. }
    LStr1 := AValue;
    LStr2 := AValue;
    { Set the length of the String to hold the conversion. }
    SetLength(LStr2, Length(LStr1) * 4);
    { Call the binary to hexadecimal conversion procedure. }
  //  BinToHex(PChar(LStr1[1]), PChar(LStr2), Length(LStr1) * SizeOf(Char));
    Classes.BinToHex(LStr1[1], PWideChar(LStr2), Length(LStr1) * SizeOf(Char));
    { Put the results in Memo2. }
    Result := LStr2;
  {$ELSE}
    LStr1 := AValue;
    LStr2 := AValue;
    { Set the length of the String to hold the conversion. }
    SetLength(LStr2, Length(LStr1));
    { Call the binary to hexadecimal conversion procedure. }
//    HexToBin(@LStr1, PChar(LStr2), Length(LStr1) * SizeOf(LStr2) - 1);
    Classes.HexToBin(@LStr1, @LStr2, Length(LStr1) * SizeOf(Char));
    { Put the results in Memo2. }
    Result := LStr2;
  {$ENDIF}



//    LStr1 := AValue;
//    LStr2 := AValue;
//    { Set the length of the String to hold the conversion. }
//    SetLength(LStr2, Length(LStr1) * 4);
//    { Call the binary to hexadecimal conversion procedure. }
//    HexToBin(pChar(LStr1), @LStr2, SizeOf(LStr2) - 1);
//    { Put the results in Memo2. }
//    Result := LStr2;

end;

class function TMyEncryption.Encrypting(const AValue: string): string;
begin
//    {$IFDEF VER220}
//      Result := lDegestMd5.HashStringAsHex(lValorDigest);
//    {$ELSE}
//      Result := lDegestMd5.AsHex(lDegestMd5.HashValue(lValorDigest));
//    {$ENDIF}

  Result := HexToBin(StringToHex(TRpEncryptionCiffer.Encrypting(AValue)));
end;

class function TMyEncryption.Decrypting(const AValue : String): string;
begin
  Result := TRpEncryptionCiffer.Decrypting(HexToString(BinToHex(AValue)));

//  Result := TRpCriptografiaCifrada.Descriptografa(HexToString(BinToHexa(AValue)));
end;

class function TMyEncryption.Hex2Dec(const S: string): Longint;
var
  HexStr: string;
begin
  if Pos('$', S) = 0 then
		HexStr := '$' + S
  else
		HexStr := S;
  Result := StrToIntDef(HexStr, 0);
end;

class function TMyEncryption.HexToBin(const AValue: String): String;
var
  LStr1, LStr2: WideString;
begin
  {$IFDEF UNICODE}
    { Store the text in the memo to a String variable. }
    LStr1 := AValue;
    LStr2 := AValue;
    { Set the length of the String to hold the conversion. }
    SetLength(LStr2, Length(LStr1) div 4);
    { Call the hexadecimal to binary conversion procedure. }
    Classes.HexToBin(PWideChar(LStr1), LStr2[1], Length(LStr1) div SizeOf(Char));
  //  HexToBin(PChar(LStr1), PChar(LStr2[1]), Length(LStr1) div SizeOf(Char));
    { Output the results to Memo1. }
    Result := LStr2;
  {$ELSE}
    LStr1 := AValue;
    LStr2 := AValue;
    { Set the length of the String to hold the conversion. }
    SetLength(LStr2, Length(LStr1));
    { Call the hexadecimal to binary conversion procedure. }
//    HexToBin(pChar(LStr1), @LStr2, Length(LStr1) * SizeOf(LStr1));
    Classes.HexToBin(@LStr1, @LStr2, Length(LStr1) div SizeOf(Char));
//    LStr1[SizeOf(LStr1) - 1] := #0;
    { Output the results to Memo1. }
    Result := LStr2;
  {$ENDIF}

//    LStr1 := AValue;
//    LStr2 := AValue;
//    { Set the length of the String to hold the conversion. }
//    SetLength(LStr2, Length(LStr1) div 4);
//    { Call the hexadecimal to binary conversion procedure. }
//    HexToBin(pChar(LStr1), @LStr2, SizeOf(LStr1));
//    LStr1[SizeOf(LStr1) - 1] := #0;
//    { Output the results to Memo1. }
//    Result := LStr2;

end;

class function TMyEncryption.HexToString(const AValue: string): string;
const
	HEX_STR = '0123456789ABCDEF';
var
	I: Integer;
	bt, cStr: string;
  lValor: string;
begin
	cStr := '';
	lValor := UpperCase(AValue);

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

class function TMyEncryption.StringToHex(const AValue: string): string;
var
  I: Integer;
  lArray : Array of Integer;
begin
  SetLength(lArray, Length(AValue));
  for I := 0 to Length(AValue) - 1 do
    lArray[I] := Ord(AValue[I + 1]);

  for I := 0 to Length(lArray) - 1 do
    Result := Result + IntToHex(lArray[I], 4);
end;

{ TRpEncryptionBase64 }

function TRpEncryptionBase64.Encrypt(const AValue: string): string;
begin
  Algorithms.Base64Encode(AValue, Result);
end;

function TRpEncryptionBase64.Decrypt(const AValue: string): string;
begin
  Algorithms.Base64Decode(AValue, Result);
end;

{ TRpEncryptionBase64Compressed }

function TRpEncryptionBase64Compressed.Encrypt(const AValue: string): string;
begin
  Result := Algorithms.Base64CompressedString(AValue);
end;

function TRpEncryptionBase64Compressed.Decrypt(const AValue: string): string;
begin
  Result := Algorithms.Base64DecompressedString(AValue);
end;

end.
