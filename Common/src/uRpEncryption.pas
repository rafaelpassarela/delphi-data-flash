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
    class function Generate(const AValue: string; const AMaxLength : Integer = -1): string; // -1 = All
    class function GenerateFromFile(const AFileName: string): string;
    class function Validate(const AValue, AValueMD5: string): Boolean;
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
    class function Hex2Dec(const AValue: string): Longint;
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
  lStreamValue : TStringStream;
begin
  Result := EmptyStr;
  lStreamValue := TStringStream.Create(AValue);
  try
    Result := FEncoder.Encode(lStreamValue);
  finally
    lStreamValue.Free;
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
  lStreamValue : TStringStream;
begin
  Result := EmptyStr;
  lStreamValue := TStringStream.Create(EmptyStr);
  try
    FDecoder.DecodeBegin(lStreamValue);
    FDecoder.Decode(AValue);
    FDecoder.DecodeEnd;
    Result := lStreamValue.DataString;
  finally
    lStreamValue.Free;
  end;
end;

destructor TRpEncryptionCiffer.Destroy;
begin
  FreeAndNil(FEncoder);
  FreeAndNil(FDecoder);
  inherited;
end;

function TRpEncryptionCiffer.GetMD5(const AValue : string): string;
begin
  Result := TRpEncryptionMD5.Generate(AValue);
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

class function TRpEncryptionMD5.Generate(const AValue: string; const AMaxLength : Integer): string;
var
  lDegestMd5: TIdHashMessageDigest5;
  lDigestValue: string;
  lStream: TStringStream;
begin
  lStream := TStringStream.Create('');
  lDegestMd5 := TIdHashMessageDigest5.Create;
  try
    lDigestValue := AValue + PRIVATE_KEY;
    lStream.WriteString(lDigestValue);
    {$IFDEF UNICODE}
      Result := lDegestMd5.HashStringAsHex(lDigestValue);
    {$ELSE}
      Result := lDegestMd5.AsHex(lDegestMd5.HashValue(lDigestValue));
    {$ENDIF}
    if AMaxLength <> -1 then
      Result := Copy(Result, 1 , AMaxLength);
  finally
    lStream.Size := 0;
    lStream.Free;
    lDegestMd5.Free;
  end;
end;

class function TRpEncryptionMD5.GenerateFromFile(const AFileName: string): string;
var
  lDegestMd5: TIdHashMessageDigest5;
  lStream: TFileStream;
  lRandomValues : array[1..2048] of char;
begin
  Randomize;
  Result := Generate(lRandomValues[Random(Length(lRandomValues))]);

  if FileExists(AFileName) then
  begin
    lStream := TFileStream.Create(AFileName, fmOpenRead);
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

class function TRpEncryptionMD5.Validate(const AValue, AValueMD5: string): Boolean;
var
  lMd5Local : string;
begin
  lMd5Local := TRpEncryptionMD5.Generate(AValue);
  Result := CompareStr(lMd5Local,AValueMD5) = 0;
end;

{ TMyEncryption }

class function TMyEncryption.BinToHex(const AValue: string): String;
var
  lStr1, lStr2: WideString;
begin
  {$IFDEF UNICODE}
    { Store the text in the memo to a String variable. }
    lStr1 := AValue;
    lStr2 := AValue;
    { Set the length of the String to hold the conversion. }
    SetLength(lStr2, Length(lStr1) * 4);
    { Call the binary to hexadecimal conversion procedure. }
    Classes.BinToHex(lStr1[1], PWideChar(lStr2), Length(lStr1) * SizeOf(Char));
    { Put the results in Memo2. }
    Result := lStr2;
  {$ELSE}
    lStr1 := AValue;
    lStr2 := AValue;
    { Set the length of the String to hold the conversion. }
    SetLength(lStr2, Length(lStr1));
    { Call the binary to hexadecimal conversion procedure. }
    Classes.HexToBin(@lStr1, @lStr2, Length(lStr1) * SizeOf(Char));
    { Put the results in Memo2. }
    Result := lStr2;
  {$ENDIF}
end;

class function TMyEncryption.Encrypting(const AValue: string): string;
begin
  Result := HexToBin(StringToHex(TRpEncryptionCiffer.Encrypting(AValue)));
end;

class function TMyEncryption.Decrypting(const AValue : String): string;
begin
  Result := TRpEncryptionCiffer.Decrypting(HexToString(BinToHex(AValue)));
end;

class function TMyEncryption.Hex2Dec(const AValue: string): Longint;
var
  lHexStr: string;
begin
  if Pos('$', AValue) = 0 then
		lHexStr := '$' + AValue
  else
		lHexStr := AValue;
  Result := StrToIntDef(lHexStr, 0);
end;

class function TMyEncryption.HexToBin(const AValue: String): String;
var
  lStr1, lStr2: WideString;
begin
  {$IFDEF UNICODE}
    { Store the text in the memo to a String variable. }
    lStr1 := AValue;
    lStr2 := AValue;
    { Set the length of the String to hold the conversion. }
    SetLength(lStr2, Length(lStr1) div 4);
    { Call the hexadecimal to binary conversion procedure. }
    Classes.HexToBin(PWideChar(lStr1), lStr2[1], Length(lStr1) div SizeOf(Char));
    { Output the results to Memo1. }
    Result := lStr2;
  {$ELSE}
    lStr1 := AValue;
    lStr2 := AValue;
    { Set the length of the String to hold the conversion. }
    SetLength(lStr2, Length(lStr1));
    { Call the hexadecimal to binary conversion procedure. }
    Classes.HexToBin(@lStr1, @lStr2, Length(lStr1) div SizeOf(Char));
    Result := lStr2;
  {$ENDIF}
end;

class function TMyEncryption.HexToString(const AValue: string): string;
const
	HEX_STR = '0123456789ABCDEF';
var
	I: Integer;
	lBase, cStr: string;
  lValue: string;
begin
	cStr := '';
	lValue := UpperCase(AValue);

	// Limpa string
	for I := 1 to Length(lValue) do begin
		lBase := Copy(lValue, I, 1);
		if Pos(lBase , HEX_STR) > 0 then
			cStr := cStr + lBase;
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
