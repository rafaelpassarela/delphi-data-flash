unit uRpAlgoritmos;

{$I RpInc.inc}

interface

uses
  {$IFDEF XE3UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  ZLib, IdCoderMIME, IdHashMessageDigest, IdHash;

type
  Algoritimos = class
  public
    class procedure StreamCompression(const AInStream : TStream; const AOutStream : TStream);
    class procedure StreamDecompression(const AInStream : TStream; const AOutStream : TStream);
    class procedure Base64Decode(const ABase64String : string; const OutStream : TStream); overload;
    class procedure Base64Decode(const ABase64String : string; var OutString : string); overload;
    class procedure Base64Encode(const AInStream : TStream; var OutString : string); overload;
    class procedure Base64Encode(const AInString : string; var OutString : string); overload;
    class function Base64CompressedString(const AString : string): string;
    class function Base64DecompressedString(const AString : string): string;
    class function MD5FromFile(const AFileName : TFileName) : string;
    class function MD5FromString(const AString : string) : string;
  end;

implementation

{ Algorithm }

class function Algoritimos.Base64CompressedString(const AString: string): string;
var
  lInputStream: TStringStream;
  lOutputStream: TStringStream;
begin
  lInputStream := TStringStream.Create(AString);
  lOutputStream := TStringStream.Create('');
  try
    Algoritimos.StreamCompression(lInputStream, lOutputStream);
    Algoritimos.Base64Encode(lOutputStream, Result);
  finally
    lInputStream.Free;
    lOutputStream.Free;
  end;
end;

class procedure Algoritimos.Base64Decode(const ABase64String: string; const OutStream: TStream);
var
  lDecode: TIdDecoderMIME;
begin
  OutStream.Size := 0;
  OutStream.Position := 0;

  lDecode := TIdDecoderMIME.Create;
  try
    lDecode.DecodeBegin(OutStream);
    lDecode.Decode(ABase64String);
    lDecode.DecodeEnd;

    OutStream.Position := 0;
  finally
    FreeAndNil(lDecode);
  end;
end;

class procedure Algoritimos.Base64Decode(const ABase64String: string; var OutString: string);
var
  lStrStream: TStringStream;
begin
  lStrStream := TStringStream.Create('');
  try
    Base64Decode(ABase64String, lStrStream);
    lStrStream.Position := 0;
    OutString := lStrStream.DataString;
  finally
    lStrStream.Free;
  end;
end;

class function Algoritimos.Base64DecompressedString(const AString: string): string;
var
  lInputStream: TStringStream;
  lOutputStream: TStringStream;
begin
  lInputStream := TStringStream.Create('');
  lOutputStream := TStringStream.Create('');
  try
    Algoritimos.Base64Decode(AString, lInputStream);
    Algoritimos.StreamDecompression(lInputStream, lOutputStream);
    Result := lOutputStream.DataString;
  finally
    lInputStream.Free;
    lOutputStream.Free;
  end;
end;

class procedure Algoritimos.Base64Encode(const AInString: string; var OutString: string);
var
  lStrStream: TStringStream;
begin
  lStrStream := TStringStream.Create(AInString);
  try
    lStrStream.Position := 0;
    Base64Encode(lStrStream, OutString);
  finally
    lStrStream.Free;
  end;
end;

class procedure Algoritimos.Base64Encode(const AInStream: TStream; var OutString: string);
var
  lEncode: TIdEncoderMIME;
begin
  AInStream.Position := 0;

  lEncode := TIdEncoderMIME.Create;
  try
    OutString := lEncode.Encode(AInStream);
    AInStream.Position := 0;
  finally
    FreeAndNil(lEncode);
  end;
end;

class function Algoritimos.MD5FromFile(const AFileName: TFileName): string;
var
  lIdMD5 : TIdHashMessageDigest5;
  lFileStream : TFileStream;
{$IFNDEF UNICODE}
  lRet: T4x4LongWordRecord;
{$ENDIF}
begin
  Result := '';
  if FileExists(AFileName) then
  begin
    lIdMD5 := TIdHashMessageDigest5.Create;
    lFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
{$IFDEF UNICODE}
      Result := lIdMD5.HashStreamAsHex(lFileStream);
{$ELSE}
      lRet := lIdMD5.HashValue(lFileStream);
      Result := lIdMD5.AsHex(lRet);
{$ENDIF}
    finally
      lFileStream.Free;
      lIdMD5.Free;
    end;
  end;
end;

class function Algoritimos.MD5FromString(const AString: string): string;
var
  lIdMD5 : TIdHashMessageDigest5;
begin
  Result := '';
  lIdMD5 := TIdHashMessageDigest5.Create;
  try
{$IFDEF UNICODE}
    Result := lIdMD5.HashStringAsHex(AString);
{$ELSE}
    Result := lIdMD5.AsHex(lIdMD5.HashValue(AString));
{$ENDIF}
  finally
    lIdMD5.Free;
  end;
end;

class procedure Algoritimos.StreamCompression(const AInStream, AOutStream: TStream);
var
  lSourceStream: TCompressionStream;
begin
  AOutStream.Size := 0;
  AOutStream.Position := 0;

  lSourceStream := TCompressionStream.Create(clDefault, AOutStream);
  try
    AInStream.Position := 0;
    lSourceStream.CopyFrom(AInStream, AInStream.Size);
    AInStream.Position := 0;
  finally
    FreeAndNil(lSourceStream);
  end;

  AOutStream.Position := 0;
end;

class procedure Algoritimos.StreamDecompression(const AInStream, AOutStream: TStream);
const
  BufferSize = 4096;
var
  lOriginalStream : TDecompressionStream;
  lBuffer: array[0..BufferSize-1] of Byte;
  lSize : Int64;
begin
  AInStream.Position := 0;

  AOutStream.Size := 0;
  AOutStream.Position := 0;

  lOriginalStream := TDecompressionStream.Create(AInStream);
  try
    lSize := 1;
    while lSize <> 0 do
    begin
      lSize := lOriginalStream.Read(lBuffer, BufferSize);
      AOutStream.Write(lBuffer, lSize);
    end;
    AOutStream.Position := 0;
  finally
    FreeAndNil(lOriginalStream);
  end;
end;

end.
