unit uRpDataFlash.CommandHelper;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses
  uRpDataFlash.Components, uRpDataFlash.Command, uRpDataFlash.Types,
  uRpSerialization, SysUtils, Classes, Windows;

type
  TLRDataFlashComandoHelper = class(TRpDataFlashCommand)
  protected
    procedure DoRegisterParams; override;
    function DoExecute : Boolean; override;
  end;

implementation

{ TSPITCPComandoHelperBase }

function TLRDataFlashComandoHelper.DoExecute: Boolean;
var
  lContinuar: Boolean;
  lClass: TCustomSerializableObjectClass;
  lObjeto: TCustomSerializableObject;
  lClassName: string;
  lOperacao: TRpDataFlashHelperAction;
  lIntf : ISerializableBaseHelper;
  lDataComponent: TComponent;
begin
  lIntf := nil;
  lObjeto := nil;
  Result := False;
  lClassName := Param['ObjectClass'].AsString;
  lClass := SerializationClassRegistrer.GetClass(lClassName);
  if lClass = nil then
    raise Exception.CreateFmt('Classe do objeto %s n�o foi encontrada.', [lClassName])
  else
  begin
    lContinuar := True;
    try
      lObjeto := lClass.Create(nil);
      lObjeto.LoadFromString(Param['Object'].AsBase64);
      lOperacao := TRpDataFlashHelperAction(Param['Operacao'].AsInteger);

      if Assigned(GetServer.OnObjectRequest) then
        GetServer.OnObjectRequest(lOperacao, lObjeto, lContinuar);

      if lContinuar then
      begin
        if Supports(lObjeto, ISerializableBaseHelper, lIntf) then
        begin
          if Assigned(Executor) then
            lDataComponent := Executor.GetDataComponent
          else
            lDataComponent := nil;

          case lOperacao of
            haSave:    Result := lIntf.DoSave(lDataComponent);
            haLoad:    Result := lIntf.DoLoad(lDataComponent);
            haDelete:  Result := lIntf.DoDelete(lDataComponent);
            haExecute: Result := lIntf.DoExecute(lDataComponent);
          end;

          if Result then
            Param['Object'].AsBase64 := lObjeto.SaveToXmlString;
        end
        else
          raise Exception.CreateFmt('A classe "%s" n�o implementa a interface "IFileBaseHelper".', [lClassName]);
      end;
    finally
      if lIntf <> nil then
        lIntf := nil;

      if Assigned(lObjeto) then
        lObjeto := nil;

      if lObjeto <> nil then
        OutputDebugString('O objeto n�o foi liberado corretamente em TLRDataFlashComandoHelper.DoExecutar.');
    end;
  end;
end;

procedure TLRDataFlashComandoHelper.DoRegisterParams;
begin
  inherited;
  NewParam('Object', tvpBase64, True);
  NewParam('ObjectClass', tvpString);
  NewParam('Operacao', tvpInteger);
end;

initialization
  TCPClassRegistrer.Registrar(TLRDataFlashComandoHelper, C_GROUP_INTERNAL);

end.
