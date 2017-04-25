unit uLRDF.ComandoHelper;

interface

uses
  uLRDF.Component, uLRDF.Comando, uLRDF.Types, uRpFileHelper, SysUtils, Classes,
  Windows;

type
  TLRDataFlashComandoHelper = class(TLRDataFlashComando)
  protected
    procedure DoRegistrarParametros; override;
    function DoExecutar : Boolean; override;
  end;

implementation

{ TSPITCPComandoHelperBase }

function TLRDataFlashComandoHelper.DoExecutar: Boolean;
var
  lContinuar: Boolean;
  lClass: TFileCustomObjectClass;
  lObjeto: TFileCustomObject;
  lClassName: string;
  lOperacao: TLRDataFlashHelperAction;
  lIntf : IFileBaseHelper;
  lDataComponent: TComponent;
begin
  lIntf := nil;
  lObjeto := nil;
  Result := False;
  lClassName := Parametro['ObjectClass'].AsString;
  lClass := FileClassRegistrer.GetClass(lClassName);
  if lClass = nil then
    raise Exception.CreateFmt('Classe do objeto %s não foi encontrada.', [lClassName])
  else
  begin
    lContinuar := True;
    try
      lObjeto := lClass.Create(nil);
      lObjeto.LoadFromString(Parametro['Object'].AsBase64);
      lOperacao := TLRDataFlashHelperAction(Parametro['Operacao'].AsInteger);

      if Assigned(GetServer.OnObjectRequest) then
        GetServer.OnObjectRequest(lOperacao, lObjeto, lContinuar);

      if lContinuar then
      begin
        if Supports(lObjeto, IFileBaseHelper, lIntf) then
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
            Parametro['Object'].AsBase64 := lObjeto.SaveToXmlString;
        end
        else
          raise Exception.CreateFmt('A classe "%s" não implementa a interface "IFileBaseHelper".', [lClassName]);
      end;
    finally
      if lIntf <> nil then
        lIntf := nil;

      if Assigned(lObjeto) then
        lObjeto := nil;

      if lObjeto <> nil then
        OutputDebugString('O objeto não foi liberado corretamente em TLRDataFlashComandoHelper.DoExecutar.');
    end;
  end;
end;

procedure TLRDataFlashComandoHelper.DoRegistrarParametros;
begin
  inherited;
  NovoParametro('Object', tvpBase64, True);
  NovoParametro('ObjectClass', tvpString);
  NovoParametro('Operacao', tvpInteger);
end;

initialization
  TCPClassRegistrer.Registrar(TLRDataFlashComandoHelper, C_GRUPO_INTERNO);

end.
