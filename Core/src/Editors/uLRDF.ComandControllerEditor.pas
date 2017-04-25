unit uLRDF.ComandControllerEditor;

interface

uses
  DesignEditors;

type
  TLRDataFlashComandControllerEditor = class(TComponentEditor)
  private
    procedure EditarComandos;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TLRDataFlashComandControllerProperty = class(TComponentProperty)

  end;

implementation

uses
  SysUtils,
  uLRDF.Component;

{ TLRDataFlashComandControllerEditor }

procedure TLRDataFlashComandControllerEditor.EditarComandos;
begin
  (Component as TLRDataFlashComandController).EditarComandos;
end;

procedure TLRDataFlashComandControllerEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0 : EditarComandos;
  end;
end;

function TLRDataFlashComandControllerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := 'Editar Comandos';
    else
      raise Exception.Create('Índice ' + IntToStr(Index) + ' não suportado !');
  end;
end;

function TLRDataFlashComandControllerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
