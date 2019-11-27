unit uRpDataFlash.CommandControllerEditor;

interface

uses
  DesignEditors;

type
  TRpDataFlashComandControllerEditor = class(TComponentEditor)
  private
    procedure EditarComandos;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TRpDataFlashComandControllerProperty = class(TComponentProperty)

  end;

implementation

uses
  SysUtils,
  uRpDataFlash.Components;

{ TRpDataFlashComandControllerEditor }

procedure TRpDataFlashComandControllerEditor.EditarComandos;
begin
  (Component as TRpDataFlashComandController).EditarComandos;
end;

procedure TRpDataFlashComandControllerEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0 : EditarComandos;
  end;
end;

function TRpDataFlashComandControllerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := 'Editar Comandos';
    else
      raise Exception.Create('Índice ' + IntToStr(Index) + ' não suportado !');
  end;
end;

function TRpDataFlashComandControllerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
