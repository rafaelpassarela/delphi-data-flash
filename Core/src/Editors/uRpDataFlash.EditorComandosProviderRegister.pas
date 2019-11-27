unit uRpDataFlash.EditorComandosProviderRegister;

interface

uses
  DesignEditors;

type
  TRpDataFlashEditorComandosProvider = class(TComponentEditor)
  private
    procedure EditarProvider;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

uses
  SysUtils,
  uRpDataFlash.DataSetProvider;

{ TRpDataFlashEditorComandosProvider }

procedure TRpDataFlashEditorComandosProvider.EditarProvider;
begin
  (Component as TRpDataFlashDataSetProvider).EditarProvider;
end;

procedure TRpDataFlashEditorComandosProvider.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0 : EditarProvider;
  end;
end;

function TRpDataFlashEditorComandosProvider.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := 'Editar Provider';
    else
      raise Exception.Create('Índice ' + IntToStr(Index) + ' não suportado !');
  end;
end;

function TRpDataFlashEditorComandosProvider.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
