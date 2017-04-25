unit uLRDF.EditorComandosProvider;

interface

uses
  DesignEditors;

type
  TLRDataFlashEditorComandosProvider = class(TComponentEditor)
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
  uLRDF.DataSetProvider;

{ TLRDataFlashEditorComandosProvider }

procedure TLRDataFlashEditorComandosProvider.EditarProvider;
begin
  (Component as TLRDataFlashDataSetProvider).EditarProvider;
end;

procedure TLRDataFlashEditorComandosProvider.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0 : EditarProvider;
  end;
end;

function TLRDataFlashEditorComandosProvider.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := 'Editar Provider';
    else
      raise Exception.Create('Índice ' + IntToStr(Index) + ' não suportado !');
  end;
end;

function TLRDataFlashEditorComandosProvider.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
