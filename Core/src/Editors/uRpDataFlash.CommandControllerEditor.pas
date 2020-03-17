unit uRpDataFlash.CommandControllerEditor;

interface

uses
  DesignEditors;

type
  TRpDataFlashComandControllerEditor = class(TComponentEditor)
  private
    procedure EditCommands;
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

procedure TRpDataFlashComandControllerEditor.EditCommands;
begin
  (Component as TRpDataFlashCommandController).EditCommands;
end;

procedure TRpDataFlashComandControllerEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0 : EditCommands;
  end;
end;

function TRpDataFlashComandControllerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := 'Edit Commands';
    else
      raise Exception.Create('Índice ' + IntToStr(Index) + ' não suportado !');
  end;
end;

function TRpDataFlashComandControllerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
