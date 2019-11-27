//{$D-}
unit uRpDataFlash.ThreadConnection;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses
  Classes, Windows;

type
  TRpDataFlashThreadConnection = class(TThread)
  private
    FConexao: TComponent; //TRpDataFlashConexaoCliente;
    FConectando: Boolean;
    FExecutou : Boolean;
    function GetTerminated: Boolean;
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    constructor Create;
    destructor Destroy; override;

    property Conexao : TComponent {TRpDataFlashConexaoCliente} read FConexao write FConexao;
    property Conectando : Boolean read FConectando;
    property IsTerminated: Boolean read GetTerminated;
  end;

implementation

uses
  uRpDataFlash.Components;

{ TRpDataFlashThreadConnection }

constructor TRpDataFlashThreadConnection.Create;
begin
  inherited Create(True);
//  FreeOnTerminate := True;
end;

//procedure TRpDataFlashThreadConnection.DoTerminate;
//begin
//  inherited;
//  FConexao.FThreadConexao := nil;
//end;

destructor TRpDataFlashThreadConnection.Destroy;
begin
{$WARN SYMBOL_DEPRECATED OFF}
  if not FExecutou then
    Resume;
  inherited;
{$WARN SYMBOL_DEPRECATED ON}
end;

procedure TRpDataFlashThreadConnection.DoTerminate;
begin
  inherited;
  //  FConexao.FThreadConexao := nil;
end;

procedure TRpDataFlashThreadConnection.Execute;
var
  lConectado: Boolean;
begin
  inherited;
  if not Terminated then
  begin
    FConectando := True;
    FExecutou := True;

    repeat
      lConectado := TRpDataFlashThreadInternalAdapter(Conexao).DoInternalConectar;

      if not lConectado then
      begin
        if TRpDataFlashThreadInternalAdapter(Conexao).TimeOutConexao <= 0 then
          Sleep(1000)
        else
          Sleep(TRpDataFlashThreadInternalAdapter(Conexao).TimeOutConexao)
      end;
    until lConectado or Terminated;

    FConectando := False;
  end
  else
    FExecutou := True;
end;

function TRpDataFlashThreadConnection.GetTerminated: Boolean;
begin
  Result := Self.Terminated;
end;

end.
