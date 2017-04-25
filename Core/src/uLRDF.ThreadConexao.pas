//{$D-}
unit uLRDF.ThreadConexao;

{$I ..\..\Common\src\RpInc.inc}

interface

uses
  Classes;

type
  TThreadConexao = class(TThread)
  private
    FConexao: TComponent; //TLRDataFlashConexaoCliente;
    FConectando: Boolean;
    FExecutou : Boolean;
    function GetTerminated: Boolean;
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    constructor Create;
    destructor Destroy; override;

    property Conexao : TComponent {TLRDataFlashConexaoCliente} read FConexao write FConexao;
    property Conectando : Boolean read FConectando;
    property IsTerminated: Boolean read GetTerminated;
  end;

implementation

uses
  uLRDF.Component, Windows;

{ TThreadConexao }

constructor TThreadConexao.Create;
begin
  inherited Create(True);
//  FreeOnTerminate := True;
end;

//procedure TThreadConexao.DoTerminate;
//begin
//  inherited;
//  FConexao.FThreadConexao := nil;
//end;

destructor TThreadConexao.Destroy;
begin
{$WARN SYMBOL_DEPRECATED OFF}
  if not FExecutou then
    Resume;
  inherited;
{$WARN SYMBOL_DEPRECATED ON}
end;

procedure TThreadConexao.DoTerminate;
begin
  inherited;
  //  FConexao.FThreadConexao := nil;
end;

procedure TThreadConexao.Execute;
var
  lConectado: Boolean;
begin
  inherited;
  if not Terminated then
  begin
    FConectando := True;
    FExecutou := True;

    repeat
      lConectado := TLRDataFlashThreadInternalAdapter(Conexao).DoInternalConectar;

      if not lConectado then
      begin
        if TLRDataFlashThreadInternalAdapter(Conexao).TimeOutConexao <= 0 then
          Sleep(1000)
        else
          Sleep(TLRDataFlashThreadInternalAdapter(Conexao).TimeOutConexao)
      end;
    until lConectado or Terminated;

    FConectando := False;
  end
  else
    FExecutou := True;
end;

function TThreadConexao.GetTerminated: Boolean;
begin
  Result := Self.Terminated;
end;

end.
