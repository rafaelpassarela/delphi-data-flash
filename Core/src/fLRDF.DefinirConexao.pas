unit fLRDF.DefinirConexao;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TfrmLRDataFlashDefinirConexao = class(TForm)
    pnlConfirmacao: TPanel;
    pnlInfo: TPanel;
    lblServidor: TLabel;
    lblPorta: TLabel;
    btnSalvar: TButton;
    btnCancelar: TButton;
    edtServer: TEdit;
    edtPorta: TEdit;
    procedure btnSalvarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FPorta: Integer;
    FServer: string;
    { Private declarations }
  public
    property Porta : Integer read FPorta write FPorta;
    property Server : string read FServer write FServer;
  end;

var
  frmLRDataFlashDefinirConexao: TfrmLRDataFlashDefinirConexao;

implementation

{$R *.dfm}

procedure TfrmLRDataFlashDefinirConexao.btnSalvarClick(Sender: TObject);
begin
  if edtServer.Text = '' then
    raise Exception.Create('Servidor não pode estar em branco !');

  if StrToIntDef(edtPorta.Text, 0) = 0 then
    raise Exception.Create('Porta deve ser maior que 0 !');

  FServer := edtServer.Text;
  FPorta := StrToIntDef(edtPorta.Text, 0);
end;

procedure TfrmLRDataFlashDefinirConexao.FormShow(Sender: TObject);
begin
  edtServer.Text := FServer;
  edtPorta.Text := IntToStr( FPorta );
end;

end.
