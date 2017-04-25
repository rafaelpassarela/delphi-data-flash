unit fLRDF.ComandoView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, uLRDF.Comando, uLRDF.Types, Buttons, 
  uLRDF.ComandController;

type

  TfrmComandoView = class(TForm)
    ctrlTipoProcessamento: TRadioGroup;
    ctrlNomeComando: TEdit;
    lblNome: TLabel;
    pnlSelecao: TPanel;
    BitBtn1: TBitBtn;
  public
    procedure CarregarComando(const AComandos : TLRDataFlashComandList;
      out AComando : TLRDataFlashComandItem);
  end;

implementation

{$R *.dfm}

procedure TfrmComandoView.CarregarComando(const AComandos : TLRDataFlashComandList;
  out AComando : TLRDataFlashComandItem);
begin
  AComando := AComandos.Novo as TLRDataFlashComandItem;
  AComando.Nome := ctrlNomeComando.Text;
end;

end.
