unit uRpDataFlash.ComandoView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, uRpDataFlash.Command, uRpDataFlash.Types, Buttons,
  uRpDataFlash.CommandController;

type

  TfrmComandoView = class(TForm)
    ctrlTipoProcessamento: TRadioGroup;
    ctrlNomeComando: TEdit;
    lblNome: TLabel;
    pnlSelecao: TPanel;
    BitBtn1: TBitBtn;
  public
    procedure CarregarComando(const AComandos : TRpDataFlashComandList;
      out AComando : TRpDataFlashComandItem);
  end;

implementation

{$R *.dfm}

procedure TfrmComandoView.CarregarComando(const AComandos : TRpDataFlashComandList;
  out AComando : TRpDataFlashComandItem);
begin
  AComando := AComandos.Novo as TRpDataFlashComandItem;
  AComando.Nome := ctrlNomeComando.Text;
end;

end.
