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
    procedure CarregarComando(const AComandos : TRpDataFlashCommandList;
      out AComando : TRpDataFlashCommandItem);
  end;

implementation

{$R *.dfm}

procedure TfrmComandoView.CarregarComando(const AComandos : TRpDataFlashCommandList;
  out AComando : TRpDataFlashCommandItem);
begin
  AComando := AComandos.Novo as TRpDataFlashCommandItem;
  AComando.Name := ctrlNomeComando.Text;
end;

end.
