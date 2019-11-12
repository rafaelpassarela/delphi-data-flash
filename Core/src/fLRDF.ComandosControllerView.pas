unit fLRDF.ComandosControllerView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Buttons, fLRDF.ComandoView,
  uRpDataFlash.CommandController;

type
  TfrmComandosControllerView = class(TForm)
    ctrlComandos: TListView;
    pnlComandos: TPanel;
    btnAdicionar: TBitBtn;
    btnExcluir: TBitBtn;
    BitBtn1: TBitBtn;
    procedure btnAdicionarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FComandos: TLRDataFlashComandList;
  public
    property Comandos : TLRDataFlashComandList read FComandos write FComandos;
    procedure AtualizarListaComandos;
  end;

implementation

{$R *.dfm}

procedure TfrmComandosControllerView.AtualizarListaComandos;
var
  lIterator: TLRDataFlashComandList.TIteratorComand;
  lAtual: TLRDataFlashComandItemBase;
begin
  ctrlComandos.Clear;
  if FComandos <> nil then
  begin
    lIterator := FComandos.Iterator;
    try
      while lIterator.TemProximo do
      begin
        lAtual := lIterator.Proximo;
        ctrlComandos.AddItem(lAtual.Nome, lAtual);
      end;
    finally
      FreeAndNil(lIterator);
    end;
  end;
end;

procedure TfrmComandosControllerView.btnAdicionarClick(Sender: TObject);
var
  lView: TfrmComandoView;
  lComando: TLRDataFlashComandItem;
begin
  lView := TfrmComandoView.Create(Self);
  if lView.ShowModal = mrOk then
  begin
    lView.CarregarComando(FComandos, lComando);
    ctrlComandos.AddItem(lComando.Nome, lComando);
  end;
//  AtualizarListaComandos;
  lView.Free;
end;

procedure TfrmComandosControllerView.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmComandosControllerView.FormShow(Sender: TObject);
begin
  AtualizarListaComandos;
end;

end.
