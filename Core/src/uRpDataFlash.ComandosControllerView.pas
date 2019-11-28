unit uRpDataFlash.ComandosControllerView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Buttons, uRpDataFlash.ComandoView,
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
    FCommands: TRpDataFlashCommandList;
  public
    property Commands : TRpDataFlashCommandList read FCommands write FCommands;
    procedure AtualizarListaComandos;
  end;

implementation

{$R *.dfm}

procedure TfrmComandosControllerView.AtualizarListaComandos;
var
  lIterator: TRpDataFlashCommandList.TIteratorComand;
  lAtual: TRpDataFlashCommandItemBase;
begin
  ctrlComandos.Clear;
  if FCommands <> nil then
  begin
    lIterator := FCommands.Iterator;
    try
      while lIterator.TemProximo do
      begin
        lAtual := lIterator.Proximo;
        ctrlComandos.AddItem(lAtual.Name, lAtual);
      end;
    finally
      FreeAndNil(lIterator);
    end;
  end;
end;

procedure TfrmComandosControllerView.btnAdicionarClick(Sender: TObject);
var
  lView: TfrmComandoView;
  lComando: TRpDataFlashCommandItem;
begin
  lView := TfrmComandoView.Create(Self);
  if lView.ShowModal = mrOk then
  begin
    lView.CarregarComando(FCommands, lComando);
    ctrlComandos.AddItem(lComando.Name, lComando);
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
