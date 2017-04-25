unit fLRDF.EditorComandosProvider;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Buttons;

type
  TfrmEditorComandosProvider = class(TForm)
    PageControlSQL: TPageControl;
    TabSheetSelect: TTabSheet;
    TabSheetInsert: TTabSheet;
    TabSheetUpdate: TTabSheet;
    TabSheetDelete: TTabSheet;
    PanelBotoes: TPanel;
    MemoSelect: TMemo;
    MemoInsert: TMemo;
    MemoUpdate: TMemo;
    MemoDelete: TMemo;
    BitBtnOk: TBitBtn;
    BitBtn2: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure MemoSelectChange(Sender: TObject);
    procedure MemoInsertChange(Sender: TObject);
    procedure MemoUpdateChange(Sender: TObject);
    procedure MemoDeleteChange(Sender: TObject);
  private
    { Private declarations }
    procedure DoValidarConteudo(const pTabSheet : TTabSheet; const pMemo : TMemo);
  public
    { Public declarations }
  end;

var
  frmEditorComandosProvider: TfrmEditorComandosProvider;

implementation

{$R *.dfm}

{ TfrmEditorComandosProvider }

procedure TfrmEditorComandosProvider.DoValidarConteudo(
  const pTabSheet: TTabSheet; const pMemo: TMemo);
var
  lTexto : string;
begin
  lTexto := pTabSheet.Caption;
  if Pos(' [', lTexto) > 0 then
    Delete(lTexto, Pos(' [', lTexto), Length(lTexto) );

  if Trim(pMemo.Text) = EmptyStr then
    pTabSheet.Caption := lTexto + ' [ EMPTY ]'
  else
    pTabSheet.Caption := lTexto + ' [ OK ]'; 
end;

procedure TfrmEditorComandosProvider.FormShow(Sender: TObject);
begin
  PageControlSQL.ActivePage := TabSheetSelect;
  
  DoValidarConteudo(TabSheetDelete, MemoDelete);
  DoValidarConteudo(TabSheetInsert, MemoInsert);
  DoValidarConteudo(TabSheetSelect, MemoSelect);
  DoValidarConteudo(TabSheetUpdate, MemoUpdate);      
end;

procedure TfrmEditorComandosProvider.MemoDeleteChange(Sender: TObject);
begin
  DoValidarConteudo(TabSheetDelete, MemoDelete);
end;

procedure TfrmEditorComandosProvider.MemoInsertChange(Sender: TObject);
begin
  DoValidarConteudo(TabSheetInsert, MemoInsert);
end;

procedure TfrmEditorComandosProvider.MemoSelectChange(Sender: TObject);
begin
  DoValidarConteudo(TabSheetSelect, MemoSelect);
end;

procedure TfrmEditorComandosProvider.MemoUpdateChange(Sender: TObject);
begin
  DoValidarConteudo(TabSheetUpdate, MemoUpdate);
end;

end.
