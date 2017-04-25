unit fLRDF.EditorComandosSemAutenticacao;

interface

uses
  DesignEditors, Classes, DesignIntf, uLRDF.Comando, Windows, Messages, SysUtils,
  Variants, Graphics, Controls, Forms, Dialogs, Grids, DBGrids, DB, DBClient,
  StdCtrls, Buttons, ExtCtrls, DBCtrls;

type
  TLRDataFlashEditComandosSemAutenticacao = class(TStringProperty)
  public
    function GetAttributes : TPropertyAttributes; override;
    procedure Edit; override;
    function GetVisualValue: string; reintroduce;
  end;

  TFormComandosSemAutenticacao = class(TForm)
    DBGridComandos: TDBGrid;
    cdsComandos: TClientDataSet;
    cdsComandosCOMANDO: TStringField;
    DataSourceComandos: TDataSource;
    Panel1: TPanel;
    BitBtnOk: TBitBtn;
    BitBtnCancelar: TBitBtn;
    DBNavigator1: TDBNavigator;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure BitBtnCancelarClick(Sender: TObject);
    procedure BitBtnOkClick(Sender: TObject);
  private
    { Private declarations }
    procedure LoadFromString(const pStrings : string);
    function SaveToString : string;
  public
    { Public declarations }
  end;

var
  FormComandosSemAutenticacao: TFormComandosSemAutenticacao;

implementation

uses
  uLRDF.Component;

{$R *.dfm}

{ TLRDataFlashEditComandosSemAutenticacao }

procedure TLRDataFlashEditComandosSemAutenticacao.Edit;
begin
  if GetComponent(0) is TLRDataFlashConexaoServer then
  begin
    FormComandosSemAutenticacao := TFormComandosSemAutenticacao.Create(nil);
    try
      FormComandosSemAutenticacao.LoadFromString( TLRDataFlashConexaoServer(GetComponent(0)).ComandosSemAutenticacao );

      if FormComandosSemAutenticacao.ShowModal = mrOk then
      begin
        TLRDataFlashConexaoServer(GetComponent(0)).ComandosSemAutenticacao := FormComandosSemAutenticacao.SaveToString;
//        Value := 'Total: ' + IntToStr(FormComandosSemAutenticacao.cdsComandos.RecordCount);
      end;
    finally
      FreeAndNil(FormComandosSemAutenticacao);
    end;
  end;
end;

function TLRDataFlashEditComandosSemAutenticacao.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TLRDataFlashEditComandosSemAutenticacao.GetVisualValue: string;
begin
  inherited GetVisualValue;
  Result := 'Teste de GetVisualValue';
end;

{ TFormComandosSemAutenticacao }

procedure TFormComandosSemAutenticacao.BitBtnCancelarClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormComandosSemAutenticacao.BitBtnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormComandosSemAutenticacao.FormCreate(Sender: TObject);
begin
  cdsComandos.CreateDataSet;
end;

procedure TFormComandosSemAutenticacao.FormDestroy(Sender: TObject);
begin
  cdsComandos.EmptyDataSet;
  cdsComandos.Close;
end;

procedure TFormComandosSemAutenticacao.FormResize(Sender: TObject);
begin
  DBGridComandos.Columns[0].Width := DBGridComandos.Width - 31;
end;

procedure TFormComandosSemAutenticacao.LoadFromString(const pStrings: string);
var
  i: Integer;
  lAux: TStrings;
begin
  if not cdsComandos.Active then
    cdsComandos.Active;

  lAux := TStringList.Create;
  try
    lAux.LineBreak := ';';
    lAux.Text := pStrings;

    cdsComandos.DisableControls;
    for i := 0 to lAux.Count - 1 do
      if lAux[i] <> EmptyStr then
      begin
        cdsComandos.Append;
        cdsComandos.FieldByName('COMANDO').AsString := lAux[i];
        cdsComandos.Post;
      end;
  finally
    FreeAndNil(lAux);
    cdsComandos.EnableControls;
  end;
end;

function TFormComandosSemAutenticacao.SaveToString: string;
begin
  Result := EmptyStr;

  cdsComandos.DisableControls;
  cdsComandos.First;
  while not cdsComandos.Eof do
  begin
    Result := Result + Trim( cdsComandos.FieldByName('COMANDO').AsString ) + ';';
    cdsComandos.Next;
  end;

  if Result <> EmptyStr then
    Result := ';' + Result;
end;

end.
