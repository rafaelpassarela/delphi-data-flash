object frmLRDataFlashDefinirConexao: TfrmLRDataFlashDefinirConexao
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Conex'#227'o'
  ClientHeight = 162
  ClientWidth = 251
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlConfirmacao: TPanel
    Left = 0
    Top = 121
    Width = 251
    Height = 41
    Align = alBottom
    TabOrder = 0
    object btnSalvar: TButton
      Left = 148
      Top = 7
      Width = 93
      Height = 25
      Caption = 'Conectar'
      ModalResult = 1
      TabOrder = 0
      OnClick = btnSalvarClick
    end
    object btnCancelar: TButton
      Left = 49
      Top = 7
      Width = 93
      Height = 25
      Caption = 'Cancelar'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object pnlInfo: TPanel
    Left = 0
    Top = 0
    Width = 251
    Height = 121
    Align = alClient
    TabOrder = 1
    object lblServidor: TLabel
      Left = 8
      Top = 9
      Width = 40
      Height = 13
      Caption = 'Servidor'
      Transparent = True
    end
    object lblPorta: TLabel
      Left = 8
      Top = 59
      Width = 26
      Height = 13
      Caption = 'Porta'
      Transparent = True
    end
    object edtServer: TEdit
      Left = 8
      Top = 32
      Width = 233
      Height = 21
      TabOrder = 0
    end
    object edtPorta: TEdit
      Left = 8
      Top = 78
      Width = 233
      Height = 21
      MaxLength = 5
      NumbersOnly = True
      TabOrder = 1
    end
  end
end
