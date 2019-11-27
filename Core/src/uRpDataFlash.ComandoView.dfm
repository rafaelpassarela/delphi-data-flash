object frmComandoView: TfrmComandoView
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Editor de Comando'
  ClientHeight = 391
  ClientWidth = 548
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblNome: TLabel
    Left = 8
    Top = 11
    Width = 27
    Height = 13
    Caption = 'Nome'
  end
  object ctrlTipoProcessamento: TRadioGroup
    Left = 8
    Top = 35
    Width = 530
    Height = 38
    Caption = ' Tipo Processamento '
    Columns = 3
    ItemIndex = 1
    Items.Strings = (
      'Local'
      'Ponte'
      'Somente Ponte')
    TabOrder = 0
  end
  object ctrlNomeComando: TEdit
    Left = 40
    Top = 8
    Width = 185
    Height = 21
    TabOrder = 1
  end
  object pnlSelecao: TPanel
    Left = 0
    Top = 354
    Width = 548
    Height = 37
    Align = alBottom
    TabOrder = 2
    object BitBtn1: TBitBtn
      Left = 463
      Top = 7
      Width = 75
      Height = 25
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 0
    end
  end
end
