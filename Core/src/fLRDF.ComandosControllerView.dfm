object frmComandosControllerView: TfrmComandosControllerView
  Left = 0
  Top = 0
  Caption = 'Controlador de Comandos'
  ClientHeight = 413
  ClientWidth = 516
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ctrlComandos: TListView
    Left = 0
    Top = 41
    Width = 516
    Height = 372
    Align = alClient
    BorderStyle = bsNone
    Columns = <
      item
        Caption = 'Nome'
        Width = 300
      end
      item
        Caption = 'Tipo'
        Width = 150
      end>
    TabOrder = 0
    ViewStyle = vsReport
    ExplicitWidth = 471
  end
  object pnlComandos: TPanel
    Left = 0
    Top = 0
    Width = 516
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 471
    object btnAdicionar: TBitBtn
      Left = 16
      Top = 10
      Width = 57
      Height = 25
      Caption = '+'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 0
      OnClick = btnAdicionarClick
    end
    object btnExcluir: TBitBtn
      Left = 79
      Top = 10
      Width = 57
      Height = 25
      Caption = '-'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 1
    end
    object BitBtn1: TBitBtn
      Left = 184
      Top = 10
      Width = 75
      Height = 25
      Caption = 'BitBtn1'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 2
    end
  end
end
