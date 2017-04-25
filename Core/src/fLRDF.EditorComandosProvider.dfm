object frmEditorComandosProvider: TfrmEditorComandosProvider
  Left = 0
  Top = 0
  Caption = 'Editor de Comandos SQL'
  ClientHeight = 398
  ClientWidth = 716
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControlSQL: TPageControl
    Left = 0
    Top = 0
    Width = 716
    Height = 357
    ActivePage = TabSheetSelect
    Align = alClient
    TabOrder = 0
    object TabSheetSelect: TTabSheet
      Caption = '&Select'
      DesignSize = (
        708
        329)
      object MemoSelect: TMemo
        Left = 3
        Top = 3
        Width = 701
        Height = 323
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'MemoSelect')
        ParentFont = False
        TabOrder = 0
        OnChange = MemoSelectChange
      end
    end
    object TabSheetInsert: TTabSheet
      Caption = '&Insert'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        708
        329)
      object MemoInsert: TMemo
        Left = 3
        Top = 3
        Width = 701
        Height = 323
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'Memo1')
        ParentFont = False
        TabOrder = 0
        OnChange = MemoInsertChange
      end
    end
    object TabSheetUpdate: TTabSheet
      Caption = '&Update'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        708
        329)
      object MemoUpdate: TMemo
        Left = 3
        Top = 3
        Width = 701
        Height = 323
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'Memo1')
        ParentFont = False
        TabOrder = 0
        OnChange = MemoUpdateChange
      end
    end
    object TabSheetDelete: TTabSheet
      Caption = '&Delete'
      ImageIndex = 3
      DesignSize = (
        708
        329)
      object MemoDelete: TMemo
        Left = 3
        Top = 3
        Width = 701
        Height = 323
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'Memo1')
        ParentFont = False
        TabOrder = 0
        OnChange = MemoDeleteChange
      end
    end
  end
  object PanelBotoes: TPanel
    Left = 0
    Top = 357
    Width = 716
    Height = 41
    Align = alBottom
    TabOrder = 1
    object BitBtnOk: TBitBtn
      Left = 488
      Top = 6
      Width = 105
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
    end
    object BitBtn2: TBitBtn
      Left = 603
      Top = 6
      Width = 105
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
end
