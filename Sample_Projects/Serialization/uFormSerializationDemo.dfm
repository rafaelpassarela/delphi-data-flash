object FormSerializationDemo: TFormSerializationDemo
  Left = 0
  Top = 0
  Caption = 'Serialization Demo'
  ClientHeight = 242
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    527
    242)
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonSerialize: TButton
    Left = 8
    Top = 8
    Width = 121
    Height = 25
    Caption = 'Serialize'
    TabOrder = 0
    OnClick = ButtonSerializeClick
  end
  object MemoObject: TMemo
    Left = 135
    Top = 8
    Width = 384
    Height = 226
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'MemoObject')
    TabOrder = 1
  end
  object CheckBoxIncludeClassName: TCheckBox
    Left = 8
    Top = 39
    Width = 121
    Height = 17
    Caption = 'IncludeClassName'
    TabOrder = 2
  end
  object RadioGroupType: TRadioGroup
    Left = 8
    Top = 62
    Width = 121
    Height = 51
    Caption = 'Serialization Type'
    Columns = 2
    ItemIndex = 1
    Items.Strings = (
      'XML'
      'JSON')
    TabOrder = 3
  end
  object ButtonLoad: TButton
    Left = 8
    Top = 208
    Width = 121
    Height = 26
    Hint = '(Json -> XML / XML -> Json)'
    Caption = 'Load From String'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = ButtonLoadClick
  end
end
