object FormMainServer: TFormMainServer
  Left = 0
  Top = 0
  Caption = 'Server'
  ClientHeight = 238
  ClientWidth = 299
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    299
    238)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelNomeServer: TLabel
    Left = 16
    Top = 16
    Width = 62
    Height = 13
    Caption = 'Server Name'
  end
  object LabelPorta: TLabel
    Left = 52
    Top = 43
    Width = 26
    Height = 13
    Caption = 'Porta'
  end
  object EditNomeServer: TEdit
    Left = 84
    Top = 13
    Width = 207
    Height = 21
    ReadOnly = True
    TabOrder = 0
    Text = 'LOCALHOST'
  end
  object EditPorta: TEdit
    Left = 84
    Top = 40
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '7200'
  end
  object ButtonConectar: TButton
    Left = 84
    Top = 67
    Width = 100
    Height = 25
    Caption = 'Connect'
    TabOrder = 4
    OnClick = ButtonConectarClick
  end
  object ButtonDesconectar: TButton
    Left = 191
    Top = 67
    Width = 100
    Height = 25
    Caption = 'Disconnect'
    Enabled = False
    TabOrder = 5
    OnClick = ButtonDesconectarClick
  end
  object MemoLog: TMemo
    Left = 8
    Top = 98
    Width = 283
    Height = 132
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 6
  end
  object ButtonVerLog: TButton
    Left = 8
    Top = 67
    Width = 70
    Height = 25
    Caption = 'Log'
    TabOrder = 3
    OnClick = ButtonVerLogClick
  end
  object CheckBoxAuthCli: TCheckBox
    Left = 211
    Top = 42
    Width = 80
    Height = 17
    Caption = 'Auth Client'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = CheckBoxAuthCliClick
  end
  object RpDataFlashServerConnectionTeste: TRpDataFlashServerConnection
    ConfigREST.Enabled = True
    OnNewLog = RpDataFlashServerConnectionTesteNewLog
    OnClientConnection = RpDataFlashServerConnectionTesteClientConnection
    OnAuthenticateClient = RpDataFlashServerConnectionTesteAuthenticateClient
    BaseCommandPrefix = 'TComando'
    Left = 72
    Top = 112
  end
  object DFCControllerMath: TRpDataFlashCommandController
    Server = RpDataFlashServerConnectionTeste
    Commands = <
      item
        Name = 'AddNum'
        Description = 'Adds two numbers'
        Params = <
          item
            Name = 'A'
            Tipo = tpInput
            TipoValor = tvpFloat
          end
          item
            Name = 'B'
            Tipo = tpInput
            TipoValor = tvpFloat
          end
          item
            Name = 'X'
            Tipo = tpOutput
            TipoValor = tvpFloat
          end>
        OnExecute = DFCControllerMathCommands0Execute
      end
      item
        Name = 'Multiply'
        Description = 'Multiply two numbers'
        Params = <
          item
            Name = 'A'
            Tipo = tpInput
            TipoValor = tvpFloat
          end
          item
            Name = 'B'
            Tipo = tpInput
            TipoValor = tvpFloat
          end
          item
            Name = 'C'
            Tipo = tpOutput
            TipoValor = tvpFloat
          end>
        OnExecute = DFCControllerMathCommands1Execute
      end>
    GroupName = 'MathAndText'
    Left = 136
    Top = 176
  end
  object DFPCadastro_Pessoas: TRpDataFlashDataSetProvider
    GroupName = 'UNNAMED'
    Description = 'Persons DataBase Table'
    Server = RpDataFlashServerConnectionTeste
    InsertSQL.Strings = (
      'insert into PESSOAS (ID, NOME, IDADE, DATA_CADASTRO, SALARIO)'
      'values (:ID, :NOME, :IDADE, :DATA_CADASTRO, :SALARIO)')
    UpdateSQL.Strings = (
      'update PESSOAS'
      'set ID = :ID,'
      '    NOME = :NOME,'
      '    IDADE = :IDADE,'
      '    DATA_CADASTRO = :DATA_CADASTRO,'
      '    SALARIO = :SALARIO'
      'where (ID = :OLD_ID)')
    DeleteSQL.Strings = (
      'delete from PESSOAS'
      'where (ID = :ID)')
    SelectSQL.Strings = (
      'select ID, NOME, IDADE, DATA_CADASTRO, SALARIO'
      'from PESSOAS')
    Left = 72
    Top = 144
  end
  object DFCControllerManipulaTexto: TRpDataFlashCommandController
    Server = RpDataFlashServerConnectionTeste
    Commands = <
      item
        Name = 'TextInverter'
        Description = 'Invert Text'
        Params = <
          item
            Name = 'Text'
            Tipo = tpInput
            TipoValor = tvpString
          end
          item
            Name = 'Inverted'
            Tipo = tpOutput
            TipoValor = tvpString
          end>
        OnExecute = DFCControllerManipulaTextoCommands0Execute
      end>
    GroupName = 'MathAndText'
    Left = 104
    Top = 176
  end
  object DFCControllerFiles: TRpDataFlashCommandController
    Server = RpDataFlashServerConnectionTeste
    Commands = <
      item
        Name = 'SendFile'
        Description = 'Send File to Server'
        Params = <
          item
            Name = 'FileName'
            Tipo = tpInput
            TipoValor = tvpString
          end
          item
            Name = 'FileData'
            Tipo = tpInput
            TipoValor = tvpBase64
          end
          item
            Name = 'SavePath'
            Tipo = tpOutput
            TipoValor = tvpString
          end>
        OnExecute = DFCControllerFilesCommands0Execute
      end
      item
        Name = 'LoadFile'
        Description = 'Load a file from Server'
        Params = <
          item
            Name = 'FileName'
            Tipo = tpInput
            TipoValor = tvpString
          end
          item
            Name = 'FileData'
            Tipo = tpOutput
            TipoValor = tvpBase64
          end>
        OnExecute = DFCControllerFilesCommands1Execute
      end>
    GroupName = 'FileManager'
    Left = 72
    Top = 176
  end
  object ApplicationEvents1: TApplicationEvents
    OnException = ApplicationEvents1Exception
    Left = 16
    Top = 112
  end
end
