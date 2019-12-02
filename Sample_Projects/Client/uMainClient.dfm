object FormMainClient: TFormMainClient
  Left = 0
  Top = 0
  Caption = 'Client'
  ClientHeight = 477
  ClientWidth = 907
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    907
    477)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelNomeServer: TLabel
    Left = 8
    Top = 16
    Width = 76
    Height = 13
    Caption = 'Server Name/IP'
  end
  object LabelPorta: TLabel
    Left = 64
    Top = 43
    Width = 20
    Height = 13
    Caption = 'Port'
  end
  object LabelFiltro: TLabel
    Left = 421
    Top = 188
    Width = 62
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Default Filter'
  end
  object LabelUser: TLabel
    Left = 32
    Top = 70
    Width = 52
    Height = 13
    Caption = 'User / Pwd'
  end
  object Gauge1: TGauge
    Left = 299
    Top = 449
    Width = 417
    Height = 18
    Anchors = [akLeft, akRight, akBottom]
    Progress = 0
    ExplicitTop = 446
  end
  object LabelStatus: TLabel
    Left = 299
    Top = 432
    Width = 56
    Height = 13
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'LabelStatus'
  end
  object EditNomeServer: TEdit
    Left = 86
    Top = 13
    Width = 207
    Height = 21
    TabOrder = 0
    Text = 'LOCALHOST'
  end
  object EditPorta: TEdit
    Left = 86
    Top = 40
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '7200'
  end
  object ButtonVerLog: TButton
    Left = 8
    Top = 99
    Width = 70
    Height = 25
    Caption = 'Log'
    TabOrder = 2
    OnClick = ButtonVerLogClick
  end
  object ButtonConectar: TButton
    Left = 84
    Top = 99
    Width = 100
    Height = 25
    Caption = 'Connect'
    TabOrder = 3
    OnClick = ButtonConectarClick
  end
  object ButtonDesconectar: TButton
    Left = 191
    Top = 99
    Width = 100
    Height = 25
    Caption = 'Disconnect'
    Enabled = False
    TabOrder = 4
    OnClick = ButtonDesconectarClick
  end
  object MemoLog: TMemo
    Left = 8
    Top = 128
    Width = 285
    Height = 341
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'select ID, NOME, IDADE, DATA_CADASTRO, SALARIO'
      'from PESSOAS'
      'group by ID, NOME, IDADE, DATA_CADASTRO, '
      'SALARIO')
    TabOrder = 5
  end
  object ScrollBoxComandos: TScrollBox
    Left = 722
    Top = 0
    Width = 185
    Height = 477
    Align = alRight
    TabOrder = 6
    object ButtonSomarProxy: TButton
      AlignWithMargins = True
      Left = 8
      Top = 4
      Width = 165
      Height = 25
      Margins.Left = 8
      Margins.Top = 4
      Margins.Right = 8
      Margins.Bottom = 4
      Align = alTop
      Caption = 'Somar (Proxy)'
      TabOrder = 0
      OnClick = ButtonSomarProxyClick
    end
    object ButtonSomarExecutor: TButton
      AlignWithMargins = True
      Left = 8
      Top = 37
      Width = 165
      Height = 25
      Margins.Left = 8
      Margins.Top = 4
      Margins.Right = 8
      Margins.Bottom = 4
      Align = alTop
      Caption = 'Somar (Executor)'
      TabOrder = 1
      OnClick = ButtonSomarExecutorClick
    end
    object ButtonInverter: TButton
      AlignWithMargins = True
      Left = 8
      Top = 70
      Width = 165
      Height = 25
      Margins.Left = 8
      Margins.Top = 4
      Margins.Right = 8
      Margins.Bottom = 4
      Align = alTop
      Caption = 'Inverter (Proxy)'
      TabOrder = 2
      OnClick = ButtonInverterClick
    end
    object ButtonGetFile: TButton
      AlignWithMargins = True
      Left = 8
      Top = 136
      Width = 165
      Height = 25
      Margins.Left = 8
      Margins.Top = 4
      Margins.Right = 8
      Margins.Bottom = 4
      Align = alTop
      Caption = 'Request File'
      TabOrder = 3
      OnClick = ButtonGetFileClick
    end
    object ButtonSendFile: TButton
      AlignWithMargins = True
      Left = 8
      Top = 103
      Width = 165
      Height = 25
      Margins.Left = 8
      Margins.Top = 4
      Margins.Right = 8
      Margins.Bottom = 4
      Align = alTop
      Caption = 'Send File'
      TabOrder = 4
      OnClick = ButtonSendFileClick
    end
  end
  object DBGrid1: TDBGrid
    Left = 300
    Top = 13
    Width = 416
    Height = 164
    Anchors = [akTop, akRight]
    DataSource = DataSource1
    TabOrder = 7
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object ButtonOpenDS: TButton
    Left = 300
    Top = 183
    Width = 112
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Open DataSet'
    TabOrder = 8
    OnClick = ButtonOpenDSClick
  end
  object EditFiltro: TEdit
    Left = 507
    Top = 185
    Width = 209
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 9
  end
  object ButtonCloseDS: TButton
    Left = 300
    Top = 214
    Width = 112
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Close DataSet'
    TabOrder = 10
    OnClick = ButtonCloseDSClick
  end
  object DBNavigatorPessoas: TDBNavigator
    Left = 421
    Top = 214
    Width = 240
    Height = 25
    DataSource = DataSource1
    Anchors = [akTop, akRight]
    TabOrder = 11
  end
  object ButtonCommit: TButton
    Left = 300
    Top = 245
    Width = 112
    Height = 25
    Caption = 'Commit'
    TabOrder = 12
    OnClick = ButtonCommitClick
  end
  object ButtonXMLData: TButton
    Left = 421
    Top = 245
    Width = 75
    Height = 25
    Caption = 'XMLData'
    TabOrder = 13
    OnClick = ButtonXMLDataClick
  end
  object WebBrowser1: TWebBrowser
    Left = 300
    Top = 276
    Width = 416
    Height = 149
    TabOrder = 14
    ControlData = {
      4C000000FF2A0000660F00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object EditUser: TEdit
    Left = 86
    Top = 67
    Width = 85
    Height = 21
    TabOrder = 15
    Text = 'TESTE'
  end
  object EditSenha: TEdit
    Left = 177
    Top = 67
    Width = 85
    Height = 21
    TabOrder = 16
    Text = '1234'
  end
  object RpDataFlashClientConnectionTeste: TRpDataFlashClientConnection
    Port = 7200
    Server = 'LOCALHOST'
    UserName = 'TESTE'
    Password = '1234'
    OnNewLog = RpDataFlashClientConnectionTesteNewLog
    OnStatus = RpDataFlashClientConnectionTesteStatus
    Left = 192
    Top = 24
  end
  object DataSource1: TDataSource
    DataSet = RpDataFlashDataSetPessoas
    Left = 224
    Top = 88
  end
  object RpDataFlashDataSetFormatter1: TRpDataFlashDataSetFormatter
    DateMask = 'dd.mm.yyyy'
    DateTimeMask = 'dd.mm.yyyy hh:nn:ss'
    TimeMask = 'hh:nn:ss:zzz'
    BoolFalse = 'F'
    BoolTrue = 'T'
    QuoteChr = #39
    Left = 224
    Top = 56
  end
  object RpDataFlashCommandExecutorSomar: TRpDataFlashCommandExecutor
    ConexaoCliente = RpDataFlashClientConnectionTeste
    Command = 'AddNum'
    Params = <
      item
        Name = 'A'
        TipoValor = tvpFloat
      end
      item
        Name = 'B'
        TipoValor = tvpFloat
      end>
    ResultParams = <
      item
        Name = 'X'
        TipoValor = tvpFloat
      end>
    Left = 720
    Top = 40
  end
  object RpDataFlashDataSetPessoas: TRpDataFlashDataSet
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'ID'
        Attributes = [faRequired, faUnNamed]
        DataType = ftInteger
      end
      item
        Name = 'NOME'
        Attributes = [faUnNamed]
        DataType = ftWideString
        Size = 30
      end
      item
        Name = 'IDADE'
        Attributes = [faUnNamed]
        DataType = ftInteger
      end
      item
        Name = 'DATA_CADASTRO'
        Attributes = [faUnNamed]
        DataType = ftDateTime
      end
      item
        Name = 'SALARIO'
        Attributes = [faUnNamed]
        DataType = ftFloat
      end>
    IndexDefs = <>
    StoreDefs = True
    ClientConnection = RpDataFlashClientConnectionTeste
    ProviderClass = 'DFPCadastro_Pessoas'
    Params = <>
    Formatter = RpDataFlashDataSetFormatter1
    Left = 224
    Top = 24
    object RpDataFlashDataSetPessoasID: TIntegerField
      FieldName = 'ID'
      Origin = 'PESSOAS.ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object RpDataFlashDataSetPessoasNOME: TWideStringField
      FieldName = 'NOME'
      Origin = 'PESSOAS.NOME'
      Size = 30
    end
    object RpDataFlashDataSetPessoasIDADE: TIntegerField
      FieldName = 'IDADE'
      Origin = 'PESSOAS.IDADE'
    end
    object RpDataFlashDataSetPessoasDATA_CADASTRO: TDateTimeField
      FieldName = 'DATA_CADASTRO'
      Origin = 'PESSOAS.DATA_CADASTRO'
    end
    object RpDataFlashDataSetPessoasSALARIO: TFloatField
      FieldName = 'SALARIO'
      Origin = 'PESSOAS.SALARIO'
    end
  end
end
