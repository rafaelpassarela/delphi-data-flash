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
    Left = 8
    Top = 16
    Width = 70
    Height = 13
    Caption = 'Nome Servidor'
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
    Caption = 'Conectar'
    TabOrder = 2
    OnClick = ButtonConectarClick
  end
  object ButtonDesconectar: TButton
    Left = 191
    Top = 67
    Width = 100
    Height = 25
    Caption = 'Desconectar'
    Enabled = False
    TabOrder = 3
    OnClick = ButtonDesconectarClick
  end
  object MemoLog: TMemo
    Left = 8
    Top = 98
    Width = 283
    Height = 132
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object ButtonVerLog: TButton
    Left = 8
    Top = 67
    Width = 52
    Height = 25
    Caption = 'Ver Log'
    TabOrder = 5
    OnClick = ButtonVerLogClick
  end
  object LRDataFlashConexaoServerTeste: TLRDataFlashConexaoServer
    Porta = 0
    OnNovoLog = LRDataFlashConexaoServerTesteNovoLog
    OnConexaoCliente = LRDataFlashConexaoServerTesteConexaoCliente
    OnAutenticarCliente = LRDataFlashConexaoServerTesteAutenticarCliente
    UtilizarControllers = True
    ComandosSemAutenticacao = ';Comand 1;Comando 3;'
    PrefixoBaseComandos = 'TComando'
    Left = 168
    Top = 112
  end
  object DFCControllerMatematica: TLRDataFlashComandController
    Server = LRDataFlashConexaoServerTeste
    Comandos = <
      item
        Nome = 'Somar'
        Descricao = 'Somar Dois N'#250'meros'
        Parametros = <
          item
            Nome = 'A'
            Tipo = tpEntrada
            TipoValor = tvpFloat
          end
          item
            Nome = 'B'
            Tipo = tpEntrada
            TipoValor = tvpFloat
          end
          item
            Nome = 'X'
            Tipo = tpSaida
            TipoValor = tvpFloat
          end>
        OnExecute = DFCControllerMatematicaComandos0Execute
      end
      item
        Nome = 'Multiplicar'
        Descricao = 'Multiplica Dois Numeros'
        Parametros = <
          item
            Nome = 'ValA'
            Tipo = tpEntrada
            TipoValor = tvpFloat
          end
          item
            Nome = 'ValB'
            Tipo = tpEntrada
            TipoValor = tvpFloat
          end
          item
            Nome = 'Total'
            Tipo = tpSaida
            TipoValor = tvpFloat
          end>
      end>
    Grupo = 'Matematica'
    Left = 168
    Top = 144
  end
  object DFPCadastro_Pessoas: TLRDataFlashDataSetProvider
    Grupo = 'CADASTRO'
    Descricao = 'DataSet para controle da tabela de Pessoas'
    Server = LRDataFlashConexaoServerTeste
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
    Left = 168
    Top = 176
  end
  object DFCControllerManipulaTexto: TLRDataFlashComandController
    Server = LRDataFlashConexaoServerTeste
    Comandos = <
      item
        Nome = 'Concatenar'
        Descricao = 'Concatena Duas Strings'
        Parametros = <
          item
            Nome = 'StrA'
            Tipo = tpEntrada
            TipoValor = tvpString
          end
          item
            Nome = 'StrB'
            Tipo = tpEntrada
            TipoValor = tvpString
          end
          item
            Nome = 'StrC'
            Tipo = tpSaida
            TipoValor = tvpString
          end>
        OnExecute = DFCControllerManipulaTextoComandos0Execute
      end>
    Grupo = 'ManipulaTexto'
    Left = 200
    Top = 144
  end
  object DFCControllerArquivos: TLRDataFlashComandController
    Server = LRDataFlashConexaoServerTeste
    Comandos = <
      item
        Nome = 'GetFile'
        Descricao = 'Busca o arquivo informado no par'#226'metro no servidor'
        Parametros = <
          item
            Nome = 'FileName'
            Tipo = tpEntrada
            TipoValor = tvpString
          end
          item
            Nome = 'FileData'
            Tipo = tpSaida
            TipoValor = tvpFile
          end>
        OnExecute = DFCControllerArquivosComandos0Execute
      end
      item
        Nome = 'SendFile'
        Descricao = 'Envia um arquivo para o servidor'
        Parametros = <
          item
            Nome = 'FileData'
            Tipo = tpEntrada
            TipoValor = tvpFile
          end
          item
            Nome = 'FileName'
            Tipo = tpEntrada
            TipoValor = tvpString
          end
          item
            Nome = 'LocalSalvo'
            Tipo = tpSaida
            TipoValor = tvpString
          end>
        OnExecute = DFCControllerArquivosComandos1Execute
      end>
    Grupo = 'Arquivos'
    Left = 136
    Top = 144
  end
end
