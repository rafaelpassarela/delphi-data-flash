object DataModuleServer: TDataModuleServer
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 254
  Width = 408
  object IBDatabaseServico: TIBDatabase
    DatabaseName = 
      'LOCALHOST:C:\Rafael\CompTCPIP\ComunicacaoTCPIP\Demos\TESTEDATAFL' +
      'ASH.FDB'
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    LoginPrompt = False
    ServerType = 'IBServer'
    SQLDialect = 1
    Left = 48
    Top = 24
  end
  object IBTransactionServico: TIBTransaction
    DefaultDatabase = IBDatabaseServico
    Left = 48
    Top = 88
  end
end
