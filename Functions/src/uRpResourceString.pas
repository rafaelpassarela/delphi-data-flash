unit uRpResourceString;

interface

resourcestring
  R_DATE_FORMAT = 'dd/mm/yyyy';
  R_FIRST_UPPER_EXCPTION = 'de da das do dos a as o os na nas no nos e em para todo todos toda todas';

  R_MSG_ERROR = 'Error';
  R_MSG_WARNING = 'Warning';
  R_MSG_INFO = 'Information';
  R_MSG_QUESTION = 'Question';

  R_NO_INTERNET_CONNECTION = 'Could not find an internet connection.';
  R_NO_INTERNET_QUERY = 'The query could not be performed.';

  // Serialization
  R_SERIALIZE_FIELD_VALUE_ERROR    = 'O valor informado para o campo %s n�o � suportado.';
  R_SERIALIZE_FIELD_VALUE_ERROR_EX = 'O valor informado para o campo %s n�o � suportado (%d).';
  R_SERIALIZE_WRITE_ERROR          = 'N�o � poss�vel escrever para a classe %s. Formato desconhecido.';
  R_SERIALIZE_UNKNOWN_OBJECT_TYPE  = 'N�o � poss�vel determinar o tipo do objeto %s.';
  R_SERIALIZE_UNKNOWN_FILE_TYPE    = 'Formato de arquivo n�o identificado.';
  R_SERIALIZE_UNKNOWN_FILE_TYPE_EX = 'N�o � poss�vel realizar a escrita da classe %s. Tipo de formato n�o foi informado.';
  R_SERIALIZE_RESET_ABSTRACT       = 'M�todo "Reset" da classe "%s" n�o est� implementado.';
  R_SERIALIZE_NO_FILE_NAME         = 'File name is required.';
  R_SERIALIZE_NODE_NOT_FOUND       = 'Node not found.';
  R_SERIALIZE_CLASS_NODE_ERROR     = 'Classe de carga XML (%s) incompat�vel com o node atual (%s)!';
  R_SERIALIZE_UNKNOWN_STRING_FOTMAT= 'A string informada n�o pode ser identificada durante o load.';
  R_SERIALIZE_UNKNOWN_FILE_FORMAT  = 'O tipo do arquivo n�o foi reconhecido para efetuar load. %s%s';

  R_CLASS_NAME_NOT_REGISTERED      = 'Class %s not registered !';
  R_CLASS_NAME_INVALID             = 'Class %s is invalid !';
  R_CLASS_ID_NOT_FOUND             = 'The class identifier was not found!';

  // DataFlash
  R_DATAFLASH_CMD_NO_DESCRIPTION  = 'Function "function DoGetDescricao: string; override;" is not implemented';
  R_DATAFLASH_PROT_PREPARING_DATA = 'Preparing Message Protocol';
  R_DATAFLASH_PROT_GUID_ERROR     = 'Guid Expected: %s - Guid Received: %s';
  R_DATAFLASH_PROT_GUID_ORDER     = 'Sequence item %d not received or out of order!';

implementation

end.
