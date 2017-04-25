unit uLRDF.ObjectReg;

interface

uses
  Classes, SysUtils, Contnrs, XMLIntf, XMLDoc, Dialogs, Forms, Variants,
  uRpFileHelper, uRpAlgoritmos, uLRDF.Types, uLRDF.Comando;

type
  TPropType = (ptNone, ptNative, ptSet, ptEnum, ptClass);

  TPropInfo = packed record
  strict private
    FParent: string;
    FDataType: string;
    FPropType: TPropType;
    FName: string;
    FExtraInfo: string;
    procedure Reset;
  public
    property Parent : string read FParent write FParent;
    property DataType : string read FDataType write FDataType;
    property PropType : TPropType read FPropType write FPropType;
    property Name : string read FName write FName;
    property ExtraInfo : string read FExtraInfo write FExtraInfo;

    procedure Decode(const AValue : IXMLNode);
  end;

  TImplementationPart = class
  private
    FNome : string;
    FDependencies : TStringList;
    FScope : TStringList;
    FImplement: TStringList;
    FGerado : Boolean;
    FParent: string;
    FIsList: Boolean;
    FItemClassName: string;
    procedure GenerateScope(const AClassNode : IXMLNode);
    procedure GenerateImplement(const AClassNode : IXMLNode);
    procedure DoGenerateFromAndToNode(const AClassNode : IXMLNode);
    procedure DoGenerateReset(const AClassNode : IXMLNode);
    procedure DoGenerateFromOther(const AClassNode : IXMLNode);
    procedure DoGenerateListMethods;
  public
    constructor Create;
    destructor Destroy; override;

    property Dependencies : TStringList read FDependencies write FDependencies;
    property Scope : TStringList read FScope write FScope;
    property Implement : TStringList read FImplement write FImplement;
    property Gerado : Boolean read FGerado write FGerado;
    property Nome : string read FNome write FNome;
    property Parent : string read FParent write FParent;
    property IsList : Boolean read FIsList write FIsList;
    property ItemClassName : string read FItemClassName write FItemClassName;

    procedure GenerateCode(const AClassNode : IXMLNode);
  end;

  TImplementationList = class(TObjectList)
  private
    function GetItem(Index: Integer): TImplementationPart;
  public
    property Items[Index: Integer]: TImplementationPart read GetItem; default;

    function NewItem(const ANome, AParent : string) : TImplementationPart;
    function FindByName(const AClassName : string) : TImplementationPart;
    procedure AddEnumOrSet(const AName : string; const AValue : string);
  end;

  TLRDataFlashClassSerialization = class
  private
    FUnit : TStringList;
    FClassList : TImplementationList;
    FXmlMetadata : IXMLDocument;
    procedure AddUnitComent;
    procedure AdicionarScopo(const AClassItem : TImplementationPart);
    procedure AdicionarImplementacao(const AClassItem : TImplementationPart);
    procedure VerificarComando(const ACommandClass : TLRDataFlashComandoClass; var AGerados : string);
    function GetXMLFromObject(const AObject : TFileCustomObject) : string;
    function DoLoadFromRegistry(const AGerados : string; const ACheckIntf : Boolean) : Boolean;
    function IsHelperSupport(const ABaseClass : TFileCustomObjectClass) : Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    // prepara o XML em uma declaração de classe
    function DecodeXML(const AXML : string) : Boolean; overload;
    function DecodeXML(const ANode : IXMLNode; const AIsCompressed : Boolean) : Boolean; overload;
    // transforma uma classe em XML
    function DecodeClass(const AClass : TFileCustomObjectClass; var AClassesJaGeradas : string;
      const AClassName : string) : Boolean;

    function GetImplementedUnit(const AUnitName : string) : string;
    function GetClassMetadata : string;
    function GetErrorUnit(const AErrorMens : string) : string;

    // percorre todas as classes registradas e gera um XML unificado
    function LoadXMLFromRegistry : Boolean;
    // verifica as classes utilizadas nos comandos
    function LoadXMLFromProxyCommands(const ASelectedList : TStringList) : Boolean;
    // pega o XML unificado e separa nas devidas classes contidas nele
    function LoadClassFromMetadata(const AXMLString : string) : Boolean;
  end;

implementation

{ TSPITCPClassDeSerialization }

procedure TLRDataFlashClassSerialization.AdicionarImplementacao(
  const AClassItem: TImplementationPart);
begin
  if AClassItem.Implement.Count > 0 then
  begin
    FUnit.Add(TrimRight(AClassItem.Implement.Text));
    FUnit.Add('');
  end;
end;

procedure TLRDataFlashClassSerialization.AdicionarScopo(const AClassItem: TImplementationPart);
var
  i: Integer;
  lDependencia: TImplementationPart;
begin
  // verifica por dependencias
  for i := 0 to AClassItem.Dependencies.Count - 1 do
  begin
    lDependencia := FClassList.FindByName(AClassItem.Dependencies[i]);
    if Assigned(lDependencia) and (not lDependencia.Gerado) then
      AdicionarScopo(lDependencia);
  end;

  if not AClassItem.Gerado then
  begin
    FUnit.Add(TrimRight(AClassItem.Scope.Text));
    AClassItem.Gerado := True;
  end;
end;

constructor TLRDataFlashClassSerialization.Create;
begin
  FClassList := TImplementationList.Create;

  FXmlMetadata := TXMLDocument.Create(nil);
  FXmlMetadata.Active := True;
  FXmlMetadata.Encoding := 'iso-8859-1';
  FXmlMetadata.Version := '1.0';

  FUnit := TStringList.Create;
end;

function TLRDataFlashClassSerialization.DecodeClass(const AClass: TFileCustomObjectClass;
  var AClassesJaGeradas : string; const AClassName : string): Boolean;
var
  i: Integer;
  lBase : TFileCustomObject;
  lParentClass: TFileCustomObjectClass;
  lNode: IXMLNode;
  lInfo: TPropInfo;
begin
  Result := False;

  if AClassesJaGeradas = EmptyStr then
    AClassesJaGeradas := ';';

  //localiza o root
  lNode := FXmlMetadata.ChildNodes.FindNode('root');
  if lNode = nil then
    lNode := FXmlMetadata.AddChild('root');

  if (AClass = nil) then
  begin
    if lNode.ChildNodes.FindNode(AClassName) = nil then
    begin
      lNode[AClassName] := Algoritimos.Base64CompressedString(
        Format('<?xml version="1.0" encoding="iso-8859-1"?><root><%s ClassName="%s" ParentClass="Classe não registrada %s"/></root>',
              [AClassName, AClassName, AClassName]));
    end;
    Exit;
  end;

  lBase := AClass.Create(nil);
  try
    lBase.ProxyMode := True;

    // localiza o node da classe
    if lNode.ChildNodes.FindNode(AClass.ClassName) = nil then
    begin
      lNode[AClass.ClassName] := GetXMLFromObject(lBase);

      // verifica as classes parent
      for i := 0 to lBase.Node.ChildNodes.Count - 1 do
      begin
        lNode := lBase.Node.ChildNodes[i];
        lInfo.Decode(lNode);
        // subclasse
        if (lInfo.PropType = ptClass) and (Pos(';' + lInfo.DataType + ';', AClassesJaGeradas) = 0) then
        begin
          AClassesJaGeradas := AClassesJaGeradas + lInfo.DataType + ';';
          if lInfo.DataType <> lBase.ClassName then
          begin
            lParentClass := FileClassRegistrer.GetClass(lInfo.DataType);
            DecodeClass( lParentClass, AClassesJaGeradas, lInfo.DataType );
          end;
        end;
        // classe pai
        if Pos(';' + lInfo.Parent + ';', AClassesJaGeradas) = 0 then
        begin
          AClassesJaGeradas := AClassesJaGeradas + lInfo.Parent + ';';
          if lInfo.Parent <> lBase.ClassName then
          begin
            lParentClass := FileClassRegistrer.GetClass(lInfo.Parent);
            DecodeClass( lParentClass, AClassesJaGeradas, lInfo.Parent );
          end;
        end;
      end;
      // para listas, verifica a dependencia dos itens
      if lBase is TFileCustomList then
      begin
        if Pos(';' + TFileCustomList(lBase).ItemClassName + ';', AClassesJaGeradas) = 0 then
        begin
          AClassesJaGeradas := AClassesJaGeradas + TFileCustomList(lBase).ItemClassName + ';';
          lParentClass := FileClassRegistrer.GetClass( TFileCustomList(lBase).ItemClassName );
          DecodeClass( lParentClass, AClassesJaGeradas, TFileCustomList(lBase).ItemClassName);
        end;
      end;
    end;
    Result := True;
  finally
    FreeAndNil(lBase);
  end;
end;

function TLRDataFlashClassSerialization.DecodeXML(const ANode: IXMLNode; const AIsCompressed : Boolean): Boolean;
var
  lValorNode: string;
begin
  if AIsCompressed then
    lValorNode := Algoritimos.Base64DecompressedString(ANode.Text)
  else
    lValorNode := ANode.Text;

  Result := DecodeXML(lValorNode);
end;

function TLRDataFlashClassSerialization.DecodeXML(const AXML: string): Boolean;
var
  lXML : IXMLDocument;
  lInfo: TImplementationPart;
  lStream : TStringStream;
  lRootNode: IXMLNode;
  i: Integer;
begin
  Result := False;
  lStream := TStringStream.Create(AXML);
  try
    lXML := TXMLDocument.Create(nil);
    lXML.LoadFromStream(lStream);

    lRootNode := lXML.ChildNodes.FindNode('root');
    if lRootNode <> nil then
    begin
      lRootNode := lRootNode.ChildNodes[0];

      if FClassList.FindByName(lRootNode.Attributes['ClassName']) = nil then
      begin
        lInfo := FClassList.NewItem(lRootNode.Attributes['ClassName'], VarToStrDef(lRootNode.Attributes['ParentClass'], 'TBase'));
        // verifica por dependencias de "sets", "enums" e parent
        for i := 0 to lRootNode.AttributeNodes.Count - 1 do
        begin
          if (lRootNode.AttributeNodes[i].NodeName <> 'ClassName')  then
          begin
            // so adiciona a dependencia, a classe sera gerada depois
            if lRootNode.AttributeNodes[i].NodeName = 'ParentClass' then
              lInfo.Dependencies.Add(lRootNode.AttributeNodes[i].NodeValue)
            else
              if lRootNode.AttributeNodes[i].NodeName = 'itemClass' then
              begin
                lInfo.ItemClassName := lRootNode.AttributeNodes[i].NodeValue;
                lInfo.Dependencies.Add(lInfo.ItemClassName);
              end
              else
                if (lRootNode.AttributeNodes[i].NodeName <> 'isList') then
                begin
                  lInfo.Dependencies.Add(lRootNode.AttributeNodes[i].NodeName);
                  FClassList.AddEnumOrSet(lRootNode.AttributeNodes[i].NodeName, lRootNode.AttributeNodes[i].NodeValue);
                end
                else
                  if (lRootNode.AttributeNodes[i].NodeName = 'isList')
                  and (lRootNode.AttributeNodes[i].NodeValue = 'true')  then
                    lInfo.IsList := True;
          end;
        end;

        // verifica por subclasses
        for i := 0 to lRootNode.ChildNodes.Count - 1 do
        begin
          // so adiciona a dependencia, a classe sera gerada depois
          if (lRootNode.ChildNodes[i].Attributes['ClassName'] <> Null)
          and (lRootNode.ChildNodes[i].Attributes['ClassName'] <> '') then
            lInfo.Dependencies.Add(lRootNode.ChildNodes[i].Attributes['ClassName']);
        end;

        lInfo.GenerateCode(lRootNode);
      end;
      Result := True;
    end;
  finally
    FreeAndNil(lStream);
    lXML := nil;
  end;
end;

destructor TLRDataFlashClassSerialization.Destroy;
begin
  FreeAndNil(FClassList);
  FreeAndNil(FUnit);
  FXmlMetadata := nil;
  inherited;
end;

function TLRDataFlashClassSerialization.DoLoadFromRegistry(
  const AGerados: string; const ACheckIntf: Boolean): Boolean;
var
  i: Integer;
  lClass: TFileCustomObjectClass;
  lGerados: string;
begin
  Result := False;
  lClass := nil;

  lGerados := AGerados;
  try
    for i := 0 to FileClassRegistrer.Count - 1 do
    begin
      lClass := FileClassRegistrer.GetClass(i);
      if ACheckIntf then
      begin
        if IsHelperSupport(lClass) then
          Result := DecodeClass(lClass, lGerados, lClass.ClassName)
        else
          Result := True;
      end
      else
        Result := DecodeClass(lClass, lGerados, lClass.ClassName);

      if not Result then
        Break;
    end;
  except
    on E:Exception do
      raise Exception.CreateFmt('Erro gerando metadata para a classe %s. %s', [lClass.ClassName, E.Message]);
  end;
end;

function TLRDataFlashClassSerialization.GetClassMetadata: string;
begin
  if FXmlMetadata.XML.Count <= 1 then
    Result := EmptyStr
  else
    Result := FXmlMetadata.XML.Text;
end;

function TLRDataFlashClassSerialization.GetErrorUnit(
  const AErrorMens: string): string;
begin
  FUnit.Clear;
  FUnit.Add('unit ' + C_TMP_UNIT_CLASS + ';');
  FUnit.Add('');
  FUnit.Add('interface');
  FUnit.Add('');
  FUnit.Add('{');
  FUnit.Add(' ATENÇÃO');
  FUnit.Add(' Não foi possível gerar as classes registradas no servidor.');

  if Trim(AErrorMens) <> EmptyStr then
    FUnit.Add(' ' + AErrorMens);
  FUnit.Add('}');

  FUnit.Add('');
  FUnit.Add('implementation');
  FUnit.Add('');
  FUnit.Add('end.');

  Result := FUnit.Text;
end;

function TLRDataFlashClassSerialization.GetImplementedUnit(const AUnitName : string) : string;
var
  i: Integer;
begin
  FUnit.Clear;
  try
    FUnit.Add('unit ' + AUnitName + ';');
    FUnit.Add('');
    AddUnitComent;
    FUnit.Add('');
    FUnit.Add('interface');
    FUnit.Add('');
    FUnit.Add('uses');
    FUnit.Add('  Classes, XMLIntf, SysUtils, uRpFileHelper;');
    FUnit.Add('');
    FUnit.Add('type');

    // gera as classes básicas (sem implementação)
    for i := 0 to FClassList.Count - 1 do
      if Trim(FClassList[i].Implement.Text) = '' then
        AdicionarScopo(FClassList[i]);

    // gera as classes de objetos
    for i := 0 to FClassList.Count - 1 do
      AdicionarScopo(FClassList[i]);

    FUnit.Add('');
    FUnit.Add('implementation');
    FUnit.Add('');

    // gera as classes de objetos
    for i := 0 to FClassList.Count - 1 do
      AdicionarImplementacao(FClassList[i]);

    FUnit.Add('end.');
  finally
    Result := FUnit.Text;
  end;
end;

function TLRDataFlashClassSerialization.GetXMLFromObject(
  const AObject: TFileCustomObject): string;
begin
  Result := Algoritimos.Base64CompressedString(AObject.SaveToXmlString);
end;

function TLRDataFlashClassSerialization.IsHelperSupport(
  const ABaseClass: TFileCustomObjectClass): Boolean;
var
  InterfaceEntry: PInterfaceEntry;
  lBase: TFileCustomObject;
begin
  lBase := nil;
  lBase := ABaseClass.Create(nil);
  try
    InterfaceEntry := lBase.GetInterfaceEntry(IFileBaseHelper);
    Result := InterfaceEntry <> nil;
  finally
    FreeAndNil(lBase);
  end;
end;

procedure TLRDataFlashClassSerialization.AddUnitComent;
begin
  FUnit.Add('//   Não modifique esta Unit, seu código é gerado automaticamente pelo Cliente de');
  FUnit.Add('// TCP buscando as classes de serviço registradas no servidor.');
  FUnit.Add('//   A classe deve ser do tipo TFileCustomObject e implementar a interface ');
  FUnit.Add('// IFileBaseHelper ou ser utilizada por um comando TCP registrado.' );
  FUnit.Add('// - Gerado em...: '  + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now) );
  FUnit.Add('// - App Servidor: "' + Application.ExeName + '"');
  FUnit.Add('// - Server......: '  + TLRDataFlashUtils.GetNomeComputadorLocal);
end;

function TLRDataFlashClassSerialization.LoadClassFromMetadata(const AXMLString: string): Boolean;
var
  lXML : IXMLDocument;
  lStream: TStringStream;
  lNodeRoot: IXMLNode;
  i: Integer;
begin
  Result := False;
  lStream := TStringStream.Create(AXMLString);
  try
    lXML := TXMLDocument.Create(nil);
    lXML.LoadFromStream(lStream);
    lNodeRoot := lXML.ChildNodes.FindNode('root');
    for i := 0 to lNodeRoot.ChildNodes.Count - 1 do
    begin
      Result := DecodeXML(lNodeRoot.ChildNodes[i], True);
      if not Result then
        Exit;
    end;
  finally
    lXML := nil;
    FreeAndNil(lStream);
  end;
end;

function TLRDataFlashClassSerialization.LoadXMLFromProxyCommands(
  const ASelectedList: TStringList): Boolean;
var
  lRegistro: TTcpClassRegister;
  lItem: TTcpClassRegisterItem;
  i: Integer;
  lGerados: string;

  function ComandoNaLista : Boolean;
  begin
    Result := ASelectedList.IndexOf(lItem.ProxyGroup + '|' + lItem.ProxyClass.ClassName) >= 0;
  end;

begin
  TCPClassRegistrer.Registrados(lRegistro);
  lGerados := EmptyStr;
  try
    for i := 0 to lRegistro.Count - 1 do
    begin
      lItem := lRegistro.Items[i];
      if (ASelectedList = nil) or ComandoNaLista then
        VerificarComando(TLRDataFlashComandoClass(lItem.ProxyClass), lGerados);
    end;

    // verifica quais classes possuem a interface IBaseHelper assinada
    DoLoadFromRegistry(lGerados, True);

    Result := True;
  finally
    FreeAndNil(lRegistro);
  end;
end;

function TLRDataFlashClassSerialization.LoadXMLFromRegistry: Boolean;
begin
  Result := DoLoadFromRegistry('', False);
end;

procedure TLRDataFlashClassSerialization.VerificarComando(
  const ACommandClass: TLRDataFlashComandoClass; var AGerados: string);
var
  lCmd: TLRDataFlashComando;
  i: Integer;
  lClass: TFileCustomObjectClass;
begin
  // cria o comando e verifica nos parametros se possui algum tvpBase com classe identificada
  lCmd := ACommandClass.Create;
  try
    AGerados := EmptyStr;
    for i := 0 to lCmd.Parametros.Count - 1 do
    begin
      if (lCmd.Parametros[i].TipoValor = tvpBase) and (lCmd.Parametros[i].BaseClass <> EmptyStr) then
      begin
        lClass := FileClassRegistrer.GetClass(lCmd.Parametros[i].BaseClass);
        DecodeClass(lClass, AGerados, lCmd.Parametros[i].BaseClass);
      end;
    end;
  finally
    FreeAndNil(lCmd);
  end;
end;

{ TImplementationPart }

constructor TImplementationPart.Create;
begin
  FScope := TStringList.Create;
  FImplement := TStringList.Create;
  FDependencies := TStringList.Create;
  FNome := EmptyStr;
  FParent := EmptyStr;
  FGerado := False;
  FIsList := False;
  FItemClassName := EmptyStr;
end;

destructor TImplementationPart.Destroy;
begin
  FreeAndNil(FScope);
  FreeAndNil(FImplement);
  FreeAndNil(FDependencies);
  inherited;
end;

procedure TImplementationPart.GenerateCode(const AClassNode: IXMLNode);
begin
  GenerateScope(AClassNode);
  GenerateImplement(AClassNode);
end;

procedure TImplementationPart.DoGenerateFromAndToNode(const AClassNode: IXMLNode);
var
  i: Integer;
  lNode: IXMLNode;
  lEnum: Boolean;
  lInfo: TPropInfo;
begin
  if AClassNode.ChildNodes.Count > 0 then
  begin
  // SaveToNode
    FImplement.Add('procedure ' + FNome + '.DoSaveToNode;');
    FImplement.Add('begin');
    FImplement.Add('  inherited;');
    lEnum := False;
    for i := 0 to AClassNode.ChildNodes.Count - 1 do
    begin
      lNode := AClassNode.ChildNodes[i];
      lInfo.Decode(lNode);

      if lInfo.Parent = FNome then
      begin
        if lInfo.PropType = ptClass then
          FImplement.Add(Format('  ToNode(''%s'', F%s);', [lInfo.Name, lInfo.Name]))
        else if lInfo.PropType = ptEnum then
        begin
          lEnum := True;
          FImplement.Add(Format('  ToNode(''%s'', Ord(F%s), TypeInfo(%s) );', [lInfo.Name, lInfo.Name, lInfo.DataType]));
        end else
        if lInfo.PropType = ptSet then
          FImplement.Add(Format('  ToNode(''%s'', TypeInfo(%s), F%s);', [lInfo.Name, lInfo.DataType, lInfo.Name]))
        else
        begin
          if lInfo.ExtraInfo = 'AsBase64' then
            FImplement.Add(Format('  ToNode(''%s'', F%s, True);', [lInfo.Name, lInfo.Name]))
          else
            FImplement.Add(Format('  ToNode(''%s'', F%s);', [lInfo.Name, lInfo.Name]));
        end;
      end;
    end;
    FImplement.Add('end;');
    FImplement.Add('');
  // Load
    FImplement.Add('procedure ' + FNome + '.DoLoadFromNode(const ANode: IXMLNode);');
    if lEnum then
    begin
      FImplement.Add('var');
      FImplement.Add('  lCardinal: Cardinal;');
    end;
    FImplement.Add('begin');
    FImplement.Add('  inherited;');

    for i := 0 to AClassNode.ChildNodes.Count - 1 do
    begin
      lNode := AClassNode.ChildNodes[i];
      lInfo.Decode(lNode);
      if lInfo.Parent = FNome then
      begin
        if lInfo.PropType = ptClass then
          FImplement.Add(Format('  FromNode(''%s'', F%s);', [lInfo.Name, lInfo.Name]))
        else if lInfo.PropType = ptEnum then
        begin
            FImplement.Add(Format('  FromNode(''%s'', lCardinal, TypeInfo(%s) );', [lInfo.Name, lInfo.DataType]));
            FImplement.Add(Format('  F%s := %s(lCardinal);', [lInfo.Name, lInfo.DataType]));
        end else
        if lInfo.PropType = ptSet then
          FImplement.Add(Format('  FromNode(''%s'', TypeInfo(%s), F%s);', [lInfo.Name, lInfo.DataType, lInfo.Name]))
        else
        begin
          if lInfo.ExtraInfo = 'AsBase64' then
            FImplement.Add(Format('  FromNode(''%s'', F%s, True);', [lInfo.Name, lInfo.Name]))
          else
            FImplement.Add(Format('  FromNode(''%s'', F%s);', [lInfo.Name, lInfo.Name]));
        end;
      end;
    end;
    FImplement.Add('end;');
  end;
end;

procedure TImplementationPart.DoGenerateFromOther(const AClassNode: IXMLNode);
var
  i: Integer;
  lNode: IXMLNode;
  lInfo: TPropInfo;
begin
  if AClassNode.ChildNodes.Count > 0 then
  begin
    FImplement.Add('');
    FImplement.Add('procedure ' + FNome + '.FromOther(const AOther: TBase);');
    FImplement.Add('var');
    FImplement.Add('  lOther : ' + FNome + ' absolute AOther;');
    FImplement.Add('begin');
    FImplement.Add('  inherited;');
    for i := 0 to AClassNode.ChildNodes.Count - 1 do
    begin
      lNode := AClassNode.ChildNodes[i];
      lInfo.Decode(lNode);
      if lInfo.Parent = FNome then
      begin
        if lInfo.PropType = ptClass then
          FImplement.Add(Format('  F%s.FromOther(lOther.%s);', [lInfo.Name, lInfo.Name]))
        else
          FImplement.Add(Format('  F%s := lOther.%s;', [lInfo.Name, lInfo.Name]));
      end;
    end;
    FImplement.Add('end;');
  end;
end;

procedure TImplementationPart.DoGenerateListMethods;
begin
  if FIsList then
  begin
    FImplement.Add('function ' + FNome + '.GetItem(Index: Integer): ' + FItemClassName + ';');
    FImplement.Add('begin');
    FImplement.Add('  Result := ' + FItemClassName + '(inherited GetItem(Index));');
    FImplement.Add('end;');
    FImplement.Add('');
    FImplement.Add('function ' + FNome + '.GetItemClass: TBaseClass;');
    FImplement.Add('begin');
    FImplement.Add('  Result := ' + FItemClassName + ';');
    FImplement.Add('end;');
  end;
end;

procedure TImplementationPart.GenerateImplement(const AClassNode: IXMLNode);
var
  i: Integer;
  lNode: IXMLNode;
  lAddCreate: Boolean;
  lInfo: TPropInfo;
begin
  lAddCreate := False;
  FImplement.Add('{ ' + FNome + ' }');
  FImplement.Add('');

  // verifica se tem q implementar o Initialize e Finalize
  for i := 0 to AClassNode.ChildNodes.Count - 1 do
  begin
    lNode := AClassNode.ChildNodes[i];
    lInfo.Decode(lNode);
    lAddCreate := (lInfo.Parent = FNome) and (lInfo.PropType = ptClass);
    if lAddCreate then
      Break;
  end;

  if lAddCreate then
  begin
// Initialize
    FImplement.Add('procedure ' + FNome + '.Initialize;');
    FImplement.Add('begin');
    FImplement.Add('  inherited;');
    for i := 0 to AClassNode.ChildNodes.Count - 1 do
    begin
      lNode := AClassNode.ChildNodes[i];
      lInfo.Decode(lNode);
      if (lInfo.Parent = FNome) and (lInfo.PropType = ptClass) then
        FImplement.Add(Format('  F%s := %s.Create(Self, ''%s'');', [
          lInfo.Name,
          lInfo.DataType,
          lInfo.Name ]));
    end;
    FImplement.Add('end;');
    FImplement.Add('');
// Finalize
    FImplement.Add('procedure ' + FNome + '.Finalize;');
    FImplement.Add('begin');
    FImplement.Add('  inherited;');
    for i := 0 to AClassNode.ChildNodes.Count - 1 do
    begin
      lNode := AClassNode.ChildNodes[i];
      lInfo.Decode(lNode);
      if (lInfo.Parent = FNome) and (lInfo.PropType = ptClass) then
        FImplement.Add(Format('  FreeAndNil(F%s);', [lInfo.Name]));
    end;
    FImplement.Add('end;');
    FImplement.Add('');
  end;

  DoGenerateFromAndToNode(AClassNode);
  DoGenerateReset(AClassNode);
  DoGenerateFromOther(AClassNode);
  DoGenerateListMethods;
end;

procedure TImplementationPart.DoGenerateReset(const AClassNode: IXMLNode);
var
  i: Integer;
  lNode: IXMLNode;
  lInfo: TPropInfo;
begin
  if AClassNode.ChildNodes.Count > 0 then
  begin
    FImplement.Add('');
    FImplement.Add('procedure ' + FNome + '.Resetar;');
    FImplement.Add('begin');
    FImplement.Add('  inherited;');

    for i := 0 to AClassNode.ChildNodes.Count - 1 do
    begin
      lNode := AClassNode.ChildNodes[i];
      lInfo.Decode(lNode);

      if lInfo.Parent = FNome then
      begin
        if lInfo.PropType = ptNative then
        begin
          if lInfo.DataType = 'String' then
            FImplement.Add(Format('  F%s := EmptyStr;', [lInfo.Name]))
          else if lInfo.DataType = 'Boolean' then
            FImplement.Add(Format('  F%s := False;', [lInfo.Name]))
          else
            FImplement.Add(Format('  F%s := 0;', [lInfo.Name]))
        end
        else
          if lInfo.PropType = ptClass then
            FImplement.Add(Format('  F%s.Resetar;', [lInfo.Name]))
          else
            if lInfo.PropType = ptEnum then
              FImplement.Add(Format('  F%s := Low(%s);', [lInfo.Name, lInfo.DataType]))
            else
              if lInfo.PropType = ptSet then
                FImplement.Add(Format('  F%s := [];', [lInfo.Name]))
      end;
    end;
    FImplement.Add('end;');
  end;
end;

procedure TImplementationPart.GenerateScope(const AClassNode: IXMLNode);
var
  i: Integer;
  lPropertyes: TStringList;
  lNode: IXMLNode;
  lAddCreate: Boolean;
  lInfo : TPropInfo;
  lAddLoadSave: Boolean;
begin
  lAddCreate := False;
  lAddLoadSave := False;
  lPropertyes := TStringList.Create;
  try
    FScope.Add('');
    FScope.Add(Format('  %s = class(%s)', [FNome, FParent]));
    FScope.Add('  private');
    for i := 0 to AClassNode.ChildNodes.Count - 1 do
    begin
      lAddLoadSave := True;
      lNode := AClassNode.ChildNodes[i];
      lInfo.Decode(lNode);
      if lInfo.Parent = FNome then
      begin
        if lInfo.PropType = ptClass then
          lAddCreate := True;

        FScope.Add(     Format('    F%s: %s;', [lInfo.Name, lInfo.DataType]));
        lPropertyes.Add(Format('    property %s : %s read F%s write F%s;', [
          lInfo.Name,
          lInfo.DataType,
          lInfo.Name,
          lInfo.Name ]));
      end;
    end;

    // se nao gerou novas propriedades remove o "private"
    if not lAddLoadSave then
      FScope.Delete(2);

    FScope.Add('  protected');
    if lAddCreate then
    begin
      FScope.Add('    procedure Initialize; override;');
      FScope.Add('    procedure Finalize; override;');
    end;
 if lAddLoadSave then
    begin
      FScope.Add('    procedure DoSaveToNode; override;');
      FScope.Add('    procedure DoLoadFromNode(const ANode : IXMLNode); override;');
    end;

    if FIsList then
    begin
      FScope.Add('    function GetItem(Index: Integer): ' + FItemClassName + ';');
      FScope.Add('    function GetItemClass : TBaseClass; override;');
    end;

    FScope.Add('  public');
    if FIsList then
      FScope.Add('    property Items[Index: Integer]: ' + FItemClassName + ' read GetItem; default;')
    else
    begin
      FScope.Add('    procedure Resetar; override;');
      FScope.Add('    procedure FromOther(const AOther : TBase); override;');
    end;

    if lAddLoadSave then
      FScope.Add( TrimRight(lPropertyes.Text) );
    FScope.Add('  end;');
  finally
    FreeAndNil(lPropertyes);
  end;
end;

{ TImplementationList }

procedure TImplementationList.AddEnumOrSet(const AName, AValue: string);
var
  lItem: TImplementationPart;
begin
  if FindByName(AName) = nil then
  begin
    lItem := NewItem(AName, '');
    if Pos('set of', AValue) > 0 then
    begin
      lItem.Scope.Add( Format('  %s = %s;', [AName, AValue]));
      lItem.Dependencies.Add(Copy(AValue, 8, Length(AValue)));
    end
    else
      lItem.Scope.Add( Format('  %s = (%s);', [AName, AValue]) );
  end;
end;

function TImplementationList.FindByName(const AClassName: string): TImplementationPart;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Self.Count - 1 do
  begin
    if Trim(Self[i].Nome) = Trim(AClassName) then
    begin
      Result := Self[i];
      Break;
    end;
  end;
end;

function TImplementationList.GetItem(Index: Integer): TImplementationPart;
begin
  Result := TImplementationPart(inherited GetItem(Index));
end;

function TImplementationList.NewItem(const ANome, AParent : string): TImplementationPart;
begin
  Result := TImplementationPart.Create;
  Result.Nome := ANome;
  Result.Parent := AParent;
  Self.Add(Result);
end;

{ TPropInfo }

procedure TPropInfo.Decode(const AValue: IXMLNode);
var
  p: Integer;
  lStr: string;
begin
  Reset;
  FName := AValue.NodeName;
  if (AValue.Attributes['ex'] <> Null) and (AValue.Attributes['ex'] <> '') then
  begin
    if AValue.Attributes['ex'] = 'enum' then
      FPropType := ptEnum
    else if AValue.Attributes['ex'] = 'set' then
      FPropType := ptSet
    else
      FPropType := ptNone;
  end
  else
    if (AValue.Attributes['ClassName'] <> Null) and (AValue.Attributes['ClassName'] <> '') then
    begin
      FPropType := ptClass;
      FParent := AValue.Attributes['ClassParent'];
      FDataType := AValue.Attributes['ClassName'];
    end
    else
      FPropType := ptNative;

  if FPropType <> ptClass then
  begin
    try
      lStr := AValue.NodeValue;
      p := Pos('.', lStr);
      if p > 0 then
      begin
        FParent := Copy(lStr, 1, p - 1);
        FDataType := Copy(lStr, p + 1, Length(lStr));
      end
      else
      begin
        FDataType := lStr;
        FParent := EmptyStr;
      end;
    except
      on E: Exception do
      begin
        FParent := 'Erro';
        FDataType := 'Erro';
      end;
    end;

    // informações adicionais
    if (AValue.Attributes['info'] <> Null) and (AValue.Attributes['info'] <> '') then
      FExtraInfo := AValue.Attributes['info'];
  end;
end;

procedure TPropInfo.Reset;
begin
  FParent := EmptyStr;
  FDataType := EmptyStr;
  FPropType := ptNone;
  FName := EmptyStr;
  FExtraInfo := EmptyStr;
end;

end.
