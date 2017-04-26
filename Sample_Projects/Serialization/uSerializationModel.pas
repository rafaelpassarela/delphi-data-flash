unit uSerializationModel;

interface

uses
  uRpSerialization, SysUtils;

type
  TGender = (geMale, geFemale, geUninformed);
  TFoods = (ftPizza, ftPasta, ftFruits, ftSalad, ftBread);
  TFoodSet = set of TFoods;

  TCar = class(TCustomSerializableObject)
  private
    FModel: string;
    FYear: Integer;
    FNeedReplace: Boolean;
  protected
    procedure DoLoadFromNode(const ANode: IXMLNode); override;
    procedure DoSaveToNode; override;
  public
    procedure Reset; override;
    procedure FromOther(const AOther: ISerializableBase); override;

    property Model : string read FModel write FModel;
    property Year : Integer read FYear write FYear;
    property NeedReplace : Boolean read FNeedReplace write FNeedReplace;
  end;

  TFamily = class(TCustomSerializableObject)
  private
    FName: string;
    FAge: Integer;
  protected
    procedure DoLoadFromNode(const ANode: IXMLNode); override;
    procedure DoSaveToNode; override;
  published
  public
    procedure Reset; override;
    procedure FromOther(const AOther: ISerializableBase); override;

    property Name : string read FName write FName;
    property Age : Integer read FAge write FAge;
  end;

  TFamilyList = class(TCustomSerializableList)
  private
    function GetItem(AIndex: Integer): TFamily;
  protected
    function GetItemClass: TCustomSerializableObjectClass; override;
  public
    property Items[AIndex : Integer] : TFamily read GetItem; default;

    function AddMember(const AName : string; const AAge : Integer) : TFamily;
  end;

  TUserInfo = class(TCustomSerializableObject)
  private
    FFirstName: string;
    FLastName: string;
    FBirthday: TDateTime;
    FGender: TGender;
    FPrefFood: TFoodSet;
    FHistory: string;
    FMyCar: TCar;
    FFamilyMembers: TFamilyList;
  protected
    procedure DoLoadFromNode(const ANode: IXMLNode); override;
    procedure DoSaveToNode; override;
    procedure Initialize; override;
    procedure Finalize; override;
  public
    procedure Reset; override;
    procedure FromOther(const AOther: ISerializableBase); override;

    property FirstName : string read FFirstName write FFirstName;
    property LastName : string read FLastName write FLastName;
    property Birthday : TDateTime read FBirthday write FBirthday;
    property Gender : TGender read FGender write FGender;
    property PrefFood : TFoodSet read FPrefFood write FPrefFood;
    property MyCar : TCar read FMyCar write FMyCar;
    property History : string read FHistory write FHistory;
    property FamilyMembers : TFamilyList read FFamilyMembers write FFamilyMembers;
  end;

implementation

{ TUserInfo }

procedure TUserInfo.DoLoadFromNode(const ANode: IXMLNode);
var
  lAux : Cardinal;
begin
  inherited;
  FromNode('FirstName', FFirstName);
  FromNode('LastName', FLastName);
  FromNode('Birthday', FBirthday);
  FromNode('History', FHistory, True);
  FromNode('MyCar', FMyCar);

  FromNode('Gender', lAux, TypeInfo(TGender));
  FGender := TGender(lAux);

  FromNode('PrefFood', TypeInfo(TFoodSet), FPrefFood);
  FromNode('FamilyMembers', FFamilyMembers);
end;

procedure TUserInfo.DoSaveToNode;
begin
  inherited;
  ToNode('FirstName', FFirstName);
  ToNode('LastName', FLastName);
  ToNode('Birthday', FBirthday);
  ToNode('Gender', Ord(FGender), TypeInfo(TGender));
  ToNode('PrefFood', TypeInfo(TFoodSet), FPrefFood);
  ToNode('MyCar', FMyCar);
  ToNode('FamilyMembers', FFamilyMembers);
  ToNode('History', FHistory, True);
end;

procedure TUserInfo.Finalize;
begin
  inherited;
  FreeAndNil(FMyCar);
  FreeAndNil(FFamilyMembers);
end;

procedure TUserInfo.FromOther(const AOther: ISerializableBase);
var
  lUser : TUserInfo absolute AOther;
begin
  inherited;
  FFirstName := lUser.FirstName;
  FLastName := lUser.LastName;
  FBirthday := lUser.Birthday;
  FGender := lUser.Gender;
  FPrefFood := lUser.PrefFood;
  FHistory := lUser.History;

  FMyCar.FromOther(lUser.MyCar);
  FFamilyMembers.FromOther(lUser.FamilyMembers);
end;

procedure TUserInfo.Initialize;
begin
  FMyCar := TCar.Create(Self);
  FFamilyMembers := TFamilyList.Create(Self);
  inherited;
end;

procedure TUserInfo.Reset;
begin
  inherited;
  FBirthday := 0;
  FFirstName := EmptyStr;
  FLastName := EmptyStr;
  FGender := geUninformed;
  FPrefFood := [];
  FHistory := EmptyStr;

  FMyCar.Reset;
  FFamilyMembers.Reset;
end;

{ TCar }

procedure TCar.DoLoadFromNode(const ANode: IXMLNode);
begin
  inherited;
  FromNode('Model', FModel);
  FromNode('Year', FYear);
  FromNode('NeedReplace', FNeedReplace);
end;

procedure TCar.DoSaveToNode;
begin
  inherited;
  ToNode('Model', FModel);
  ToNode('Year', FYear);
  ToNode('NeedReplace', FNeedReplace);
end;

procedure TCar.FromOther(const AOther: ISerializableBase);
var
  lCar : TCar absolute AOther;
begin
  inherited;
  FModel := lCar.Model;
  FYear := lCar.Year;
  FNeedReplace := lCar.NeedReplace;
end;

procedure TCar.Reset;
begin
  inherited;
  FModel := EmptyStr;
  FYear := 0;
  FNeedReplace := False;
end;

{ TFamily }

procedure TFamily.DoLoadFromNode(const ANode: IXMLNode);
begin
  inherited;
  FromNode('Age', FAge);
  FromNode('Name', FName);
end;

procedure TFamily.DoSaveToNode;
begin
  inherited;
  ToNode('Age', FAge);
  ToNode('Name', FName);
end;

procedure TFamily.FromOther(const AOther: ISerializableBase);
var
  lObj : TFamily absolute AOther;
begin
  inherited;
  FName := lObj.Name;
  FAge := lObj.Age;
end;

procedure TFamily.Reset;
begin
  inherited;
  FName := EmptyStr;
  FAge := 0;
end;

{ TFamilyList }

function TFamilyList.AddMember(const AName: string;
  const AAge: Integer): TFamily;
begin
  Result := TFamily.Create(Self);

  Result.Name := AName;
  Result.Age := AAge;

  Self.Add(Result);
end;

function TFamilyList.GetItem(AIndex: Integer): TFamily;
begin
  Result := TFamily(inherited GetItem(AIndex));
end;

function TFamilyList.GetItemClass: TCustomSerializableObjectClass;
begin
  Result := TFamily;
end;

initialization
  SerializationClassRegistrer.Registrate(TUserInfo);
  SerializationClassRegistrer.Registrate(TCar);
  SerializationClassRegistrer.Registrate(TFamily);

end.
