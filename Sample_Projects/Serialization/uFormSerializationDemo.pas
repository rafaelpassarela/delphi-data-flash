unit uFormSerializationDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uRpSerialization, StdCtrls, uSerializationModel, ExtCtrls;

type
  TFormSerializationDemo = class(TForm)
    ButtonSerialize: TButton;
    MemoObject: TMemo;
    CheckBoxIncludeClassName: TCheckBox;
    RadioGroupType: TRadioGroup;
    ButtonLoad: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonSerializeClick(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSerializationDemo: TFormSerializationDemo;

implementation

{$R *.dfm}

procedure TFormSerializationDemo.ButtonLoadClick(Sender: TObject);
var
  lUser : TUserInfo;
begin
  lUser := TUserInfo.Create(nil);
  try
    lUser.LoadFromString(MemoObject.Text, sfUnknown);

    // configurations
    lUser.IncludeClassName := CheckBoxIncludeClassName.Checked;

    if lUser.FormatType = sfJSON then
      ShowMessage(lUser.SaveToXmlString)
    else
      ShowMessage(lUser.SaveToJSONString);
  finally
    FreeAndNil(lUser);
  end;
end;

procedure TFormSerializationDemo.ButtonSerializeClick(Sender: TObject);
var
  lUser : TUserInfo;
begin
  lUser := TUserInfo.Create(nil);
  try
    // Fill the object with some info
    lUser.FirstName := 'Rafael';
    lUser.LastName := 'Passarela';
    lUser.Gender := geMale;
    lUser.Birthday := EncodeDate(1983, 11, 16);
    lUser.PrefFood := [ftPasta, ftPizza, ftBread];
    lUser.History := 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. '
                   + 'Quisque in ipsum bibendum, accumsan nisi a, dapibus ligula. '
                   + 'Ut condimentum nisl ac luctus varius. Quisque volutpat pharetra est, '
                   + 'eu tempus sem tristique sit amet. Aenean vel lectus id purus lobortis '
                   + 'maximus at euismod eros. Donec efficitur est vitae est efficitur ornare. '
                   + 'Maecenas euismod nisi vitae nisl laoreet tincidunt. Ut et tempor dui. '
                   + 'Nullam fermentum ex eget eros mattis, at fringilla mi congue.';

    lUser.MyCar.Model := 'VW Fusca';
    lUser.MyCar.Year := 1957;
    lUser.MyCar.NeedReplace := True;

    lUser.FamilyMembers.AddMember('Otto', 6);
    lUser.FamilyMembers.AddMember('Roze', 30);

    // configurations
    lUser.IncludeClassName := CheckBoxIncludeClassName.Checked;

    if RadioGroupType.ItemIndex = 0 then
      MemoObject.Text := lUser.SaveToXmlString
    else
      MemoObject.Text := lUser.SaveToJSONString;
  finally
    FreeAndNil(lUser);
  end;
end;

procedure TFormSerializationDemo.FormCreate(Sender: TObject);
begin
  MemoObject.Clear;
end;

end.
