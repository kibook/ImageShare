unit WmHome;

{$mode objfpc}
{$H+}

interface

uses
	HttpDefs,
	FpHttp,
	FpWeb,

	Classes;

type
	THomeModule = class(TFpWebModule)
	private
		procedure ReplaceTags(
			Sender          : TObject;
			const TagString : string;
			TagParams       : TStringList;
			out ReplaceText : string);
	published
		procedure Request(
			Sender      : TObject;
			ARequest    : TRequest;
			AResponse   : TResponse;
			var Handled : Boolean);
	end;

var
	HomeModule : THomeModule;

implementation

{$R *.lfm}

uses
	SysUtils,
	IniFiles;

var
	Ini : TIniFile;

function SortImages(
	List   : TStringList;
	Index1 : Integer;
	Index2 : Integer
) : Integer;
begin
	Result := StrToInt(Ini.ReadString('likes', List[Index1], '0')) -
		  StrToInt(Ini.ReadString('likes', List[Index2], '0'))
end;

procedure THomeModule.ReplaceTags(
	Sender          : TObject;
	const TagString : string;
	TagParams       : TStringList;
	out ReplaceText : string);

function GetTopImages : string;
var
	Images    : TStringList;
	Content   : TStringList;
	i         : Integer;
	TopImages : Integer;
begin
	Ini := TIniFile.Create('config.ini');
	TopImages := StrToInt(Ini.ReadString('settings', 'topimages', '0'));
	Ini.Free;

	Ini := TIniFile.Create('data/images.ini');
	Images := TStringList.Create;
	Ini.ReadSection('likes', Images);
	Images.CustomSort(@SortImages);

	Content := TStringList.Create;
	Content.LoadFromFile('templates/htm/topimages.htm');

	Result := '';

	for i := TopImages to Images.Count - 1 do
		Images.Delete(i);

	Result := Result + '<table>'+ LineEnding;

	for i := 0 to Images.Count - 1 do
	begin
		if i mod 4 = 0 then
			Result := Result + '<tr>' + LineEnding;

		Result := Result + Format(Content.Text, [i, i]);

		if (i mod 4 = 3) or (i = Images.Count - 1) then
			Result := Result + '</tr>' + LineEnding
	end;

	Result := Result + '</table>'
end;

begin
	case TagString of
		'topimages' : ReplaceText := GetTopImages
	end
end;

procedure THomeModule.Request(
	Sender      : TObject;
	ARequest    : TRequest;
	AResponse   : TResponse;
	var Handled : Boolean);
begin
	ModuleTemplate.FileName := 'templates/pages/home.htm';
	ModuleTemplate.AllowTagParams := True;
	ModuleTemplate.OnReplaceTag := @ReplaceTags;

	AResponse.Content := ModuleTemplate.GetContent;

	Handled := True
end;

initialization
	RegisterHttpModule('', THomeModule)
end.
