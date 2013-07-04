unit HomeModule;

{$mode objfpc}
{$H+}

interface

uses
	HTTPDefs,
	FPHTTP,
	FPWeb,

	Classes;

type
	THomeModule = class(TFPWebModule)
		procedure ModuleRequest(
			Sender : TObject;
			ARequest : TRequest;
			AResponse : TResponse;
			var Handle : Boolean);
	private
		procedure ReplaceTags(
			Sender          : TObject;
			const TagString : String;
			TagParams       : TStringList;
			out ReplaceText : String);
	end;

var
	AHomeModule : THomeModule;

implementation

{$R *.lfm}

uses
	SysUtils,
	IniFiles;

var
	Ini : TIniFile;

function SortImages(
	List : TStringList;
	Index1 : Integer;
	Index2 : Integer
) : Integer;
begin
	Result := StrToInt(Ini.ReadString('likes', List[Index1], '0')) -
		  StrToInt(Ini.ReadString('likes', List[Index2], '0'))
end;

procedure THomeModule.ModuleRequest(
	Sender     : TObject;
	ARequest   : TRequest;
	AResponse  : TResponse;
	var Handle : Boolean);
begin
	ModuleTemplate.FileName := 'templates/pages/home.htm';
	ModuleTemplate.AllowTagParams := True;
	ModuleTemplate.OnReplaceTag := @ReplaceTags;

	AResponse.Content := ModuleTemplate.GetContent;

	Handle := True
end;

procedure THomeModule.ReplaceTags(
	Sender          : TObject;
	const TagString : String;
	TagParams       : TStringList;
	out ReplaceText : String);

function GetTopImages : String;
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

	Result := Result + '<table>'#13#10;

	for i := 0 to Images.Count - 1 do
	begin
		if i mod 4 = 0 then
			Result := Result + '<tr>'#13#10;

		Result := Result + Format(Content.Text, [i, i]);

		if (i mod 4 = 3) or (i = Images.Count - 1) then
			Result := Result + '</tr>'#13#10
	end;

	Result := Result + '</table>'
end;

begin
	case LowerCase(TagString) of
		'topimages' : ReplaceText := GetTopImages
	end
end;

initialization
	RegisterHTTPModule('', THomeModule)
end.
