unit ViewImageModule;

{$mode objfpc}
{$H+}

interface

uses
	HTTPDefs,
	fpHTTP,
	fpWeb,

	Classes;

type
	TViewImageModule = class(TFPWebModule)
		procedure ModuleRequest(
			Sender     : TObject;
			ARequest   : TRequest;
			AResponse  : TResponse;
			var Handle : Boolean);
	private
		FImage         : String;
		FImageId       : String;
		FImageName     : String;
		FImageTags     : TStringList;
		FImageLikes    : Integer;
		FImageDislikes : Integer;
		procedure ReplaceTags(
			Sender          : TObject;
			const TagString : String;
			TagParams       : TStringList;
			out ReplaceText : String);
	end;

var
	AViewImageModule : TViewImageModule;

implementation

{$R *.lfm}

uses
	SysUtils,
	IniFiles;

procedure TViewImageModule.ModuleRequest(
	Sender     : TObject;
	ARequest   : TRequest;
	AResponse  : TResponse;
	var Handle : Boolean);
var
	Id      : String;
	Info    : TSearchRec;	
	Ini     : TIniFile;

begin
	FImageId := ARequest.QueryFields.Values['image'];

	FImage := '';
	FindFirst('images/*', faAnyFile, Info);
	repeat
		Id := Copy(Info.Name, 1, Pos('.', Info.Name) - 1);
		if FImageId = Id then
			FImage := Info.Name
	until not (FindNext(Info) = 0) or not (FImage = '');

	Ini := TIniFile.Create('data/images.ini');

	FImageName :=
		Ini.ReadString('names', FImageId, '');
	if FImageName = '' then
		FImageName := FImage;
	FImageLikes :=
		StrToInt(Ini.ReadString('likes', FImageId, '0'));
	FImageDislikes :=
		StrToInt(Ini.ReadString('dislikes', FImageId, '0'));
	
	FImageTags := TStringList.Create;
	FImageTags.CommaText := Ini.ReadString('tags', FImageId, '');

	Ini.Free;

	if not (FImage = '') then
		ModuleTemplate.FileName := 'templates/pages/viewimage.htm'
	else
	begin
		AResponse.Code := 404;
		ModuleTemplate.FileName := 'templates/pages/404.htm'
	end;

	ModuleTemplate.AllowTagParams := True;
	ModuleTemplate.OnReplaceTag := @ReplaceTags;

	AResponse.Content := ModuleTemplate.GetContent;

	Handle := True
end;

procedure TViewImageModule.ReplaceTags(
	Sender          : TObject;
	const TagString : String;
	TagParams       : TStringList;
	out ReplaceText : String);
begin
	case LowerCase(TagString) of
		'image'    : ReplaceText := FImage;
		'name'     : ReplaceText := FImageName;
		'likes'    : ReplaceText := IntToStr(FImageLikes);
		'dislikes' : ReplaceText := IntToStr(FImageDislikes);
		'id'       : ReplaceText := FImageId;
		'tags'     : ReplaceText := FImageTags.Text
	end
end;

initialization
	RegisterHTTPModule('view', TViewImageModule)
end.
