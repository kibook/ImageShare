unit WmView;

{$mode objfpc}
{$H+}

interface

uses
	HttpDefs,
	FpHttp,
	FpWeb,
	Classes;

type
	TViewImageModule = class(TFpWebModule)
	private
		FImage         : string;
		FImageId       : string;
		FImageName     : string;
		FImageTags     : TStringList;
		FImageLikes    : Integer;
		FImageDislikes : Integer;
		procedure ReplaceTags(
			Sender          : TObject;
			const TagString : string;
			TagParams       : TStringList;
			out ReplaceText : string);

	published
		procedure Request(
			Sender     : TObject;
			ARequest   : TRequest;
			AResponse  : TResponse;
			var Handle : Boolean);
	end;

var
	ViewImageModule : TViewImageModule;

implementation

{$R *.lfm}

uses
	SysUtils,
	IniFiles;

procedure TViewImageModule.Request(
	Sender     : TObject;
	ARequest   : TRequest;
	AResponse  : TResponse;
	var Handle : Boolean);
var
	Id      : string;
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
	const TagString : string;
	TagParams       : TStringList;
	out ReplaceText : string);
begin
	case TagString of
		'image'    : ReplaceText := FImage;
		'name'     : ReplaceText := FImageName;
		'likes'    : ReplaceText := IntToStr(FImageLikes);
		'dislikes' : ReplaceText := IntToStr(FImageDislikes);
		'id'       : ReplaceText := FImageId;
		'tags'     : ReplaceText := FImageTags.Text
	end
end;

initialization
	RegisterHttpModule('view', TViewImageModule)
end.
