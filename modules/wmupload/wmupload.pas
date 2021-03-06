unit WmUpload;

{$mode objfpc}
{$H+}

interface

uses
	HttpDefs,
	FpHttp,
	FpWeb,
	Classes;

type
	TUploadModule = class(TFpWebModule)
	private
		FImageId : string;
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
	UploadModule : TUploadModule;

implementation

{$R *.lfm}

uses
	SysUtils,
	IniFiles;

procedure TUploadModule.Request(
	Sender     : TObject;
	ARequest   : TRequest;
	AResponse  : TResponse;
	var Handle : Boolean);
var
	Temp        : string;
	Extension   : string;
	ImageName   : string;
	ImageTags   : TStringList;
	Count       : Integer = 0;
	i           : Integer;
	MaxSize     : Int64;
	FileContent : TMemoryStream;
	Info        : TSearchRec;
	Ini         : TIniFile;
	Valid       : Boolean = False;

procedure AddImage;
begin
	FindFirst('images/*', faAnyFile, Info);
	repeat
		if not (Info.Name[1] = '.') then
			Inc(Count)
	until not (FindNext(Info) = 0);

	FImageId := IntToStr(Count);

	FileContent := TMemoryStream.Create;
	FileContent.LoadFromFile(Temp);
	FileContent.SaveToFile('images/' + FImageId + '.' + Extension);
	FileContent.Free;
	
	DeleteFile(Temp);

	Ini := TIniFile.Create('data/images.ini');
	Ini.CacheUpdates := True;
	Ini.WriteString('names', FImageId, ImageName);
	Ini.WriteString('likes', FImageId, '0');
	Ini.WriteString('dislikes', FImageId, '0');
	Ini.WriteString('tags', FImageId, ImageTags.CommaText);
	Ini.Free;

	ModuleTemplate.FileName := 'templates/pages/upload/redirect.htm'
end;

procedure BadUpload;
begin
	ModuleTemplate.FileName := 'templates/pages/upload/bad.htm'
end;

function CleanText(Raw : string) : string;
const
	ValidChars = ['a'..'z', 'A'..'Z', ' '];

var
	c : Char;
begin
	Result := '';
	for c in Raw do
		if c in ValidChars then
			Result := Result + c
end;
	
begin
	if ARequest.Files.Count = 1 then
	begin
		Valid := True;
		ImageName := ARequest.ContentFields.Values['name'];
		ImageName := CleanText(ImageName);

		ImageTags := TStringList.Create;
		ImageTags.CommaText :=
			ARequest.ContentFields.Values['tags'];

		for i := 0 to ImageTags.Count - 1 do
			ImageTags[i] := CleanText(ImageTags[i]);

		Temp := ARequest.Files[0].LocalFileName;

		Ini := TIniFile.Create('config.ini');
		MaxSize := StrToInt(
			Ini.ReadString('settings', 'maxsize', '0'));

		if ARequest.Files[0].Size > MaxSize then
			Valid := False;

		case ARequest.Files[0].ContentType of
			'image/jpeg' : Extension := 'jpg';
			'image/png' :  Extension := 'png';
			'image/gif' :  Extension := 'gif'
		else
			Valid := False
		end;

		if Valid then
			AddImage
		else
			BadUpload
	end
	else
		ModuleTemplate.FileName :=
			'templates/pages/upload/upload.htm';
	
	ModuleTemplate.AllowTagParams := True;
	ModuleTemplate.OnReplaceTag := @ReplaceTags;

	AResponse.Content := ModuleTemplate.GetContent;

	Handle := True
end;

procedure TUploadModule.ReplaceTags(
	Sender          : TObject;
	const TagString : string;
	TagParams       : TStringList;
	out ReplaceText : string);
begin
	case TagString of
		'id' : ReplaceText := FImageId
	end
end;

initialization
	RegisterHttpModule('upload', TUploadModule)
end.
