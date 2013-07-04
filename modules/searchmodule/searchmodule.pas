unit SearchModule;

{$mode objfpc}
{$H+}

interface

uses
	HTTPDefs,
	FPHTTP,
	FPWeb,
	Classes;

type
	TSearchModule = class(TFPWebModule)
		procedure ModuleRequest(
			Sender     : TObject;
			ARequest   : TRequest;
			AResponse  : TResponse;
			var Handle : Boolean);
	private
		FKeywords : TStringList;
		procedure ReplaceTags(
			Sender          : TObject;
			const TagString : String;
			TagParams       : TStringList;
			out ReplaceText : String);
	end;

var
	ASearchModule : TSearchModule;

implementation

{$R *.lfm}

uses
	SysUtils,
	IniFiles;

procedure TSearchModule.ModuleRequest(
	Sender     : TObject;
	ARequest   : TRequest;
	AResponse  : TResponse;
	var Handle : Boolean);
var
	Query : String;
begin
	Query := Trim(ARequest.QueryFields.Values['q']);

	FKeywords := TStringList.Create;	

	if not (Query = '') then
	begin
		FKeywords.Delimiter := ' ';
		FKeywords.DelimitedText := Query;

		ModuleTemplate.FileName :=
			'templates/pages/search/results.htm'
	end
	else
		ModuleTemplate.FileName :=
			'templates/pages/search/search.htm';

	ModuleTemplate.AllowTagParams := True;
	ModuleTemplate.OnReplaceTag := @ReplaceTags;

	AResponse.Content := ModuleTemplate.GetContent;
	
	Handle := True;

	FKeywords.Free
end;

procedure TSearchModule.ReplaceTags(
	Sender          : TObject;
	const TagString : String;
	TagParams       : TStringList;
	out ReplaceText : String); 

function GetSearchResults : String;
var
	Tags    : TStringList;
	Images  : TStringList;
	Content : TStringList;
	i       : Integer;
	k       : Integer = 0;
	Ini     : TIniFile;

function ImageMatchesQuery : Boolean;
var
	Tags      : TStringList;
	ImageName : String;
	m         : Integer;
	n         : Integer;
begin
	Result := False;

	Tags := TStringList.Create;
	Tags.CommaText := Ini.ReadString('tags', Images[i], '');

	ImageName := Ini.ReadString('names', Images[i], '');

	for m := 0 to FKeywords.Count - 1 do
	for n := 0 to Tags.Count - 1 do
		Result :=
			Result or
			(Pos(FKeywords[m], Tags[n]) > 0) or
			(Pos(Tags[n], FKeywords[m]) > 0) or
			(Pos(FKeywords[m], ImageName) > 0)
end;

begin
	Ini := TIniFile.Create('data/images.ini');
	Images := TStringList.Create;
	Ini.ReadSection('tags', Images);

	Tags := TStringList.Create;

	Content := TStringList.Create;
	Content.LoadFromFile('templates/htm/searchresult.htm');

	Result := '';

	Result := Result + '<table>'#13#10;

	for i := 0 to Images.Count - 1 do
	begin
		if k mod 4 = 0 then
			Result := Result + '<tr>'#13#10;

		if ImageMatchesQuery then
		begin
			Result := Result + Format(Content.Text, [i, i]);
			Inc(k)
		end;

		if (k mod 4 = 3) or (i = Images.Count - 1) then
			Result := Result + '</tr>'#13#10
	end;
	
	Content.Free;
	Ini.Free;
	Tags.Free
end;

begin
	case LowerCase(TagString) of
		'query'   : ReplaceText := FKeywords.Text;
		'results' : ReplaceText := GetSearchResults
	end
end;

initialization
	RegisterHTTPModule('search', TSearchModule)
end.
