unit WmRandom;

{$mode objfpc}
{$H+}

interface

uses
	HttpDefs,
	FpHttp,
	FpWeb,
	
	Classes;

type
	TRandomModule = class(TFpWebModule)
	private
		FImageId : string;
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
	RandomModule : TRandomModule;

implementation

{$R *.lfm}

uses
	SysUtils;

procedure TRandomModule.ReplaceTags(
	Sender          : TObject;
	const TagString : String;
	TagParams       : TStringList;
	out ReplaceText : String);
begin
	case TagString of
		'id' : ReplaceText := FImageId
	end
end;

procedure TRandomModule.Request(
	Sender      : TObject;
	ARequest    : TRequest;
	AResponse   : TResponse;
	var Handled : Boolean);
var
	Info  : TSearchRec;
	Count : Integer = 0;
begin
	FindFirst('images/*', faAnyFile, Info);
	repeat
		if not (Info.Name[1] = '.') then
			Inc(Count)
	until not (FindNext(Info) = 0);

	Randomize;
	FImageId := IntToStr(Random(Count));
	
	ModuleTemplate.FileName := 'templates/pages/random.htm';
	ModuleTemplate.AllowTagParams := True;
	ModuleTemplate.OnReplaceTag := @ReplaceTags;

	AResponse.Content := ModuleTemplate.GetContent;

	Handled := True
end;

initialization
	RegisterHttpModule('random', TRandomModule)
end.
