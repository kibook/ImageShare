unit RandomModule;

{$mode objfpc}
{$H+}

interface

uses
	HTTPDefs,
	FPHTTP,
	FPWeb,
	
	Classes;

type
	TRandomModule = class(TFPWebModule)
		procedure ModuleRequest(
			Sender     : TObject;
			ARequest   : TRequest;
			AResponse  : TResponse;
			var Handle : Boolean);
	private
		FImageId : String;
		procedure ReplaceTags(
			Sender          : TObject;
			const TagString : String;
			TagParams       : TStringList;
			out ReplaceText : String);
	end;

var
	ARandomModule : TRandomModule;

implementation

{$R *.lfm}

uses
	SysUtils;

procedure TRandomModule.ModuleRequest(
	Sender     : TObject;
	ARequest   : TRequest;
	AResponse  : TResponse;
	var Handle : Boolean);
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

	Handle := True
end;

procedure TRandomModule.ReplaceTags(
	Sender          : TObject;
	const TagString : String;
	TagParams       : TStringList;
	out ReplaceText : String);
begin
	case LowerCase(TagString) of
		'id' : ReplaceText := FImageId
	end
end;

initialization
	RegisterHTTPModule('random', TRandomModule)
end.
