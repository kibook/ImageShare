unit AddVoteModule;

{$mode objfpc}
{$H+}

interface

uses
	HTTPDefs,
	fpHTTP,
	fpWeb,
	
	Classes;

type
	TAddVoteModule = class(TFPWebModule)
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
	AnAddVoteModule : TAddVoteModule;

implementation

{$R *.lfm}

uses
	SysUtils,
	IniFiles;

procedure TAddVoteModule.ModuleRequest(
	Sender     : TObject;
	ARequest   : TRequest;
	AResponse  : TResponse;
	var Handle : Boolean);
var
	Like     : Boolean;
	Likes    : TIniFile;
	Dislikes : TIniFile;
	Ini      : TIniFile;
	VoterIp  : String;
	List     : TStringList;
begin
	Like := not (ARequest.ContentFields.Values['like'] = 'false');
	FImageId := ARequest.ContentFields.Values['image'];
	VoterIp := ARequest.RemoteAddr;

	Likes := TIniFile.Create('data/likes.ini');
	Dislikes := TIniFile.Create('data/dislikes.ini');

	if Like then
	begin
		Dislikes.DeleteKey(FImageId, VoterIp);
		Likes.WriteString(FImageId, VoterIp, '')
	end else
	begin
		Likes.DeleteKey(FImageId, VoterIp);
		Dislikes.WriteString(FImageId, VoterIp, '')
	end;

	Ini := TIniFile.Create('data/images.ini');
	Ini.CacheUpdates := True;	

	List := TStringList.Create;
	Likes.ReadSection(FImageId, List);
	Ini.WriteString('likes', FImageId, IntToStr(List.Count));
	Dislikes.ReadSection(FImageId, List);
	Ini.WriteString('dislikes', FImageId, IntToStr(List.Count));

	List.Free;
	Likes.Free;
	Dislikes.Free;
	Ini.Free;

	ModuleTemplate.FileName := 'templates/pages/vote.htm';
	ModuleTemplate.AllowTagParams := True;
	ModuleTemplate.OnReplaceTag := @ReplaceTags;

	AResponse.Content := ModuleTemplate.GetContent;

	Handle := True
end;

procedure TAddVoteModule.ReplaceTags(
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
	RegisterHTTPModule('vote', TAddVoteModule)
end.
