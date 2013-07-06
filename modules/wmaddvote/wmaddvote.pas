unit WmAddVote;

{$mode objfpc}
{$H+}

interface

uses
	HttpDefs,
	FpHttp,
	FpWeb,
	
	Classes;

type
	TAddVoteModule = class(TFpWebModule)
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
	AddVoteModule : TAddVoteModule;

implementation

{$R *.lfm}

uses
	SysUtils,
	IniFiles;

procedure TAddVoteModule.ReplaceTags(
	Sender          : TObject;
	const TagString : string;
	TagParams       : TStringList;
	out ReplaceText : string);
begin
	case TagString of
		'id' : ReplaceText := FImageId
	end
end;

procedure TAddVoteModule.Request(
	Sender      : TObject;
	ARequest    : TRequest;
	AResponse   : TResponse;
	var Handled : Boolean);
var
	Like     : Boolean;
	Likes    : TIniFile;
	Dislikes : TIniFile;
	Ini      : TIniFile;
	VoterIp  : string;
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

	Handled := True
end;

initialization
	RegisterHttpModule('vote', TAddVoteModule)
end.
