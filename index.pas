program ImageShare;

uses
	FPCGI,
	IniFiles,

	{ Modules }
	WmView,
	WmAddVote,
	WmUpload,
	WmHome,
	WmRandom,
	WmSearch;

var
	Ini : TIniFile;
begin
	Application.Initialize;

	Ini := TIniFile.Create('config.ini');

	Application.Administrator :=
		Ini.ReadString('info', 'admin', 'webmaster');
	Application.Email :=
		Ini.ReadString('info', 'email', 'webmaster@server');

	Ini.Free;

	Application.Run
end.
