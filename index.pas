program ImageShare;

uses
	FPCGI,
	IniFiles,

	{ Modules }
	ViewImageModule,
	AddVoteModule,
	UploadModule,
	HomeModule,
	RandomModule,
	SearchModule;

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
