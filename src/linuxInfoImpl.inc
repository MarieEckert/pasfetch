{
	vim: filetype=pascal
}

uses
	Classes,
	Math,
	uUtil;

function GetSingleLine(const path: String; const linenum: Integer): String;
var
	fl: TextFile;
	ix: Integer;
begin
	ix := 0;
	result := '';

	AssignFile(fl, path);
	try
		ReSet(fl);
		while (ix < linenum) and not Eof(fl) do
		begin
			Inc(ix);
			ReadLn(fl, result);
		end;
	finally
		CloseFile(fl);
	end;
end;

function Host: String;
const
	PATH = '/proc/sys/kernel/hostname';
begin
	result := GetSingleLine(PATH, 1);
end;

function OsName: String;
const
	OS_RELEASE_PATH = '/etc/os-release';
var
	distName	: String;
	fl			: TStringList;
	ix			: Integer;
begin
	result := '?';

	fl := TStringList.Create;
	try
		try
			fl.LoadFromFile(OS_RELEASE_PATH);
			for ix := 0 to fl.Count - 1 do
				if Pos('NAME=', fl.Strings[ix]) = 1 then
				begin
					result := StringReplace(
						fl.Strings[ix],
						'"',
						'',
						[rfReplaceAll]
					);
					Delete(result, 1, Pos('=', result));
					break;
				end;
		except
			on e: EInOutError do
			begin
				WriteLn('error: EInOutError whilst trying to read os-release.');
				WriteLn('  -> ', e.Message);
				Halt(4);
			end;
		end;
	finally
		fl.Free;
	end;
end;

function CPU: String;
var
	s			: String;
	split		: TStringDynArray;
	cpuInfoFile	: TextFile;
begin
	AssignFile(cpuInfoFile, '/proc/cpuinfo');
	Reset(cpuInfoFile);
	while not Eof(cpuInfoFile) do
	begin
		ReadLn(cpuInfoFile, s);
		split := SplitString(SplitString(s, ':')[0], ' ');
		if split[0] = 'model' then
		begin
			s := SplitString(s, ':')[1];
			exit(Copy(s, 2, Length(s) - 1));
		end;
	end;

	CloseFile(cpuInfoFile);
	exit('?');
end;

function PkgCount: String;
begin
	result := '0';

	if DirectoryExists('/var/lib/pacman/local') then
		Result := IntToStr(CountEntries('/var/lib/pacman/local') - 1);
end;

function Uptime: String;
const
	PATH = '/proc/uptime';
var
	uptimeTxt: String;
	seconds, minutes, hours: Real;
	minutesRem, hoursRem, days: Integer;
begin
	result := '';

	uptimeTxt := GetSingleLine(PATH, 1);

	seconds		:= StrToFloat(Copy(uptimeTxt, 1, Pos(' ', uptimeTxt)));
	minutes		:= seconds / 60;
	hours		:= minutes / 60;
	days		:= Floor(hours / 24);
	minutesRem	:= Floor(minutes mod 60);
	hoursRem	:= Floor(hours mod 24);

	result := Format(
		'%d days, %d hours, %d minutes',
		[days, hoursRem, minutesRem]
	);
end;

function MemoryUsage: String;
begin
	result := 'TODO';
end;

function Kernel: String;
const
	PATH = '/proc/sys/kernel/osrelease';
begin
	result := GetSingleLine(PATH, 1);
end;
