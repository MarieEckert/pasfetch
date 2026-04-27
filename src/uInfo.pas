{$mode objfpc}
unit uInfo;

{$H+} {$CodePage UTF8}
{$ScopedEnums On} {$WriteableConst Off}

interface

uses
	Dos,
	SysUtils,
	StrUtils,
	Types;

type
	TInfo = record
		name, value: String;
	end;

	TInfoDynArray = array of TInfo;

function Host: String;
function OsName: String;
function CPU: String;
function PkgCount: String;
function Uptime: String;
function MemoryUsage: String;
function Kernel: String;

function CollectInformation(const infos: TStringDynArray): TInfoDynArray;

implementation

{$IF defined(LINUX)}
	{$INCLUDE linuxInfoImpl.inc}
{$ELSEIF defined(DARWIN)}
	{$INCLUDE macosInfoImpl.inc}
{$ELSE}
	{$ERROR unsupported platform}
{$ENDIF}

function CollectInformation(const infos: TStringDynArray): TInfoDynArray;
var
	str	: String;
	ix	: SizeInt;
begin
	SetLength(result, Length(infos));

	ix := Low(result);

	for str in infos do
	begin
		if StartsStr('env:', str) then
		begin
			result[ix].name := Copy(str, 5, Length(str) - 4);
			result[ix].value := GetEnv(result[ix].name);

			Inc(ix);
			continue;
		end;

		result[ix].name := UpperCase(str);
		if result[ix].name = 'MEM' then
			result[ix].value := MemoryUsage
		else if result[ix].name = 'OS' then
			result[ix].value := OsName
		else if result[ix].name = 'CPU' then
			result[ix].value := CPU
		else if result[ix].name = 'HOST' then
			result[ix].value := Host
		else if result[ix].name = 'UPTIME' then
			result[ix].value := Uptime
		else if result[ix].name = 'PKGS' then
			result[ix].value := PkgCount
		else if result[ix].name = 'KERNEL' then
			result[ix].value := Kernel
		else begin
			WriteLn(StdErr, 'error: invalid info "', result[ix].name,'"');
			Halt(4);
		end;

		Inc(ix);
	end;
end;

end.
