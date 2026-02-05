{$mode objfpc}
unit uInfo;

{$H+} {$CodePage UTF8}
{$ScopedEnums On} {$WriteableConst Off}

interface

uses
	Dos,
	fgl,
	SysUtils,
	StrUtils,
	Types;

type
	TInfoMap = specialize TFPGMap<String, String>;

function Host: String;
function OsName: String;
function CPU: String;
function PkgCount: String;
function Uptime: String;
function MemoryUsage: String;
function Kernel: String;

function CollectInformation(const infos: TStringDynArray): TInfoMap;

implementation

{$IF defined(LINUX)}
	{$INCLUDE linuxInfoImpl.inc}
{$ELSEIF defined(DARWIN)}
	{$INCLUDE macosInfoImpl.inc}
{$ELSE}
	{$ERROR unsupported platform}
{$ENDIF}

function CollectInformation(const infos: TStringDynArray): TInfoMap;
var
	str, name, value: String;
begin
	result := TInfoMap.Create;
	result.sorted := True;

	for str in infos do
	begin
		if StartsStr('env:', str) then
		begin
			name := Copy(str, 5, Length(str) - 4);
			result.Add(name, GetEnv(name));
			continue;
		end;

		name := UpperCase(str);
		if name = 'MEM' then
			value := MemoryUsage
		else if name = 'OS' then
			value := OsName
		else if name = 'CPU' then
			value := CPU
		else if name = 'HOST' then
			value := Host
		else if name = 'UPTIME' then
			value := Uptime
		else if name = 'PKGS' then
			value := PkgCount
		else if name = 'KERNEL' then
			value := Kernel
		else begin
			WriteLn(StdErr, 'error: invalid info "', name,'"');
			Halt(4);
		end;

		result.Add(name, value);
	end;
end;

end.
