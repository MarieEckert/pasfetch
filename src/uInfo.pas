{$mode objfpc}
unit uInfo;

{$H+} {$CodePage UTF8}
{$ScopedEnums On} {$WriteableConst Off}

interface

uses
	fgl,
	Types;

type
	TInfoMap = specialize TFPGMap<String, String>;

function Host: String;
function OsName: String;
function CPU: String;
function PkgCount: String;
function Uptime: String;
function MemoryUsage: String;

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
begin
end;

end.
