unit uInfo;

interface

function Host: String;
function OsName: String;
function CPU: String;
function PkgCount: String;
function Uptime: String;
function MemoryUsage: String;

implementation

{$IF defined(LINUX}
	{$INCLUDE linuxInfoImpl.inc}
{$ELSEIF defined(DARWIN)}
	{$INCLUDE macosInfoImpl.inc}
{$ELSE}
	{$ERROR unsupported platform}
{$ENDIF}

end.
