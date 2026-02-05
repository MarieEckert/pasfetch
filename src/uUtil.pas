{$mode objfpc}
unit uUtil;

interface

uses
	SysUtils;

function CountEntries(const path: String): Integer;

implementation

function CountEntries(const path: String): Integer;
var
	searchRec: TSearchRec;
begin
	result := 0;

	if FindFirst(
		path + DirectorySeparator + '*',
		faAnyFile,
		searchRec
	) <> 0 then
		exit;

	repeat
		with searchRec do
			if (name <> '.') and (name <> '..') then
				Inc(result);
	until FindNext(searchRec) <> 0;

	FindClose(searchRec);
end;

end.
