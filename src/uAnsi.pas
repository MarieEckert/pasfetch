{$mode objfpc}
unit uAnsi;

{$H+} {$CodePage UTF8}
{$ScopedEnums On} {$WriteableConst Off}

interface

type
	TColor = (Black,
			  Red,
			  Green,
			  Yellow,
			  Blue,
			  Magenta,
			  Cyan,
			  White,
			  BrightBlack,
			  BrightRed,
			  BrightGreen,
			  BrightYellow,
			  BrightBlue,
			  BrightMagenta,
			  BrightCyan,
			  BrightWhite,
			  Auto);

	TStyle = (Normal = 0,
			  Bold,
			  Dim,
			  Italic,
			  Underline,
			  SlowBlink,
			  RapidBlink,
			  Reverse);

	TStyles = set of TStyle;

function ParseColor(const str: String): TColor;
function ParseStyle(const str: String): TStyle;
function ParseStyleList(const str: String): TStyles;

implementation

function ParseColor(const str: String): TColor;
const
	NAMES: array [TColor] of String = (
		'black',
		'red',
		'green',
		'yellow',
		'blue',
		'magenta',
		'cyan',
		'white',
		'brightblack',
		'brightred',
		'brightgreen',
		'brightyellow',
		'brightblue',
		'brightmagenta',
		'brightcyan',
		'brightwhite',
		'auto'
	);
var
	color: TColor;
begin
	for color := Low(TColor) to High(TColor) do
		if str = NAMES[color] then
			exit(color);

	WriteLn(StdErr, 'error: unknown color "', str, '"');
	Halt(3);
end;

function ParseStyle(const str: String): TStyle;
const
	NAMES: array [TStyle] of String = (
		'normal',
		'bold',
		'dim',
		'italic',
		'underline',
		'slowblink',
		'rapidblink',
		'reverse'
	);
var
	style: TStyle;
begin
	for style := Low(TStyle) to High(TStyle) do
		if str = NAMES[style] then
			exit(style);

	WriteLn(StdErr, 'error: unknown style "', str, '"');
	Halt(3);
end;

function ParseStyleList(const str: String): TStyles;
var
	currentPos	: Integer;
	commaPos	: Integer;
begin
	currentPos := 1;
	result := [];
	repeat
		commaPos := Pos(',', str, currentPos);
		if commaPos = 0 then
			commaPos := Length(str) + 1;
		result += [ParseStyle(Copy(str, currentPos, commaPos - currentPos))];
		currentPos := commaPos + 1;
	until currentPos > Length(str);
end;

end.
