{$mode objfpc}
program pasfetch;

{ pasfetch.pas ; System Fetcher written in Freepascal/Delphi }
{ Author: Marie Eckert                                       }
{ Contributors: array-in-a-matrix, polluks                   }
{ Licensed under the ISC license                             }
{************************************************************}
{ NOTE: This program is my first real "project" in pascal    }
{ thus most of the original code is a bit fugly. I plan to   }
{ rewrite this at sometime in the future. If anyone else has }
{ the motivation to do so, feel free to do it :)             }

{$H+} {$CodePage UTF8}
{$ScopedEnums On} {$WriteableConst Off}

uses
	GetOpts,
	Math,
	SysUtils,
	StrUtils,
	Types,
	uLogos,
	uInfo;

const VERSION = '2.0.0';

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
			  BrightWhite);

	TStyle = (Normal,
			  Bold,
			  Dim,
			  Italic,
			  Underline,
			  SlowBlink,
			  RapidBlink,
			  Reverse);

	TStyles = set of TStyle;

	TLogo = (Auto, Arch);

	TExecOpts = record
		config		: String;
		infos		: TStringDynArray;
		logo		: TLogo;
		color		: TColor;
		infoStyles	: TStyles;
		textStyles	: TStyles;
		logoStyles	: TStyles;
	end;

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
		'brightwhite'
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

function ParseLogo(const str: String): TLogo;
begin
	{ TODO }
end;

procedure ShowHelp;
const
	{ The indentation here is fine, just to help with not exceeding 80 columns
	  in the code or the final help text output.
	}
	TEXT: array of String = (
'pasfetch version ' + VERSION,
'Copyright (c) 2026,  Marie Eckert',
'Licensed under the ISC License.',
'',
'USAGE: pasfetch',
'       pasfetch OPTIONS...',
'',
'    System fetch program written in pascal',
'',
'OPTIONS',
'    -h, --help',
'        Show this help text.',
'    --config [CONFIG]',
'        Use a configuration file and ignore these command line parameters.',
'        A path to the configuration file can be provided, overwise pasfetch',
'        will try its best to find a configuration file itself',
'    -c',
'        Disable colored output. This takes precedence over -C',
'    -C <COLOR>',
'        Set which color should be used for colored output',
'    -i <INFOS>',
'        Comma seperated list of information to be displayed. See pafetch(1)',
'        for more information.',
'    --info-style <STYLES>',
'        Comma seperated list specifying how to style information labels.',
'        See pasfetch(1) for more information.',
'    --text-style <STYLES>',
'        Comma seperated list specifying how to style information text.',
'        See pasfetch(1) for more information.',
'    --logo-style <STYLES>',
'        Comma seperated list specifying how to style the logo.',
'        See pasfetch(1) for more information.'
);
var
	s: String;
begin
	for s in TEXT do
		WriteLn(StdErr, s);
	Halt;
end;

procedure ParseOpts(var execOpts: TExecOpts);
const
	CONFIG_OPT		= 2;
	INFO_STYLE_OPT	= 3;
	TEXT_STYLE_OPT	= 4;
	LOGO_STYLE_OPT	= 5;
	LOGO_OPT		= 6;
var
	ch		: Char;
	optIx	: LongInt;
	opts	: array [1..6] of TOption;
begin
	execOpts := Default(TExecOpts);

	with opts[1] do
	begin
		name	:= 'help';
		has_arg	:= 0;
		flag	:= Nil;
		value	:= #0;
	end;
	with opts[CONFIG_OPT] do
	begin
		name	:= 'config';
		has_arg	:= Optional_Argument;
		flag	:= Nil;
		value	:= #0;
	end;
	with opts[INFO_STYLE_OPT] do
	begin
		name	:= 'info-style';
		has_arg	:= 1;
		flag	:= Nil;
		value	:= #0;
	end;
	with opts[TEXT_STYLE_OPT] do
	begin
		name	:= 'text-style';
		has_arg	:= 1;
		flag	:= Nil;
		value	:= #0;
	end;
	with opts[LOGO_STYLE_OPT] do
	begin
		name	:= 'logo-style';
		has_arg	:= 1;
		flag	:= Nil;
		value	:= #0;
	end;
	with opts[LOGO_OPT] do
	begin
		name	:= 'logo';
		has_arg	:= 1;
		flag	:= Nil;
		value	:= #0;
	end;

	repeat
		ch := GetLongOpts('cC:hi:', @opts[1], optIx);
		case ch of
		#0: begin
			if optIx = 1 then
				ShowHelp
			else if optIx = CONFIG_OPT then
				execOpts.config := OptArg
			else if optIx = INFO_STYLE_OPT then
				execOpts.infoStyles := ParseStyleList(OptArg)
			else if optIx = TEXT_STYLE_OPT then
				execOpts.textStyles := ParseStyleList(OptArg)
			else if optIx = LOGO_STYLE_OPT then
				execOpts.logoStyles := ParseStyleList(OptArg)
			else if optIx = LOGO_OPT then
				execOpts.logo := ParseLogo(OptArg);
		end;
		'h': ShowHelp;
		'C': execOpts.color := ParseColor(OptArg);
		'i': execOpts.infos := SplitString(OptArg, ',');
		'?', ':': Halt(1);
		end;
	until ch = EndOfOptions;
end;

var
	execOpts: TExecOpts;
	str: String;
	infoMap: TInfoMap;
	i, widest: Integer;
begin
	ParseOpts(execOpts);
	infoMap := CollectInformation(execOpts.infos);

	widest := 0;
	for i := 0 to infoMap.Count - 1 do
		if Length(infoMap.Keys[i]) > widest then
			widest := Length(infoMap.Keys[i]);

	for i := 0 to Max(infoMap.Count, Length(ARCH)) - 1 do
	begin
		if i < Length(ARCH) then
			Write(ARCH[i]);

		if i < infoMap.Count then
		begin
			Write('    ', infoMap.Keys[i]:widest, ' ', infoMap.Data[i]);
		end;
		WriteLn;
	end;

	WriteLn;
end.
