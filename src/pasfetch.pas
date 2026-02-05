{$mode objfpc}
program pasfetch;

{ pasfetch.pas ; System Fetcher written in Free Pascal }
{ Author: Marie Eckert                                 }
{ Contributors: array-in-a-matrix, polluks             }
{ Licensed under the ISC license                       }

{$H+} {$CodePage UTF8}
{$ScopedEnums On} {$WriteableConst Off}

uses
	Dos,
	GetOpts,
	IniFiles,
	Math,
	SysUtils,
	StrUtils,
	Types,
	uAnsi,
	uInfo,
	uLogos;

const VERSION = '2.0.0';

type
	TExecOpts = record
		useConfig	: Boolean;
		config		: String;
		infos		: TStringDynArray;
		logo		: TLogo;
		disableColor: Boolean;
		color		: TColor;
		infoStyles	: TStyles;
		textStyles	: TStyles;
		logoStyles	: TStyles;
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
	execOpts.color := TColor.Auto;
	execOpts.logo := TLogo.Auto;

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
			begin
				execOpts.config := OptArg;
				execOpts.useConfig := True;
				exit;
			end else if optIx = INFO_STYLE_OPT then
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
		'c': execOpts.disableColor := True;
		'?', ':': Halt(1);
		end;
	until ch = EndOfOptions;
end;

procedure LoadConfig(var execOpts: TExecOpts);
var
	path	: String;
	config	: TIniFile;
begin
	if Length(execOpts.config) > 0 then
		path := execOpts.config
	else if GetEnv('XDG_CONFIG_HOME') <> '' then
		path := GetEnv('XDG_CONFIG_HOME')+'/pasfetch/config.ini'
	else
		path := GetEnv('HOME')+'/.config/pasfetch/config.ini';

	config := TIniFile.Create(path);

	if not FileExists(path) then
	begin
		WriteLn(StdErr, 'error: could not find config file at "', path, '"');
		Halt(5);
	end;

	execOpts.disableColor := config.ReadBool('PASFETCH', 'nocolor', False);
	if not execOpts.disableColor then
		execOpts.color :=
			ParseColor(
				Lowercase(config.ReadString('PASFETCH', 'color', 'auto'))
			);
	execOpts.infos :=
		SplitString(config.ReadString('PASFETCH', 'infos', 'env:USER'), ',');
	execOpts.logo :=
		ParseLogo(Lowercase(config.ReadString('PASFETCH', 'logo', 'auto')));
	execOpts.infoStyles :=
		ParseStyleList(config.ReadString('PASFETCH', 'infolabelstyle', ''));
	execOpts.textStyles :=
		ParseStyleList(config.ReadString('PASFETCH', 'infotextstyle', ''));
	execOpts.logoStyles :=
		ParseStyleList(config.ReadString('PASFETCH', 'logostyle', ''));
end;

function BuildAnsiPrefix(const styles: TStyles; const colorStr: String): String;
var
	style: TStyle;
begin
	result := #27'[';
	for style in styles do
		result += IntToStr(Integer(style)) + ';';

	if Length(colorStr) > 0 then
		result += colorStr + 'm'
	else
		result[High(result)] := 'm';
end;

procedure Run(execOpts: TExecOpts);
var
	infoMap		: TInfoMap;
	i, widest	: Integer;

	logoAscii	: TStringDynArray;
	spacer		: String;

	{ ansi formatting }
	colorStr	: String;
	logoPrefix	: String;
	labelPrefix	: String;
	textPrefix	: String;
begin
	infoMap := CollectInformation(execOpts.infos);

	if execOpts.logo = TLogo.Auto then
		if infoMap.Find('OS', i) then
			execOpts.logo := ParseLogo(infoMap.Data[i])
		else
			execOpts.logo := ParseLogo(OsName);

	logoAscii := GetLogo(execOpts.logo);

	if not execOpts.disableColor then
	begin
		if execOpts.color = TColor.Auto then
			execOpts.color := GetLogoColor(execOpts.logo);

		if execOpts.color >= TColor.BrightBlack then
			colorStr := IntToStr(Integer(execOpts.color) + 82)
		else
			colorStr := IntToStr(Integer(execOpts.color) + 30);
	end else
		colorStr := '';

	logoPrefix	:= BuildAnsiPrefix(execOpts.logoStyles, colorStr) + ' ';
	labelPrefix	:= BuildAnsiPrefix(execOpts.infoStyles, colorStr);
	textPrefix	:= BuildAnsiPrefix(execOpts.textStyles, '');

	widest := 0;
	for i := 0 to infoMap.Count - 1 do
		if Length(infoMap.Keys[i]) > widest then
			widest := Length(infoMap.Keys[i]);

	spacer := StringOfChar(' ', Length(logoAscii[High(logoAscii)]) + 1);

	for i := 0 to Max(infoMap.Count, Length(logoAscii)) - 1 do
	begin
		if i < Length(logoAscii) then
			Write(logoPrefix, logoAscii[i], #27'[0m')
		else
			Write(spacer);

		if i < infoMap.Count then
			Write(
				'    ',
				labelPrefix,
				infoMap.Keys[i]:widest,
				#27'[0m ',
				textPrefix,
				infoMap.Data[i],
				#27'[0m'
			);
		WriteLn;
	end;

	WriteLn;
end;

var
	execOpts	: TExecOpts;
begin
	ParseOpts(execOpts);

	if execOpts.useConfig then
		LoadConfig(execOpts);

	Run(execOpts);
end.
