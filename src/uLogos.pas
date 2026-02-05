{$mode objfpc}
unit uLogos;

{$H+} {$codepage utf8}
{$scopedenums on} {$writeableconst off} {$modeswitch arrayoperators}

interface

uses
	Types,
	uAnsi;

type
	TLogo = (Auto,
			 Alpine,
			 Arch,
			 ArchBang,
			 Arco,
			 Artix,
			 Debian,
			 Arch7,
			 Elementary,
			 Gentoo,
			 Mint,
			 NetBSD,
			 Ubuntu,
			 MacOS,
			 Unknown);

function ParseLogo(const str: String): TLogo;
function GetLogo(const logo: TLogo): TStringDynArray;
function GetLogoColor(const logo: TLogo): TColor;

const
	ALPINE : array of String = (
			'    /\ /\     ',
			'   /  \  \    ',
			'  /    \  \   ',
			' /      \  \  ',
			'/        \  \ ',
			'          \  \'
	);
	ALPINE_COLOR = TColor.Blue;
			 {**************}
	ARCH : array of String = (
			'      /\      ',
			'     /  \     ',
			'    /\   \    ',
			'   /      \   ',
			'  /   ,,   \  ',
			' /   |  |  -\ ',
			'/_-''''    ''''-_\'
	);
	ARCH_COLOR = TColor.Blue;
				{***************}
	ARCH_BANG : array of String = (
				'          ____ ',
				'      /\ /   /',
				'     /  /   / ',
				'    /   / /   ',
				'   /   /_/\   ',
				'  /   __   \  ',
				' /   /_/\   \ ',
				'/_-''''    ''''-_\'
	);
	ARCH_BANG_COLOR = TColor.Cyan;
				{**************}
	ARCO : array of String = (
				'      /\      ',
				'     /  \     ',
				'    / /\ \    ',
				'   / /  \ \   ',
				'  / /    \ \  ',
				' / / _____\ \ ',
				'/_/  `----.\_\'
	);
	ARCO_COLOR = TColor.Blue;
				{**************}
	ARTIX : array of String = (
				'      /\       ',
				'     /  \      ',
				'    /`''.,\    ',
				'   /     '',   ',
				'  /      ,`\   ',
				' /   ,.''`.  \ ',
				'/.,''`     `''.\ '
	);
	ARTIX_COLOR = TColor.Cyan;
				{*********}
	DEBIAN : array of String = (
				'  _____  ',
				' /  __ \ ',
				'|  /    |',
				'|  \___- ',
				'-_       ',
				'  --_    ',
				'         '
	);
	DEBIAN_COLOR = TColor.Red;
				{****************}
	ARCH7 : array of String = (
				' _______        ',
				'|____   \ \     ',
				'    / /  \      ',
				'   / /__\ \     ',
				'  / /____\ \    ',
				' /_/      \_\   '
	);
	ARCH7_COLOR = TColor.Cyan;
				{****************}
	ELEMENTARY : array of String = (
				'  _______       ',
				' / ____  \      ',
				'/  |  /  /\     ',
				'|__\ /  / |     ',
				'\   /__/  /     ',
				' \_______/      '
	);
	ELEMENTARY_COLOR = TColor.Cyan;
				{****************}
	GENTOO : array of String = (
				'   _-----_      ',
				'  (       \     ',
				'  \    0   \    ',
				'   \        )   ',
				'   /      _/    ',
				'  (     _-      ',
				'  \____-        '
	);
	GENTOO_COLOR = TColor.Magenta;
				{****************}
	MINT : array of String = (
				'  _____________ ',
				' |_            \',
				'  |  | _____  | ',
				'  |  | | | |  | ',
				'  |  | | | |  | ',
				'  |  \_____/  | ',
				'  \___________/ '
	);
	MINT_COLOR = TColor.Green;
				{*******************}
	NETBSD : array of String = (
				'\\`-______,----__  ',
				' \\        __,---`_',
				'  \\       `.____  ',
				'   \\-______,----`-',
				'    \\             ',
				'     \\            ',
				'      \\           '
	);
	NETBSD_COLOR = TColor.Yellow;
				{****************}
	UBUNTU : array of String =(
				'          _     ',
				'      ---(_)    ',
				'  _/  ---  \    ',
				' (_) |   |      ',
				'   \  --- _/    ',
				'      ---(_)    '
	);
	UBUNTU_COLOR = TColor.Red;

	MACOS : array of String = (
				#27'[31m       .:''    ',
				#27'[31m    _ :''_     ',
				#27'[32m  .''`_`-''_``. ',
				#27'[32m :________.-'' ',
				#27'[33m :_______:    ',
				#27'[34m  :_______`-; ',
				#27'[35m   `._.-._.''  '
	);

	FALLBACK : array of String = ('No Logo for distro');

implementation

function ParseLogo(const str: String): TLogo;
begin
	if str = 'Arch Linux' then
		exit(TLogo.Arch)
	else if str = 'Alpine Linux' then
		exit(TLogo.Alpine)
	else if str = 'Arch bang Linux' then
		exit(TLogo.ArchBang)
	else if str = 'ArcoLinux' then
		exit(TLogo.Arco)
	else if str = 'Artix Linux' then
		exit(TLogo.Artix)
	else if str = 'Debian GNU/Linux' then
		exit(TLogo.Debian)
	else if str = 'Arch7' then
		exit(TLogo.Arch7)
	else if str = 'elementary OS' then
		exit(TLogo.Elementary)
	else if str = 'Gentoo' then
		exit(TLogo.Gentoo)
	else if str = 'Linux Mint' then
		exit(TLogo.Mint)
	else if str = 'NetBSD' then
		exit(TLogo.NetBSD)
	else if str = 'Ubuntu' then
		exit(TLogo.Ubuntu)
	else if str = 'MacOS' then
		exit(TLogo.MacOS)
	else if str = 'auto' then
		exit(TLogo.Auto)
	else
		exit(TLogo.Unknown);
end;

function GetLogo(const logo: TLogo): TStringDynArray;
begin
	case logo of
	TLogo.Arch: 		exit(ARCH);
	TLogo.Alpine:		exit(ALPINE);
	TLogo.ArchBang:		exit(ARCH_BANG);
	TLogo.Arco:			exit(ARCO);
	TLogo.Artix:		exit(ARTIX);
	TLogo.Debian:		exit(DEBIAN);
	TLogo.Arch7:		exit(ARCH7);
	TLogo.Elementary:	exit(ELEMENTARY);
	TLogo.Gentoo:		exit(GENTOO);
	TLogo.Mint:			exit(MINT);
	TLogo.NetBSD:		exit(NETBSD);
	TLogo.Ubuntu:		exit(UBUNTU);
	TLogo.MacOS:		exit(MACOS);
	else
		exit(FALLBACK);
	end;
end;

function GetLogoColor(const logo: TLogo): TColor;
begin
	case logo of
	TLogo.Arch: 		exit(ARCH_COLOR);
	TLogo.Alpine:		exit(ALPINE_COLOR);
	TLogo.ArchBang:		exit(ARCH_BANG_COLOR);
	TLogo.Arco:			exit(ARCO_COLOR);
	TLogo.Artix:		exit(ARTIX_COLOR);
	TLogo.Debian:		exit(DEBIAN_COLOR);
	TLogo.Arch7:		exit(ARCH7_COLOR);
	TLogo.Elementary:	exit(ELEMENTARY_COLOR);
	TLogo.Gentoo:		exit(GENTOO_COLOR);
	TLogo.Mint:			exit(MINT_COLOR);
	TLogo.NetBSD:		exit(NETBSD_COLOR);
	TLogo.Ubuntu:		exit(UBUNTU_COLOR);
	else
		exit(ARCH_COLOR);
	end;
end;

end.
