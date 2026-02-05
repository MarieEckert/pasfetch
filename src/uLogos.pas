{$mode objfpc}
unit uLogos;

{$H+} {$codepage utf8}
{$scopedenums on} {$writeableconst off} {$modeswitch arrayoperators}

interface
    uses uAnsiCrt, Types;

    function GetLogo(const os: String): TStringDynArray;
    function GetColor(const os: String): Byte;

    // LOGOS ARE FIXE WIDTH OF 
    const
        ALPINE : array of String = ('    /\ /\     ',
			     '   /  \  \    ',
			     '  /    \  \   ',
			     ' /      \  \  ',
			     '/        \  \ ',
			     '          \  \');
        ALPINE_COLOR = BLUE;
                 {**************}
        ARCH : array of String =   ('      /\      ',
			     '     /  \     ', 
			     '    /\   \    ', 
			     '   /      \   ',
			     '  /   ,,   \  ',
			     ' /   |  |  -\ ',
			     '/_-''''    ''''-_\');
        ARCH_COLOR = CYAN;
                    {***************}
        ARCH_BANG : array of String = ('          ____ ',
			        '      /\ /   /',
			        '     /  /   / ',
			        '    /   / /   ',
			        '   /   /_/\   ',
			        '  /   __   \  ',
			        ' /   /_/\   \ ',
			        '/_-''''    ''''-_\');
        ARCH_BANG_COLOR = CYAN;
                    {**************}
        ARCO : array of String =   ('      /\      ',
			        '     /  \     ',
			        '    / /\ \    ',
			        '   / /  \ \   ',
			        '  / /    \ \  ',
			        ' / / _____\ \ ',
			        '/_/  `----.\_\');
        ARCO_COLOR = BLUE;
                    {**************}
        ARTIX : array of String = 
			        ('      /\       ',
			        '     /  \      ',
			        '    /`''.,\    ',
			        '   /     '',   ',
			        '  /      ,`\   ',
			        ' /   ,.''`.  \ ',
			        '/.,''`     `''.\ ');
        ARTIX_COLOR = CYAN;
                    {*********}
        DEBIAN : array of String = ('  _____  ',
                    ' /  __ \ ',
                    '|  /    |',
                    '|  \___- ',
                    '-_       ',
                    '  --_    ',
                    '         ');
        DEBIAN_COLOR = RED;
                    {****************}
        ARCH7 : array of String =  (' _______        ',
                    '|____   \ \     ',
                    '    / /  \      ',
                    '   / /__\ \     ',
                    '  / /____\ \    ',
                    ' /_/      \_\   ');
        ARCH7_COLOR = CYAN;
                    {****************}
        ELEMENTARY : array of String =(
                    '  _______       ',
                    ' / ____  \      ',
                    '/  |  /  /\     ',
                    '|__\ /  / |     ',
                    '\   /__/  /     ',
                    ' \_______/      ');
        ELEMENTARY_COLOR = CYAN;
                    {****************}
        GENTOO : array of String = ('   _-----_      ',
                    '  (       \     ',
                    '  \    0   \    ',
                    '   \        )   ',
                    '   /      _/    ',
                    '  (     _-      ',
                    '  \____-        ');
        GENTOO_COLOR = MAGENTA;
                    {****************}
        MINT : array of String =   ('  _____________ ',
                    ' |_            \',
                    '  |  | _____  | ',
                    '  |  | | | |  | ',
                    '  |  | | | |  | ',
                    '  |  \_____/  | ',
                    '  \___________/ ');
        MINT_COLOR = GREEN;
                    {*******************}
        NETBSD : array of String = ('\\`-______,----__  ',
                    ' \\        __,---`_',
                    '  \\       `.____  ',
                    '   \\-______,----`-',
                    '    \\             ',
                    '     \\            ',
                    '      \\           ');
        NETBSD_COLOR = YELLOW;
                    {****************}
        UBUNTU : array of String = ('          _     ',
                    '      ---(_)    ',
                    '  _/  ---  \    ',
                    ' (_) |   |      ',
                    '   \  --- _/    ',
                    '      ---(_)    ');
        UBUNTU_COLOR = RED;

        MACOS : array of String =  (#27'[31m       .:''    ',
                    #27'[31m    _ :''_     ',
                    #27'[32m  .''`_`-''_``. ',
                    #27'[32m :________.-'' ',
                    #27'[33m :_______:    ',
                    #27'[34m  :_______`-; ',
                    #27'[35m   `._.-._.''  ');

		FALLBACK : array of String = ('No Logo for distro');

    var
        FOverrideColor: byte;  // 0 = do not override color.

implementation

function GetLogo(const os: String): TStringDynArray;
begin
    case os of
        '"Arch Linux"':       exit(ARCH);
        '"Alpine Linux"':     exit(ALPINE);
        '"Arch bang Linux"':  exit(ARCH_BANG);
        '"ArcoLinux"':        exit(ARCO);
        '"Artix Linux"':      exit(ARTIX);
        '"Debian GNU/Linux"': exit(DEBIAN);
        '"Arch7"':            exit(ARCH7);
        '"elementary OS"':    exit(ELEMENTARY);
        '"Gentoo"':           exit(GENTOO);
        '"Linux Mint"':       exit(MINT);
        '"NetBSD"':           exit(NETBSD);
        '"Ubuntu"':           exit(UBUNTU);
        'MacOS':              exit(MACOS);
    else
        exit(FALLBACK);
    end;
end;

function GetColor(const os: String): Byte;
begin
    if (FOverrideColor <> 0) then exit(FOverrideColor);
    case os of
        '"Arch Linux"':       exit(ARCH_COLOR);
        '"Alpine Linux"':     exit(ALPINE_COLOR);
        '"Arch bang Linux"':  exit(ARCH_BANG_COLOR);
        '"ArcoLinux"':        exit(ARCO_COLOR);
        '"Artix Linux"':      exit(ARTIX_COLOR);
        '"Debian GNU/Linux"': exit(DEBIAN_COLOR);
        '"Arch7"':            exit(ARCH7_COLOR);
        '"elementary OS"':    exit(ELEMENTARY_COLOR);
        '"Gentoo"':           exit(GENTOO_COLOR);
        '"Linux Mint"':       exit(MINT_COLOR);
        '"NetBSD"':           exit(NETBSD_COLOR);
        '"Ubuntu"':           exit(UBUNTU_COLOR);
    else
        exit(ARCH_COLOR);
    end;
end;

end.
