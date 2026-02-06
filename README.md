# pasfetch

A System fetch program written in Pascal.

## Screenshot

![screenshot of pasfetch with the example config](/screenshot.png "Screenshot")

## Installation

### Local Build

#### Requirements

For a complete release build, the following software is required:

- Make
- fpc (>= 3.2.2)
- asciidoctor

#### Instructions

To immediately build and install pasfetch simply execute the make install
target:

```sh
sudo make install
```

The install locations can be adjusted via the following variables:

- `BINDIR` (default `/usr/bin/`)
- `MANDIR` (default `/usr/share/man/man1/`)
- `LICENSEDIR` (default `/usr/share/licenses/pasfetch`)

### Arch

pasfetch is on the [AUR](https://aur.archlinux.org/packages/pasfetch). You can install it using paru: `paru -S pasfetch` <br>
Alternatively you can clone the AUR package and install it manually using makepkg.

## Usage

See the [manpage (`pasfetch.adoc`)](/doc/pasfetch.adoc) / documentation in `doc/`.
