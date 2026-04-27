BINDIR     ?= /usr/bin
MANDIR     ?= /usr/share/man/man1
LICENSEDIR ?= /usr/share/licenses/pasfetch

# ============================================================================ #
# Additional build- & featureflags.
#
# In order for these to take proper effect, you may have to run `make clean`
# first.
#
# Specify the flags in the format of `-d<FLAGNAME>[=<OPTIONAL VALUE>]`
#
# NO_CPU_NAME_NORMALIZE
#     Disable the normalization of multiple spaces into singular spaces
#     for CPU model names.
#
# NO_DEFAULT_INFORMATION
#     Do not use a list of default informations when no information is specified
#     via the command line or via a configuration file.

FPC_FLAGS   = -FE"obj/" -dNO_CPU_NAME_NORMALIZE

.PHONY: debug
debug:
	@mkdir -p obj
	fpc src/pasfetch.pas $(FPC_FLAGS) -gl

doc/pasfetch.1: doc/pasfetch.adoc
	asciidoctor -b manpage $^

.PHONY: release
release: doc/pasfetch.1
	@mkdir -p obj
	fpc src/pasfetch.pas $(FPC_FLAGS) -O4 -XX -Xs

.PHONY: clean
clean:
	find -iname '*.o' -type f -exec rm {} ';'
	find -iname '*.ppu' -type f -exec rm {} ';'
	rm -f pasfetch

.PHONY: install
install: release
	install -m 0755 ./obj/pasfetch $(BINDIR)
	install -m 0644 ./doc/pasfetch.1 $(MANDIR)
	install -Dm 0644 LICENSE $(LICENSEDIR)/LICENSE
