BINDIR     ?= /usr/bin
MANDIR     ?= /usr/share/man/man1
LICENSEDIR ?= /usr/share/licenses/pasfetch
FPC_FLAGS   = -FE"obj/"

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
