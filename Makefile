CC=ghc
CFLAGS=-ilib -O2
LDFLAGS=

.PHONY: all clean

%: %.hs
	$(CC) $(CFLAGS) $<
	strip $@

bins=locatewrapper/mylocate fcitx-switch-quote/fcitx-switch-quote \
  routespeed/routespeed x/x sendmail/sendmail swapview/swapview \
  pyindent_finder/pyindent_finder \
  swapview/swapviewml

swapview/swapviewml: swapview/swapview.ml
	ocamlopt str.cmxa $< -o $@
	strip $@

all: $(bins)

clean:
	-rm -f */*.o */*.hi $(bins)
