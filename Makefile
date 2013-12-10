CC=ghc
CFLAGS=-ilib -O2
LDFLAGS=

.PHONY: all clean

%: %.hs
	$(CC) $(CFLAGS) $<
	strip $@

bins=locatewrapper/mylocate fcitx-switch-quote/fcitx-switch-quote \
  routespeed/routespeed x/x sendmail/sendmail \
  pyindent_finder/pyindent_finder \
  swapview/swapview \
  swapview/swapview2 \
  swapview/swapviewml

all: $(bins)

swapview/swapviewml: swapview/swapview.ml
	ocamlopt str.cmxa $< -o $@
	strip $@

clean:
	-rm -f */*.o */*.hi $(bins)
