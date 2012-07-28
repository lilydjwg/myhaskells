CC=ghc
CFLAGS=-ilib -O2
LDFLAGS=

.PHONY: all clean

%: %.hs
	$(CC) $(CFLAGS) $<
	strip $@

all: locatewrapper/mylocate fcitx-switch-quote/fcitx-switch-quote \
  routespeed/routespeed x/x sendmail/sendmail

clean:
	-rm */*.o */*.hi
