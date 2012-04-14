#!/usr/bin/env python3
# vim:fileencoding=utf-8

import os

m = str.maketrans('“”‘’『』「」', '『』「」“”‘’')
file = os.path.expanduser("~/.config/fcitx/data/punc.mb.zh_CN")

c = open(file).read().translate(m)
open(file, 'w').write(c)
os.execvp('fcitx-remote', ['fcitx-remote', '-r'])
