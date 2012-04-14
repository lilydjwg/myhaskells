#!/usr/bin/env python3
# vim:fileencoding=utf-8

import sys

def catafile(fp):
  for l in fp:
    sys.stdout.write(l)

def getFileArgs():
  filenames = []
  args = []
  for arg in sys.argv[1:]:
    if arg == '-':
      l = filenames
    elif arg.startswith('-'):
      l = args
    else:
      l = filenames
    l.append(arg)
  return filenames, args

def main():
  fns = getFileArgs()[0]
  if fns:
    for f in fns:
      if f == '-':
        catafile(sys.stdin)
      else:
        catafile(open(f))
  else:
    catafile(sys.stdin)

if __name__ == '__main__':
  main()
