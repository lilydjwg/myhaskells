#!/usr/bin/env python3

import os

topDir = "/var/lib/pacman/local"

def checkPackage(file):
  for l in open(file):
    l = l.rstrip()
    if l == '%NAME%':
      next = 'name'
    elif l == '%ARCH%':
      next = 'arch'
    else:
      if next == 'name':
        name = l
      elif next == 'arch':
        return name, l != 'any'
      next = ''

def main():
  for name in os.listdir(topDir):
    if name.startswith('.'):
      continue
    file = '%s/%s/desc' % (topDir, name)
    name, show = checkPackage(file)
    if show:
      print(name)

if __name__ == '__main__':
  main()
