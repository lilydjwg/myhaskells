import LocateMapper (readLocate, transform)

main = readLocate ["-b", "--regex"] >>= putStr . transform
