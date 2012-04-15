import LocateMapper (readLocate, transform)

main = readLocate [] >>= putStr . transform
