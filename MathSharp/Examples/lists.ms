square: R -> R
square(x: R) = x^2

xs = [1, 2, 3, 4]

main() = fold((acc, x: R) -> acc + x, 0, xs)