square: N -> N
square(x: N) = x^2

readList(n: N) = {
    [], n = 0
    cons(readInt(), readList(n-1)), n > 0
}

readArray() = readList(readInt())

xs = readArray()
xs_2 = map(square, xs)

summa = fold((acc: N, x: N) -> acc + x, 0, xs_2)

main() = println("Input array:", xs, "\nSum of squared elements =", summa)