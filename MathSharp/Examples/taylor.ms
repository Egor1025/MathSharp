fact: N -> N
fact(n: N) = {
    1, n = 0
    n * fact(n-1), n > 0
}

term(x: R, n: N) = x^n / fact(n)

ns = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

taylorExp: R -> R
taylorExp(x: R) =
    fold((acc: R, k: N) -> acc + term(x, k), 0, ns)

approxE() = taylorExp(3.0)

errorE() = |approxE() - E^3| / E^3 * 100

main() =
    println("approx e^3:", approxE())
    and println("true e^3:", E^3)
    and println("error (%):", errorE())