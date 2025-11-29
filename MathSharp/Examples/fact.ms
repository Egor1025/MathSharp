fact: N -> N
fact(n: N) = {
    1, n = 0
    n * fact(n-1), n > 0
}

main() = fact(5)