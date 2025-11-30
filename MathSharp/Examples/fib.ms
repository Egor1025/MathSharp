F: N -> N
F(n: N) = {
    0, n = 0
    1, n = 1
    F(n-1) + F(n-2), n >= 2
}

main() = println("Fibonacci 10th =", F(10))