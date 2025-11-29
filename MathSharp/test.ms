mu = 10
sigma = 5

F: N -> N
F(n: N) = {
    0, n = 0
    1, n = 1
    F(n-1) + F(n-2), n >= 2
}

X_A: X -> {0, 1}
X_A(x) = {
    1, x in A
    0, otherwise
}

main() = F(10)