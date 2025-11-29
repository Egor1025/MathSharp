A = [1, 3, 5, 7]

X_A: N -> {0, 1}
X_A(x: N) = {
    1, x in A
    0, otherwise
}

main() = X_A(3)