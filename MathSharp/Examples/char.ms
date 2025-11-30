A = [1, 3, 5, 7]

X_A: N -> {0, 1}
X_A(x: N) = {
    true, x in A
    false, otherwise
}

main() = X_A(3)