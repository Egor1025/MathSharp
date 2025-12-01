mu = 10
sigma = -6.25

f: R -> R
f(x: R) = {
    1 / (sigma * sqrt(2 * PI)) * exp(-(x - mu)^2 / (2 * sigma^2)), sigma > 0
    fail("std must be positive"), otherwise
}

main() = f(-10.5)