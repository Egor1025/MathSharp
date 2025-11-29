mu = 10
sigma = 5

f: R -> R
f(x: R) = 1 / (sigma * sqrt(2 * PI)) * exp(-(x - mu)^2 / (2 * sigma^2))

main() = f(10.0)