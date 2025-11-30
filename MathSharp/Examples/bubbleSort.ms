isSorted(xs) = {
    true, isEmpty(xs)
    true, isEmpty(tail(xs))
    head(xs) <= head(tail(xs)) and isSorted(tail(xs)), otherwise
}

bubblePass(xs) = {
    xs, isEmpty(xs)
    xs, isEmpty(tail(xs))
    cons(head(xs), bubblePass(tail(xs))), head(xs) <= head(tail(xs))
    cons(head(tail(xs)), bubblePass(cons(head(xs), tail(tail(xs))))), otherwise
}

bubbleSort(xs) = {
    xs, isSorted(xs)
    bubbleSort(bubblePass(xs)), otherwise
}

xs = [5, 1, 4, 2, 8]

main() = println(xs) and println(bubbleSort(xs))