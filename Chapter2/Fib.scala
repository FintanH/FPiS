def fib(n: Int): Int = {
    def go(n: Int, n1: Int, n2: Int): Int = {
        if (n <= 0) n1
        else go(n-1, n2, n1+n2)
    }

    go(n, 0, 1)
}
