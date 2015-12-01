def factorial(n: Int): Int = {
    //tail-recursive call for factorial
    //called go or loop by convention
    //annotation forces the compiler to tell us if the function
    //is non tail-recursive
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
        if (n <= 0) acc
        else go(n-1, n*acc)
    }

    go(n, 1)
}
