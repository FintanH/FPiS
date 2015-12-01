// import factorial

object MyModule {
    def abs(n: Int): Int =
        if (n < 0) -n
        else n

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

    private def formatResult(name: String, n: Int, f: Int => Int) = {
        val msg = "The %s of %d is %d"
        msg.format(name, n, f(n))
    }

    private def formatAbs(x: Int) = {
        formatResult("absolute value", x, abs)
    }

    private def formatFactorial(x: Int) = {
        formatResult("factorial", x, factorial)
    }

    def main(args: Array[String]): Unit = {
        println(formatAbs(-42))
        println(formatFactorial(7))
    }
}
