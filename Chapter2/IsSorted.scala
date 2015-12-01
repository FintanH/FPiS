def isSorted[A](as: Array[A], order: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(as: Array[A], i1: Int, i2: Int, order: (A, A) => Boolean): Boolean =
    {
        if (i1 == as.length - 1) true
        else if (order(as(i1), as(i2))) go(as, i1+1, i2+1, order)
        else false
    }

    go(as, 0, 1, order)
}
