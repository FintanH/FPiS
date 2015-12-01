
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
        case Nil => l2
        case Cons(a, as) => Cons(a, append(as, l2))
    }

    def tail[A](as: List[A]): List[A] =
        drop(as, 1)

    def init[A](l: List[A]): List[A] = l match {
        case Nil          => Nil
        case Cons(a, Nil) => Nil
        case Cons(a, as)  => Cons(a, init(as))
    }

    def setHead[A](as: List[A], v: A): List[A] = as match {
        case Nil => Nil
        case Cons(x, xs) => Cons(v, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
        case (Nil, _) => Nil
        case (Cons(a, as), n) => drop(as, n-1)
    }

    def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match {
        case Nil => Nil
        case Cons(a, as) => if (p(a)) dropWhile(as, p) else Cons(a, as)
    }

    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    def product(doubles: List[Double]): Double = doubles match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(d, ds) => d * product(ds)
    }

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
}
