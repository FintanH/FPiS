
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

    // Group arguments to help type inference
    // Use looks like dropWhile(as)(p) i.e. curried
    def dropWhile[A](l: List[A])(p: A => Boolean): List[A] = l match {
        case Nil => Nil
        case Cons(a, as) => if (p(a)) dropWhile(as)(p) else Cons(a, as)
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }
    }

    def sum(ints: List[Int]): Int =
        foldRight(ints, 0)(_ + _)

    def product(doubles: List[Double]): Double =
        foldRight(doubles, 1.0)(_ * _)

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
}
