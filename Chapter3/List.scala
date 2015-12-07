
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

    def append[A](l1: List[A], l2: List[A]): List[A] =
        foldRight(l1, l2)((a, b) => Cons(a, b))

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

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
        // reverse the list to access the back
        // then swap arguments of function f
        foldLeft(reverse(as), z)((a, b) => f(b, a))

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
        def go[A, B](as: List[A], z: B, f: (B, A) => B): B = as match {
            case Nil => z
            case Cons(x, xs) => go(xs, f(z, x), f)
        }

        go(as, z, f)
    }

    def map[A, B](as: List[A])(f: A => B): List[B] =
        foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
        foldLeft(map(as)(f), Nil: List[B])(append(_, _))

    def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
        @annotation.tailrec
        def go[A, B, C](as: List[A], bs: List[B], cs: List[C])(f: (A, B) => C): List[C] =
            (as, bs) match {
                case (Nil, Nil) => cs
                case (Nil, Cons(_, _)) => cs
                case (Cons(_, _), Nil) => cs
                case (Cons(x, xs), Cons(y, ys)) => go(xs, ys, Cons(f(x, y), cs))(f)
            }

        reverse(go(as, bs, Nil)(f))
    }

    // TODO: Not correct implementation need to revisit
    // def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    //     def checkSuper(supI: List[A], sub: List[A]): Boolean =
    //         (supI, sub) match {
    //             case (Nil, Nil) => true
    //             case (Cons(x, xs), Cons(y, ys)) => if (x == y) checkSuper(xs, ys)
    //                                                else false
    //             case _ => false
    //         }
    //
    //     sup match {
    //         case Nil => false
    //         case Cons(x, xs) => if (checkSuper(Cons(x, xs), sub)) true
    //                             else hasSubsequence(xs, sub)
    //     }
    // }

    def filter[A](as: List[A])(p: A => Boolean): List[A] =
        foldRight(as, Nil: List[A])((a, b) => if (p(a)) Cons(a, b) else b)

    def filter2[A](as: List[A])(p: A => Boolean): List[A] =
        flatMap(as)(a => if (p(a)) List(a) else Nil)

    def length[A](as: List[A]): Int =
        foldLeft(as, 0)((b, a) => 1 + b)

    def reverse[A](as: List[A]): List[A] =
        foldLeft(as, Nil: List[A])((b , a) => Cons(a, b))

    def flatten[A](as: List[List[A]]): List[A] =
        foldLeft(as, Nil: List[A])((b, a) => append(b, a))

    def sum(ints: List[Int]): Int =
        foldLeft(ints, 0)(_ + _)

    def product(doubles: List[Double]): Double =
        foldLeft(doubles, 1.0)(_ * _)

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
}
