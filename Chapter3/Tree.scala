sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    def size[A](t: Tree[A]): Int = t match {
        case Leaf(_) => 1
        case Branch(l: Tree[A], r: Tree[A]) => 1 + size(l) + size(r)
    }

    def maximum[A](t: Tree[Int]): Int = t match {
        case Leaf(v) => v
        case Branch(l: Tree[Int], r: Tree[Int]) => maximum(l) max maximum(r)
    }

    // TODO: depth is wrong
    def depth[A](t: Tree[A]): Int = t match {
        case Leaf(_) => 1
        case Branch(l: Tree[A], r: Tree[A]) => (size(l) - 1) max (size(r) - 1)
    }

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    // def foldLeft[A, B](t: Tree[A], z: B)(f: (B, A) => B): B =
    //     t match {
    //         Leaf(v) => f(z, v)
    //         Branch(l, r) => f(l, foldLeft())
    //     }
}
