trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, ri) = rng.nextInt
    val (d, rd) = double(ri)
    ((i, d), rd)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rd1) = double(rng)
    val (d2, rd2) = double(rd1)
    val (d3, rd3) = double(rd2)
    ((d1, d2, d3), rd3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(s: RNG, count: Int, acc: List[Int]): (List[Int], RNG) = {
      if (count == 0) (acc.reverse, s)
      else {
        val (i, r) = s.nextInt
        go(r, count - 1, i::acc)
      }
    }
    go(rng, count, Nil)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng)
    }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rc => {
      val (a, r) = ra(rc)
      val (b, rPrime) = rb(r)
      (f(a, b), rPrime)
    }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r) = f(rng)
      g(a)(r)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(_ / Int.MaxValue.toDouble + 1)

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    // Initial attempt but can be generalised to fold
    // rng => {
    //   fs match {
    //     case Nil => (Nil, rng)
    //     case (r::rs) => map2(r, sequence(rs))(_ :: _)(rng)
    //   }
    // }
    fs.foldRight(unit(Nil: List[A]))((a, b) => map2(a, b)(_ :: _))

  def intsViaSeq(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))
}
