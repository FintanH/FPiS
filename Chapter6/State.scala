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

import State._

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, ss) = run(s)
      f(a).run(ss)
  })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight(unit[S, List[A]](Nil))((a, b) => a.map2(b)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  def insertCoin: State[Machine, Unit] =
    modify({
      case Machine(false, candies, coins) => Machine(false, candies, coins)
      case Machine(true, candies, coins) => Machine(false, candies, coins+1)
    })

  def turnHandle: State[Machine, Unit] =
    modify({
      case Machine(false, candies, coins) if candies == 0
        => Machine(false, candies, coins)
      case Machine(false, candies, coins) if candies > 0
        => Machine(true, candies-1, coins)
      case Machine(true, candies, coint) => Machine(true, candies, coins)
    })

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def go(inputs: List[Input]): List[State[Machine, Unit]] = inputs match {
      case Nil => Nil
      case (i::is) => i match {
        case Coin => insertCoin::go(is)
        case Turn => turnHandle::go(is)
      }
    }

    for {
      _ <- sequence(go(inputs))
      m <- get
    } yield (m.coins, m.candies)
  }
}
