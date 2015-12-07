import scala.{Option => _, Either => _, _}
import math.{pow}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
      map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
      map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
      flatMap(a => if (f(a)) Some(a) else None)

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def apply[A, B](f: Option[A => B]): Option[A] => Option[B] = f match {
    case None => _ => None
    case Some(k) => _ map k
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
          a => b => f(a, b)
      }

      apply(lift(curry(f))(a))(b)
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))

}

def mean(xs: Seq[Double]): Option[Double] =
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
