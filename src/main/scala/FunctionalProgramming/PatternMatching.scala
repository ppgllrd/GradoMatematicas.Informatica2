package FunctionalProgramming

/******************************************************************************
  * Functional Programming in Scala
  *
  * Pepe Gallardo, 2019
  *
  ****************************************************************************/


object PatternMatching extends App {

  // Booleans
  def not(x: Boolean): Boolean = x match {
    case false => true
    case true  => false
  }

  def or(x: Boolean, y: Boolean): Boolean = x match {
    case false => y
    case true  => true
  }

  def and(x: Boolean, y: Boolean): Boolean = x match {
    case false => false
    case true  => y
  }

  def xor(x: Boolean, y: Boolean): Boolean = x match {
    case false => y match { // or just y
      case false => false
      case true  => true
    }
    case true => y match { // or just not(y)
      case false => true
      case true  => false
    }
  }

  def xor2(x: Boolean, y: Boolean): Boolean = x match {
    case false => y
    case true  => not(y)
  }

  // worse as it creates an intermediate tuple
  def xor3(x: Boolean, y: Boolean): Boolean = (x, y) match {
    case (false, true) => true
    case (true, false) => true
    case (_, _)        => false
  }


  // Integers
  def f(x: Int): String = x match {
    case 0 => "zero"
    case 1 => "one"
    case _ => "other"
  }

  def factorial(x: Int): Int = x match {
    case 0          => 1
    case n if n > 0 => n * factorial(n - 1)
    case _          => sys.error("factorial: negative argument")
  }


  // BigInts
  def bigFactorial(x: BigInt): BigInt = x match {
    case n if n==0 => 1
    case n if n>0  => n * bigFactorial(n-1)
    case _         => sys.error("bigFactorial: negative argument")
  }


  // Tuples
  def fst2[A, B](t: (A, B)): A = t match {
    case (x, _) => x
  }

  def snd2[A, B](t: (A, B)): B = t match {
    case (_, y) => y
  }

  def swap2[A, B](t: (A, B)): (B, A) = t match {
    case (x, y) => (y, x)
  }

  def fst3[A,B,C](t: (A, B, C)): A = t match {
    case (x, _, _) => x
  }

  def snd3[A,B,C](t: (A, B, C)): B = t match {
    case (_, y, _) => y
  }

  def thd3[A,B,C](t: (A, B, C)): C = t match {
    case (_, _, z) => z
  }


  // Options
  def option[A,B](x:B, f: A => B, opt: Option[A]): B = opt match {
    case None    => x
    case Some(y) => f(y)
  }

  def isNone[A](opt: Option[A]): Boolean = opt match {
    case None    => true
    case Some(_) => false
  }

  def isSome[A](opt: Option[A]): Boolean = opt match {
    case None    => false
    case Some(_) => true
  }

  def fromSome[A](opt: Option[A]): A = opt match {
    case None    => sys.error("fromSome: argument is None")
    case Some(x) => x
  }

  def optionToList[A](opt: Option[A]): List[A] = opt match {
    case None    => List()
    case Some(x) => List(x)
  }


  // Lists
  def isEmpty[A](xs: List[A]): Boolean = xs match {
    case List() => true
    case _::_   => false
  }

  def head[A](xs: List[A]): A = xs match {
    case List() => sys.error("head: empty list")
    case x::_   => x
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case List() => sys.error("tail: empty list")
    case _::xs  => xs
  }

  def length[A](xs: List[A]): Int = xs match {
    case List() => 0
    case _::xs  => 1 + length(xs)
  }

  def map[A,B](f: A => B, xs: List[A]): List[B] = xs match {
    case List() => List()
    case x::xs  => f(x) :: map(f, xs)
  }

  def filter[A](p: A => Boolean, xs: List[A]): List[A] = xs match {
    case List() => List()
    case x::xs  =>
      if(p(x)) x :: filter(p, xs)
      else filter(p, xs)
  }

  def foldRight[A,B](f: (A,B) => B, z: B, xs: List[A]): B = xs match {
    case List() => z
    case x::xs  => f(x, foldRight(f, z, xs))
  }

  def zip[A,B](xs: List[A], ys: List[B]): List[(A,B)] = xs match {
    case List() => List()
    case x::xs  => ys match {
      case List() => List()
      case y::ys  => (x, y) :: zip(xs, ys)
    }
  }

  def concat[A](xs: List[A], ys: List[A]): List[A] = xs match {
    case List() => ys
    case x::xs  => x :: concat(xs, ys)
  }

  def sum[A](xs: List[A])(implicit num: Numeric[A]): A = {
    import num._
    xs match {
      case List() => zero
      case x::xs  => x + sum(xs)
    }
  }

  def ordered[A](xs: List[A])(implicit ord: Ordering[A]): Boolean = {
    import ord._
    xs match {
      case List()   => true
      case List(_)  => true
      case x::y::zs => (x <= y) && ordered(y::zs)
    }
  }
}
