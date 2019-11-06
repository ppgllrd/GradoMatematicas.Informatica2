package FunctionalProgramming

/******************************************************************************
  * Functional Programming in Scala
  *
  * Pepe Gallardo, 2019
  *
  ****************************************************************************/


object TailRecursion extends App {

  def length[A](xs: List[A]): Int = {
    def go(ac: Int, xs: List[A]): Int = xs match {
      case List() => ac
      case x::xs  => go(ac+1, xs)
    }

    go(0, xs)
  }


  def reverse[A](xs: List[A]): List[A] = {
    def go(ac: List[A], xs: List[A]): List[A] = xs match {
      case List() => ac
      case x::xs  => go(x::ac, xs)
    }

    go(List(), xs)
  }


  def foldLeft[A,B](f: (B, A) => B, z: B, xs: List[A]): B = {
    def go(ac: B, xs: List[A]): B = xs match {
      case List() => ac
      case x::xs  => go(f(ac, x), xs)
    }

    go(z, xs)
  }


  def sum[A](xs: List[A])(implicit num: Numeric[A]): A = {
    import num._

    def go(ac: A, xs:List[A]): A = xs match {
      case List() => ac
      case x::xs  => go(ac+x, xs)
    }

    go(zero, xs)
  }

  def max[A](xs: List[A])(implicit ord: Ordering[A]): A = {
    import ord._

    def go(ac: A, xs:List[A]): A = xs match {
      case List() => ac
      case x::xs  => go(if(x>ac) x else ac, xs)
    }

    xs match {
      case List() => sys.error("max: empty list")
      case x::xs  => go(x, xs)
    }
  }

  def factorial(x: Int): Int = {
    def go(ac: Int, x: Int):Int = x match {
      case 0 => ac
      case n => go(ac*n, n-1)
    }

    if(x>=0) go(1, x)
    else sys.error("factorial: negative number")
  }

  println(factorial(5))
}
