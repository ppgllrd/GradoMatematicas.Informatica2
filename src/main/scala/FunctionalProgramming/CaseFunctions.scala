package FunctionalProgramming

/******************************************************************************
  * Functional Programming in Scala
  *
  * Pepe Gallardo, 2019
  *
  ****************************************************************************/


object CaseFunctions extends App {

  // Booleans
  def not: Boolean => Boolean = {
    case false => true
    case true  => false
  }

  println(not(true))


  // Integers
  def f: Int => String = {
    case 0 => "zero"
    case 1 => "one"
    case _ => "other"
  }


  def inc: Int => Int = {
    case x => x + 1
  }

  def factorial: Int => Int = {
    case 0        => 1
    case n if n>0 => n * factorial(n-1)
    case _        => sys.error("factorial: negative argument")
  }

  println(f(1))
  println(factorial(5))


  // BigInts
  def bigFactorial: BigInt => BigInt = {
    case n if n==0 => 1
    case n if n>0  => n * bigFactorial(n-1)
    case _         => sys.error("bigFactorial: negative argument")
  }

  println(bigFactorial(100))



  // Tuples
  def fst2[A,B]: (A, B) => A = {
    case (x, _) => x
  }

  def snd2[A,B]: (A, B) => B = {
    case (_, y) => y
  }

  def map2[A,B,C,D]: (A => C) => (B => D) => (A, B) => (C, D) =
    f => g => {
      case (x, y) => (f(x), g(y))
    }

  println(map2(inc)(not)(10, true))
  println(map2((x: Int) => x + 1)((x: Boolean) => !x)(10, true))

  println(map2[Int,Boolean,Int,Boolean](_ + 1)(!_)(10, true))

  def h[B,D] = map2[Int,B,Int,D](x => x + 1)
  println(h(not)(10, true))


  def fst3[A,B,C]: (A, B, C) => A = {
    case (x, _, _) => x
  }

  def snd3[A,B,C]: (A, B, C) => B = {
    case (_, y, _) => y
  }

  def thd3[A,B,C]: (A, B, C) => C = {
    case (_, _, z) => z
  }


  // Lists
  def length[A]: List[A] => Int = {
    case List() => 0
    case _::xs  => 1 + length(xs)
  }

  val ns = List.range(0, 10)
  println(length(ns))


  def map[A,B]: (A => B) => List[A] => List[B] =
    f => {
      case List() => List()
      case x::xs  => f(x) :: map(f)(xs)
    }

  println(map((x:Int) => x + 1)(ns))


  val comp = map((x: Int) => x + 1) andThen map((x: Int) => x * 2) andThen map((x: Int) => x * x)
  println(comp(ns))
}
