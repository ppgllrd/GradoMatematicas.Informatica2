package FunctionalProgramming


/******************************************************************************
  * Functional Programming in Scala
  *
  * Pepe Gallardo, 2019
  *
  ****************************************************************************/


object ValFunctions extends App {

  // val functions
  val twice: Int => Int =
    x => x + x

  val add: (Int, Int) => Int =
    (x, y) => x + y

  println(twice(10))
  println(add(10,20))


  // composing functions
  val n = add(twice(10), 100)


  // curried val functions
  val curriedAdd: Int => Int => Int =
    x => y => x + y

  val add10: Int => Int = // add10 is defined as a partial application of curriedAdd
    curriedAdd(10)

  // Each (group of) parameter(s) wrapped by parentheses
  println(curriedAdd(10)(20))
  println(curriedAdd(twice(10))(20))
  println(add10(20))


  val curriedMult: Int => Int => Int =
    x => y => x * y


  // forward function composition
  val compo1 = curriedAdd(10) andThen curriedMult(5) andThen curriedAdd(1000)

  println(compo1(1))

  // PROBLEM!!!. val functions can't use parametric polymorphism (aka generics)

}


// a function is an object
object global extends Function[Int,Int] {
  def apply(x: Int):Int =
    x + 10
}

object testGlobal extends App {
  println(global(100))
}