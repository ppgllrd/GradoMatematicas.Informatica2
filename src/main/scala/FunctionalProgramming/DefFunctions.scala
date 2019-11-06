package FunctionalProgramming

/******************************************************************************
  * Functional Programming in Scala
  *
  * Pepe Gallardo, 2019
  *
  ****************************************************************************/


object DefFunctions extends App {

  // Functions are values in Scala and are introduced using lambda functions. A function
  // can be named by using a val declaration.
  // On the other hand, def doesn't really introduce a function but a method. Methods are
  // not values in Scala and they are associated to an object or class.
  // When a methods is passed as a parameter to a higher order function, Scala
  // compiler turns such method into a function under the hood by doing eta-expansion

  // def "functions"
  def twice(x: Int): Int =
    x + x

  def add(x: Int, y: Int): Int =
    x + y

  println(twice(10))
  println(add(10,20))



  // curried def "functions"
  def curriedAdd(x: Int)(y: Int): Int =
    x + y

  def curriedMult(x: Int)(y: Int): Int =
    x * y

  // Each (group of) parameter(s) wrapped by parentheses
  println(curriedAdd(10)(20))
  println(curriedAdd(twice(10))(20))

  // define new function by partially applying existing one
  def add10: Int => Int =
    curriedAdd(10)

  println(add10(20))



  // parametric polymorphism and functions as parameters
  def curriedApplyTwice[A](f: A => A)(x: A): A =
    f(f(x))

  // partial application can be passed as parameter
  println(curriedApplyTwice(curriedAdd(10))(100))
  println(curriedApplyTwice(add10)(100))


  def add20: Int => Int =
    curriedApplyTwice(add10)

  println(add20(100))


  def add30: Int => Int =
    curriedApplyTwice(curriedAdd(30))

  println(add30(100))



  // forward function composition

  // Unfortunately this fails as curriedAdd(10) is not a function but a method and andThen is a method for functions

  // val compo1 = curriedAdd(10) andThen curriedMult(5) andThen curriedAdd(1000)

  // Can be fixed in several ways:
  val compo2 = (curriedAdd(10): Int => Int) andThen curriedMult(5) andThen curriedAdd(1000)

  val compo3 = curriedAdd(10)_ andThen curriedMult(5) andThen curriedAdd(1000)

  val compo4 = {
    def add10: Int => Int = curriedAdd(10) // writing explicit type of introduced function is mandatory
    add10 andThen curriedMult(5) andThen curriedAdd(1000)
  }

  val compo5 = {
    val add10: Int => Int = curriedAdd(10) // writing explicit type of introduced function is mandatory
    add10 andThen curriedMult(5) andThen curriedAdd(1000)
  }

  println(compo2(1))
  println(compo3(1))
  println(compo4(1))
  println(compo5(1))
}
