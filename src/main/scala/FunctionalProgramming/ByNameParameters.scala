package FunctionalProgramming

/******************************************************************************
  * Functional Programming in Scala
  *
  * Pepe Gallardo, 2019
  *
  ****************************************************************************/


object ByNameParameters extends App {

  def second(x: Int, y: Int): Int = y

  try {
    println(second(10/0, 100))
  } catch {
    case e => println(e)
  }


  def byNameSecond(x: =>Int, y: =>Int): Int = y
  println(byNameSecond(10/0, 100))
}
