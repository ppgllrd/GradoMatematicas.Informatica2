package FunctionalProgramming

/******************************************************************************
  * Functional Programming in Scala
  *
  * Pepe Gallardo, 2019
  *
  ****************************************************************************/


object ListComprehensions extends App {

  val numbers = List.range(0, 10)

  // mapping
  val squares = for(x <- numbers) yield x*x
  println(squares)

  // filtering
  def odd(x: Int): Boolean = x%2 != 0
  val odds = for(x <- numbers if odd(x)) yield x
  println(odds)

  // filtering and mapping
  val squaredOdds = for(x <- numbers if odd(x)) yield x*x
  println(squaredOdds)

  // local definitions
  val pairs = for(x <-numbers; y = 2*x) yield (x, y)
  println(pairs)

  // multiple generators
  val product = for(x <- numbers; y <- List(100, 200)) yield (x,y)
  println(product)


  def quickSort[A](xs: List[A])(implicit ord: Ordering[A]): List[A] = {
    import ord._
    xs match {
      case List() => List()
      case h::xs  =>
        val ys = for(x <- xs if x<h) yield x
        val zs = for(x <- xs if x>=h) yield x
        quickSort(ys) ++ (h::quickSort(zs))
    }
  }

  println(quickSort(List(3,1,2,5,3,1,8,6,3,5)))
}
