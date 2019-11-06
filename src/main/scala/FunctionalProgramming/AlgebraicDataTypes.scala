package FunctionalProgramming

/******************************************************************************
  * Functional Programming in Scala
  *
  * Pepe Gallardo, 2019
  *
  ****************************************************************************/

object AlgebraicDataTypes extends App {

  // Enumerated data type
  sealed trait Direction extends Ordered[Direction] {
    /* DOESN'T WORK!!
    override def equals(that: Any): Boolean = this match {
      case North => that match {
        case North => true
        case _     => false
      }
      case South => that match {
        case South => true
        case _     => false
      }
      case East => that match {
        case East => true
        case _    => false
      }
      case West => that match {
        case West => true
        case _    => false
      }
    }*/

    override def compare(that: Direction): Int = (this, that) match {
      case (North, North) => 0
      case (North, _)     => -1

      case (South, North) => 1
      case (South, South) => 0
      case (South, _)     => -1


      case (East, North) => 1
      case (East, South) => 1
      case (East, East)  => 0
      case (East, _)     => -1


      case (West, North) => 1
      case (West, South) => 1
      case (West, East)  => 1
      case (West, _)     => 0
    }
  }

  case object North extends Direction
  case object South extends Direction
  case object East extends Direction
  case object West extends Direction

  val directions: List[Direction] = List(North, South, East, West)

  println(North == East)

  val sortedDirections = List(East,South,North,East,West,North).sorted
  println(sortedDirections)

  def directionToInt(d: Direction): Int = d match {
    case North => 0
    case South => 1
    case East  => 2
    case West  => 3
  }


  // Binary Search trees
  sealed trait Tree[+A]
  case object Empty extends Tree[Nothing]
  case class Node[A](left: Tree[A], root: A, right: Tree[A]) extends Tree[A]


  def insert[A](x: A, tree: Tree[A])(implicit ord: Ordering[A]): Tree[A] = tree match {
    case Empty => Node(Empty, x, Empty)
    case Node(left, root, right) =>
      val cmp = ord.compare(x, root)
      if(cmp == 0) Node(left, x, right)
      else if(cmp<0) Node(insert(x, left), root, right)
      else Node(left, root, insert(x, right))
  }


  val xs = List(10,7,3,4,2,1,9,8)

  val t = xs.foldRight(Empty:Tree[Int])(insert)
  println(t)
}
