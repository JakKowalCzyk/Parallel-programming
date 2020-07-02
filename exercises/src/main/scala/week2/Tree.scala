package week2


sealed abstract class Tree[A]

case class Leaf[A](value: A) extends Tree[A]

case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree extends App {

  def map[A, B](t: Tree[A], f: A => B): Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Node(left, right) => Node(map[A, B](left, f), map[A, B](right, f))
  }

  def reduce[A](t: Tree[A], f: (A, A) => A): A = t match {
    case Leaf(value) => value
    case Node(left, right) => f(reduce[A](left, f), reduce[A](right, f))
  }

  def reducePar[A](t: Tree[A], f: (A, A) => A): A = t match {
    case Leaf(value) => value
    case Node(left, right) => {
      val (v1, v2) = parallel(reducePar[A](left, f), reducePar[A](right, f))
      f(v1, v2)
    }
  }


  def tree = Node(Leaf(1), Node(Leaf(3), Leaf(8)))

  println(reduce[Int](tree, (x: Int, y: Int) => x - y))
  println(reducePar[Int](tree, (x: Int, y: Int) => x - y))

  println(reduce(map(tree, List(_)), (x, y) => x ++ y))
}