package ch3

import cats.Functor

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value): Tree[A]
}

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A])
  extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object Functors {
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(a) => Leaf(f(a))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }
}

object Test extends App {

  import Functors.treeFunctor
  import cats._
  import cats.implicits._

  private val value: Tree[Int] = Tree.branch(Tree.leaf(4), Tree.leaf(4)).map(_ * 2)
  println(value)

}