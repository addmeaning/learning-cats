package ch4

import cats.data.OptionT

import scala.annotation.tailrec

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A])
  extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object Trees extends App {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  import cats.Monad

  implicit val treeMonad = new Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(value) => f(value)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def loop(open: List[Tree[Either[A, B]]],
        closed: List[Tree[B]]): List[Tree[B]] = open match {
        case Branch(l, r) :: next =>
          l match {
            case Branch(_, _) => loop(l :: r :: next, closed)
            case Leaf(Left(value)) => loop(f(value) :: r :: next, closed)
            case Leaf(Right(value)) => loop(r :: next, pure(value) :: closed)
          }
        case Leaf(Left(value)) :: next =>
          loop(f(value) :: next, closed)
        case Leaf(Right(value)) :: next =>
          closed match {
            case head :: tail =>
              loop(next, Branch(head, pure(value)) :: tail)
            case Nil =>
              loop(next, pure(value) :: closed)
          }
        case Nil =>
          closed
      }
      loop(List(f(a)), Nil).head
    }
  }

  import cats.instances.list._
  import cats.syntax.applicative._ // for pure
  type ListOption[A] = OptionT[List, A]
  println(None.pure[ListOption])
}

