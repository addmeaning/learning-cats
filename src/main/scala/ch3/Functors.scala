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
  implicit val stringCodec: Codec[String] =
    new Codec[String] {
      def encode(value: String): String = value
      def decode(value: String): String = value
    }
  implicit val doubleCodec: Codec[Double] = stringCodec.imap(_.toDouble, _.toString)

  case class Box[A](value: A)
  implicit def boxCodec[A](implicit wrappedCodec: Codec[A]): Codec[Box[A]] =
    wrappedCodec.imap(Box(_), _.value)


  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)
  def decode[A](value: String)(implicit c: Codec[A]): A =
    c.decode(value)



}