package ch1

trait Printable[A] {
  def format(a: A): String
}

object PrintableInstances {
  implicit val intPrinter: Printable[Int] = (a: Int) => a.toString
  implicit val stringPrinter: Printable[String] = (a: String) => a
  implicit val catPrinter: Printable[Cat] = (a: Cat) => s"${a.name} is a ${a.age} year-old ${a.color} cat."
}

object Printable {
 implicit class PrintableOps[A](input: A){
  def format(implicit p: Printable[A]): String = p.format(input)

  def print(implicit p: Printable[A]): Unit = println(p.format(input))
 }
}

final case class Cat(name: String, age: Int, color: String)

object TestApp extends App{
  import PrintableInstances._
  import Printable.PrintableOps
  import cats.Show
  import cats.instances.int._
  import cats.syntax.show._
  val showInt = Show[Int]
  123.show
  Cat("Dr. Purrrificator", 2, "#FFF").print
}