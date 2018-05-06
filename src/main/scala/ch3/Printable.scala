package ch3

trait Printable[A] {

  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] =
    (value: B) => format(func(value))
}