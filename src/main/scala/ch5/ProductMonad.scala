package ch5
import scala.language.higherKinds

object ProductMonad extends App {
  import cats.Monad
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = {
    x.flatMap(a => y.map(b => (a, b)))
  }
}
