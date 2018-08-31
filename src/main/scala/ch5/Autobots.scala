package ch5

import cats.data.EitherT

import scala.concurrent.{Await, Future}

object Autobots extends App {

  import scala.concurrent.ExecutionContext.Implicits.global

  type Response[A] = EitherT[Future, String, A]

  import cats.implicits._

  def getPowerLevel(autobot: String): Response[Int] = {

    powerLevels.get(autobot) match {
      case Some(value) => value.pure[Response]
      case None => EitherT.left(Future(s"ally $autobot is unreachable"))
    }
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    for {
      x <- getPowerLevel(ally1)
      y <- getPowerLevel(ally2)
    } yield x + y > 15
  }

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

def pr(response: Response[_]): Unit = {
  import scala.concurrent.duration._
  println(Await.result(response.value, 1.second))
}
  pr(getPowerLevel("Jazz"))
  pr(getPowerLevel("Foo"))
  pr(canSpecialMove("Jazz", "Bumblebee"))
  pr(canSpecialMove("Jazz", "Hot Rod"))
}
