package ch4

import cats.data.Reader
import javax.print.attribute.standard.JobOriginatingUserName

case class Db(
  usernames: Map[Int, String],
  passwords: Map[String, String]
)

object Db{
  type DbReader[A] = Reader[Db, A]

  import cats.syntax.applicative._
  def findUsername(userId: Int): DbReader[Option[String]] = Reader(db => db.usernames.get(userId))

  def checkPassword(userName: String, password: String): DbReader[Boolean] = Reader(db => db.passwords.get(userName).contains(password))

  def checkLogin(userId: Int,password: String): DbReader[Boolean] = for {
    x <- findUsername(userId)
    y <- x.map(username => checkPassword(username, password)).getOrElse(false.pure[DbReader])
  } yield y
//  def checkLogin(userId: Int,password: String): DbReader[Boolean] = findUsername(userId).flatMap(x => x.map(t => checkPassword(t, password)).getOrElse(false.pure[DbReader]))
}
object DbTest extends App{
  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade"  -> "zerocool",
    "kate"  -> "acidburn",
    "margo" -> "secret")

  val db = Db(users, passwords)

  Db.checkLogin(1, "zerocool").run(db)

}
