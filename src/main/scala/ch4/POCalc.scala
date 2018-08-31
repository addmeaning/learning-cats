package ch4

object POCalc extends App {

  import cats.data.State

  type CalcState[A] = State[List[Int], A]

  def operand(num: Int): CalcState[Int] = {
    State[List[Int], Int] { s =>
      (num :: s, num)
    }
  }

  def operator(function: (Int, Int) => Int): CalcState[Int] = State[List[Int], Int] { s =>
    val result = function(s.head, s.tail.head)
    (result :: s.tail.tail, result)
  }

  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case num => operand(num.toInt)
  }

  def evalAll(input: List[String]): CalcState[Int] = {
    import cats.syntax.applicative._
    input.foldLeft(0.pure[CalcState]){(a, b) => a.flatMap{_ => evalOne(b)}}
  }


  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    ans <- evalOne("+")
  } yield ans

  println(program.runA(Nil).value)
}
