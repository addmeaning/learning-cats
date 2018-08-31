package ch4

object States extends App{

  import cats.data.State


  val a = State[Int, String] { s => (s, s"the state is $s")}
  println(a.run(4).value)
  println(a.runA(4).value)
  println(a.runS(4).value)

  println(s"the ver: ${getClass.getPackage.getImplementationVersion}")
}
