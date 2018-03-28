package monads

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._

object WriterMonadExampleAndExercise {
  val vec1 = Writer(Vector("It was the best of times",
    "It was the worst of times"), 1859)

  type Logged[A] = Writer[Vector[String], A]

  val intLogged = 123.pure[Logged]

  val vectorLog = Vector("msg1", "msg2", "msg3").tell

  val a = Writer(Vector("msg1", "msg2", "msg3"), 123)

  val b = 123.writer(Vector("msg1", "msg2", "msg3"))

  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b

  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))

  val writer3 = writer1.bimap(
    log => log.map(_.toUpperCase),
    res => res * 100)

  val writer4 = writer1.mapBoth { (log, res) =>
    val log2 = log.map(_ + "!")
    val res2 = res * 1000
    (log2, res2)
  }

  val writer5 = writer1.reset

  val writer6 = writer1.swap

  //Exercise D.5
  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)
  
  def factorial(n: Int): Logged[Int] = {
    for {
      ans <- if (n == 0)
               1.pure[Logged]
             else
               slowly(factorial(n-1).map(_ * n))
      log <- Vector(s"fact $n $ans").tell
    }yield ans
  }


}
