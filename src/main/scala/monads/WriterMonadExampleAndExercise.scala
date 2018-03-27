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

}
