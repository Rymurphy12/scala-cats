package monads

object CatsMonadExamples{
  import cats.Monad
  import cats.instances.option._
  import cats.instances.list._
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  import scala.language.higherKinds

  val opt1 = Monad[Option].pure(3)

  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))

  val opt3 = Monad[Option].map(opt2)(a => 100 * a)

  val list1 = Monad[List].pure(3)

  val list2 = Monad[List].flatMap(List(1,2,3))(a => List(a, a * 10))

  val list3 = Monad[List].map(list2)(a => a + 123)

  def sumSquared[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    a.flatMap(x => b.map(y => x * x + y * y))

  def sumSquaredForComp[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x * x + y * y

}

object IdImplementation{
  import cats.Id
  def pure[A](value: A): Id[A] = value

  def map[A, B](init: Id[A])(f: A => B): Id[B] =
    f(init)
  //Exercise D.2
  def flatMap[A, B](init: Id[A])(f: A => Id[B]): Id[B] = f(init)

}
