package functors

import scala.language.higherKinds
import cats.Functor
import cats.instances.list._
import cats.instances.option._
import cats.instances.function._
import cats.syntax.functor._

object CatsFunctors {
  val list1 = List(1,2,3)

  val list2 = Functor[List].map(list1)(_ * 2)

  val option1 = Option(123)

  val option2 = Functor[Option].map(option1)(_.toString)

  val func = (x: Int) => x + 1

  val liftedFunc = Functor[Option].lift(func)

  val liftedResult =  liftedFunc(Option(1))

  val func1 = (a: Int) => a + 1
  val func2 = (a: Int) => a * 2
  val func3 = (a: Int) => a + "!"
  val func4 = func1.map(func2).map(func3)

  val mappedFuncResult = func4(123)

  def doMath[F[_]](start: F[Int])
    (implicit functor: Functor[F]): F[Int] =
    start.map(n => n + 1 * 2)

  val doMathResult1 = doMath(Option(20))

  val doMathResult2 = doMath(List(1,2,3))

}

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)
}

object TreeFunctorInstances {
  //Exercise C.1
  implicit val treeFunctor: Functor[Tree] =
    new Functor[Tree] {
      def map[A, B](value: Tree[A])(func: A => B): Tree[B] = value match {
        case Leaf(v) => Leaf(func(v))
        case Branch(l, r) => Branch(map(l)(func), map(r)(func))  
      }
    }
}
