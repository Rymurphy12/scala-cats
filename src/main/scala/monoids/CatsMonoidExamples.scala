package monoids

object CatsMonoidExamples{
  import cats.instances.int._
  import cats.syntax.semigroup._
  import cats.Monoid

  //Exercise B.3
  def add(items: List[Int]): Int = {
    items.foldLeft(Monoid[Int].empty)(_ |+| _)
  }

  //Exercise B.4
  def addGeneric[A: Monoid](items: List[A]): A = {
    items.foldLeft(Monoid[A].empty)(_ |+| _)
  }

  //Exercise B.5
  case class Order(totalCost: Double, qunatity: Double)

  implicit val orderMonoid: Monoid[Order] =
    new Monoid[Order]{
      def combine(order1: Order, order2: Order): Order =
        Order(order1.totalCost + order2.totalCost,
          order1.qunatity  + order2.qunatity)
      def empty = Order(0, 0)
    }
  }

