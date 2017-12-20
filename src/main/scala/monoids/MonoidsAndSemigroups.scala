package monoids

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) =
    monoid
}

//Exercise B.1
object BooleanMonoidOps{
  implicit val booleanAndMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(x: Boolean, y: Boolean) = x && y
      def empty = true
    }

  implicit val booleanOrMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(x: Boolean, y: Boolean) = x || y
      def empty = false
    }

  implicit val booleanXOrMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(x: Boolean, y: Boolean) = (x && !y) || (!x && y)
      def empty = false
    }

  implicit val booleanXNOrMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(x: Boolean, y: Boolean) = (!x || y) && (x || !y)
      def empty = true
    }
 
}

//Exercise B.2
object SetMonoidOps{
  implicit def setUnionMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def combine(x: Set[A], y: Set[A]) = x union y
      def empty = Set.empty[A]
    }

  implicit def setIntersectionSemigroup[A]: Semigroup[Set[A]] =
    new Semigroup[Set[A]] {
      def combine(x: Set[A], y: Set[A]) = x intersect y
    }

  implicit def setSymentricDifferenceSemigroup[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def combine(x: Set[A], y: Set[A]) =
        (x diff y) union (y diff x)
      def empty = Set.empty[A]
    }

}



