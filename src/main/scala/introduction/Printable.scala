package introduction

//1.3 Exercise 1
sealed trait Printable[A]{
  def format(value: A): String
}

//1.3 Exercise 2
object PrintableInstances{
  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      def format(value: String): String =
        value
    }

  implicit val intPrintable: Printable[Int] =
    new Printable[Int] {
      def format(value: Int): String =
        value.toString()
    }

  implicit val catPrintable: Printable[Cat] =
    new Printable[Cat] {
      def format(value: Cat): String =
        value.name + " is a " + value.age + " year-old " + value.color + " cat."
    }
}

//1.3 Exercise 3
object Printable{
  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  def print[A](value: A)(implicit p: Printable[A]): Unit =
    println(format(value))
}

final case class Cat(name: String, age: Int, color: String)
