package introduction

import cats.syntax.show._
import cats.Show
import cats.instances.int._
import cats.instances.string._

//Exercise A.4
object ShowExamples{
  implicit val catShow: Show[Cat] =
    Show.show{ cat =>
      val name = cat.name.show
      val age = cat.age.show
      val color = cat.color.show
      s"$name is a $age year-old $color cat."
    }
}
