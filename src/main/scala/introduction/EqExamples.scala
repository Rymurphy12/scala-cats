package introduction

import cats.Eq
import cats.instances.int._
import cats.instances.string._
import cats.syntax.eq._

object CatsEqExercise {
  
  implicit val catsEq: Eq[Cat] =
    Eq.instance[Cat] {(cat1, cat2) =>
      cat1.name  === cat2.name &&
      cat1.age   === cat2.age  &&
      cat1.color === cat2.color
    }
}
