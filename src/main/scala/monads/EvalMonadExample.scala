package Monad

import cats.Eval

object EvalMonadExample {

  //Exercise D.4
  def foldRightHelper[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
    case h :: t =>  Eval.defer(fn(h, foldRightHelper(t, acc)(fn)))
    case Nil => acc
  }
 //  The implementation does not work. Error: (A,B) does not take parameters
 //  def foldRight[A,B](as: List[A], acc: B)(fn: (A, B)): B =
 //    foldRightHelper(as, Eval.now(acc)){ (a, b) => b.map((fn(a , _))) }.value
}
