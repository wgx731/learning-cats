package day2

import simulacrum._
import cats._
import cats.instances.all._

@typeclass trait CanTruthy[A] { self ⇒
  /** Return true, if `a` is truthy. */
  def truthy(a: A): Boolean
}

object CanTruthy {
  def fromTruthy[A](f: A => Boolean): CanTruthy[A] = new CanTruthy[A] {
    def truthy(a: A): Boolean = f(a)
  }
}

@typeclass trait CanAppend[A] {
  @op("|+|") def append(a1: A, a2: A): A
}

object Main extends App {

  implicit val intCanTruthy: CanTruthy[Int] = CanTruthy.fromTruthy({
    case 0 ⇒ false
    case _ ⇒ true
  })

  import CanTruthy.ops._

  println(s"10.truthy: ${10.truthy}")

  implicit val intCanAppend: CanAppend[Int] = new CanAppend[Int] {
    def append(a1: Int, a2: Int): Int = a1 + a2
  }

  import CanAppend.ops._

  println(s"1 |+| 2: ${{1 |+| 2}.truthy}")

  println(s"""Functor[List].map(List(1, 2, 3)) { _ + 1 }: ${Functor[List].map(List(1, 2, 3)) { _ + 1 }}""")

}
