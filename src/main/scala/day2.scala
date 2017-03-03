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

  // List of Int -> List of Int
  println(s"""Functor[List].map(List(1, 2, 3)) { _ + 1 }: ${Functor[List].map(List(1, 2, 3)) { _ + 1 }}""")

  // List of Int -> List of String
  println(s"""Functor[List].map(List(1, 2, 3)) { _ + "|" }: ${Functor[List].map(List(1, 2, 3)) { _ + "|" }}""")

  import cats.syntax.functor._

  println(s"""(Right(1): Either[String, Int]) map { _ + 1 }: ${(Right(2): Either[String, Int]) map { _ + 1 }}""")

  println(s"""(Left("boom!"): Either[String, Int]) map { _ + 1 }: ${(Left("boom!"): Either[String, Int]) map { _ + 1 }}""")

  // Function as functor
  val h = ((x: Int) => x + 1) map {_ * 7}

  println(s"h(3): ${h(3)}")

  // lifting a function
  val lifted = Functor[List].lift {(_: Int) * 2}

  println(s"lifted(List(1,2,3)): ${lifted(List(1,2,3))}")

  // functor law
  import cats.syntax.eq._
  val x: Either[String, Int] = Right(1)

  assert { (x map identity) === x }

  val f = {(_: Int) * 3}

  val g = {(_: Int) + 1}

  assert { (x map (f map g)) === (x map f map g) }
}
