package day4

import cats._
import cats.instances.all._
import cats.syntax.eq._
import cats.syntax.semigroup._
import cats.syntax.foldable._

class Disjunction(val unwrap: Boolean) extends AnyVal
object Disjunction {

  @inline def apply(b: Boolean): Disjunction = new Disjunction(b)

  implicit val disjunctionMonoid: Monoid[Disjunction] = new Monoid[Disjunction] {
    def combine(a1: Disjunction, a2: Disjunction): Disjunction =
      Disjunction(a1.unwrap || a2.unwrap)
    def empty: Disjunction = Disjunction(false)
  }

  implicit val disjunctionEq: Eq[Disjunction] = new Eq[Disjunction] {
    def eqv(a1: Disjunction, a2: Disjunction): Boolean =
      a1.unwrap == a2.unwrap
  }

}

class Conjunction(val unwrap: Boolean) extends AnyVal
object Conjunction {

  @inline def apply(b: Boolean): Conjunction = new Conjunction(b)

  implicit val conjunctionMonoid: Monoid[Conjunction] = new Monoid[Conjunction] {
    def combine(a1: Conjunction, a2: Conjunction): Conjunction =
      Conjunction(a1.unwrap && a2.unwrap)
    def empty: Conjunction = Conjunction(true)    
  }

  implicit val conjunctionEq: Eq[Conjunction] = new Eq[Conjunction] {
    def eqv(a1: Conjunction, a2: Conjunction): Boolean =
      a1.unwrap == a2.unwrap
  }

}

object Main extends App {

  assert { (3 * 2) * (8 * 5) === 3 * (2 * (8 * 5)) }

  assert { List("la") ++ (List("di") ++ List("da")) === (List("la") ++ List("di")) ++ List("da") }

  println(s"List(1, 2, 3) |+| List(4, 5, 6): ${List(1, 2, 3) |+| List(4, 5, 6)}")

  println(s""""one" |+| "two": ${"one" |+| "two"}""")

  def doSomething[A: Semigroup](a1: A, a2: A): A = a1 |+| a2

  println(s"doSomething(3, 5): ${doSomething(3, 5)}")

  println(s"Semigroup[Int].combine(3, 5): ${Semigroup[Int].combine(3, 5)}")

  val x1 = Disjunction(true) |+| Disjunction(false)

  println(s"x1.unwrap: ${x1.unwrap}")

  val x2 = Monoid[Disjunction].empty |+| Disjunction(true)

  println(s"x2.unwrap: ${x2.unwrap}")

  val x3 = Conjunction(true) |+| Conjunction(false)

  println(s"x3.unwrap: ${x3.unwrap}")

  val x4 = Monoid[Conjunction].empty |+| Conjunction(true)

  println(s"x4.unwrap: ${x4.unwrap}")

  println(s"""Foldable[List].foldLeft(List(1, 2, 3), 1) {_ * _}:
    |${Foldable[List].foldLeft(List(1, 2, 3), 1) {_ * _}}""".stripMargin)

  println(s"""Foldable[List].fold(List(1, 2, 3))(Monoid[Int]):
    |${Foldable[List].fold(List(1, 2, 3))(Monoid[Int])}""".stripMargin)

  println(s"""List(1, 2, 3).foldMap(identity)(Monoid[Int]):
    |${List(1, 2, 3).foldMap(identity)(Monoid[Int])}""".stripMargin)

  val x5 = List(true, false, true) foldMap {Conjunction(_)}

  println(s"x5.unwrap: ${x5.unwrap}")

}
