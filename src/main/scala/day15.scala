package day15

import cats._
import cats.data._
import cats.implicits._
import cats.arrow.Arrow

sealed trait Person {}
case object John extends Person {}
case object Mary extends Person {}
case object Sam extends Person {}

sealed trait Breakfast {}
case object Eggs extends Breakfast {}
case object Oatmeal extends Breakfast {}
case object Toast extends Breakfast {}
case object Coffee extends Breakfast {}

object Isomorphisms {
  trait Isomorphism[Arrow[_, _], A, B] { self =>
    def to: Arrow[A, B]
    def from: Arrow[B, A]
  }
  type IsoSet[A, B] = Isomorphism[Function1, A, B]
  type <=>[A, B] = IsoSet[A, B]
}

import Isomorphisms._

sealed trait Family {}
case object Mother extends Family {}
case object Father extends Family {}
case object Child extends Family {}

sealed trait Relic {}
case object Feather extends Relic {}
case object Stone extends Relic {}
case object Flower extends Relic {}

val isoFamilyRelic = new (Family <=> Relic) {
  val to: Family => Relic = {
    case Mother => Feather
    case Father => Stone
    case Child  => Flower
  }
  val from: Relic => Family = {
    case Feather => Mother
    case Stone   => Father
    case Flower  => Child
  }
}


object Main extends App {

  val a: Set[Person] = Set[Person](John, Mary, Sam)

  println(s"""
    a
    ${a}
  """)

  val favoriteBreakfast: Person => Breakfast = {
    case John => Eggs
    case Mary => Coffee
    case Sam  => Coffee
  }

  println(s"""
    favoriteBreakfast(John)
    ${favoriteBreakfast(John)}
  """)

  val favoritePerson: Person => Person = {
    case John => Mary
    case Mary => John
    case Sam  => Mary
  }

  println(s"""
    favoritePerson(Mary)
    ${favoritePerson(Mary)}
  """)

  println(s"""
    identity(John)
    ${identity(John)}
  """)

  val favoritePersonsBreakfast =
    favoriteBreakfast compose favoritePerson

  println(s"""
    favoritePersonsBreakfast(Mary)
    ${favoritePersonsBreakfast(Mary)}
  """)

  val johnPoint: Unit => Person = { case () => John }

  val johnFav = favoriteBreakfast compose johnPoint

  println(s"""
    johnFav(())
    ${johnFav(())}
  """)

  val f = (_:Int) + 1
  val g = (_:Int) * 100

  println(s"""
    (f >>> g)(2)
    ${(f >>> g)(2)}
  """)

  println(s"""
    (f <<< g)(2)
    ${(f <<< g)(2)}
  """)

  val f_first = f.first[Int]

  println(s"""
    f_first((1, 1))
    ${f_first((1, 1))}
  """)

  val f_second = f.second[Int]

  println(s"""
    f_second((1, 1))
    ${f_second((1, 1))}
  """)

  println(s"""
    (f split g)((1, 1))
    ${(f split g)((1, 1))}
  """)

}
