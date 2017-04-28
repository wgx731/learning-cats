package day16

import cats._
import cats.data._
import cats.implicits._

case class LString(value: String)

object Main extends App {

  val f: Int => LString = (x: Int) => LString(if (x < 0) "" else x.toString)

  println(s"""
    f(-1)
    ${f(-1)}
  """)

  println(s"""
    f(10)
    ${f(10)}
  """)

  println(s"""
    10 |+| Monoid[Int].empty
    ${10 |+| Monoid[Int].empty}
  """)

  assert((1 |+| 1.inverse) === Monoid[Int].empty)

  println(s"""
    (1 |+| 1.inverse)
    ${(1 |+| 1.inverse)}
  """)

}
