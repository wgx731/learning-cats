package day13

import cats._
import cats.data._
import cats.implicits._

object OddEven0 {
  def odd(n: Int): String = even(n - 1)
  def even(n: Int): String = if (n <= 0) "done" else odd(n - 1)
}

object OddEven1 {
  def odd(n: Int): Eval[String] = Eval.defer {even(n - 1)}
  def even(n: Int): Eval[String] =
    Eval.now { n <= 0 } flatMap {
      case true => Eval.now {"done"}
      case _    => Eval.defer { odd(n - 1) }
    }
}

object Main extends App {

  val one: Id[Int] = 1

  println(s"""
    Functor[Id].map(one) { _ + 1 }
    ${Functor[Id].map(one) { _ + 1 }}
  """)

  println(s"""
    Apply[Id].ap({ _ + 1 }: Id[Int => Int])(one)
    ${Apply[Id].ap({ _ + 1 }: Id[Int => Int])(one)}
  """)

  println(s"""
    FlatMap[Id].flatMap(one) { _ + 1 }
    ${FlatMap[Id].flatMap(one) { _ + 1 }}
  """)

  var g: Int = 0

  g = 2

  val x = Eval.later {
    g = g + 1
    g
  }

  println(s"""
    x.value
    ${x.value}
  """)

  println(s"""
    x.value
    ${x.value}
  """)

  val y = Eval.now {
    g = g + 1
    g
  }

  println(s"""
    y.value
    ${y.value}
  """)

  println(s"""
    y.value
    ${y.value}
  """)

  val z = Eval.always {
    g = g + 1
    g
  }

  println(s"""
    z.value
    ${z.value}
  """)

  println(s"""
    z.value
    ${z.value}
  """)

  /*
  println(s"""
    OddEven0.even(200000)
    ${OddEven0.even(200000)}
  """)
  */

  println(s"""
    OddEven1.even(200000).value
    ${OddEven1.even(200000).value}
  """)

}
