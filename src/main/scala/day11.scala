package day11

import cats._
import cats.data._
import cats.implicits._

trait Read[A] {
  def reads(s: String): Option[A]
}

trait ReadInstances {

  implicit val stringRead: Read[String] =
    Read.read[String] { Some(_) }

  implicit val intRead: Read[Int] =
      Read.read[Int] { s =>
        try {
          Some(s.toInt)
        } catch {
          case e: NumberFormatException => None
        }
      }
}


object Read extends ReadInstances {

  def read[A](f: String => Option[A]): Read[A] = new Read[A] {
    def reads(s: String): Option[A] = f(s)
  }

  def apply[A: Read]: Read[A] = implicitly[Read[A]]
}


sealed abstract class Fix[S[_], A] extends Serializable {
  def out: S[Fix[S, A]]
}
object Fix {
  case class In[S[_], A](out: S[Fix[S, A]]) extends Fix[S, A]
}

sealed trait ListF[+Next, +A]
object ListF {
  case class NilF() extends ListF[Nothing, Nothing]
  case class ConsF[A, Next](a: A, n: Next) extends ListF[Next, A]
}
object GenericList {
  type GenericList[A] = Fix[ListF[+?, A], A]
  def nil[A]: GenericList[A] = Fix.In[ListF[+?, A], A](ListF.NilF())
  def cons[A](a: A, xs: GenericList[A]): GenericList[A] =
    Fix.In[ListF[+?, A], A](ListF.ConsF(a, xs))
}

object Main extends App {

  def triangle4: Unit = {
    println("*")
    println("**")
    println("***")
    println("****")
    println()
  }

  println("triangle4:")
  triangle4

  def triangle(side: Int): Unit = {
    (1 to side) foreach { row =>
      (1 to row) foreach { col =>
        print("*")
      }
      println()
    }
  }

  println("triangle:")
  triangle(4)

  def append[A](list: List[A], ys: List[A]): List[A] =
    list.foldLeft(ys) { (acc, x) => x :: acc }

  println(s"""
  append(List(1, 2, 3), List(4, 5, 6))
  ${append(List(1, 2, 3), List(4, 5, 6))}
  """)

  def sum(list: List[Int]): Int =
    list.foldLeft(0) { _ + _ }

  println(s"""
  sum(List(1, 2, 3))
  ${sum(List(1, 2, 3))}
  """)

  println(s"""
  Read[Int].reads("1")
  ${Read[Int].reads("1")}
  """)

  import GenericList.{ cons, nil }

  println(s"""
  cons(1, nil)
  ${cons(1, nil)}
  """)


  println(s"""
  Const(1) map { (_: String) + "!" }
  ${Const(1) map { (_: String) + "!" }}
  """)

  println(s"""
  Const(2).retag[String => String] ap Const(1).retag[String]
  ${Const(2).retag[String => String] ap Const(1).retag[String]}
  """)



}
