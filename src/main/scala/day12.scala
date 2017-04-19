package day12

import cats._
import cats.data._
import cats.implicits._

object Main extends App {

  println(s"""
    List(1, 2, 3) traverse[Id, Int] { (x: Int) => x + 1 }
    ${List(1, 2, 3) traverse[Id, Int] { (x: Int) => x + 1 }}
  """)

  println(s"""
    List(1, 2, 3) traverse { (x: Int) => (Some(x + 1): Option[Int]) }
    ${List(1, 2, 3) traverse { (x: Int) => (Some(x + 1): Option[Int]) }}
  """)

  println(s"""
    List(1, 2, 3) traverse { (x: Int) => None }
    ${List(1, 2, 3) traverse { (x: Int) => None }}
  """)

  def reduce[A, B, F[_]](fa: F[A])(f: A => B)
  (implicit FF: Traverse[F], BB: Monoid[B]): B =
  {
    val g: A => Const[B, Unit] = { (a: A) => Const((f(a))) }
    val x = FF.traverse[Const[B, ?], A, Unit](fa)(g)
    x.getConst
  }

  println(s"""
    reduce(List('a', 'b', 'c')) { c: Char => c.toInt }
    ${reduce(List('a', 'b', 'c')) { c: Char => c.toInt }}
  """)

  import scala.concurrent.{Future, ExecutionContext, Await}
  import scala.concurrent.duration._

  val x = {
    implicit val ec = scala.concurrent.ExecutionContext.global
    List(Future { 1 }, Future { 2 }).sequence
  }

  println(s"""
    Await.result(x, 1 second)
    ${Await.result(x, 1 second)}
  """)

  println(s"""
    List(Right(1): Either[String, Int]).sequenceU
    ${List(Right(1): Either[String, Int]).sequenceU}
  """)

  println(s"""
    List(Right(1): Either[String, Int], Left("boom"): Either[String, Int]).sequenceU
    ${List(Right(1): Either[String, Int], Left("boom"): Either[String, Int]).sequenceU}
  """)

  println(s"""
    List(1, 2, 3) filterA { x => List(true, false) } 
    ${List(1, 2, 3) filterA { x => List(true, false) }}
  """)

  println(s"""
    Vector(1, 2, 3) filterA { x => Vector(true, false) }
    ${Vector(1, 2, 3) filterA { x => Vector(true, false) }}
  """)

  def foo[F[_]: Applicative](fa: F[Int]): F[Int] = fa

  //foo(Right(1): Either[String, Int])

  def fooU[FA](fa: FA)(implicit U: Unapply[Applicative, FA]): U.M[U.A] =
             U.subst(fa)

  println(s"""
    fooU(Right(1): Either[String, Int]) 
    ${fooU(Right(1): Either[String, Int])}
  """)

  def contents[F[_], A](fa: F[A])(implicit FF: Traverse[F]): Const[List[A], F[Unit]] =
  {
    val contentsBody: A => Const[List[A], Unit] = { (a: A) => Const(List(a)) }
    FF.traverseU(fa)(contentsBody)
  }

  println(s"""
    contents(Vector(1, 2, 3)).getConst
    ${contents(Vector(1, 2, 3)).getConst}
  """)

  import Func.{ appFunc, appFuncU }

  type Count[A] = Const[Int, A]

  def liftInt(i: Int): Count[Unit] = Const(i)
  def count[A](a: A): Count[Unit] = liftInt(1)
  val countChar: AppFunc[Count, Char, Unit] = appFunc(count)

  val text = ("Faith, I must leave thee, love, and shortly too.\n" +
    "My operant powers their functions leave to do.\n").toList

  println(s"""
    countChar traverse text
    ${countChar traverse text}
  """)

}
