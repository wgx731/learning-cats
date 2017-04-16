package day9

import cats._
import cats.data._
import cats.implicits._
import scala.util.Try
import scala.annotation.tailrec

case class Prob[A](list: List[(A, Double)])

trait ProbInstances { self =>

  def flatten[B](xs: Prob[Prob[B]]): Prob[B] = {
    def multall(innerxs: Prob[B], p: Double) =
      innerxs.list map { case (x, r) => (x, p * r) }
    Prob((xs.list map { case (innerxs, p) => multall(innerxs, p) }).flatten)
  }

  implicit val probInstance: Monad[Prob] = new Monad[Prob] {
    def pure[A](a: A): Prob[A] = Prob((a, 1.0) :: Nil)
    def flatMap[A, B](fa: Prob[A])(f: A => Prob[B]): Prob[B] = self.flatten(map(fa)(f))
    override def map[A, B](fa: Prob[A])(f: A => B): Prob[B] =
      Prob(fa.list map { case (x, p) => (f(x), p) })
    def tailRecM[A, B](a: A)(f: A => Prob[Either[A, B]]): Prob[B] = {
      val buf = List.newBuilder[(B, Double)]
      @tailrec def go(lists: List[List[(Either[A, B], Double)]]): Unit =
        lists match {
          case (ab :: abs) :: tail => ab match {
            case (Right(b), p) =>
              buf += ((b, p))
              go(abs :: tail)
            case (Left(a), p) =>
              go(f(a).list :: abs :: tail)
          }
            case Nil :: tail => go(tail)
            case Nil => ()
        }
        go(f(a).list :: Nil)
        Prob(buf.result)
    }
  }

  implicit def probShow[A]: Show[Prob[A]] = Show.fromToString
}

case object Prob extends ProbInstances

sealed trait Coin

object Coin {

  case object Heads extends Coin
  case object Tails extends Coin

  implicit val coinEq: Eq[Coin] = new Eq[Coin] {
    def eqv(a1: Coin, a2: Coin): Boolean = a1 == a2
  }

  def heads: Coin = Heads
  def tails: Coin = Tails
}

object Main extends App {

  def join[F[_]: FlatMap, A](fa: F[F[A]]): F[A] =
    fa.flatten

  println(s"""
    join(1.some.some)
    ${join(1.some.some)}
  """)

  println(s"""
    FlatMap[Option].flatten(1.some.some)
    ${FlatMap[Option].flatten(1.some.some)}
  """)

  def binSmalls(acc: Int, x: Int): Option[Int] =
    if (x > 9) none[Int] else (acc + x).some

  println(s"""
    (Foldable[List].foldM(List(2, 8, 3, 1), 0) {binSmalls})
    ${(Foldable[List].foldM(List(2, 8, 3, 1), 0) {binSmalls})}
  """)

  println(s"""
    (Foldable[List].foldM(List(2, 11, 3, 1), 0) {binSmalls})
    ${(Foldable[List].foldM(List(2, 11, 3, 1), 0) {binSmalls})}
  """)

  def foldingFunction(list: List[Double], next: String): Option[List[Double]] =
    (list, next) match {
      case (x :: y :: ys, "*") => ((y * x) :: ys).some
      case (x :: y :: ys, "+") => ((y + x) :: ys).some
      case (x :: y :: ys, "-") => ((y - x) :: ys).some
      case (xs, numString) => parseInt(numString) map {_ :: xs}
    }

  def parseInt(x: String): Option[Int] =
    (scala.util.Try(x.toInt) map { Some(_) }
      recover { case _: NumberFormatException => None }).get

  println(s"""
    foldingFunction(List(3, 2), "*")
    ${foldingFunction(List(3, 2), "*")}
  """)

  println(s"""
    foldingFunction(Nil, "*")
    ${foldingFunction(Nil, "*")}
  """)

  println(s"""
    foldingFunction(Nil, "haha")
    ${foldingFunction(Nil, "haha")}
  """)

  def solveRPN(s: String): Option[Double] =
    for {
      List(x) <- (
        Foldable[List].foldM(
          s.split(' ').toList,
          Nil: List[Double]
        ) {foldingFunction}
      )
    } yield x

  println(s"""
    solveRPN("1 2 * 4 +")
    ${solveRPN("1 2 * 4 +")}
  """)

  println(s"""
    solveRPN("1 2 * 4")
    ${solveRPN("1 2 * 4")}
  """)

  println(s"""
    solveRPN("1 haha * 4 +")
    ${solveRPN("1 haha * 4 +")}
  """)

  val f = Kleisli { (x: Int) => (x + 1).some }
  val g = Kleisli { (x: Int) => (x * 100).some }

  println(s"""
    4.some >>= (f compose g).run
    ${4.some >>= (f compose g).run}
  """)

  println(s"""
    4.some >>= (f andThen g).run
    ${4.some >>= (f andThen g).run}
  """)

  val l = f.lift[List]

  println(s"""
    List(1, 2, 3) >>= l.run
    ${List(1, 2, 3) >>= l.run}
  """)

  println(s"""
    Prob((3, 0.5) :: (5, 0.25) :: (9, 0.25) :: Nil) map {-_}
    ${Prob((3, 0.5) :: (5, 0.25) :: (9, 0.25) :: Nil) map {-_}}
  """)

  import Coin.{heads, tails}

  def coin: Prob[Coin] = Prob(heads -> 0.5 :: tails -> 0.5 :: Nil)

  def loadedCoin: Prob[Coin] = Prob(heads -> 0.1 :: tails -> 0.9 :: Nil)

  def flipThree: Prob[Boolean] = for {
    a <- coin
    b <- coin
    c <- loadedCoin
  } yield { List(a, b, c) forall {_ === tails} }

  println(s"""
    flipThree
    ${flipThree}
  """)

  val xss = List(List(1), List(2, 3), List(4))

  println(s"""
    xss.flatten
    ${xss.flatten}
  """)

  println(s"""
    xss.foldLeft(List(): List[Int]) { _ ++ _ }
    ${xss.foldLeft(List(): List[Int]) { _ ++ _ }}
  """)

}
