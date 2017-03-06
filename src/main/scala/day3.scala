package day3

import cats._
import cats.instances.all._

object Catnip {
  implicit class IdOp[A](val a: A) extends AnyVal {
    def some: Option[A] = Some(a)
  }
  def none[A]: Option[A] = None
}

object Main extends App {

  val hs = Functor[List].map(List(1, 2, 3, 4)) ({(_: Int) * (_:Int)}.curried)

  println(s"Functor[List].map(hs) {_(9)}: ${Functor[List].map(hs) {_(9)}}")

  // Catersian

  import Catnip._

  println(s"9.some: ${9.some}")
  println(s"none[Int]: ${none[Int]}")

  import cats.syntax.cartesian._

  println(s"(3.some |@| 5.some) map { _ - _ }: ${(3.some |@| 5.some) map { _ - _ }}")

  println(s"(none[Int] |@| 5.some) map { _ - _ }: ${(none[Int] |@| 5.some) map { _ - _ }}")
 
  println(s"(3.some |@| none[Int]) map { _ - _ }: ${(3.some |@| none[Int]) map { _ - _ }}")

  println(s"""(List("ha", "heh", "hmm") |@| List("?", "!", ".")) map {_ + _}: 
    |${(List("ha", "heh", "hmm") |@| List("?", "!", ".")) map {_ + _}}""".stripMargin)

  println(s"1.some <* 2.some: ${1.some <* 2.some}")

  println(s"none[Int] <* 2.some: ${none[Int] <* 2.some}")

  println(s"1.some *> 2.some: ${1.some *> 2.some}")

  println(s"none[Int] *> 2.some: ${none[Int] *> 2.some}")

  // Apply

  println(s"""Apply[Option].ap({{(_: Int) + 3}.some })(9.some):
    |${Apply[Option].ap({{(_: Int) + 3}.some })(9.some)}""".stripMargin)

  println(s"""Apply[Option].ap({{(_: Int) + 3}.some })(10.some):
    |${Apply[Option].ap({{(_: Int) + 3}.some })(10.some)}""".stripMargin)

  println(s"""Apply[Option].ap({{(_: String) + "hahah"}.some })(none[String]):
    |${Apply[Option].ap({{(_: String) + "hahah"}.some })(none[String])}""".stripMargin)

  println(s"""Apply[Option].ap({ none[String => String] })("woot".some):
    |${Apply[Option].ap({ none[String => String] })("woot".some)}""".stripMargin)

  import cats.syntax.apply._

  println(s"""({(_: Int) + 3}.some) ap 9.some:
    |${({(_: Int) + 3}.some) ap 9.some}""".stripMargin)

  println(s"""({(_: Int) + 3}.some) ap 10.some:
    |${({(_: Int) + 3}.some) ap 10.some}""".stripMargin)

  println(s"""({(_: String) + "hahah"}.some) ap none[String]:
    |${({(_: String) + "hahah"}.some) ap none[String]}""".stripMargin)

  println(s"""(none[String => String]) ap "woot".some:
    |${(none[String => String]) ap "woot".some}""".stripMargin)

  println(s"""(3.some |@| List(4).some) map { _ :: _ }:
    |${(3.some |@| List(4).some) map { _ :: _ }}""".stripMargin)

  // map2
  println(s"""Apply[Option].map2(3.some, List(4).some) { _ :: _ }:
    |${Apply[Option].map2(3.some, List(4).some) { _ :: _ }}""".stripMargin)

  // ap2
  println(s"""Apply[Option].ap2({{ (_: Int) :: (_: List[Int]) }.some })(3.some, List(4).some):
    |${Apply[Option].ap2({{ (_: Int) :: (_: List[Int]) }.some })(3.some, List(4).some)}""".stripMargin)

  // tuple2
  println(s"""Apply[Option].tuple2(1.some, 2.some):
    |${Apply[Option].tuple2(1.some, 2.some)}""".stripMargin)

  // pure
  println(s"""Applicative[List].pure(1):
    |${Applicative[List].pure(1)}""".stripMargin)

  println(s"""Applicative[Option].pure(1):
    |${Applicative[Option].pure(1)}""".stripMargin)

  val F = Applicative[Option]

  println(s"""F.ap({ F.pure((_: Int) + 3) })(F.pure(9)):
    |${F.ap({ F.pure((_: Int) + 3) })(F.pure(9))}""".stripMargin)

  def sequenceA[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = list match {
    case Nil     => Applicative[F].pure(Nil: List[A])
    case x :: xs => (x |@| sequenceA(xs)) map {_ :: _} 
  }

  // sequenceA example

  println(s"""sequenceA(List(1.some, 2.some)):
    |${sequenceA(List(1.some, 2.some))}""".stripMargin)

  println(s"""sequenceA(List(3.some, none[Int], 1.some)):
    |${sequenceA(List(3.some, none[Int], 1.some))}""".stripMargin)

  println(s"""sequenceA(List(List(1, 2, 3), List(4, 5, 6))):
    |${sequenceA(List(List(1, 2, 3), List(4, 5, 6)))}""".stripMargin)

  val f = sequenceA[Function1[Int, ?], Int](List((_: Int) + 3, (_: Int) + 2, (_: Int) + 1))

  println(s"f(3): ${f(3)}")
}
