package day5

import cats._
import cats.instances.all._
import cats.syntax.flatMap._


object Catnip {
  implicit class IdOp[A](val a: A) extends AnyVal {
    def some: Option[A] = Some(a)
  }
  def none[A]: Option[A] = None
}

object Main extends App {

  println(s"""${(Right(3): Either[String, Int]) flatMap {
    x ⇒ Right(x + 1)
  }}""")

  import Catnip._
  println(s"""${"wisdom".some map { _ + "!" }}""")
  println(s"""${none[String] map { _ + "!" }}""")

  import cats.syntax.apply._
  println(s"""${({(_: Int) + 3}.some) ap 3.some}""")
  println(s"""${none[String ⇒ String] ap "greed".some}""")
  println(s"""${({(_: String).toInt}.some) ap none[String]}""")

  println(s"""${3.some flatMap {
    (x: Int) ⇒ (x + 1).some 
  }}""")
  println(s"""${"smile".some flatMap { 
    (x: String) ⇒  (x + " :)").some
  }}""")
  println(s"""${
    none[Int] flatMap { (x: Int) ⇒ (x + 1).some }
  }""")
  println(s"""${
    none[String] flatMap { (x: String) ⇒  (x + " :)").some }
  }""")

  type Birds = Int
  case class Pole(left: Birds, right: Birds) {
    def landLeft(n: Birds): Pole = copy(left = left + n)
    def landRight(n: Birds): Pole = copy(right = right + n)
  }
  
  println(s"""${
     Pole(0, 0).landLeft(2)
  }""")
  println(s"""${
    Pole(1, 2).landRight(1)
  }""")
  println(s"""${
    Pole(1, 2).landRight(-1)
  }""")
  println(s"""${
    Pole(0, 0).landLeft(1).landRight(1).landLeft(2)
  }""")
  println(s"""${
    Pole(0, 0).landLeft(1).landRight(4).landLeft(-1).landRight(-2)
  }""")

  case class Pole2(left: Birds, right: Birds) {
    def landLeft(n: Birds): Option[Pole2] =
      if (math.abs((left + n) - right) < 4) copy(left = left + n).some
      else none[Pole2]
    def landRight(n: Birds): Option[Pole2] =
      if (math.abs(left - (right + n)) < 4) copy(right = right + n).some
      else none[Pole2]
    def banana: Option[Pole2] = none[Pole2]
  }
  println(s"""${
     Pole2(0, 0).landLeft(2)
  }""")
  println(s"""${
     Pole2(0, 3).landLeft(10)
  }""")
  println(s"""${
    Monad[Option].pure(Pole2(0, 0)) >>= {_.landRight(2)} >>=
    {_.landLeft(2)} >>= {_.landRight(2)}
  }""")
  println(s"""${
    Monad[Option].pure(Pole2(0, 0)) >>= {_.landLeft(1)} >>=
    {_.landRight(4)} >>= {_.landLeft(-1)} >>= {_.landRight(-2)}
  }""")
  println(s"""${
    Monad[Option].pure(Pole2(0, 0)) >>= {_.landLeft(1)} >>=
    {_.banana} >>= {_.landRight(1)}
  }""")

  println(s"""${
    none[Int] >> 3.some
  }""")
  println(s"""${
    3.some >> 4.some
  }""")

  import cats._
  import cats.syntax.show._

  println(s"""${
    3.some >>= { x ⇒ "!".some >>= { y ⇒ (x.show + y).some } }
  }""")

  println(s"""${
    for {
      x <- 3.some
      y <- "!".some
    } yield (x.show + y)
  }""")

  import cats._
  import cats.instances.all._
  import cats.syntax.cartesian._

  println(s"""${
    (List(1, 2, 3) |@| List(10, 100, 100)) map { _ * _ }
  }""")

  import cats.syntax.flatMap._

  println(s"""${
    List(3, 4, 5) >>= { x ⇒ List(x, -x) }
  }""")

  println(s"""${
    for {
      n <- List(1, 2)
      ch <- List('a', 'b')
    } yield (n, ch)
  }""")

  println(s"""${
    for {
      x <- (1 to 50).toList if x.show contains '7'
    } yield x
  }""")

  import cats.syntax.functorFilter._
  val english = Map(1 -> "one", 3 -> "three", 10 -> "ten")
  println(s"""${
    (1 to 50).toList mapFilter { english.get(_) }
  }""")

  case class KnightPos(c: Int, r: Int) {
    def move: List[KnightPos] =
      for {
        KnightPos(c2, r2) <- List(KnightPos(c + 2, r - 1), KnightPos(c + 2, r + 1),
          KnightPos(c - 2, r - 1), KnightPos(c - 2, r + 1),
          KnightPos(c + 1, r - 2), KnightPos(c + 1, r + 2),
          KnightPos(c - 1, r - 2), KnightPos(c - 1, r + 2)) if (
            ((1 to 8).toList contains c2) && ((1 to 8).toList contains r2))
      } yield KnightPos(c2, r2)
    def in3: List[KnightPos] =
      for {
        first <- move
        second <- first.move
        third <- second.move
      } yield third
    def canReachIn3(end: KnightPos): Boolean = in3 contains end
  }
  println(s"""${
    KnightPos(6, 2).move
  }""")
  println(s"""${
    KnightPos(6, 2) canReachIn3 KnightPos(6, 1)
  }""")


}
