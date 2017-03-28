package day6

import collection.immutable.BitSet

object Main extends App {

  def foo = for {
    x <- Some(3)
    y <- Some("!")
  } yield x.toString + y

  println(s"$foo")

  val bits = BitSet(1, 2, 3)

  println(s"""${
  for {
    x <- bits
  } yield x.toFloat
  }""")

  println(s"""${
  for {
    i <- List(1, 2, 3)
    j <- Some(1)
  } yield i + j
  }""")

  println(s"""${
  for {
    i <- Map(1 -> 2)
    j <- Some(3)
  } yield j
  }""")

  def isBigGang(x: Int): (Boolean, String) =
    (x > 9, "Compared gang size to 9.")

  implicit class PairOps[A](pair: (A, String)) {
    def applyLog[B](f: A => (B, String)): (B, String) = {
      val (x, log) = pair
      val (y, newlog) = f(x)
      (y, log ++ newlog)
    }
  }

  println(s"""${
  (3, "Smallish gang.") applyLog isBigGang
  }""")

  import cats.data.Writer

  println(s"${Writer("Smallish gang.", 3)}")
  // could not find implicit value for evidence parameter of type cats.Monoid[String]
  //println(s"${Writer.value[String, Int](3)}")
  println(s"${Writer.tell[String]("Log something")}")

  import cats._
  import cats.instances.all._
  import cats.syntax.functor._

  println(s"""${
    val a = (_: Int) * 2
    a(4)
  }""")

  import cats.syntax.flatMap._

  val addStuff: Int => Int = for {
    a <- (_: Int) * 2
    b <- (_: Int) + 10
  } yield a + b

  println(s"${addStuff(3)}")

}
