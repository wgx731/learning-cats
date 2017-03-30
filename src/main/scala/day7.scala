package day7

import cats._
import cats.implicits._
import cats.data._
import cats.data.{ NonEmptyList => NEL }

object Main extends App {

  println(s"""${
    NEL.of(1)
  }""")

  for {
    e1 <- Ior.right[NEL[String], Int](1)
    e2 <- Ior.both[NEL[String], Int](NEL.of("event 2 warning"), e1 + 1)
    e3 <- Ior.both[NEL[String], Int](NEL.of("event 3 warning"), e2 + 1)
  } yield (e1 |+| e2 |+| e3)

}
