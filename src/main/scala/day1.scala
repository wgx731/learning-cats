package day1

import cats._
import cats.instances.all._

object Main extends App {

  import cats.syntax.eq._
  /* Eq - https://github.com/typelevel/cats/blob/master/kernel/src/main/scala/cats/kernel/Eq.scala */
  println(s"1 === 1: ${1 === 1}")
  // 1 === "foo"

  /* TrafficLight Type */
  sealed trait TrafficLight
  object TrafficLight {
    def red: TrafficLight = Red
    def yellow: TrafficLight = Yellow
    def green: TrafficLight = Green
    case object Red extends TrafficLight
    case object Yellow extends TrafficLight
    case object Green extends TrafficLight
  }

  implicit val trafficLightEq: Eq[TrafficLight] =
    new Eq[TrafficLight] {
      def eqv(a1: TrafficLight, a2: TrafficLight): Boolean = a1 == a2
  }

  println(s"TrafficLight.red === TrafficLight.yellow: ${TrafficLight.red === TrafficLight.yellow}")


  /* Order - https://github.com/typelevel/cats/blob/master/kernel/src/main/scala/cats/kernel/Order.scala */
  // 1 compare 2.0
  println(s"1.0 max 2.0: ${1.0 max 2.0}")

  import cats.syntax.partialOrder._
  /* PartialOrder - https://github.com/typelevel/cats/blob/master/kernel/src/main/scala/cats/kernel/PartialOrder.scala */
  println(s"1 tryCompare 2: ${1 tryCompare 2}")
  println(s"1.0 tryCompare Double.NaN: ${1.0 tryCompare Double.NaN}")

  def lt[A: PartialOrder](a1: A, a2: A): Boolean = a1 <= a2
  // println(s"lt[Int](1, 2.0): ${lt[Int](1, 2.0)}")
  println(s"lt(1, 2.0): ${lt(1, 2.0)}")

  /* Show - https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/Show.scala */
  import cats.syntax.show._
  println(s"3.show: ${3.show}")
  println(s"""\"hello\".show: ${"hello".show}""")

  case class Person(name: String, age: Int)
  case class Car(model: String)

  implicit val personShow = Show.show[Person](_.name)
  println(s"""Person("Alice", 18).show: ${Person("Alice", 18).show}""")
  implicit val carShow = Show.fromToString[Car]
  println(s"""Car("CR-V"): ${Car("CR-V")}""")

  /* Read - no equivalent */

  /* Enum - no equivalent */

  /* Numeric - no equivalent */

}
