package day8

sealed trait Toy[+A, +Next]

object Toy {
  case class Output[A, Next](a: A, next: Next) extends Toy[A, Next]
  case class Bell[Next](next: Next) extends Toy[Nothing, Next]
  case class Done() extends Toy[Nothing, Nothing]
}

sealed trait CharToy[+Next]

object CharToy {
  case class CharOutput[Next](a: Char, next: Next) extends CharToy[Next]
  case class CharBell[Next](next: Next) extends CharToy[Next]
  case class CharDone() extends CharToy[Nothing]

  def output[Next](a: Char, next: Next): CharToy[Next] = CharOutput(a, next)
  def bell[Next](next: Next): CharToy[Next] = CharBell(next)
  def done: CharToy[Nothing] = CharDone()
}


object Main extends App {

  println(s"""
    Toy.Output('A', Toy.Done()):
    ${Toy.Output('A', Toy.Done())}
  """)

  println(s"""
    Toy.Bell(Toy.Output('A', Toy.Done())):
    ${Toy.Bell(Toy.Output('A', Toy.Done()))}
  """)

  import CharToy._

  println(s"""
    output('A', done)
    ${output('A', done)}
  """)

  println(s"""
    bell(output('A', done))
    ${bell(output('A', done))}
  """)

  import cats._
  import cats.data._
  import cats.implicits._
  import cats.free.{Free, Trampoline}

  def even[A](ns: List[A]): Trampoline[Boolean] =
    ns match {
      case Nil => Trampoline.done(true)
      case x :: xs => Trampoline.suspend(odd(xs))
    }
  def odd[A](ns: List[A]): Trampoline[Boolean] =
    ns match {
      case Nil => Trampoline.done(false)
      case x :: xs => Trampoline.suspend(even(xs))
    }

  println(s"""
    even(List(2, 4)).run
    ${even(List(2, 4)).run}
  """)

  println(s"""
    even((0 to 3000).toList).run
    ${even((0 to 3000).toList).run}
  """)

  import cats._
  import cats.data._
  import cats.implicits._

  case class LongProduct(value: Long)
  implicit val longProdMonoid: Monoid[LongProduct] = new Monoid[LongProduct] {
    def empty: LongProduct = LongProduct(1)
    def combine(x: LongProduct, y: LongProduct): LongProduct = LongProduct(x.value * y.value)
  }

  def powWriter(x: Long, exp: Long): Writer[LongProduct, Unit] =
    exp match {
      case 0 => Writer(LongProduct(1L), ())
      case m =>
        Writer(LongProduct(x), ()) >>= { _ => powWriter(x, exp - 1) }
    }

  println(s"""
    powWriter(2, 3).run
    ${powWriter(2, 3).run}
  """)

  /* stack overflow
  println(s"""
   powWriter(1, 10000).run
    ${powWriter(1, 10000).run}
  """)
  */

 def tailRecM[A, B] = FlatMap[Writer[Vector[String], ?]].tailRecM[A, B] _

 def powWriter2(x: Long, exp: Long): Writer[LongProduct, Unit] =
   FlatMap[Writer[LongProduct, ?]].tailRecM(exp) {
     case 0L      => Writer.value[LongProduct, Either[Long, Unit]](Right(()))
     case m: Long => Writer.tell(LongProduct(x)) >>= { _ => Writer.value(Left(m - 1)) }
   }

 println(s"""
   powWriter2(1, 10000).run
   ${powWriter2(1, 10000).run}
   """)


}
