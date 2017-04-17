package day10

import java.net.URI
import scala.concurrent.{Future, ExecutionContext}
import cats._
import cats.data._
import cats.implicits._

case class User(
  id: Long,
  parentId: Long,
  name: String,
  email: String
)

trait HttpService {
  def get(uri: URI): String
}

trait UserRepo {
  def get(id: Long): Option[User]
  def find(name: String): Option[User]
}

trait Config {
  def userRepo: UserRepo
  def httpService: Option[HttpService]
}

sealed trait Error
object Error {
  final case class UserNotFound(userId: Long) extends Error
  final case class ConnectionError(message: String) extends Error
}

object Main extends App {

  type ReaderTOption[A, B] = Kleisli[Option, A, B]

  object ReaderTOption {
    def ro[A, B](f: A => Option[B]): ReaderTOption[A, B] = Kleisli(f)
  }

  type StateTReaderTOption[C, S, A] = StateT[({type l[X] = ReaderTOption[C, X]})#l, S, A]

  object StateTReaderTOption {
    def state[C, S, A](f: S => (S, A)): StateTReaderTOption[C, S, A] =
      StateT[({type l[X] = ReaderTOption[C, X]})#l, S, A] {
        s: S => Monad[({type l[X] = ReaderTOption[C, X]})#l].pure(f(s))
      }
    def get[C, S]: StateTReaderTOption[C, S, S] =
      state { s => (s, s) }
    def put[C, S](s: S): StateTReaderTOption[C, S, Unit] =
      state { _ => (s, ()) }
    def ro[C, S, A](f: C => Option[A]): StateTReaderTOption[C, S, A] =
      StateT[({type l[X] = ReaderTOption[C, X]})#l, S, A] {
        s: S =>
          ReaderTOption.ro[C, (S, A)]{
            c: C => f(c) map {(s, _)}
          }
      }
  }


  trait Users {
    def getUser[S](id: Long): StateTReaderTOption[Config, S, User] =
      StateTReaderTOption.ro[Config, S, User] {
        case config => config.userRepo.get(id)
      }
    def findUser[S](name: String): StateTReaderTOption[Config, S, User] =
      StateTReaderTOption.ro[Config, S, User] {
        case config => config.userRepo.find(name)
      }
  }

  trait Https {
    def getHttp(uri: URI): ReaderTOption[Config, String] =
      ReaderTOption.ro {
        case config => config.httpService map {_.get(uri)}
      }
  }

  trait Program extends Users {
    def stackManip: StateTReaderTOption[Config, Stack, Unit] =
      for {
        u <- getUser(2)
        a <- push(u.name)
      } yield(a)
  }

  object TestMain extends Program {
    def run(s: Stack, config: Config): Option[(Stack, Unit)] =
      stackManip.run(s).run(config)
  }

  val testUsers = List(User(0, 0, "Vito", "vito@example.com"),
    User(1, 0, "Michael", "michael@example.com"),
    User(2, 0, "Fredo", "fredo@example.com"))

  val dummyConfig: Config = new Config {

    def userRepo: UserRepo = new UserRepo {
      def get(id: Long): Option[User] =
        testUsers find { _.id === id }
      def find(name: String): Option[User] =
        testUsers find { _.name === name }
    }

    def httpService: Option[HttpService] = None
  }

  type Stack = List[String]

  import StateTReaderTOption.{get, put}

  val pop: StateTReaderTOption[Config, Stack, String] =
    for {
      s <- get[Config, Stack]
      (x :: xs) = s
      _ <- put(xs)
    } yield x

  def push(x: String): StateTReaderTOption[Config, Stack, Unit] =
    for {
      xs <- get[Config, Stack]
      r <- put(x :: xs)
    } yield r

  def stackManip: StateTReaderTOption[Config, Stack, String] =
    for {
      _ <- push("Fredo")
      a <- pop
      b <- pop
    } yield(b)

  println(s"""
    stackManip.run(List("Hyman Roth")).run(dummyConfig)
    ${stackManip.run(List("Hyman Roth")).run(dummyConfig)}
  """)

  println(s"""
    TestMain.run(List("Hyman Roth"), dummyConfig)
    ${TestMain.run(List("Hyman Roth"), dummyConfig)}
  """)

  def followers(userId: Long)
  (implicit ec: ExecutionContext): EitherT[Future, Error, List[User]] =
    userId match {
      case y if (y >= 0L && y < 3L) =>
        EitherT.right(Future { List(
          testUsers(y.toInt) 
        ) })
      case x =>
        println("not found")
        EitherT.left(Future.successful { Error.UserNotFound(x) })
    }

  def isFriends(user1: Long, user2: Long)
  (implicit ec: ExecutionContext): EitherT[Future, Error, Boolean] =
    for{
      a <- followers(user1)
      b <- followers(user2)
    } yield a.exists(_.id == user2) && b.exists(_.id == user1)


  implicit val ec = scala.concurrent.ExecutionContext.global
  import scala.concurrent.Await
  import scala.concurrent.duration._

  println(s"""
    Await.result(isFriends(0, 1).value, 1 second)
    ${Await.result(isFriends(0, 1).value, 1 second)}
  """)

  println(s"""
    Await.result(isFriends(0, 0).value, 1 second)
    ${Await.result(isFriends(0, 0).value, 1 second)}
  """)

  println(s"""
    Await.result(isFriends(2, 3).value, 1 second)
    ${Await.result(isFriends(2, 3).value, 1 second)}
  """)

}
