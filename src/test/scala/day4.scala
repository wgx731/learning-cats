package day4

import cats._
import cats.kernel.laws.GroupLaws

import org.specs2.Specification
import org.typelevel.discipline.specs2.Discipline
import cats.instances.AllInstances
import cats.syntax.AllSyntax

trait CatsSpec extends Specification with Discipline with AllInstances with AllSyntax

class IntSpec extends CatsSpec { def is = s2"""
  (Int, +) should
  form a monoid $e1
  """

  def e1 = checkAll("Int", GroupLaws[Int].monoid(Monoid[Int]))
}
