package aoc21
package day06

import aoc21.PureDay
import cats.effect.IO
import cats.syntax.all.*
import scala.annotation.tailrec

class LanternFishes(state: Map[Int, Long]):
  def next = LanternFishes(
    state.toList
      .map((i, v) => if i == 0 then Map(6 -> v, 8 -> v) else Map(i - 1 -> v))
      .combineAll
  )

  def size = state.values.sum

object Program extends PureDay:
  type A = LanternFishes
  def parse(input: List[String]): IO[this.A] =
    input match
      case List(line) =>
        line
          .split(",")
          .toList
          .traverse(_.toIntIO)
          .map(_.groupMapReduce(identity)(_ => 1L)(_ + _))
          .map(LanternFishes(_))
      case _ => IO.raiseError(new Exception("Expecting single line input"))

  @tailrec
  def parts(remaining: Int, fish: LanternFishes): LanternFishes =
    if remaining == 0 then fish
    else parts(remaining - 1, fish.next)

  def part1(input: this.A): String =
    parts(80, input).size.toString

  def part2(input: this.A): String =
    parts(256, input).size.toString
end Program
