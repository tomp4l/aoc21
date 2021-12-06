package aoc21
package day06

import aoc21.PureDay
import cats.effect.IO
import cats.syntax.all.*
import scala.annotation.tailrec

case class LanternFishes(state: Map[Int, Long]):
  def next = LanternFishes(
    state.filterNot(_._1 == 0).map((k, v) => (k - 1) -> v) |+|
      state.filter(_._1 == 0).flatMap((_, v) => Map(6 -> v, 8 -> v))
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
          .map(_.foldLeft(Map.empty[Int, Long])(_.updatedWith(_) {
            case Some(v) => Some(v + 1)
            case None => Some(1)
          }))
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
