package aoc21
package day06

import aoc21.PureDay
import cats.effect.IO
import cats.syntax.all.*

case class LanternFish(state: Int):
  def next =
    if state == 0 then List(LanternFish(6), LanternFish(8))
    else List(LanternFish(state - 1))

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

  def part1(input: this.A): String =
    def loop(remaining: Int, fish: LanternFishes): LanternFishes =
      if remaining == 0 then fish
      else loop(remaining - 1, fish.next)
    loop(80, input).size.toString

  def part2(input: this.A): String =
    def loop(remaining: Int, fish: LanternFishes): LanternFishes =
      if remaining == 0 then fish
      else loop(remaining - 1, fish.next)
    loop(256, input).size.toString
end Program
