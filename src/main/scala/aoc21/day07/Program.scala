package aoc21
package day07

import cats.effect.IO

import cats.syntax.all.*

object Program extends PureDay:
  type A = List[Int]
  def parse(input: List[String]): IO[this.A] =
    input match
      case List(line) => line.split(",").map(_.toIntIO).toList.sequence
      case _ => IO.raiseError(new Exception("Expecting single line"))
  def part1(input: this.A): String =
    val costs =
      (input.min to input.max).map(p => input.map(i => math.abs(p - i)).sum)
    costs.min.toString

  def part2(input: this.A): String =
    val costs = (input.min to input.max).map(p =>
      input
        .map(i =>
          val n = math.abs(p - i)
          n * (n + 1) / 2
        )
        .sum
    )
    costs.min.toString
