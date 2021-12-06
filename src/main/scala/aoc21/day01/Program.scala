package aoc21
package day01

object Program extends IntDay with PureDay:

  def part1(input: List[Int]): String =
    input
      .sliding(2)
      .collect { case List(a, b) => a - b }
      .count(_ < 0)
      .toString
  def part2(input: List[Int]): String =
    input
      .sliding(3)
      .map(_.sum)
      .toList
      .sliding(2)
      .collect { case List(a, b) => a - b }
      .count(_ < 0)
      .toString
