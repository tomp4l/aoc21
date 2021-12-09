package aoc21
package day09

import aoc21.StringDay
import aoc21.PureDay
import aoc21.Point2d
import cats.syntax.all.*

object Program extends PureDay:

  type A = Map[Point2d, Int]

  def parse(input: List[String]) =
    input
      .map(_.zipWithIndex)
      .zipWithIndex
      .flatMap((c, y) =>
        c.map((i, x) => i.toString.toIntIO.map(Point2d(x, y) -> _))
      )
      .sequence
      .map(_.toMap)

  def neighbours(point: Point2d, map: A) =
    point.neighbours.flatMap(map.get(_))

  def lowPoints(input: this.A) =
    input.filter((p, i) => neighbours(p, input).forall(_ > i))

  def part1(input: this.A): String =
    lowPoints(input).values
      .map(_ + 1)
      .sum
      .toString

  def part2(input: this.A): String =
    def fillBasin(acc: Set[Point2d]): Set[Point2d] =
      val neighbours = acc.flatMap(_.neighbours) -- acc
      val inBasin = neighbours.filter(i => input.get(i).getOrElse(9) < 9)
      val total = inBasin ++ acc
      if total == acc then acc
      else fillBasin(total)

    lowPoints(input).keys
      .map((p) => fillBasin(Set(p)).size)
      .toList
      .sorted
      .takeRight(3)
      .product
      .toString
  end part2

end Program
