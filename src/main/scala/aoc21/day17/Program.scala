package aoc21
package day17

import cats.effect.IO
import cats.syntax.all.*
import cats.data.State
import cats.kernel.Semigroup
import cats.data.Ior
import cats.data.Ior.Both

case class Target(bottomLeft: Point2d, topRight: Point2d)

object Program extends PureDay:
  type A = Target
  def parse(s: List[String]): IO[A] =
    s match
      case List(s"target area: x=$x1..$x2, y=$y1..$y2") =>
        (x1.toIntIO, y1.toIntIO, x2.toIntIO, y2.toIntIO).mapN(
          (x1, y1, x2, y2) => Target(Point2d(x1, y1), Point2d(x2, y2))
        )
      case _ => IO.raiseError(new Exception("Expecting single line"))

  def part1(input: A): String =
    findMaxY(input).toString

  def findMaxY(input: A) =
    val min = (-1 * input.bottomLeft.y)
    (min * (min - 1)) / 2

  def validIterationsX(input: A, x: Int, maxYIterations: Int) =
    val max = input.topRight.x
    val min = input.bottomLeft.x
    LazyList
      .iterate((0, x))((p, v) => (p + v, v - 1))
      .zipWithIndex
      .map((t, i) => (i, t._1, t._2))
      .take(maxYIterations)
      .takeWhile((_, p, _) => p <= max)
      .dropWhile((_, p, _) => p < min)
      .map(p => p._1)
      .toSet

  def validIterationsY(input: A, y: Int) =
    val min = input.bottomLeft.y
    val max = input.topRight.y
    LazyList
      .iterate((0, y))((p, v) => (p + v, v - 1))
      .zipWithIndex
      .map((t, i) => (i, t._1, t._2))
      .dropWhile((_, p, _) => p > max)
      .takeWhile((_, p, _) => p >= min)
      .map(p => p._1)
      .toSet

  def part2(input: A): String =
    val maxYIterations = (-input.bottomLeft.y) * 2 + 1
    val xs =
      (1 to (input.topRight.x))
        .map(validIterationsX(input, _, maxYIterations))
        .filter(_.nonEmpty)
        .toList

    val ys =
      ((input.bottomLeft.y) to (-input.bottomLeft.y))
        .map(validIterationsY(input, _))
        .filter(_.nonEmpty)
        .toList

    (xs, ys)
      .mapN((x, y) => x.intersect(y).nonEmpty)
      .count(identity)
      .toString

end Program
