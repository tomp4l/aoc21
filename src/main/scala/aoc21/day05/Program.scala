package aoc21
package day05

import aoc21.PureDay
import cats.effect.IO
import cats.syntax.all.*

case class Line(from: Point2d, to: Point2d):
  def isHorizontal = from.y == to.y
  def isVeritical = from.x == to.x
  def points =
    def simplify(x: Int, y: Int, primes: LazyList[Int]): Point2d =
      val nextPrime = primes.head
      if nextPrime > x && nextPrime > y then Point2d(x, y)
      else if x % nextPrime == 0 && y % nextPrime == 0 then
        simplify(x / nextPrime, y / nextPrime, primes)
      else simplify(x, y, primes.tail)
    val (min, max) = if from < to then (from, to) else (to, from)
    val gradient = simplify(max.x - min.x, max.y - min.y, primes)
    def loop(
        current: Point2d,
        acc: Set[Point2d]
    ): Set[Point2d] =
      if current <= max then loop(current + gradient, acc + current)
      else acc
    loop(min, Set.empty)

object Line:
  private val regex = """(\d+),(\d+) -> (\d+),(\d+)""".r
  def parse(input: String): IO[Line] =
    input match
      case regex(x1, y1, x2, y2) =>
        (x1.toIntIO, y1.toIntIO, x2.toIntIO, y2.toIntIO).mapN(
          (x1, y1, x2, y2) => Line(Point2d(x1, y1), Point2d(x2, y2))
        )
      case l => IO.raiseError(new Exception(s"Couldn't parse line: $l"))

extension (lines: List[Line])
  def onlyHorizontalOrVertical =
    lines.filter(l => l.isHorizontal || l.isVeritical)

object Program extends PureDay:
  type A = List[Line]

  def parse(input: List[String]): IO[this.A] =
    input.map(Line.parse).sequence

  // Members declared in aoc21.PureDay
  def part1(input: this.A): String =
    val allVents = input.onlyHorizontalOrVertical.map(_.points)
    val vents =
      allVents.foldLeft(Map.empty[Point2d, Int])((map, points) =>
        points.foldLeft(map)((m, p) =>
          m.updatedWith(p) {
            case Some(i) => Some(i + 1)
            case None => Some(1)
          }
        )
      )
    val multiHit = vents.filter(_._2 > 1).size
    multiHit.toString
  def part2(input: this.A): String =
    val allVents = input.map(_.points)
    val vents =
      allVents.foldLeft(Map.empty[Point2d, Int])((map, points) =>
        points.foldLeft(map)((m, p) =>
          m.updatedWith(p) {
            case Some(i) => Some(i + 1)
            case None => Some(1)
          }
        )
      )
    val multiHit = vents.filter(_._2 > 1).size
    multiHit.toString
end Program
