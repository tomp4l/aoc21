package aoc21
package day13

import cats.effect.IO
import cats.syntax.all.*
import cats.data.State
import cats.Eval

enum Fold:
  case X(coordinate: Int)
  case Y(coordinate: Int)

case class Cave(coordinates: List[Point2d], folds: List[Fold])

object Program extends PureDay:
  type A = Cave

  def parse(input: List[String]): IO[A] =
    val (coordinates, folds) = input.span(_ != "")
    val parsedCoordinates = coordinates.traverse({
      case s"$x,$y" => (x.toIntIO, y.toIntIO).mapN((x, y) => Point2d(x, y))
      case l => IO.raiseError(new Exception(s"failed to parse cooridinate: $l"))
    })
    val parsedFolds = folds.tail.traverse {
      case s"fold along x=$x" => x.toIntIO.map(Fold.X(_))
      case s"fold along y=$y" => y.toIntIO.map(Fold.Y(_))
      case l => IO.raiseError(new Exception(s"failed to parse fold: $l"))
    }
    (parsedCoordinates, parsedFolds).mapN(Cave(_, _))

  def doFold(fold: Fold): State[Set[Point2d], Unit] =
    State.modify[Set[Point2d]](points =>
      fold match
        case Fold.X(c) => foldBy(c, points, _.x, (p, x) => p.copy(x = x))
        case Fold.Y(c) => foldBy(c, points, _.y, (p, y) => p.copy(y = y))
    )

  def foldBy(
      coordinate: Int,
      points: Set[Point2d],
      get: Point2d => Int,
      set: (Point2d, Int) => Point2d
  ) =
    points.map(p =>
      if get(p) > coordinate then
        val diff = get(p) - coordinate
        set(p, coordinate - diff)
      else p
    )

  def show(points: Seq[Point2d]) =
    points.map(_ -> "#").toMap.show

  def part1(input: A): String =
    doFold(input.folds.head).runS(input.coordinates.toSet).value.size.toString

  def part2(input: A): String =
    "\n" + show(
      input.folds
        .traverse(doFold)
        .runS(input.coordinates.toSet)
        .value
        .toSeq
    )
end Program
