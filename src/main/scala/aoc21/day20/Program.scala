package aoc21
package day20

import cats.Show
import cats.effect.IO
import cats.syntax.all.*
import cats.data.State

enum Pixel:
  case Dark
  case Light

object Pixel:
  def fromChar(c: Char): IO[Pixel] = c match
    case '.' => Dark.pure
    case '#' => Light.pure
    case p => IO.raiseError(new Exception(s"Bad pixel: $p"))

given Show[Pixel] = {
  case Pixel.Dark => "."
  case Pixel.Light => "#"
}

object Program extends PureDay:
  type A = (Vector[Pixel], Map[Point2d, Pixel])

  def parse(input: List[String]): IO[A] =
    input match
      case algorithm :: "" :: pixels =>
        val parsedAlgorithm =
          algorithm.toCharArray.toVector.traverse(Pixel.fromChar)
        val parsedPixels =
          pixels.to2dMap(_.split("").toList, c => Pixel.fromChar(c.head))
        (parsedAlgorithm, parsedPixels).tupled
      case _ => IO.raiseError(new Exception("Bad input"))

  def padImage(
      image: Map[Point2d, Pixel],
      padWith: Pixel,
      mins: Point2d,
      maxs: Point2d
  ): Map[Point2d, Pixel] =
    val points = image.keySet
    val minX = mins.x
    val minY = mins.y
    val maxX = maxs.x
    val maxY = maxs.y
    val xs = ((minX - 1) to (maxX + 1))
    val ys = ((minY - 1) to (maxY + 1))
    val extraPoints =
      xs.flatMap(x => Set(Point2d(x, minY - 1), Point2d(x, maxY + 1))) ++
        ys.flatMap(y => Set(Point2d(minX - 1, y), Point2d(maxX + 1, y)))
    val toAdd = extraPoints.map(p => p -> padWith)
    image ++ toAdd

  val pixelToInt =
    var memo: Map[List[Pixel], Int] = Map.empty
    (pixels: List[Pixel]) =>
      memo.getOrElse(
        pixels, {
          val p = pixels
            .map {
              case Pixel.Dark => "0"
              case Pixel.Light => "1"
            }
            .mkString
            .toBinaryInt
          memo = memo + (pixels -> p)
          p
        }
      )

  val offsets = (-1 to 1).flatMap(y => (-1 to 1).map(x => Point2d(x, y))).toList

  def step(algorithm: Vector[Pixel]) =
    State.modify[(Map[Point2d, Pixel], (Pixel, (Point2d, Point2d)))](
      (image, s) =>
        val infinityState = s._1
        val (mins, maxs) = s._2
        val padded = padImage(image, infinityState, mins, maxs)
        padded.map((p, v) =>
          val pixelValue =
            offsets.map(relative =>
              padded.getOrElse(p + relative, infinityState)
            )
          p -> algorithm(pixelToInt(pixelValue))
        ) -> ((infinityState match
          case Pixel.Dark => algorithm.head
          case Pixel.Light => algorithm.last
        ) -> (mins + Point2d(-1, -1), maxs + Point2d(1, 1)))
    )

  def startState(a: A) = (
    a._2,
    (Pixel.Dark, (Point2d(0, 0), a._2.keySet.maxBy(p => p.x + p.y)))
  )

  def part1(input: A): String =
    val stepWithAlg = step(input._1)
    val twoSteps = stepWithAlg >> stepWithAlg
    val ran = twoSteps
      .runS(startState(input))
      .value
    ran._1.count(_._2 == Pixel.Light).toString

  def part2(input: A): String =
    val stepWithAlg = step(input._1)
    (1 to 50).toList
      .traverse(_ => stepWithAlg)
      .runS(startState(input))
      .value
      ._1
      .count(_._2 == Pixel.Light)
      .toString
end Program
