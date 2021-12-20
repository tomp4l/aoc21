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
      padWith: Pixel
  ): Map[Point2d, Pixel] =
    val points = image.keySet
    val minX = points.minBy(_.x).x
    val minY = points.minBy(_.y).y
    val maxX = points.maxBy(_.x).x
    val maxY = points.maxBy(_.y).y
    val xs = ((minX - 1) to (maxX + 1)).toSet
    val ys = ((minY - 1) to (maxY + 1)).toSet
    val extraPoints =
      xs.map(x => Point2d(x, minY - 1)) ++ xs.map(x => Point2d(x, maxY + 1)) ++
        ys.map(y => Point2d(minX - 1, y)) ++ ys.map(y => Point2d(maxX + 1, y))
    val toAdd = extraPoints.map(p => p -> padWith).toMap
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
    State.modify[(Map[Point2d, Pixel], Pixel)]((image, infinityState) =>
      val padded = padImage(image, infinityState)
      padded.map((p, v) =>
        val pixelValue =
          offsets.map(relative => padded.getOrElse(p + relative, infinityState))
        p -> algorithm(pixelToInt(pixelValue))
      ) -> (infinityState match
        case Pixel.Dark => algorithm.head
        case Pixel.Light => algorithm.last
      )
    )

  def part1(input: A): String =
    val stepWithAlg = step(input._1)
    val twoSteps = stepWithAlg >> stepWithAlg
    val ran = twoSteps.runS((input._2, Pixel.Dark)).value
    ran._1.count(_._2 == Pixel.Light).toString

  def part2(input: A): String =
    val stepWithAlg = step(input._1)
    (1 to 50).toList
      .traverse(_ => stepWithAlg)
      .runS((input._2, Pixel.Dark))
      .value
      ._1
      .count(_._2 == Pixel.Light)
      .toString
end Program
