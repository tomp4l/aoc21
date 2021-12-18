package aoc21
package day18

import io.circe.Codec
import io.circe.Decoder
import io.circe.DecodingFailure
import cats.effect.IO
import cats.syntax.all.*
import cats.Show

private enum Path:
  case Left
  case Right

private enum ReductionStep:
  case Split(path: List[Path])
  case Explode(path: List[Path], left: Int, right: Int)
  case NoChange

  def apply(s: Snailfish): Snailfish =
    this match
      case NoChange => s
      case Explode(path, l, r) => explode(s, path, l, r)
      case Split(path) => split(s, path.reverse)

  private def split(s: Snailfish, path: List[Path]) =
    replaceAt(
      s,
      path,
      s =>
        s match
          case Snailfish.Number(i) =>
            val split = i / 2
            val remainder = i % 2
            Snailfish.Pair(
              Snailfish.Number(split),
              Snailfish.Number(split + remainder)
            )
          case p => throw new Exception(s"Can't split pair: $p")
    )

  private def explode(s: Snailfish, path: List[Path], left: Int, right: Int) =
    val leftPath = path.dropWhile(_ == Path.Left)
    val rightPath = path.dropWhile(_ == Path.Right)
    val withLeft =
      if leftPath.isEmpty then s
      else
        def replaceRight: Snailfish => Snailfish = {
          case Snailfish.Number(i) => Snailfish.Number(i + left)
          case Snailfish.Pair(l, r) =>
            Snailfish.Pair(l, replaceRight(r))
        }
        val fullPath = (Path.Left :: leftPath.tail).reverse
        replaceAt(s, fullPath, replaceRight)
    val withRight =
      if rightPath.isEmpty then withLeft
      else
        def replaceLeft: Snailfish => Snailfish = {
          case Snailfish.Number(i) => Snailfish.Number(i + right)
          case Snailfish.Pair(l, r) =>
            Snailfish.Pair(replaceLeft(l), r)
        }
        val fullPath = (Path.Right :: rightPath.tail).reverse
        replaceAt(withLeft, fullPath, replaceLeft)
    replaceAt(withRight, path.reverse, _ => Snailfish.Number(0))
  end explode

  private def replaceAt(
      s: Snailfish,
      path: List[Path],
      replacement: Snailfish => Snailfish
  ): Snailfish =
    path match
      case Nil => replacement(s)
      case direction :: rest =>
        s match
          case Snailfish.Pair(l, r) =>
            direction match
              case Path.Left =>
                Snailfish.Pair(replaceAt(l, rest, replacement), r)
              case Path.Right =>
                Snailfish.Pair(l, replaceAt(r, rest, replacement))
          case Snailfish.Number(i) => throw new Exception("Invalid path")
end ReductionStep

enum Snailfish:
  case Number(value: Int)
  case Pair(left: Snailfish, right: Snailfish)

  def +(other: Snailfish) =
    Snailfish.Pair(this, other).reduced

  def reduced: Snailfish =
    val step = explodeStep(this, List())
    if step == ReductionStep.NoChange then
      val otherStep = splitStep(this, List())
      if otherStep == ReductionStep.NoChange then this
      else otherStep(this).reduced
    else step(this).reduced

  private def explodeStep(s: Snailfish, path: List[Path]): ReductionStep =
    s match
      case n @ Number(i) =>
        ReductionStep.NoChange
      case p @ Pair(l, r) =>
        val explode =
          if path.length == 3 then
            (l, r) match
              case (Pair(Number(li), Number(ri)), _) =>
                ReductionStep.Explode(Path.Left :: path, li, ri)
              case (_, Pair(Number(li), Number(ri))) =>
                ReductionStep.Explode(Path.Right :: path, li, ri)
              case _ => ReductionStep.NoChange
          else ReductionStep.NoChange
        if explode != ReductionStep.NoChange then explode
        else
          val leftChange = explodeStep(l, Path.Left :: path)
          if leftChange != ReductionStep.NoChange then leftChange
          else explodeStep(r, Path.Right :: path)

  private def splitStep(s: Snailfish, path: List[Path]): ReductionStep =
    s match
      case n @ Number(i) =>
        if i >= 10 then ReductionStep.Split(path)
        else ReductionStep.NoChange
      case p @ Pair(l, r) =>
        val leftChange = splitStep(l, Path.Left :: path)
        if leftChange != ReductionStep.NoChange then leftChange
        else splitStep(r, Path.Right :: path)

  def magnitude: Long =
    this match
      case Number(i) => i.toLong
      case Pair(l, r) => 3 * l.magnitude + 2 * r.magnitude
end Snailfish

given Show[Snailfish] = {
  case Snailfish.Number(i) => i.show
  case Snailfish.Pair(l, r) => "[" + l.show + "," + r.show + "]"
}

given Decoder[Snailfish] =
  (c) =>
    c.as[Int]
      .map(Snailfish.Number(_))
      .orElse(
        c.as[List[Snailfish]].flatMap {
          case List(l, r) => Right(Snailfish.Pair(l, r))
          case l => Left(DecodingFailure(s"Expecting pair, got: $l", c.history))
        }
      )

object Program extends PureDay:
  type A = List[Snailfish]
  def parse(input: List[String]): IO[A] =
    IO.fromEither(
      input
        .traverse(io.circe.parser.parse(_))
        .flatMap(_.traverse(_.as[Snailfish]))
    )

  def part1(input: A): String =
    val result = input.reduce(_ + _)
    result.magnitude.toString
  def part2(input: A): String =
    input
      .flatMap(r => input.filterNot(_ == r).map(_ -> r))
      .map((a, b) => a + b)
      .map(_.magnitude)
      .max
      .toString
