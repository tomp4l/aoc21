package aoc21.day08

import aoc21.PureDay
import cats.effect.IO
import cats.syntax.all.*

case class Digits(
    signals: List[String],
    output: List[String]
)

case class UnknownDigit(wires: Set[Char]):
  def toKnown(value: Int) = KnownDigit(value, wires)
case class KnownDigit(value: Int, wires: Set[Char])

object Program extends PureDay:
  type A = List[Digits]
  def parse(input: List[String]): IO[this.A] =
    input
      .traverse {
        case s"$s | $i" =>
          IO.pure(Digits(s.split(" ").toList, i.split(" ").toList))
        case l => IO.raiseError(new Exception(s"couldn't parse line $l"))
      }

  def part1(input: this.A): String =
    val easyLengths = Set(2, 4, 3, 7)
    input.flatMap(_.output).count(l => easyLengths(l.length)).toString

  def solve(digits: Digits): Int =
    val unknown = digits.signals.map(s => UnknownDigit(s.toSet))
    val (easy, rest) = unknown.partitionMap(d =>
      d.wires.size match
        case 2 => Left(d.toKnown(1))
        case 3 => Left(d.toKnown(7))
        case 4 => Left(d.toKnown(4))
        case 7 => Left(d.toKnown(8))
        case _ => Right(d)
    )
    val four = easy.find(_.value == 4)
    val seven = easy.find(_.value == 7)
    (four, seven)
      .mapN((four, seven) =>
        val hard = rest.map(u =>
          (
            u.wires.intersect(four.wires).size,
            u.wires.intersect(seven.wires).size
          ) match
            case (3, 3) if (u.wires.size == 6) => u.toKnown(0)
            case (2, 2) if (u.wires.size == 5) => u.toKnown(2)
            case (3, 3) if (u.wires.size == 5) => u.toKnown(3)
            case (3, 2) if (u.wires.size == 5) => u.toKnown(5)
            case (3, 2) if (u.wires.size == 6) => u.toKnown(6)
            case (4, 3) if (u.wires.size == 6) => u.toKnown(9)
            case mismatch =>
              throw new Exception(s"mismatch $mismatch, ${u.wires.size}")
        )
        val all = (easy ++ hard).map(d => d.wires -> d.value).toMap
        digits.output.map(s => all(s.toSet).toString).mkString.toInt
      )
      .getOrElse(throw new Exception("Missing four or seven"))
  end solve

  def part2(input: this.A): String =
    input.map(solve).sum.toString
end Program
