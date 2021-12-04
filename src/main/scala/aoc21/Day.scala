package aoc21

import cats.effect.IO
import cats.syntax.all.*

trait Day:
  type A
  def parse(input: List[String]): IO[A]
  def runPart1(input: A): IO[String]
  def runPart2(input: A): IO[String]

trait PureDay extends Day:
  def part1(input: A): String
  def part2(input: A): String

  def runPart1(input: A): IO[String] = IO.delay(part1(input))
  def runPart2(input: A): IO[String] = IO.delay(part2(input))

trait StringDay extends Day:
  type A = List[String]
  def parse(input: List[String]) = IO.pure(input)

trait IntDay extends Day:
  type A = List[Int]
  def parse(l: List[String]): IO[List[Int]] =
    l.map(_.toIntIO).sequence
