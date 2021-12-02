package aoc21

import cats.effect.IO
import cats.syntax.all.*

trait Day:
  def runPart1(input: List[String]): IO[String]
  def runPart2(input: List[String]): IO[String]

trait StringDay extends Day:
  def part1(input: List[String]): String
  def part2(input: List[String]): String

  def runPart1(input: List[String]): IO[String] = IO.delay(part1(input))
  def runPart2(input: List[String]): IO[String] = IO.delay(part2(input))

trait IntDay extends Day:
  def part1(input: List[Int]): String
  def part2(input: List[Int]): String

  private def parseList(l: List[String]): IO[List[Int]] =
    l.map(s =>
      IO.fromOption(s.toIntOption)(
        new Exception(s"Failed to parse int from $s")
      )
    ).sequence

  def runPart1(input: List[String]): IO[String] =
    parseList(input).map(part1)

  def runPart2(input: List[String]): IO[String] =
    parseList(input).map(part2)
