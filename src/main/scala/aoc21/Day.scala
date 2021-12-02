package aoc21

import cats.effect.IO
import cats.syntax.all.*

trait Day[A]:

  def run(input: List[String]): IO[Unit] =
    for
      input <- parse(input)
      part1 <- runPart1(input)
      _ <- IO.println(s"Part 1 answer: $part1")
      part2 <- runPart2(input)
      _ <- IO.println(s"Part 2 answer: $part2")
    yield ()

  def parse(input: List[String]): IO[List[A]]
  def runPart1(input: List[A]): IO[String]
  def runPart2(input: List[A]): IO[String]

trait PureDay[A] extends Day[A]:
  def part1(input: List[A]): String
  def part2(input: List[A]): String

  def runPart1(input: List[A]): IO[String] = IO.delay(part1(input))
  def runPart2(input: List[A]): IO[String] = IO.delay(part2(input))

trait StringDay extends Day[String]:
  def parse(input: List[String]) = IO.pure(input)

trait IntDay extends Day[Int]:
  def parse(l: List[String]): IO[List[Int]] =
    l.map(s =>
      IO.fromOption(s.toIntOption)(
        new Exception(s"Failed to parse int from $s")
      )
    ).sequence
