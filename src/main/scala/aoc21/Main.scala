package aoc21

import cats.effect.IOApp
import cats.effect.IO
import cats.syntax.all.*
import fs2.io.file.Files
import fs2.io.file.Path
import fs2.{Stream, text}

object Main extends IOApp.Simple:

  private val days: Map[Int, Day] = Map(
    1 -> day01.Program,
    2 -> day02.Program,
    3 -> day03.Program,
    4 -> day04.Program,
    5 -> day05.Program,
    6 -> day06.Program
  )

  def run: IO[Unit] =
    for
      _ <- IO.println("Which day?")
      day <- IO.readLine
      _ <- day match
        case "all" =>
          days.keySet.toList.sorted.map(runDay).sequence
        case i => i.toIntIO.flatMap(runDay)
    yield ()

  private def runDay(day: Int) = for
    program <- IO.fromOption(days.get(day))(
      new Exception(s"Day $day is not defined yet")
    )
    _ <- IO.println(s"Running day $day")
    input <- readInput(day)
    parsed <- program.parse(input)
    part1 <- program.runPart1(parsed)
    _ <- IO.println(s"Part 1 answer: $part1")
    part2 <- program.runPart2(parsed)
    _ <- IO.println(s"Part 2 answer: $part2")
  yield ()

  private def readInput(day: Int): IO[List[String]] =
    val filename = s"inputs/day$day.txt"
    val path = Path(filename)
    Files[IO]
      .exists(path)
      .flatMap(exists =>
        if exists then
          Files[IO]
            .readAll(path)
            .through(text.utf8.decode)
            .through(text.lines)
            .compile
            .toList
        else
          IO.println(s"Missing $filename, using empty list as input") >> IO
            .pure(
              List.empty
            )
      )
end Main
