package aoc21

import cats.effect.IOApp
import cats.effect.IO
import fs2.io.file.Files
import fs2.io.file.Path
import fs2.{Stream, text}

object Main extends IOApp.Simple:

  private val days: Map[Int, Day] = Map(
    1 -> day01.Program()
  )

  def run: IO[Unit] =
    for
      _ <- IO.println("Which day?")
      day <- IO.readLine
        .map(_.toIntOption)
        .flatMap(IO.fromOption(_)(new Exception("Couldn't parse day")))
      program <- IO.fromOption(days.get(day))(
        new Exception(s"Day $day is not defined yet")
      )
      _ <- IO.println(s"Running day $day")
      input <- readInput(day)
      part1 <- program.runPart1(input)
      _ <- IO.println(s"Part 1 answer: $part1")
      part2 <- program.runPart2(input)
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
