package aoc21

import cats.effect.IOApp
import cats.effect.IO
import cats.syntax.all.*
import fs2.io.file.Files
import fs2.io.file.Path
import fs2.{Stream, text}
import cats.effect.ExitCode
import cats.effect.kernel.Clock
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object Main extends IOApp:

  private val days: Map[Int, Day] = Map(
    1 -> day01.Program,
    2 -> day02.Program,
    3 -> day03.Program,
    4 -> day04.Program,
    5 -> day05.Program,
    6 -> day06.Program,
    7 -> day07.Program,
    8 -> day08.Program,
    9 -> day09.Program,
    10 -> day10.Program,
    11 -> day11.Program,
    12 -> day12.Program,
    13 -> day13.Program,
    14 -> day14.Program,
    15 -> day15.Program,
    16 -> day16.Program,
    17 -> day17.Program,
    18 -> day18.Program,
    19 -> day19.Program,
    20 -> day20.Program,
    21 -> day21.Program
  )

  def run(args: List[String]): IO[ExitCode] =
    for
      day <- args match
        case List(i) => IO(i)
        case _ =>
          IO.println("Which day?") >> IO.readLine
      _ <- day match
        case "all" =>
          days.keySet.toList.sorted.map(runDay).sequence
        case i => i.toIntIO.flatMap(runDay)
    yield ExitCode.Success

  extension (d: FiniteDuration)
    def formatMillis(dp: Int) =
      BigDecimal(d.toUnit(TimeUnit.MILLISECONDS))
        .setScale(dp, BigDecimal.RoundingMode.HALF_UP)
        .toString

  private def runDay(day: Int) = for
    program <- IO.fromOption(days.get(day))(
      new Exception(s"Day $day is not defined yet")
    )
    _ <- IO.println(s"Running day $day")
    input <- readInput(day)
    parsed <- Clock[IO].timed(program.parse(input))
    _ <- IO.println(s"Parsed in ${parsed._1.formatMillis(2)} ms")
    part1 <- Clock[IO].timed(program.runPart1(parsed._2))
    _ <- IO.println(
      s"Part 1 answer: ${part1._2} in ${part1._1.formatMillis(2)} ms"
    )
    part2 <- Clock[IO].timed(program.runPart2(parsed._2))
    _ <- IO.println(
      s"Part 2 answer: ${part2._2} in ${part2._1.formatMillis(2)} ms"
    )
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
