package aoc21

import cats.effect.IO

extension (s: String)
  def toIntIO: IO[Int] =
    IO.fromOption(s.toIntOption)(new Exception(s"Couldn't parse int from: $s"))
