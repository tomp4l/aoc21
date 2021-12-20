package aoc21

import cats.effect.IO
import cats.syntax.all.*

extension (s: String)
  def toIntIO: IO[Int] =
    IO.fromOption(s.toIntOption)(new Exception(s"Couldn't parse int from: $s"))

  def toBinaryInt: Int =
    Integer.parseInt(s, 2)

  def toBinaryLong: Long =
    BigInt(s, 2).longValue

extension [A](option: Option[A])
  def toIO(t: => Throwable) = IO.fromOption(option)(t)
  def toIOException(message: => String) =
    option.toIO(new Exception(message))

extension [A](list: List[A])
  def split(a: A): List[List[A]] =
    def loop(remainder: List[A], acc: List[List[A]]): List[List[A]] =
      val (l, r) = remainder.span(_ != a)
      r match
        case _ :: rest =>
          loop(rest, l :: acc)
        case Nil => (l :: acc).reverse
    loop(list, List.empty)

extension (list: List[String])
  def to2dMap[A](
      splitLine: String => List[String],
      parseItem: String => IO[A]
  ): IO[Map[Point2d, A]] =
    list
      .map(l => splitLine(l).zipWithIndex)
      .zipWithIndex
      .flatMap((c, y) => c.map((i, x) => parseItem(i).map(Point2d(x, y) -> _)))
      .sequence
      .map(_.toMap)
