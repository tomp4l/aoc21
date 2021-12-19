package aoc21

import cats.effect.IO

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
