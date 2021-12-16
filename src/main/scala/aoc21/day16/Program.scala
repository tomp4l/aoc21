package aoc21
package day16

import cats.effect.IO
import cats.data.StateT

enum Bits:
  case Literal(version: Int, value: Long)
  case Operator(version: Int, id: Int, subPackets: List[Bits])

object Program extends PureDay:

  type A = Bits

  def toBinaryString(hex: String) = hex.flatMap {
    case '0' => "0000"
    case '1' => "0001"
    case '2' => "0010"
    case '3' => "0011"
    case '4' => "0100"
    case '5' => "0101"
    case '6' => "0110"
    case '7' => "0111"
    case '8' => "1000"
    case '9' => "1001"
    case 'A' => "1010"
    case 'B' => "1011"
    case 'C' => "1100"
    case 'D' => "1101"
    case 'E' => "1110"
    case 'F' => "1111"
  }

  case class ParseState(remaining: String, parsed: Int)
  type S[A] = StateT[IO, ParseState, A]

  def parse(input: List[String]): IO[A] =
    input match
      case List(line) =>
        parseBits
          .runA(ParseState(toBinaryString(line), 0))
      case _ => IO.raiseError(new Exception("Expecting single line input"))

  def take(i: Int) =
    StateT[IO, ParseState, String]((s) =>
      val (a, b) = s.remaining.splitAt(i)
      if a.length < i then
        IO.raiseError(
          new Exception(s"Couldn't take $i bits from ${s.remaining}")
        )
      else IO.pure((ParseState(b, s.parsed + i), a))
    )

  def parseBits: S[Bits] =
    for
      v <- take(3).map(_.toBinaryInt)
      id <- take(3).map(_.toBinaryInt)
      p <-
        if id == 4 then parseLiteral(v)
        else parseOperator(v, id)
    yield p

  def parseLiteral(version: Int) =
    for n <- parseLiteralNumber.map(_.mkString.toBinaryLong)
    yield Bits.Literal(version, n)

  def parseLiteralNumber: S[List[String]] =
    for
      i <- take(1).map(_.toBinaryLong)
      n <- take(4)
      r <-
        if (i == 0) then StateT.pure[IO, ParseState, List[String]](List(n))
        else parseLiteralNumber.map(l => n :: l)
    yield r

  def parseOperator(version: Int, id: Int) =
    for
      i <- take(1).map(_.toBinaryLong)
      l <- take(if i == 0 then 15 else 11).map(_.toBinaryInt)
      packets <-
        if (i == 0) then parseCurrentBits(l)
        else parseCurrent(l)
    yield Bits.Operator(version, id, packets)

  def parseCurrentBits(remainingBits: Int): S[List[Bits]] =
    for
      s <- StateT.get[IO, ParseState]
      c <-
        if remainingBits == 0 then StateT.empty[IO, ParseState, List[Bits]]
        else if (remainingBits < 0) then
          StateT[IO, ParseState, List[Bits]](c =>
            IO.raiseError(new Exception(s"Overparsed $c"))
          )
        else
          for
            p <- parseBits
            s2 <- StateT.get[IO, ParseState]
            c <- parseCurrentBits(remainingBits - (s2.parsed - s.parsed))
          yield p :: c
    yield c

  def parseCurrent(remaining: Int): S[List[Bits]] =
    if remaining == 0 then StateT.empty[IO, ParseState, List[Bits]]
    else
      for
        p <- parseBits
        c <- parseCurrent(remaining - 1)
      yield p :: c

  def versionSum(bits: Bits): Int =
    bits match
      case Bits.Literal(v, _) => v
      case Bits.Operator(v, _, others) => v + others.map(versionSum).sum

  def interpret(input: A): Long =
    input match
      case Bits.Literal(_, i) => i
      case Bits.Operator(_, id, others) =>
        id match
          case 0 => others.map(interpret).sum
          case 1 => others.map(interpret).product
          case 2 => others.map(interpret).min
          case 3 => others.map(interpret).max
          case i =>
            val List(a, b) = others.map(interpret)
            i match
              case 5 => if a > b then 1 else 0
              case 6 => if a < b then 1 else 0
              case 7 => if a == b then 1 else 0

  def part1(input: A): String =
    versionSum(input).toString
  def part2(input: A): String =
    interpret(input).toString
end Program
