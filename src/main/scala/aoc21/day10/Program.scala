package aoc21
package day10

import aoc21.PureDay
import cats.syntax.all.*
import cats.data.State

case class ParseState(parsed: List[Char], error: Option[Char]):
  def score = error match
    case None => 0
    case Some(')') => 3
    case Some(']') => 57
    case Some('}') => 1197
    case Some('>') => 25137
    case _ => throw new Exception("undefined score")

object Program extends PureDay with StringDay:

  private def parseLine(line: String) =
    val chars = line.toCharArray.toList
    chars
      .traverse((c) =>
        for
          s <- State.get[ParseState]
          m <- s.error.fold {
            c match
              case '{' | '(' | '<' | '[' =>
                State.set(ParseState(c :: s.parsed, None))
              case _ =>
                s.parsed match
                  case h :: tl =>
                    (h, c) match
                      case ('(', ')') | ('[', ']') | ('{', '}') | ('<', '>') =>
                        State.set(ParseState(tl, None))
                      case _ => State.set(ParseState(s.parsed, Some(c)))
                  case Nil => State.set(ParseState(List.empty, Some(c)))
          }(_ => State.empty[ParseState, Unit])
        yield ()
      )
      .runS(ParseState(List.empty, None))
      .value

  def part1(input: A) =
    input.map(parseLine(_).score).sum.toString

  def part2(input: A) =
    val valid = input.map(parseLine).filter(_.score == 0)
    valid
      .map(
        _.parsed
          .traverse(c =>
            val score = c match
              case '(' => 1
              case '[' => 2
              case '{' => 3
              case '<' => 4
            State.modify[Long](s => s * 5 + score)
          )
          .runS(0)
          .value
      )
      .sorted
      .drop(valid.length / 2)
      .head
      .toString
end Program
