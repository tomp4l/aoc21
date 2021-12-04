package aoc21
package day04

import cats.effect.IO
import cats.syntax.all.*
import cats.Show

case class Board(values: Map[(Int, Int), Int])
object Board:
  def parse(input: List[String]): IO[(Board, List[String])] =
    def loop(
        remaining: List[String],
        current: Map[(Int, Int), Int] = Map.empty,
        row: Int = 0
    ): IO[(Board, List[String])] = remaining match
      case Nil => IO.pure((Board(current), List.empty))
      case line :: rest =>
        if line.isBlank then IO.pure((Board(current), rest))
        else
          val split =
            line.toList
              .grouped(3)
              .map(_.mkString.trim.toIntIO)
              .toList
              .sequence
          val asMap =
            split.map(_.zipWithIndex.map((v, i) => (row, i) -> v).toMap)
          asMap.flatMap(m => loop(rest, current ++ m, row + 1))
    loop(input)
case class Bingo(drawn: List[Int], boards: List[Board])
object Bingo:
  def parse(input: List[String]): IO[Bingo] =
    val drawn =
      input.headOption
        .toIOException("missing input")
        .flatMap(_.split(",").map(_.toIntIO).toList.sequence)
    def loop(
        remaining: List[String],
        current: Map[(Int, Int), Int] = Map.empty,
        row: Int = 0
    ): IO[List[Board]] =
      remaining match
        case Nil => IO.pure(List.empty)
        case line :: rest =>
          if line.isBlank then loop(rest)
          else Board.parse(line :: rest).flatMap((b, r) => loop(r).map(b :: _))
    val boards = loop(input.drop(2))
    (drawn, boards).mapN(Bingo(_, _))

case class BoardState(board: Map[(Int, Int), Int], matched: List[Int]):
  val width = board.keySet.maxBy(_._2)._2 + 1
  val height = board.keySet.maxBy(_._1)._1 + 1

  def mark(i: Int) = if board.values.exists(_ == i) then
    copy(matched = i :: matched)
  else this

  val matchedKeys = board.filter((_, i) => matched.contains(i)).keySet

  def hasLine =
    matchedKeys.groupBy(_._1).map(_._2.size).exists(_ == height) ||
      matchedKeys.groupBy(_._2).map(_._2.size).exists(_ == width)

  def score =
    val remaining = board -- matchedKeys
    remaining.map(_._2).sum * matched.head

given Show[Board] = Show[Map[(Int, Int), Int]].contramap[Board](_.values)

given Show[Bingo] = (b: Bingo) =>
  (List("Bingo!", b.drawn.mkString(", ")) ++ b.boards.map(_.show))
    .mkString("\n\n")

object BoardState:
  def fromBoard(board: Board) = BoardState(board.values, List.empty)

object Program extends PureDay:
  type A = Bingo

  def parse(input: List[String]): IO[this.A] =
    Bingo.parse(input)

  def part1(input: this.A): String =
    def loop(marks: List[Int], boards: List[BoardState]): String =
      marks match
        case Nil => "unmatched"
        case mark :: rest =>
          val nextBoards = boards.map(_.mark(mark))
          nextBoards.find(_.hasLine) match
            case Some(v) => v.score.toString
            case _ => loop(rest, nextBoards)
    loop(input.drawn, input.boards.map(BoardState.fromBoard))

  def part2(input: this.A): String =
    def loop(
        marks: List[Int],
        boards: List[BoardState],
        bestBoard: Option[String]
    ): String =
      marks match
        case Nil => bestBoard.getOrElse("unmatched")
        case mark :: rest =>
          val nextBoards = boards.map(_.mark(mark))
          nextBoards.find(_.hasLine) match
            case Some(v) =>
              loop(
                rest,
                nextBoards.filterNot(_.hasLine),
                Some(v.score.toString)
              )
            case _ => loop(rest, nextBoards, bestBoard)
    loop(input.drawn, input.boards.map(BoardState.fromBoard), None)
end Program
