package aoc21
package day25

import cats.effect.IO
import cats.syntax.all.*
import cats.Show
import cats.data.State

enum SeaCucumber:
  case East
  case South

enum SeaFloor:
  case Unoccupied
  case Occupied(cucumber: SeaCucumber)

given Show[SeaFloor] = {
  case SeaFloor.Unoccupied => "."
  case SeaFloor.Occupied(SeaCucumber.East) => ">"
  case SeaFloor.Occupied(SeaCucumber.South) => "v"
}

object Program extends Some2dDay with PureDay:

  type I = SeaFloor

  def parseItem(s: String) =
    s match
      case "." => SeaFloor.Unoccupied.pure
      case ">" => SeaFloor.Occupied(SeaCucumber.East).pure
      case "v" => SeaFloor.Occupied(SeaCucumber.South).pure
      case l => IO.raiseError(new Exception(s"Unparsed: $l"))

  def moveAll(filter: SeaCucumber => Boolean, next: Point2d => Point2d) =
    (m: Map[Point2d, SeaCucumber]) =>
      val open =
        m.filter((p, v) => filter(v) && !m.isDefinedAt(next(p)))
      val moved = open.map((p, v) => next(p) -> v)
      m -- (open.keySet) ++ moved

  def step(maxX: Int, maxY: Int) =
    State
      .get[Map[Point2d, SeaCucumber]]
      .flatMap(initial =>
        State.modify[Map[Point2d, SeaCucumber]](
          moveAll(
            _ == SeaCucumber.East,
            p =>
              if p.x == maxX then p.copy(x = 0)
              else p + Point2d(1, 0)
          )
        ) *>
          State.modify[Map[Point2d, SeaCucumber]](
            moveAll(
              _ == SeaCucumber.South,
              p =>
                if p.y == maxY then p.copy(y = 0)
                else p + Point2d(0, 1)
            )
          )
          *> State.inspect(s => s == initial)
      )

  def part1(input: A): String =
    val maxX = input.keySet.maxBy(_.x).x
    val maxY = input.keySet.maxBy(_.y).y
    val doStep = step(maxX, maxY)
    val doStepWithCount =
      for
        s <- State.get[(Map[Point2d, SeaCucumber], Int)]
        f <- (State.set(s._1) *> doStep)
          .contramap[(Map[Point2d, SeaCucumber], Int)](_._1)
          .modify(_ -> (s._2 + 1))
      yield f
    val sparseMap = input.collect { case (p, SeaFloor.Occupied(c)) => p -> c }
    val result = doStepWithCount
      .iterateUntil(identity)
      .runS(sparseMap -> 0)
      .value
    result._2.toString
  end part1

  def part2(input: A): String =
    "Merry Christmas!"
end Program
