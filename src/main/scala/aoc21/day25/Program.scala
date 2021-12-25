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

  def moveAll(filter: SeaFloor => Boolean, next: Point2d => Point2d) = (m: A) =>
    val open =
      m.filter((p, v) => filter(v) && m(next(p)) == SeaFloor.Unoccupied)
    val moved = open.map((p, v) => next(p) -> v)
    m ++ (open.keySet.map(_ -> SeaFloor.Unoccupied)) ++ moved

  def step(maxX: Int, maxY: Int) =
    State
      .get[A]
      .flatMap(initial =>
        State.modify[A](
          moveAll(
            _ == SeaFloor.Occupied(SeaCucumber.East),
            p =>
              if p.x == maxX then p.copy(x = 0)
              else p + Point2d(1, 0)
          )
        ) *>
          State.modify[A](
            moveAll(
              _ == SeaFloor.Occupied(SeaCucumber.South),
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
        s <- State.get[(A, Int)]
        f <- (State.set(s._1) *> doStep)
          .contramap[(A, Int)](_._1)
          .modify(_ -> (s._2 + 1))
      yield f
    val result = doStepWithCount.iterateUntil(identity).runS((input, 0)).value
    result._2.toString

  def part2(input: A): String =
    "Merry Christmas!"
end Program
