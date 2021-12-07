package aoc21
package day02

import cats.effect.IO
import cats.syntax.all.*
import cats.data.State

sealed trait Direction

case class Forward(amount: Int) extends Direction
case class Down(amount: Int) extends Direction
case class Up(amount: Int) extends Direction

object Direction:

  def parse(s: String): IO[Direction] =
    s match
      case s"forward $i" => i.toIntIO.map(Forward(_))
      case s"down $i" => i.toIntIO.map(Down(_))
      case s"up $i" => i.toIntIO.map(Up(_))
      case _ =>
        IO.raiseError(new Exception(s"Unable to parse direction from $s"))

case class Position(horizontal: Int, depth: Int)

case class AimedPosition(horizontal: Int, depth: Int, aim: Int)

object Program extends PureDay:
  type A = List[Direction]
  def parse(input: List[String]): IO[List[Direction]] =
    input.traverse(Direction.parse)
  def part1(input: List[Direction]): String =
    val updatePosition = (direction: Direction) =>
      State.modify[Position](position =>
        direction match
          case Forward(amount) =>
            position.copy(horizontal = position.horizontal + amount)
          case Down(amount) => position.copy(depth = position.depth + amount)
          case Up(amount) => position.copy(depth = position.depth - amount)
      )
    val finalPosition =
      input.traverse(updatePosition).runS(Position(0, 0)).value
    (finalPosition.depth * finalPosition.horizontal).toString
  def part2(input: List[Direction]): String =
    val updatePosition = (direction: Direction) =>
      State.modify[AimedPosition](position =>
        direction match
          case Forward(amount) =>
            position.copy(
              horizontal = position.horizontal + amount,
              depth = position.depth + position.aim * amount
            )
          case Down(amount) => position.copy(aim = position.aim + amount)
          case Up(amount) => position.copy(aim = position.aim - amount)
      )
    val finalPosition =
      input.traverse(updatePosition).runS(AimedPosition(0, 0, 0)).value
    (finalPosition.depth * finalPosition.horizontal).toString
end Program
