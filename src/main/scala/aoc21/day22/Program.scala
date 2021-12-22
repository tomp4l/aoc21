package aoc21
package day22

import cats.effect.IO
import cats.syntax.all.*
import cats.data.State
import cats.syntax.comonad

enum CubeState:
  case On
  case Off

object CubeState:
  def parse(s: String): IO[CubeState] = s.toLowerCase match
    case "on" => On.pure
    case "off" => Off.pure
    case l => IO.raiseError(new Exception(s"Unknown command: $l"))

case class Point3d(x: Int, y: Int, z: Int):
  def +(other: Point3d) = Point3d(x + other.x, y + other.y, z + other.z)

case class RebootCommand(command: CubeState, from: Point3d, to: Point3d):
  def cuboid = Cuboid(from, to + Point3d(1, 1, 1))

case class Cuboid(from: Point3d, to: Point3d):

  def slice(point: Point3d): Set[Cuboid] =
    val insideX = point.x > from.x && point.x < to.x
    val insideY = point.y > from.y && point.y < to.y
    val insideZ = point.z > from.z && point.z < to.z
    val splitFromX = if insideX then point.x else from.x
    val splitFromY = if insideY then point.y else from.y
    val splitFromZ = if insideZ then point.z else from.z
    val splitToX = if insideX then point.x else to.x
    val splitToY = if insideY then point.y else to.y
    val splitToZ = if insideZ then point.z else to.z
    Set(
      Cuboid(
        Point3d(from.x, from.y, from.z),
        Point3d(splitToX, splitToY, splitToZ)
      ),
      Cuboid(
        Point3d(splitFromX, from.y, from.z),
        Point3d(to.x, splitToY, splitToZ)
      ),
      Cuboid(
        Point3d(from.x, splitFromY, from.z),
        Point3d(splitToX, to.y, splitToZ)
      ),
      Cuboid(
        Point3d(from.x, from.y, splitFromZ),
        Point3d(splitToX, splitToY, to.z)
      ),
      Cuboid(
        Point3d(splitFromX, splitFromY, from.z),
        Point3d(to.x, to.y, splitToZ)
      ),
      Cuboid(
        Point3d(splitFromX, from.y, splitFromZ),
        Point3d(to.x, splitToY, to.z)
      ),
      Cuboid(
        Point3d(from.x, splitFromY, splitFromZ),
        Point3d(splitToX, to.y, to.z)
      ),
      Cuboid(
        Point3d(splitFromX, splitFromY, splitFromZ),
        Point3d(to.x, to.y, to.z)
      )
    )
  end slice

  def remove(other: Cuboid): Set[Cuboid] =
    if (other.from.x > to.x || other.to.x < from.x)
      || (other.from.y > to.y || other.to.y < from.y)
      || (other.from.z > to.z || other.to.z < from.z)
    then Set(this)
    else
      val sliced = this.slice(other.from).flatMap(_.slice(other.to))
      val diced = other.slice(from).flatMap(_.slice(to))
      Cuboid.reduceSet(sliced -- diced)

  def volume: Long =
    (to.x - from.x).toLong * (to.y - from.y) * (to.z - from.z)

end Cuboid

object Cuboid:
  private def rejoin(
      c: Set[Cuboid],
      interest: Point3d => Int,
      others: Point3d => (Int, Int)
  ) =
    c.foldLeft(c)((cs, c) =>
      if !cs(c) then cs
      else
        cs.find(x =>
          interest(x.from) == interest(c.to) &&
            others(x.from) == others(c.from) &&
            others(x.to) == others(c.to)
        ).fold(cs)(p =>
          val combine = c.copy(to = p.to)
          cs - c - p + combine
        )
    )

  def reduceSet(c: Set[Cuboid]) =
    val cx = rejoin(c, _.x, p => (p.y, p.z))
    val cxy = rejoin(cx, _.y, p => (p.x, p.z))
    rejoin(cxy, _.z, p => (p.x, p.y))

object Program extends PureDay:
  type A = List[RebootCommand]

  def parse(input: List[String]): IO[A] =
    input.traverse({
      case s"$command x=$x1..$x2,y=$y1..$y2,z=$z1..$z2" =>
        (
          CubeState.parse(command),
          x1.toIntIO,
          y1.toIntIO,
          z1.toIntIO,
          x2.toIntIO,
          y2.toIntIO,
          z2.toIntIO
        ).mapN((c, x1, y1, z1, x2, y2, z2) =>
          RebootCommand(c, Point3d(x1, y1, z1), Point3d(x2, y2, z2))
        )
      case l => IO.raiseError(new Exception(s"Unable to parse line: $l"))
    })

  def doCommand(command: RebootCommand) =
    State.modify[Set[Cuboid]](s =>
      if s.isEmpty then
        command.command match
          case CubeState.On => Set(command.cuboid)
          case CubeState.Off => Set.empty
      else
        val removed = s.flatMap(c => c.remove(command.cuboid))
        command.command match
          case CubeState.On =>
            removed + command.cuboid
          case CubeState.Off =>
            removed
    )

  def part1(input: A): String =
    val take = 20
    input
      .take(take)
      .traverse(doCommand)
      .runS(Set.empty)
      .value
      .toList
      .map(_.volume)
      .sum
      .toString
  def part2(input: A): String =
    input
      .traverse(doCommand)
      .runS(Set.empty)
      .value
      .toList
      .map(_.volume)
      .sum
      .toString
end Program
