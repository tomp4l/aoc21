package aoc21
package day19

import cats.effect.IO
import cats.syntax.all.*
import cats.Id

trait Addable[A]:
  def plus(a: A, b: A): A
  def diff(a: A, b: A): A
  def manhatten(a: A, b: A): Int
  def zero: A

object Addable:
  def apply[A: Addable] = implicitly[Addable[A]]

extension [A: Addable](a: A)
  def +(b: A) = Addable[A].plus(a, b)
  def -(b: A) = Addable[A].diff(a, b)

trait Rotatable[A]:
  type B
  def rotations: Set[B]
  def rotate(a: A, b: B): A

case class Point3d(x: Int, y: Int, z: Int)

object Point3d:
  def parse(input: String): IO[Point3d] =
    input match
      case s"$x,$y,$z" => (x.toIntIO, y.toIntIO, z.toIntIO).mapN(Point3d.apply)
      case l => IO.raiseError(new Exception(s"Unparsed Point: $l"))

given Rotatable[Point3d] with
  enum Direction:
    case PosX
    case PosY
    case PosZ
    case NegX
    case NegY
    case NegZ
  enum Amount:
    case Zero
    case Ninety
    case OneEighty
    case TwoSeventy

  type B = (Direction, Amount)
  def rotations =
    Direction.values.toSet.flatMap(d => Amount.values.map(a => d -> a))
  def rotate(a: Point3d, rotation: B) =
    val orietented = rotation._1 match
      case Direction.PosX => Point3d(-a.z, a.y, a.x)
      case Direction.NegX => Point3d(a.z, a.y, -a.x)
      case Direction.PosY => Point3d(a.x, -a.z, a.y)
      case Direction.NegY => Point3d(a.x, a.z, -a.y)
      case Direction.PosZ => a
      case Direction.NegZ => Point3d(-a.x, a.y, -a.z)
    rotation._2 match
      case Amount.Zero => orietented
      case Amount.Ninety => Point3d(orietented.y, -orietented.x, orietented.z)
      case Amount.OneEighty =>
        Point3d(-orietented.x, -orietented.y, orietented.z)
      case Amount.TwoSeventy =>
        Point3d(-orietented.y, orietented.x, orietented.z)
end given

given Addable[Point3d] with
  def zero = Point3d(0, 0, 0)
  def plus(a: Point3d, b: Point3d) = Point3d(a.x + b.x, a.y + b.y, a.z + b.z)
  def diff(a: Point3d, b: Point3d) = Point3d(a.x - b.x, a.y - b.y, a.z - b.z)
  def manhatten(a: Point3d, b: Point3d) =
    math.abs(a.x - b.x) + math.abs(a.y - b.y) + math.abs(a.z - b.z)

case class Scanner[A](id: Int, points: Set[A]):
  def relativePoints(using Addable[A]): Map[A, Set[A]] =
    points
      .map(p => p -> (points.map(p2 => p2 - p).toSet - Addable[A].zero))
      .toMap

  def manhattenPoints(using Addable[A]): Map[A, Set[Int]] =
    points
      .map(p => p -> (points.map(p2 => Addable[A].manhatten(p, p2)).toSet - 0))
      .toMap

  def orientations(using rotater: Rotatable[A]): Set[Scanner[A]] =
    rotater.rotations.map(r =>
      Scanner(id, points.map(p => rotater.rotate(p, r)))
    )

object Scanner:
  def parse[A](input: List[String], parseA: String => IO[A]): IO[Scanner[A]] =
    input match
      case id :: points =>
        val parsedId = id match
          case s"--- scanner $i ---" => i.toIntIO
          case l => IO.raiseError(new Exception(s"Unparsed ID: $l"))
        val parsedPoints = points.traverse(parseA).map(_.toSet)
        (parsedId, parsedPoints).mapN(Scanner.apply)
      case Nil => IO.raiseError(new Exception("Empty input for scanner"))

  def tryAlignScanner[A: Addable: Rotatable](
      a: Scanner[A],
      b: Scanner[A],
      alignment: Int
  ): Option[(A, Scanner[A])] =
    val target = a.points.size - alignment + 1
    val mpa = a.manhattenPoints
    val mpb = b.manhattenPoints
    val intersections =
      mpa.count((k, v) => mpb.exists((_, v2) => v.diff(v2).size <= target))
    if intersections < alignment then None
    else
      b.orientations.foldLeft(Option.empty)((e, s) =>
        e.orElse(tryAlignScannerOrientation(a, s, alignment))
      )

  private def tryAlignScannerOrientation[A: Addable](
      a: Scanner[A],
      b: Scanner[A],
      alignment: Int
  ): Option[(A, Scanner[A])] =
    val rpa = a.relativePoints
    val rpb = b.relativePoints

    val intersection = rpa.flatMap((k, v) =>
      rpb
        .find((_, v2) => v.intersect(v2).size == (alignment - 1))
        .map((k2, v2) => (k, k2) -> v.intersect(v2))
    )
    if intersection.size == alignment then
      val examplePoint = intersection.headOption
      (
        examplePoint.map(_._1._1),
        examplePoint.map(_._1._2)
      ).mapN((a, b) => a - b)
        .map(diff => diff -> b.copy(points = b.points.map(p => diff + p)))
    else None

end Scanner

object Program extends PureDay:
  type Scanner3d = Scanner[Point3d]
  type A = (Set[Scanner3d], Set[Point3d])
  def parse(input: List[String]): IO[A] =
    input
      .split("")
      .traverse(Scanner.parse[Point3d](_, Point3d.parse))
      .map(alignScanners)

  def alignScanners(input: List[Scanner3d]): (Set[Scanner3d], Set[Point3d]) =
    val initial = input.head
    val rest = input.tail
    val origin = Point3d(0, 0, 0)

    def align(
        current: Scanner3d,
        remaining: List[Scanner3d],
        previouslyAligned: List[Scanner3d],
        positions: List[Point3d]
    ): (List[Scanner3d], List[Point3d]) =
      val aligned =
        remaining.flatMap(s => Scanner.tryAlignScanner(current, s, 12))

      aligned.foldLeft((previouslyAligned ++ aligned.map(_._2)) -> positions)(
        (a, s) =>
          val alignedIds = a._1.map(_.id).toSet
          val unalignedRemaining = remaining.filterNot(s => alignedIds(s.id))
          align(s._2, unalignedRemaining, a._1, s._1 :: a._2)
      )

    val (p, m) = align(
      initial,
      rest,
      List(initial),
      List(Point3d(0, 0, 0))
    )
    (p.toSet, m.toSet)
  end alignScanners

  def part1(input: A): String =
    input._1.flatMap(_.points).size.toString
  def part2(input: A): String =
    val locs = input._2
    locs
      .flatMap(a => locs.map(b => Addable[Point3d].manhatten(a, b)))
      .max
      .toString

end Program
