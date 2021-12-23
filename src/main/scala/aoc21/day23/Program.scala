package aoc21
package day23

import cats.effect.IO
import cats.syntax.all.*
import scala.annotation.targetName
import cats.Show
import cats.data.State
import simulacrum.op

enum Amphipod:
  case Amber
  case Bronze
  case Copper
  case Desert

given Show[Amphipod] = {
  case Amphipod.Amber => "A"
  case Amphipod.Bronze => "B"
  case Amphipod.Copper => "C"
  case Amphipod.Desert => "D"
}

object Amphipod:
  def parse(s: String) = s match
    case "A" => IO.pure(Amphipod.Amber)
    case "B" => IO.pure(Amphipod.Bronze)
    case "C" => IO.pure(Amphipod.Copper)
    case "D" => IO.pure(Amphipod.Desert)
    case s => IO.raiseError(new Exception(s"Unrecognised amphipod: $s"))

enum Halls:
  case Room(target: Amphipod)
  case Hallway
  case Enterance

  def isRoom = this match
    case Room(_) => true
    case _ => false

case class Burrow(map: Map[Point2d, Halls], amphipods: Map[Point2d, Amphipod]):
  def rooms = map.collect { case (p, x: Halls.Room) => p -> x }
  def hallways = map.filter(_._2 == Halls.Hallway).keySet

given Show[Burrow] = burrow =>
  val map = burrow.map.map((p, v) => p -> "*")
  val amphipods = burrow.amphipods.map((p, v) => p -> v.show)
  (map ++ amphipods).show

object Program extends PureDay:

  type A = Burrow

  def parse(input: List[String]): IO[A] =
    val asMap = input.to2dMap(_.split("").toList, s => IO.pure(s))
    asMap.flatMap(m =>
      val filtered = m.filterNot((_, v) => v == "#" || v == " ")
      filtered.toList
        .traverse((p, v) =>
          v match
            case "." =>
              if filtered.get(p + Point2d(0, 1)) == None then
                IO.pure((p, Halls.Hallway, None))
              else IO.pure((p, Halls.Enterance, None))
            case a =>
              val target = p.x match
                case 3 => IO.pure(Amphipod.Amber)
                case 5 => IO.pure(Amphipod.Bronze)
                case 7 => IO.pure(Amphipod.Copper)
                case 9 => IO.pure(Amphipod.Desert)
                case x =>
                  IO.raiseError(new Exception(s"Unknown amphipod at $x"))
              (Amphipod.parse(a), target).mapN((amphipod, target) =>
                (p, Halls.Room(target), Some(amphipod))
              )
        )
        .map(halls =>
          val (map, amphipods) = halls.foldLeft(
            (Map.empty[Point2d, Halls], Map.empty[Point2d, Amphipod])
          ) { case ((m, as), (p, r, a)) =>
            (m.updated(p, r), a.fold(as)(a => as.updated(p, a)))
          }
          Burrow(map, amphipods)
        )
    )
  end parse

  def cost(amphipod: Amphipod, from: Point2d, to: Point2d) =
    val multiplier = amphipod match
      case Amphipod.Amber => 1
      case Amphipod.Bronze => 10
      case Amphipod.Copper => 100
      case Amphipod.Desert => 1000
    val distance =
      if from.y > 1 && to.y > 1 then math.abs(to.x - from.x) + to.y + from.y - 2
      else math.abs(to.x - from.x) + math.abs(to.y - from.y)
    distance * multiplier

  def openPath(burrow: Burrow, from: Point2d, to: Point2d): Boolean =
    val above = from + Point2d(0, -1)
    val below = from + Point2d(0, 1)
    val left = from + Point2d(-1, 0)
    val right = from + Point2d(1, 0)
    if from == to then true
    else
      val next =
        if from.x < to.x && burrow.map.isDefinedAt(right) then right
        else if from.x > to.x && burrow.map.isDefinedAt(left) then left
        else if (from.x != to.x) then above
        else if from.y < to.y then below
        else above

      if burrow.amphipods.isDefinedAt(next) then false
      else openPath(burrow, next, to)

  def isSolved(burrow: Burrow) =
    val rooms = burrow.map.collect { case (p, x: Halls.Room) => p -> x }
    rooms.forall((p, r) => burrow.amphipods.get(p).contains(r.target))

  def minumumCost(burrow: Burrow) =
    val remaining = burrow.amphipods.filter((p, a) =>
      burrow.map.get(p).exists {
        case Halls.Room(t) => t != a
        case _ => true
      }
    )
    remaining
      .map((p, a) =>
        val target = burrow.rooms.filter(_._2.target == a).keySet.minBy(_.y)
        cost(a, p, target)
      )
      .sum + remaining
      .groupBy(_._1)
      .map(l => ((l._2.size - 1) * l._2.size) / 2)
      .sum

  def isOpenHome(
      point: Point2d,
      room: Halls.Room,
      rooms: Map[Point2d, Halls.Room],
      amphipods: Map[Point2d, Amphipod]
  ) =
    val below = point + Point2d(0, 1)
    rooms.get(below) match
      case None =>
        !amphipods.isDefinedAt(point)
      case Some(_) =>
        !amphipods.isDefinedAt(point) &&
          rooms
            .filter((p, _) => p.x == point.x && p.y > point.y)
            .forall((p, room) => amphipods.get(p) == Some(room.target))

  def trySolution(
      burrow: Burrow,
      currentScore: Int
  ): Option[Int] =
    if isSolved(burrow) then Some(currentScore)
    else
      val rooms = burrow.rooms
      val hallways = burrow.hallways

      val openHomes =
        rooms.filter((p, r) => isOpenHome(p, r, rooms, burrow.amphipods))

      val homeMoves =
        openHomes.flatMap((to, r) =>
          burrow.amphipods
            .filter(_._2 == r.target)
            .filterNot((p, a) => rooms.get(p).exists(r => r.target == a))
            .filter((from, a) => openPath(burrow, from, to))
            .map((from, a) => (a, from, to))
        )

      val nextMoves =
        if !homeMoves.isEmpty then homeMoves
        else
          rooms
            .flatMap((p, r) =>
              val above = p + Point2d(0, -1)
              val below = p + Point2d(0, 1)
              rooms.get(below) match
                case Some(_) =>
                  if burrow.amphipods.get(p).exists(_ == r.target) &&
                    burrow.amphipods.get(below).exists(_ == r.target) ||
                    burrow.amphipods.isDefinedAt(above)
                  then None
                  else burrow.amphipods.get(p).map(p -> _)
                case None =>
                  if burrow.amphipods.get(p).exists(_ == r.target) then None
                  else
                    burrow.amphipods.get(above) match
                      case None => burrow.amphipods.get(p).map(p -> _)
                      case Some(_) => None
            )
            .flatMap((from, a) =>
              hallways
                .filter(to => openPath(burrow, from, to))
                .map(to => (a, from, to))
            )

      nextMoves.toList
        .sortBy((a, from, to) => cost(a, from, to))
        .foldLeft(Option.empty[Int]) { case (best, (a, from, to)) =>
          val score = cost(a, from, to)
          val totalScore = currentScore + score
          if best.exists(s => s <= totalScore) then best
          else
            val as = burrow.amphipods
            val moved = as - from + (to -> a)
            val newBurrow = burrow.copy(amphipods = moved)
            val optimalScore = totalScore + minumumCost(newBurrow)
            if best.exists(s => s <= optimalScore) then best
            else
              trySolution(newBurrow, totalScore)
                .map(s => best.map(b => math.min(s, b)).getOrElse(s))
                .orElse(best)
        }
    end if
  end trySolution

  def part1(input: A): String =
    trySolution(input, 0).map(_.toString).getOrElse("unsolved")

  def part2(input: A): String =
    val newRooms =
      List(4, 5)
        .flatMap(y =>
          List(
            Point2d(3, y) -> Halls.Room(Amphipod.Amber),
            Point2d(5, y) -> Halls.Room(Amphipod.Bronze),
            Point2d(7, y) -> Halls.Room(Amphipod.Copper),
            Point2d(9, y) -> Halls.Room(Amphipod.Desert)
          )
        )
        .toMap
    val newAmphipods = Map(
      Point2d(3, 3) -> Amphipod.Desert,
      Point2d(3, 4) -> Amphipod.Desert,
      Point2d(5, 3) -> Amphipod.Copper,
      Point2d(5, 4) -> Amphipod.Bronze,
      Point2d(7, 3) -> Amphipod.Bronze,
      Point2d(7, 4) -> Amphipod.Amber,
      Point2d(9, 3) -> Amphipod.Amber,
      Point2d(9, 4) -> Amphipod.Copper
    )

    val shiftedAmphipods = input.amphipods.map((p, v) =>
      if p.y == 3 then p.copy(y = 5) -> v else p -> v
    )

    val newInput =
      Burrow(input.map ++ newRooms, newAmphipods ++ shiftedAmphipods)

    trySolution(newInput, 0).map(_.toString).getOrElse("unsolved")

  end part2

end Program
