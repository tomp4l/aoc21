package aoc21
package day15

import aoc21.Int2dDay
import aoc21.PureDay
import aoc21.Point2d

import cats.syntax.all.*
import cats.data.State
import scala.collection.immutable.TreeMap

object Program extends Int2dDay with PureDay:
  def part1(input: A): String =
    val maxX = input.maxBy(_._1.x)._1.x
    val maxY = input.maxBy(_._1.y)._1.y

    Dijkstra.shortestPathTo(input, Point2d(0, 0), Point2d(maxX, maxY)).toString

  def part2(input: A): String =
    val maxX = input.maxBy(_._1.x)._1.x
    val maxY = input.maxBy(_._1.y)._1.y

    val tiles = for
      xt <- 0 until 5
      yt <- 0 until 5
    yield input.map((k, v) =>
      val x = k.x + xt * (maxX + 1)
      val y = k.y + yt * (maxY + 1)
      val risk = (v + xt + yt) % 9
      Point2d(x, y) -> (if risk == 0 then 9 else risk)
    )
    val fullInput = tiles.toList.combineAll
    val fullMaxX = fullInput.maxBy(_._1.x)._1.x
    val fullMaxY = fullInput.maxBy(_._1.y)._1.y
    Dijkstra
      .shortestPathTo(fullInput, Point2d(0, 0), Point2d(fullMaxX, fullMaxY))
      .toString
end Program

object Dijkstra:

  def shortestPathTo(
      graph: Map[Point2d, Int],
      start: Point2d,
      target: Point2d
  ) =
    def loop(
        visited: Set[Point2d],
        distances: Map[Point2d, Int],
        minMap: TreeMap[Int, Set[Point2d]]
    ): Int =
      val (distance, points) = minMap.head
      if points(target) then distance
      else
        val (newVisited, updatedDistances, newMinMap) = points.toList
          .traverse(point =>
            State.modify[
              (Set[Point2d], Map[Point2d, Int], TreeMap[Int, Set[Point2d]])
            ] { (visited, distances, minMap) =>
              val costs = point.neighbours
                .filterNot(visited)
                .flatMap(p => graph.get(p).map(p -> _))
              val (updatedDistances, updatedMinMap) =
                costs.foldLeft((distances, minMap)) { case ((d, m), (p, c)) =>
                  val cost = distance + c
                  d.get(p) match
                    case Some(v) if (v <= cost) => (d, m)
                    case _ =>
                      (
                        d + (p -> cost),
                        m.updatedWith(cost) {
                          case Some(s) => Some(s + p)
                          case None => Some(Set(p))
                        }
                      )

                }
              val newVisited = visited + point
              (newVisited, updatedDistances, updatedMinMap)
            }
          )
          .runS((visited, distances, minMap - distance))
          .value

        loop(newVisited, updatedDistances, newMinMap)
      end if
    end loop
    loop(Set.empty, distances = Map(start -> 0), TreeMap(0 -> Set(start)))
  end shortestPathTo
end Dijkstra
