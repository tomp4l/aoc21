package aoc21
package day11

import cats.syntax.all.*
import cats.data.State

object Program extends PureDay:
  type A = Map[Point2d, Int]

  def parse(input: List[String]): cats.effect.IO[A] =
    input
      .map(_.zipWithIndex)
      .zipWithIndex
      .flatMap((c, y) =>
        c.map((i, x) => i.toString.toIntIO.map(Point2d(x, y) -> _))
      )
      .sequence
      .map(_.toMap)

  def simulateFlashes(map: A): (A, Int) =
    val increased = map.map((k, v) => k -> (v + 1))
    def loop(state: A, flashed: Set[Point2d]): (A, Int) =
      val powered = state.filter((_, v) => v > 9).keySet
      val toFlash = powered -- flashed
      if toFlash.isEmpty then
        (state.map((k, v) => k -> (if v > 9 then 0 else v)), flashed.size)
      else
        val toIncrease =
          toFlash.toList.flatMap(p => p.neighbours ++ p.diagonals)
        val nextState = toIncrease
          .traverse(p => State.modify[A](_.updatedWith(p)(_.map(_ + 1))))
          .runS(state)
        loop(nextState.value, powered)
    loop(increased, Set.empty)

  def part1(input: A): String =
    def run(i: Int, a: A, c: Int): String =
      if i == 0 then c.toString
      else
        val (n, d) = simulateFlashes(a)
        run(i - 1, n, c + d)
    run(100, input, 0)

  def part2(input: A): String =
    def run(i: Int, a: A): Int =
      val (n, d) = simulateFlashes(a)
      if a.size == d then i + 1
      else run(i + 1, n)

    run(0, input).toString
end Program
