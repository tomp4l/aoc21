package aoc21
package day11

import cats.syntax.all.*
import cats.data.State
import cats.Monad

object Program extends PureDay with Int2dDay:

  def simulateFlashes(map: A): (A, Int) =
    Monad[[X] =>> State[A, X]]
      .tailRecM(Set.empty[Point2d])(flashed =>
        State
          .inspect[A, Set[Point2d]](_.filter((_, v) => v > 9).keySet -- flashed)
          .flatMap(toFlash =>
            if toFlash.isEmpty then
              State.inspect((state: A) =>
                val setZero = state.map((k, v) => k -> (if v > 9 then 0 else v))
                Right((setZero, flashed.size))
              )
            else
              toFlash.toList
                .flatMap(p => p.neighbours ++ p.diagonals)
                .traverse(p => State.modify[A](_.updatedWith(p)(_.map(_ + 1))))
                .as(Left(flashed ++ toFlash))
          )
      )
      .runA(map.map((k, v) => k -> (v + 1)))
      .value

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
