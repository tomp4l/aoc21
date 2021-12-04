package aoc21

import cats.Show
import cats.syntax.all.*

given [A: Show]: Show[Map[(Int, Int), A]] with
  def show(m: Map[(Int, Int), A]): String =
    def loop(
        remaining: List[((Int, Int), A)],
        acc: List[List[A]]
    ): List[List[String]] =
      remaining match
        case Nil => acc.map(_.map(_.show))
        case ((x, y), i) :: rest =>
          val (c, n) =
            if (y == 0) then (List.empty, acc)
            else (acc.head, acc.tail)
          loop(rest, (c :+ i) :: n)

    val groups = loop(m.toList.sortBy(_._1), List.empty)

    val columnWidths = groups
      .flatMap(_.zipWithIndex)
      .groupBy(_._2)
      .toList
      .sortBy(_._1)
      .map(_._2.map(_._1.length).max)

    val paddedGroups =
      groups.map(_.zip(columnWidths).map((s, i) => " " * (i - s.length) + s))

    paddedGroups.reverse
      .map(_.mkString(" "))
      .mkString("\n")
  end show
end given
