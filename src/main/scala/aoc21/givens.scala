package aoc21

import cats.Show
import cats.syntax.all.*
import scala.annotation.tailrec

given showIntTupleMap[A: Show]: Show[Map[(Int, Int), A]] with
  def show(m: Map[(Int, Int), A]): String =
    if m.isEmpty then ""
    else
      @tailrec
      def loop(
          remaining: List[((Int, Int), String)],
          acc: List[List[String]]
      ): List[List[String]] =
        remaining match
          case Nil => acc.map(_.map(_.show))
          case ((_, x), i) :: rest =>
            val (c, n) =
              if (x == 0) then (List.empty, acc)
              else (acc.head, acc.tail)
            loop(rest, (c :+ i) :: n)

      val maxX = m.maxBy(_._1._1)._1._1
      val maxY = m.maxBy(_._1._2)._1._2
      val minX = m.minBy(_._1._1)._1._1
      val minY = m.minBy(_._1._2)._1._2

      val allPoints = for
        x <- minX to maxX
        y <- minY to maxY
      yield (x, y)

      val filledMap = allPoints.foldLeft(m.map((c, i) => c -> i.show))((m, p) =>
        m.updatedWith(p) {
          case None => Some(".")
          case i => i
        }
      )

      val groups = loop(
        filledMap.toList
          .map { case ((x, y), a) => ((y, x), a.show) }
          .sortBy(_._1),
        List.empty
      ).reverse

      val columnWidths = groups
        .flatMap(_.zipWithIndex)
        .groupBy(_._2)
        .toList
        .sortBy(_._1)
        .map(_._2.map(_._1.length).max)

      val paddedGroups =
        groups.map(_.zip(columnWidths).map((s, i) => " " * (i - s.length) + s))

      paddedGroups
        .map(_.mkString(" "))
        .mkString("\n")
  end show
end showIntTupleMap
