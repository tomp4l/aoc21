package aoc21
package day12

import cats.syntax.all.*

import aoc21.PureDay
import cats.effect.IO
import cats.Eval

enum Cave:
  case Start
  case End
  case Big(id: String)
  case Small(id: String)

  def isSmall: Boolean =
    this match
      case Small(_) => true
      case _ => false

object Cave:
  def fromString(s: String) = s match
    case "start" => Start
    case "end" => End
    case i if i.toLowerCase == i => Small(i)
    case i => Big(i)

extension (s: String) def toCave = Cave.fromString(s)

object Program extends PureDay:
  type A = Map[Cave, Set[Cave]]

  def parse(input: List[String]): cats.effect.IO[A] =
    input
      .traverse {
        case s"$a-$b" => IO.pure((a.toCave, b.toCave))
        case l => IO.raiseError(new Exception(s"failed to parse: $l"))
      }
      .map(_.flatMap((a, b) => List((a, b), (b, a))))
      .map(_.groupMapReduce(_._1)(t => Set(t._2))(_ ++ _))

  def traverseCaves(
      current: Cave,
      graph: A,
      path: List[Cave],
      filter: (List[Cave]) => Cave => Boolean
  ): Eval[Int] =
    val filtered = filter(path)
    graph(current).toList
      .traverse[Eval, Int](c =>
        c match
          case Cave.Start => Eval.now(0)
          case Cave.End => Eval.now(1)
          case c if (filtered(c)) => Eval.now(0)
          case c => Eval.defer(traverseCaves(c, graph, c :: path, filter))
      )
      .map(_.sum)

  def part1(input: A): String =
    traverseCaves(
      Cave.Start,
      input,
      List(Cave.Start),
      _.filter(_.isSmall).toSet
    ).value.toString

  def part2(input: A): String =
    traverseCaves(
      Cave.Start,
      input,
      List(Cave.Start),
      path =>
        val small = path.filter(_.isSmall)
        val max = small.groupBy(identity).map(_._2.size).maxOption.getOrElse(0)
        if max > 1 then small.toSet
        else Set.empty
    ).value.toString
end Program
