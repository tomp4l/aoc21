package aoc21.day14

import aoc21.PureDay
import cats.effect.IO
import cats.syntax.all.*
import cats.data.State

case class Recipe(left: Char, right: Char, insert: Char)

case class Polymer(elements: List[Char], recipes: List[Recipe])

object Program extends PureDay:
  type A = Polymer
  def parse(input: List[String]): IO[A] =
    val elements = IO(input.head.toCharArray.toList)
    val recipes = input.drop(2).traverse {
      case s"$a -> $c" if a.length == 2 =>
        IO.pure(Recipe(a(0), a(1), c.head))
      case l => IO.raiseError(new Exception(s"failed to parse: $l"))
    }
    (elements, recipes).mapN(Polymer(_, _))

  def inputToMap(input: List[Char]) =
    input
      .sliding(2)
      .map(_.mkString)
      .toList
      .groupMapReduce(identity)(_ => 1L)(_ + _)

  def mapToCounts(map: Map[String, Long], start: Char, end: Char) =
    val counts = (start -> 1L) :: (end -> 1L) :: map.toList
      .flatMap((s, c) => s.toCharArray.map(_ -> c).toList)
    counts.groupMapReduce(_._1)(_._2)(_ + _).map((c, i) => c -> (i / 2))

  def recipeToMapped(recipe: Recipe) =
    val input = List(recipe.left, recipe.insert, recipe.right)
    (List(recipe.left, recipe.right).mkString, inputToMap(input))

  def applyMapped(recipes: Map[String, Map[String, Long]]) =
    State.modify[Map[String, Long]](state =>
      state
        .map((s, i) =>
          recipes.getOrElse(s, Map(s -> 1L)).map((s, j) => s -> i * j)
        )
        .toList
        .combineAll
    )

  def run(input: A, n: Int) =
    val recipes = input.recipes.map(recipeToMapped).toMap
    val elements = inputToMap(input.elements)
    val result =
      (1 to n).toList.traverse(_ => applyMapped(recipes)).runS(elements).value
    val counts =
      mapToCounts(result, input.elements.head, input.elements.last).values
    (counts.max - counts.min).toString

  def part1(input: A): String =
    run(input, 10)

  def part2(input: A): String =
    run(input, 40)
end Program
