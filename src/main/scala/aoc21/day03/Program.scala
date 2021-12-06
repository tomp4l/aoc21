package aoc21
package day03

import cats.syntax.all.*

object Program extends StringDay with PureDay:

  private def countsFromStrings(input: List[String]) =
    input
      // List of Maps with count of 1 for each Char (0/1)
      .map(_.map(c => Map(c -> 1)).toList)
      // Use Map + Int Semigroup to add up count of each char in each position
      .fold(List.empty)(_.alignCombine(_))

  private def rating(input: List[String], fitness: Map[Char, Int] => Char) =
    def loop(remaining: List[(String, String)]): String =
      if remaining.length == 1 then remaining.head._2
      else
        val counts = countsFromStrings(remaining.map(_._1))
        val char = fitness(counts.head)
        val newRemaining = remaining.filter(t => t._1.head == char).map {
          case (a, b) => (a.tail, b)
        }
        loop(newRemaining)
    loop(input.map(c => (c, c)))

  def part1(input: List[String]): String =
    val counts = countsFromStrings(input)
    val gamma = counts.map(_.maxBy(_._2)._1).mkString
    val epsilon = counts.map(_.minBy(_._2)._1).mkString
    val power = gamma.toBinaryInt * epsilon.toBinaryInt
    power.toString

  def part2(input: List[String]): String =
    val oxygen = rating(
      input,
      (m) =>
        if m.get('0') == m.get('1') then '1'
        else m.maxBy(_._2)._1
    )
    val co2 = rating(
      input,
      (m) =>
        if m.get('0') == m.get('1') then '0'
        else m.minBy(_._2)._1
    )
    val lifeSupport = oxygen.toBinaryInt * co2.toBinaryInt
    lifeSupport.toString

end Program
