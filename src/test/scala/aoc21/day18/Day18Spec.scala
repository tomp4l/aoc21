package aoc21.day18

import io.circe.parser.parse
import cats.syntax.all.*

class Day18Spec extends munit.FunSuite:
  extension (s: String)
    def toSnailfish: Snailfish =
      parse(s).flatMap(_.as[Snailfish]).getOrElse(fail("invalid json"))

  import aoc21.day18.Snailfish.*
  given Conversion[Int, Snailfish] = Number(_)

  test("Snailfish reduces number on far left with explode") {
    val number = Pair(Pair(Pair(Pair(Pair(9, 8), 1), 2), 3), 4)

    val reduced = number.reduced

    assertEquals(reduced, Pair(Pair(Pair(Pair(0, 9), 2), 3), 4))
  }

  test("Snailfish reduces number on far right with explode") {
    val number = Pair(7, Pair(6, Pair(5, Pair(4, Pair(3, 2)))))

    val reduced = number.reduced

    assertEquals(reduced, Pair(7, Pair(6, Pair(5, Pair(7, 0)))))
  }

  test("Snailfish reduces number in middle with explode") {
    val number = Pair(Pair(6, Pair(5, Pair(4, Pair(3, 2)))), 1)
    val reduced = number.reduced

    assertEquals(reduced, Pair(Pair(6, Pair(5, Pair(7, 0))), 3))
  }

  test("Snailfish reduces very nested number") {
    val number = Pair(
      Pair(3, Pair(2, Pair(1, Pair(7, 3)))),
      Pair(6, Pair(5, Pair(4, Pair(3, 2))))
    )
    val reduced = number.reduced

    assertEquals(
      reduced,
      Pair(
        Pair(3, Pair(2, Pair(8, 0))),
        Pair(9, Pair(5, Pair(7, 0)))
      )
    )
  }

  test("Snailfish adds two together") {
    val a = Pair(Pair(Pair(Pair(4, 3), 4), 4), Pair(7, Pair(Pair(8, 4), 9)))
    val b = Pair(1, 1)

    assertEquals(
      a + b,
      Pair(Pair(Pair(Pair(0, 7), 4), Pair(Pair(7, 8), Pair(6, 0))), Pair(8, 1))
    )
  }

  test("Snailfish adds two complex numbers together") {
    val a = "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]".toSnailfish
    val b = "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]".toSnailfish
    assertEquals(
      a + b,
      "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]".toSnailfish
    )
  }

end Day18Spec
