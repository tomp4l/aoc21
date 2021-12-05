package aoc21
package day05

class Day05Spec extends munit.CatsEffectSuite:
  test("it parses a line") {
    assertIO(
      Line.parse("72,504 -> 422,154"),
      Line(Point2d(72, 504), Point2d(422, 154))
    )
  }

  test("it finds points on line") {
    val line = Line(Point2d(1, 3), Point2d(3, 3))
    assertEquals(line.points, Set(Point2d(1, 3), Point2d(2, 3), Point2d(3, 3)))
  }
