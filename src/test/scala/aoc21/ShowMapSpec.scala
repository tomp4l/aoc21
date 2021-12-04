package aoc21

import cats.syntax.all.*

class ShowMapSpec extends munit.FunSuite:
  test("it shows a map with coordinates") {
    val map = Map((0, 0) -> 1, (0, 1) -> 2, (1, 0) -> 3, (1, 1) -> 4)
    assertEquals(map.show, "1 2\n3 4")
  }

  test("it creates fixed width columns") {
    val map = Map((0, 0) -> 1, (0, 1) -> 23, (1, 0) -> 345, (1, 1) -> 4)
    assertEquals(map.show, "  1 23\n345  4")
  }
