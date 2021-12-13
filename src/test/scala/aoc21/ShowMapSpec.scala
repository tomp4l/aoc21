package aoc21

import cats.syntax.all.*
import cats.Show

given Show[Int] = _.toString

class ShowMapSpec extends munit.FunSuite:
  test("it shows a map with coordinates") {
    val map = Map((0, 0) -> 1, (0, 1) -> 2, (1, 0) -> 3, (1, 1) -> 4)
    assertEquals(map.show, "13\n24")
  }

  test("it creates fixed width columns") {
    val map = Map((0, 0) -> 1, (1, 0) -> 23, (0, 1) -> 345, (1, 1) -> 4)
    assertEquals(map.show, "  123\n345 4")
  }

  test("it fills in sparse maps") {
    val map = Map((0, 0) -> 1, (2, 0) -> 2, (1, 2) -> 12)
    assertEquals(map.show, "1 .2\n. ..\n.12.")
  }

  test("it works for non zero indexed maps") {
    val map = Map((10, 10) -> 1, (10, 11) -> 2, (11, 10) -> 3, (11, 11) -> 4)
    assertEquals(map.show, "13\n24")
  }
