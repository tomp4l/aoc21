package aoc21.day19

import aoc21.Point2d

class Day19Spec extends munit.FunSuite:

  given Addable[Point2d] with
    def plus(a: Point2d, b: Point2d) = a + b
    def diff(a: Point2d, b: Point2d) = Point2d(a.x - b.x, a.y - b.y)
    def zero = Point2d(0, 0)

    def manhatten(a: Point2d, b: Point2d) =
      math.abs(a.x - b.x) + math.abs(a.y - b.y)

  given Rotatable[Point2d] with
    enum Rotation2d:
      case Zero
      case Ninety
      case OneEighty
      case TwoSeventy
    type B = Rotation2d
    def rotations = Rotation2d.values.toSet
    def rotate(p: Point2d, direction: B) = direction match
      case Rotation2d.Zero => p
      case Rotation2d.Ninety => Point2d(-p.y, p.x)
      case Rotation2d.OneEighty => Point2d(-p.x, -p.y)
      case Rotation2d.TwoSeventy => Point2d(p.y, -p.x)

  test("finds differences between points to be equal") {
    val scanner0 =
      Scanner[Point2d](0, Set(Point2d(0, 2), Point2d(4, 1), Point2d(3, 3)))
    val scanner1 =
      Scanner[Point2d](
        1,
        Set(Point2d(-1, -1), Point2d(-5, 0), Point2d(-2, 1))
      )
    assert(
      clue(scanner0.relativePoints.values.toList).forall(s =>
        clue(scanner0.relativePoints.values.toList).contains(s)
      )
    )
  }

  test("aligns two scanners with same orientation") {
    val scanner0 =
      Scanner[Point2d](0, Set(Point2d(0, 2), Point2d(4, 1), Point2d(3, 3)))
    val scanner1 =
      Scanner[Point2d](
        1,
        Set(Point2d(-1, -1), Point2d(-5, 0), Point2d(-2, 1))
      )

    val aligned = Scanner.tryAlignScanner(scanner0, scanner1, 3)

    assertEquals(aligned.map(_._2.points), Some(scanner0.points))
    assertEquals(aligned.map(_._1), Some(Point2d(5, 2)))
  }

  test("aligns two scanners with same orientation and extra points") {
    val extra = Point2d(2, 6)
    val scanner0 =
      Scanner[Point2d](
        0,
        Set(Point2d(0, 2), Point2d(4, 1), Point2d(3, 3), extra)
      )
    val scanner1 =
      Scanner[Point2d](
        1,
        Set(Point2d(-1, -1), Point2d(-5, 0), Point2d(-2, 1), Point2d(-5, -9))
      )

    val aligned = Scanner.tryAlignScanner(scanner0, scanner1, 3)

    assertEquals(
      aligned.map(_._2.points.intersect(scanner0.points)),
      Some(scanner0.points - extra)
    )
    assertEquals(aligned.map(_._2.points.size), Some(4))
    assertEquals(aligned.map(_._1), Some(Point2d(5, 2)))
  }

  test("finds four rotations for a scanner") {
    /*
    #####
    ###X#
    ##S##
    #####
    #X###

    #####
    X####
    ##S##
    ###X#
    #####

    ###X#
    #####
    ##S##
    #X###
    #####

    #####
    #X###
    ##S##
    ####X
    #####
     */
    val scanner =
      Scanner[Point2d](
        0,
        Set(Point2d(1, 1), Point2d(-1, -2))
      )
    val rotations = scanner.orientations.map(_.points)

    assertEquals(
      rotations,
      Set(
        Set(Point2d(1, 1), Point2d(-1, -2)),
        Set(Point2d(1, -1), Point2d(-2, 1)),
        Set(Point2d(-1, -1), Point2d(1, 2)),
        Set(Point2d(-1, 1), Point2d(2, -1))
      )
    )
  }

  test("aligns two rotated scanners 180") {
    val scanner0 =
      Scanner[Point2d](0, Set(Point2d(0, 2), Point2d(4, 1), Point2d(3, 3)))
    val scanner1 =
      Scanner[Point2d](
        1,
        Set(Point2d(1, 1), Point2d(5, 0), Point2d(2, -1))
      )

    val aligned = Scanner.tryAlignScanner(scanner0, scanner1, 3)

    assertEquals(aligned.map(_._2.points), Some(scanner0.points))
    assertEquals(aligned.map(_._1), Some(Point2d(5, 2)))
  }

  test("aligns two rotated scanners 90") {
    val scanner0 =
      Scanner[Point2d](0, Set(Point2d(0, 2), Point2d(4, 1), Point2d(3, 3)))
    val scanner1 =
      Scanner[Point2d](
        1,
        Set(Point2d(-1, 1), Point2d(0, 5), Point2d(1, 2))
      )

    val aligned = Scanner.tryAlignScanner(scanner0, scanner1, 3)

    assertEquals(aligned.map(_._2.points), Some(scanner0.points))
    assertEquals(aligned.map(_._1), Some(Point2d(5, 2)))
  }

  test("finds six orientations for 3d scanner on axis") {
    val scanner = Scanner(
      0,
      Set(
        Point3d(1, 0, 0)
      )
    )
    val orientations = scanner.orientations.flatMap(_.points)
    assert(clue(orientations.toList).size == 6)
  }

  test("finds twelve orientations for 3d scanner on plane") {
    val scanner = Scanner(
      0,
      Set(
        Point3d(1, 1, 0)
      )
    )
    val orientations = scanner.orientations.flatMap(_.points)
    assertEquals(orientations.size, 12)
  }

  test("finds eight orientations for 3d scanner on cube corner") {
    val scanner = Scanner(
      0,
      Set(
        Point3d(1, 1, 1)
      )
    )
    val orientations = scanner.orientations.flatMap(_.points)
    assertEquals(orientations.size, 8)
  }

  test("finds orientations for 3d scanner") {
    val scanner = Scanner(
      0,
      Set(
        Point3d(1, 2, 0)
      )
    )
    val orientations = scanner.orientations.flatMap(_.points)
    assert(orientations.contains(Point3d(1, 2, 0)))
    assert(orientations.contains(Point3d(-1, -2, 0)))
    assert(orientations.contains(Point3d(2, -1, 0)))
    assert(orientations.contains(Point3d(-2, 1, 0)))
    assert(orientations.contains(Point3d(-1, 2, 0)))

    assertEquals(orientations.size, 24)
  }

  test("finds orientations for example 3d scanner") {
    val scanner = Scanner(
      0,
      Set(
        Point3d(-1, -1, 1),
        Point3d(-2, -2, 2),
        Point3d(-3, -3, 3),
        Point3d(-2, -3, 1),
        Point3d(5, 6, -4),
        Point3d(8, 0, 7)
      )
    )
    val exampleOrientations = Set(
      scanner.points,
      Set(
        Point3d(1, -1, 1),
        Point3d(2, -2, 2),
        Point3d(3, -3, 3),
        Point3d(2, -1, 3),
        Point3d(-5, 4, -6),
        Point3d(-8, -7, 0)
      ),
      Set(
        Point3d(-1, -1, -1),
        Point3d(-2, -2, -2),
        Point3d(-3, -3, -3),
        Point3d(-1, -3, -2),
        Point3d(4, 6, 5),
        Point3d(-7, 0, 8)
      ),
      Set(
        Point3d(1, 1, -1),
        Point3d(2, 2, -2),
        Point3d(3, 3, -3),
        Point3d(1, 3, -2),
        Point3d(-4, -6, 5),
        Point3d(7, 0, 8)
      ),
      Set(
        Point3d(1, 1, 1),
        Point3d(2, 2, 2),
        Point3d(3, 3, 3),
        Point3d(3, 1, 2),
        Point3d(-6, -4, -5),
        Point3d(0, 7, -8)
      )
    )

    val orientations = scanner.orientations.map(_.points)
    assertEquals(clue(orientations).size, 24)
    assertEquals(
      orientations.intersect(exampleOrientations),
      exampleOrientations
    )
  }

  test("finds alignment between two 3d scanners") {
    val scanner0 = Scanner(
      0,
      Set(
        Point3d(404, -588, -901),
        Point3d(528, -643, 409),
        Point3d(-838, 591, 734),
        Point3d(390, -675, -793),
        Point3d(-537, -823, -458),
        Point3d(-485, -357, 347),
        Point3d(-345, -311, 381),
        Point3d(-661, -816, -575),
        Point3d(-876, 649, 763),
        Point3d(-618, -824, -621),
        Point3d(553, 345, -567),
        Point3d(474, 580, 667),
        Point3d(-447, -329, 318),
        Point3d(-584, 868, -557),
        Point3d(544, -627, -890),
        Point3d(564, 392, -477),
        Point3d(455, 729, 728),
        Point3d(-892, 524, 684),
        Point3d(-689, 845, -530),
        Point3d(423, -701, 434),
        Point3d(7, -33, -71),
        Point3d(630, 319, -379),
        Point3d(443, 580, 662),
        Point3d(-789, 900, -551),
        Point3d(459, -707, 401)
      )
    )
    val scanner1 = Scanner(
      1,
      Set(
        Point3d(686, 422, 578),
        Point3d(605, 423, 415),
        Point3d(515, 917, -361),
        Point3d(-336, 658, 858),
        Point3d(95, 138, 22),
        Point3d(-476, 619, 847),
        Point3d(-340, -569, -846),
        Point3d(567, -361, 727),
        Point3d(-460, 603, -452),
        Point3d(669, -402, 600),
        Point3d(729, 430, 532),
        Point3d(-500, -761, 534),
        Point3d(-322, 571, 750),
        Point3d(-466, -666, -811),
        Point3d(-429, -592, 574),
        Point3d(-355, 545, -477),
        Point3d(703, -491, -529),
        Point3d(-328, -685, 520),
        Point3d(413, 935, -424),
        Point3d(-391, 539, -444),
        Point3d(586, -435, 557),
        Point3d(-364, -763, -893),
        Point3d(807, -499, -711),
        Point3d(755, -354, -619),
        Point3d(553, 889, -390)
      )
    )

    val result = Scanner.tryAlignScanner(scanner0, scanner1, 12)
    assert(clue(result).nonEmpty)
    assertEquals(result.map(_._1), Some(Point3d(68, -1246, -43)))
  }
end Day19Spec
