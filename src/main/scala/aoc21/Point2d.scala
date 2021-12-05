package aoc21

import cats.kernel.Order
import cats.Show
import cats.syntax.all.toContravariantOps

final case class Point2d(x: Int, y: Int):
  def +(other: Point2d) = Point2d(x + other.x, y + other.y)

given Order[Point2d] = Order.by(p => (p.x, p.y))
given Ordering[Point2d] = Ordering.by(p => (p.x, p.y))

given showPoint2dMap[A: Show]: Show[Map[Point2d, A]] =
  Show[Map[(Int, Int), A]]
    .contramap[Map[Point2d, A]](_.map((p, v) => (p.x, p.y) -> v))
