package aoc21
package day24

import cats.effect.IO
import cats.syntax.all.*
import cats.data.State
import cats.Show

type Value = Variable | Int

object Value:
  def parse(s: String): IO[Value] =
    Variable.parse(s).recoverWith(_ => s.toIntIO)

enum Variable:
  case W
  case X
  case Y
  case Z

object Variable:
  def parse(s: String): IO[Variable] =
    s match
      case "w" => W.pure
      case "x" => X.pure
      case "y" => Y.pure
      case "z" => Z.pure
      case s => IO.raiseError(new Exception(s"Unparsed variable: $s"))

enum Instruction:
  case Input(variable: Variable)
  case Add(variable: Variable, value: Value)
  case Multiply(variable: Variable, value: Value)
  case Divide(variable: Variable, value: Value)
  case Modulo(variable: Variable, value: Value)
  case Equals(variable: Variable, value: Value)

object Instruction:
  def parse(s: String): IO[Instruction] =
    s match
      case s"inp $a" => Variable.parse(a).map(Instruction.Input.apply)
      case s"add $a $b" =>
        (Variable.parse(a), Value.parse(b)).mapN(Instruction.Add.apply)
      case s"mul $a $b" =>
        (Variable.parse(a), Value.parse(b)).mapN(Instruction.Multiply.apply)
      case s"div $a $b" =>
        (Variable.parse(a), Value.parse(b)).mapN(Instruction.Divide.apply)
      case s"mod $a $b" =>
        (Variable.parse(a), Value.parse(b)).mapN(Instruction.Modulo.apply)
      case s"eql $a $b" =>
        (Variable.parse(a), Value.parse(b)).mapN(Instruction.Equals.apply)
      case l => IO.raiseError(new Exception(s"unmatched instruction: $l"))

given Show[Instruction] = {
  case Instruction.Input(v) => s"input $v"
  case Instruction.Add(a, b) => s"$a = $a + $b"
  case Instruction.Multiply(a, b) =>
    s"$a = $a * $b"
  case Instruction.Divide(a, b) =>
    s"$a = $a / $b"
  case Instruction.Modulo(a, b) => s"$a = $a % $b"
  case Instruction.Equals(a, b) => s"$a = $a == $b"
}

object Program extends PureDay:

  type A = List[Instruction]
  def parse(input: List[String]): IO[A] =
    input.traverse(Instruction.parse)

  def monad(w: Int, z: Int, addX: Int, addW: Int, divideBy: Int): Int =
    val mod = z % 26
    val newZ = z / divideBy
    if mod + addX == w then newZ
    else newZ * 26 + w + addW

  def makeSolver(input: A) =
    val monads = input.split(Instruction.Input(Variable.W)).filterNot(_.isEmpty)

    val monadsF = monads.map(m =>
      val addX = m.collect { case Instruction.Add(Variable.X, v: Int) =>
        v
      }.head
      val addW = m.collect { case Instruction.Add(Variable.Y, v: Int) =>
        v
      }.last
      val divideBy = m.collect { case Instruction.Divide(Variable.Z, v: Int) =>
        v
      }.head
      (addX, addW, divideBy)
    )

    (free: List[Int]) =>
      monadsF.foldLeft((0, List.empty[Int], free)) {
        case ((z, n, free), (addX, addW, divideBy)) =>
          val isFree = divideBy == 1
          val w =
            if isFree then free.head
            else math.max(1, math.min(9, z % 26 + addX))
          (
            monad(w, z, addX, addW, divideBy),
            w :: n,
            if isFree then free.tail else free
          )
      }
  end makeSolver
  def part1(input: A): String =
    val trySolve = makeSolver(input)
    val digits = (9.to(1, -1))
    val frees = for
      a <- digits.iterator
      b <- digits.iterator
      c <- digits.iterator
      d <- digits.iterator
      e <- digits.iterator
      f <- digits.iterator
      g <- digits.iterator
    yield List(a, b, c, d, e, f, g)

    val solution = frees.find(l => trySolve(l)._1 == 0)

    solution.map(trySolve(_)._2.reverse.mkString).getOrElse("unsolved")
  end part1
  def part2(input: A): String =
    val trySolve = makeSolver(input)

    val digits = (1 to 9)

    val frees = for
      a <- digits.iterator
      b <- digits.iterator
      c <- digits.iterator
      d <- digits.iterator
      e <- digits.iterator
      f <- digits.iterator
      g <- digits.iterator
    yield List(a, b, c, d, e, f, g)

    val solution = frees.find(l => trySolve(l)._1 == 0)

    solution.map(trySolve(_)._2.reverse.mkString).getOrElse("unsolved")
  end part2

end Program
