package aoc21
package day21

import cats.effect.IO
import cats.syntax.all.*
import cats.data.State

case class DeterministicDie(
    current: Int = 1,
    max: Int = 100,
    rolls: Int = 0
):
  def roll =
    val next = current + 1
    DeterministicDie(
      if next > max then 1 else next,
      max,
      rolls + 1
    )

case class PlayerState(position: Int, score: Int = 0):
  def takeTurn(die: DeterministicDie) =
    val r1 = die.current
    val d1 = die.roll
    val r2 = d1.current
    val d2 = d1.roll
    val r3 = d2.current

    val newPosition = (r1 + r2 + r3 + position - 1) % 10 + 1

    (d2.roll, PlayerState(newPosition, score + newPosition))

  def takeQuantumTurn(roll: Int) =
    val newPosition = (roll + position - 1) % 10 + 1
    PlayerState(newPosition, score + newPosition)

case class GameState(
    die: DeterministicDie,
    p1: PlayerState,
    p2: PlayerState,
    won: Boolean = false
):
  def takeTurns =
    val (newDie, newP1) = p1.takeTurn(die)
    if newP1.score >= 1000 then GameState(newDie, newP1, p2, true)
    else
      val (newDie2, newP2) = p2.takeTurn(newDie)
      val won = newP2.score >= 1000
      GameState(newDie2, newP1, newP2, won)

case class DiracDie(sides: Int):
  def possibilities = (1 to sides).toList
  lazy val possibilities3 =
    val p = for
      a <- possibilities
      b <- possibilities
      c <- possibilities
    yield (a + b + c)
    p.groupMapReduce(identity)(_ => 1)(_ + _)

case class QuantumGameState(
    p1State: Map[PlayerState, Long],
    p2State: Map[PlayerState, Long],
    p1Turn: Boolean = true,
    p1Games: Long = 1,
    p2Games: Long = 1,
    p1Wins: Long = 0,
    p2Wins: Long = 0,
    turn: Int = 1
)

object Program extends PureDay:
  type A = (PlayerState, PlayerState)
  def parseLine(s: String) =
    s match
      case s"Player $p starting position: $i" => i.toIntIO.map(PlayerState(_))
      case l => IO.raiseError(new Exception(s"Unparsable player $l"))

  val takeTurn = State[GameState, Boolean](s =>
    val n = s.takeTurns
    (n, n.won)
  )

  def parse(input: List[String]): IO[A] =
    input match
      case List(p1, p2) => (parseLine(p1), parseLine(p2)).tupled
      case l => IO.raiseError(new Exception(s"Unparsable $l"))

  def part1(input: A): String =
    val end = takeTurn
      .iterateUntil(identity)
      .runS(GameState(DeterministicDie(), input._1, input._2))
      .value

    (end.die.rolls * (math.min(end.p1.score, end.p2.score))).toString

  def quantumTurn(die: DiracDie) =
    val combinations = die.possibilities3.map(_._2).sum
    State[QuantumGameState, Boolean](s =>
      val active = if s.p1Turn then s.p1State else s.p2State
      val passive = if s.p1Turn then s.p2State else s.p1State
      val games = if s.p1Turn then s.p1Games else s.p2Games
      val otherGames = if s.p1Turn then s.p2Games else s.p1Games

      val rolls = die.possibilities3
      val next = rolls
        .map((roll, rollCount) =>
          active
            .filter(_._1.score < 21)
            .map((player, scoreCount) =>
              val nextState = player.takeQuantumTurn(roll)

              nextState -> (scoreCount * rollCount)
            )
        )
        .reduce(_ |+| _)
      val wins = next.filter(_._1.score >= 21).map(_._2.toLong).sum
      val openGames = (games * combinations - wins)
      val nextState =
        if s.p1Turn then
          s.copy(
            next,
            s.p2State,
            false,
            p1Games = openGames,
            p1Wins = s.p1Wins + wins * otherGames
          )
        else
          s.copy(
            s.p1State,
            next,
            true,
            p2Games = openGames,
            p2Wins = s.p2Wins + wins * otherGames
          )

      (nextState, next.isEmpty)
    )
  end quantumTurn
  def part2(input: A): String =
    val die = DiracDie(3)
    val turn = quantumTurn(die)
    val game = QuantumGameState(
      Map(input._1 -> 1),
      Map(input._2 -> 1)
    )

    val s =
      turn
        .iterateUntil(identity)
        .runS(game)
        .value
    math.max(s.p1Wins, s.p2Wins).toString
end Program
