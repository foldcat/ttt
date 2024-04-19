package org.maidagency

import cats.effect.{IO, IOApp, Resource}
import cats.effect.ExitCode
import cats.syntax.all._
import cats.data.State
import java.io._
import scala.concurrent.duration._

enum CellState:
  case X, O, E

object Check:
  import CellState._

  def hasWinner(board: Vector[Vector[CellState]]): Boolean =
    board.exists(_.forall(_ == X)) || board.exists(_.forall(_ == O))
    (0 until board(0).length).exists(col =>
      board.forall(row => row(col) == X)
    ) ||
    (0 until board(0).length)
      .exists(col => board.forall(row => row(col) == O))
    Vector(
      (board(0)(0), board(1)(1), board(2)(2)),
      (board(0)(2), board(1)(1), board(2)(0))
    ).exists(diagonal => diagonal.forall(_ == X)) ||
    Vector(
      (board(0)(0), board(1)(1), board(2)(2)),
      (board(0)(2), board(1)(1), board(2)(0))
    ).exists(diagonal => diagonal.forall(_ == O))

object Main extends IOApp.Simple:

  val defaultSetup =
    import CellState._
    Vector(
      Vector(E, E, E),
      Vector(E, E, E),
      Vector(E, E, E)
    )

  opaque type Cell = Vector[Vector[CellState]]

  case class Target(x: Int, y: Int, swap: CellState)

  def formatTable(target: Cell): String =
    target.zipWithIndex
      .map { case (inside, index) =>
        (index + 1) + " " + inside.mkString(" ")
      }
      .mkString("\n")
      + "\n  1 2 3 "
      + "\n---"

  def mutateSetup(tree: Cell, target: Target): Cell =
    tree.updated(target.x, tree(target.x).updated(target.y, target.swap))

  extension (s: String)
    def parseState(): CellState =
      import CellState._
      s match
        case "X" => X
        case "O" => O

  extension (s: Cell)
    def isFinished(): Boolean =
      Check.hasWinner(s)

  def parseUserInput(in: String): Target =
    val splitted = in.split(" ")
    Target(
      splitted(0).toInt - 1,
      splitted(1).toInt - 1,
      splitted(2).parseState()
    )

  def gameLogic(current: Cell, action: Target): Cell =
    mutateSetup(current, action)

  def gameLoop(currentState: Cell): IO[Unit] =
    IO.readLine
      .map(line => parseUserInput(line))
      .map(action => gameLogic(currentState, action))
      .flatTap(newState => IO.println("---\n" + formatTable(newState)))
      .flatMap(newState =>
        if newState.isFinished() then IO.println("done")
        else gameLoop(newState)
      )

  def runGame(): IO[Unit] =
    IO.println("start\n") >>
      IO.println(formatTable(defaultSetup) + "\n") >>
      gameLoop(defaultSetup) >>
      IO.println("done\n")

  override final val run: IO[Unit] =
    runGame().foreverM
