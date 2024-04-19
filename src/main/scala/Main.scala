package org.maidagency

import cats.effect.{IO, IOApp, Resource}
import cats.effect.ExitCode
import cats.syntax.all._
import cats.data.State
import java.io._
import scala.concurrent.duration._

//
// // Parses the user input into a game action.
// def parseUserInput(line: String): Action
//
// // Produces the new state given the user action.
// def gameLogic(currentState: State, action: Action): State
//
// // Formats the state into a printable format.
// def formatState(state: State): List[String]
//
// def gameLoop(currentState: State): IO[Unit] =
//   IO
//     .readLine
//     .map(line => parseUserInput(line))
//     .map(action => gameLogic(currentState, action))
//     .flatTap(newState => formatState(state).traverse_(IO.println))
//     .flatMap { newState =>
//       if newState.isFinished then
//         IO.println("Game finished!")
//       else
//         gameLoop(newState)
//     }
//
// val runGame: IO[Unit] =
//   IO.println("Tic-Tac-Toe Game start") >>
//   gameLoop(State.initial) >>
//   IO.println("Play again?") >>
//   IO.readLine.flatMap {
//      case "yes" => runGame
//      case "no" => IO.unit
//      case wrongInput => IO.raiseError(IllegalInput(wrongInput)) // Or just ask again, whatever.
//   }
//
// object Main extends IOApp.Simple:
//   override final val run: IO[Unit] =
//     runGame.forerverM
//

object Main extends IOApp.Simple:
  enum CellState:
    case X, O, E

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
    target
      .map(_.mkString(" "))
      .mkString("\n")
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
      false

  def parseUserInput(in: String): Target =
    val splitted = in.split(" ")
    Target(
      splitted(0).toInt,
      splitted(1).toInt,
      splitted(2).parseState()
    )

  def gameLogic(current: Cell, action: Target): Cell =
    mutateSetup(current, action)

  def gameLoop(currentState: Cell): IO[Unit] =
    IO.readLine
      .map(line => parseUserInput(line))
      .map(action => gameLogic(currentState, action))
      .flatTap(newState => 
          IO.println("---\n" + formatTable(newState)))
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
