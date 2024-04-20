package org.maidagency

import cats.data.State
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Resource
import cats.syntax.all.*
import scala.concurrent.duration.*

enum CellState:
  case X, O, E

object Check:
  import CellState.*

  def hasWinner(board: Vector[Vector[CellState]]): Boolean =
    getWinner(board).isDefined

  def getWinner(board: Vector[Vector[CellState]]): Option[CellState] =
    val diagonal = (0 until board.length).map(offset => (offset, offset))
    val retrogradeDiagonal =
      (0 until board.length).map(offset => (offset, board.length - offset - 1))
    val horizontal = (0 until board.length).map(y =>
      (0 until board(y).length).map(x => (y, x))
    )
    val vertical = (0 until board.length).map(y =>
      (0 until board(y).length).map(x => (x, y))
    )
    val lines =
      Vector.empty :+ diagonal :+ retrogradeDiagonal :++ horizontal :++ vertical
    val asStates    = lines.map(vector => vector.map((x, y) => board(x)(y)))
    val asStateSets = asStates.map(_.distinct)
    val asSingles   = asStateSets.filter(_.length == 1).map(_(0))
    val winner: Option[CellState] = asSingles.find(_ != E)
    winner

object Main extends IOApp.Simple:

  val defaultSetup =
    import CellState.*
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
      import CellState.*
      s match
        case "X" => X
        case "O" => O

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
        Check.getWinner(newState) match
          case Some(x) => IO.println(s"$x won \n---")
          case None    => gameLoop(newState)
      )

  def runGame(): IO[Unit] =
    IO.println("---\nstart\n---\n") >>
      IO.println(formatTable(defaultSetup) + "\n") >>
      gameLoop(defaultSetup) >>
      IO.println("\n---\ndone\n---\n")

  final override val run: IO[Unit] =
    runGame().foreverM
end Main
