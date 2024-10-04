package dev.bergerg.minigames.scala2048

import dev.bergerg.minigames.scala2048.s2048.{Direction, GameOver}

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Console2048 extends UI2048 with App {
  private val cellSize = 4
  private val vertical = "|"

  private def horizontal(boardSize: Int) = s"\n${"-" * (boardSize * 5 + 1)}\n"

  override def print(game: s2048): Unit = {
    val boardSize = game.board.size
    val horizontal = Console2048.horizontal(boardSize)
    println(
      (0 until boardSize)
        .map(i => {
          (0 until boardSize).map(j => {
            game.board(i)(j) match
              case 0 => " " * cellSize
              case value => s"%-${cellSize}s".format(value)
          }).mkString(vertical, vertical, vertical)
        })
        .mkString(horizontal, horizontal, horizontal)
    )
  }

  override def print(gameOver: GameOver): Unit = {
    println("GAME OVER!")
    println("No more moves to make.")
  }

  @tailrec
  override def getNextMove: Direction =
    readLine("make a move (U|D|L|R): ") match
      case "u" | "U" => s2048.Up
      case "d" | "D" => s2048.Down
      case "l" | "L" => s2048.Left
      case "r" | "R" => s2048.Right
      case _ => getNextMove

  gameLoop(s2048.init())

}
