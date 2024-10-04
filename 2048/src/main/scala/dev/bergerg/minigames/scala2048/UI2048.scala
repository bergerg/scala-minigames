package dev.bergerg.minigames.scala2048

import dev.bergerg.minigames.scala2048.s2048.{Direction, GameOver}

import scala.annotation.tailrec
import scala.util.{Left as ScalaLeft, Right as ScalaRight}

trait UI2048:
  def print(game: s2048): Unit

  def print(gameOver: GameOver): Unit

  def getNextMove: Direction

  @tailrec
  final def gameLoop(game: s2048): Unit =
    game.makeMove(getNextMove) match
      case ScalaRight(value) =>
        print(value)
        gameLoop(value)
      case ScalaLeft(value) =>
        print(value)
