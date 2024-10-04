package dev.bergerg.minigames.scala2048

import dev.bergerg.minigames.scala2048.s2048.*

import scala.util.{Random, Left as ScalaLeft, Right as ScalaRight}

case class s2048(board: MATRIX):
  def makeMove(direction: Direction): Either[GameOver, s2048] = direction match
    case Up => collapseUp.generate
    case Down => collapseDown.generate
    case Left => collapseLeft.generate
    case Right => collapseRight.generate

  private def collapseLeft: s2048 = s2048(board.map(_.foldLeftOnlyEqNumbers))

  private def collapseUp: s2048 = s2048(board.transpose.map(_.foldLeftOnlyEqNumbers).transpose)

  private def collapseRight: s2048 = s2048(board.map(_.reverse.foldLeftOnlyEqNumbers.reverse))

  private def collapseDown: s2048 = s2048(board.transpose.map(_.reverse.foldLeftOnlyEqNumbers.reverse).transpose)

  private def generate: Either[GameOver, s2048] =
    board.randomizeEmpty.map {
      case (x, y) => ScalaRight(s2048(
        board.indices
          .map(i => board.indices
            .map(j => if (i == x && j == y) 2 else board(i)(j)))
      ))
    }.getOrElse(ScalaLeft(GameOver(board)))

object s2048:
  private type MATRIX = Seq[Seq[Int]]

  case class GameOver(finalBoard: MATRIX)

  sealed trait Direction

  case object Up extends Direction

  case object Right extends Direction

  case object Down extends Direction

  case object Left extends Direction

  private def apply(board: MATRIX) = new s2048(board)

  def init(): s2048 = new s2048(
    (0 to 3).map(_ => (0 to 3).map(_ => 0))
  ).makeMove(Up) match
    case ScalaRight(value) => value
    case _ => throw IllegalStateException()

  extension (board: MATRIX) def randomizeEmpty: Option[(Int, Int)] = {
    val boardSize = board.size
    val vacantTiles = for {
      i <- 0 until boardSize
      j <- 0 until boardSize
      zeroTile <- if (board(i)(j) == 0) Option((i, j)) else None
    } yield zeroTile

    Random().shuffle(vacantTiles).headOption
  }

  extension (intSeq: Seq[Int]) def foldLeftOnlyEqNumbers: Seq[Int] = {
    def recursive(l: Seq[Int]): Seq[Int] = l match
      case Nil => Nil
      case Seq(item) => Seq(item)
      case listHead :+ 0 :+ last => recursive(listHead :+ last)
      case listHead :+ beforeLast :+ 0 if beforeLast != 0 => recursive(listHead :+ beforeLast)
      case listHead :+ beforeLast :+ last if beforeLast == last && beforeLast != 0 => recursive(listHead :+ beforeLast + last)
      case listHead :+ beforeLast :+ last => recursive(listHead :+ beforeLast) :+ last
      case _ => throw IllegalStateException()

    recursive(intSeq).padTo(intSeq.size, 0)
  }
