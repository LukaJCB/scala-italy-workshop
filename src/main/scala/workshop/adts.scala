package workshop

import java.time.Instant

object adts {

  // Design a data type for coffee sizes, should have small medium and large
  sealed trait Size
  case object Small extends Size
  case object Medium extends Size
  case object Large extends Size

  // Model a data type for a contact that can either be an email or a phone number
  type EMail = String
  type PhoneNumber = Int
  type Contact = Either[EMail, PhoneNumber]

  // Design a data type for a chess piece and its position on the chess board
  sealed trait ChessPieceType
  case object King extends ChessPieceType
  case object Queen extends ChessPieceType
  case object Bishop extends ChessPieceType
  case object Knight extends ChessPieceType
  case object Rook extends ChessPieceType
  case object Pawn extends ChessPieceType

  type Color = Int
  case class ChessPiecePosition(x: Int, y: Int)


  case class ChessPiece(currentPos: ChessPiecePosition, color: Color, chessPieceType: ChessPieceType) {
    private def distance(target: ChessPiecePosition) = {
      val yy = Math.abs(target.y - currentPos.y)
      val xx = Math.abs(target.x - currentPos.x)

      (yy, xx)
    }

    private def canMoveDiagonally(target: ChessPiecePosition) =
      Math.abs(currentPos.y + target.y) == Math.abs(currentPos.x + currentPos.y)

    private def canMoveHorizontally(target: ChessPiecePosition) =
      target.y == currentPos.y
    private def canMoveVertically(target: ChessPiecePosition) =
      target.x == currentPos.x

    private def canJump(target: ChessPiecePosition) = {
      val yDistance = Math.abs(target.y - currentPos.y)
      val xDistance = Math.abs(target.x - currentPos.x)

      yDistance == 2 && xDistance == 1 || yDistance == 1 && yDistance == 2
    }

    def move(target: ChessPiecePosition) = chessPieceType match {
      case King if distance(currentPos)._1 == 1 && distance(currentPos)._2 == 0 || distance(currentPos)._1 == 0 && distance(currentPos)._2 == 1 => Some(target) // fixme
      case Queen if canMoveDiagonally(target) || canMoveVertically(target) || canMoveHorizontally(target) => Some(target)
      case Bishop if canMoveVertically(target) => Some(target)
      case Knight if canJump(target) => Some(target)
      case Rook if canMoveVertically(target) || canMoveHorizontally(target) => Some(target)
      case Pawn if target.x == currentPos.x && target.y == currentPos.y + 1 => Some(target)
      case _ => None
    }
  }

  // Model a data type that stores time spans
  sealed trait TimeUnit {
    val value: Int
  }

  case class Hours(value: Int = 3) extends TimeUnit
  case class Minutes(value: Int = 2) extends TimeUnit
  case class Seconds(value: Int = 1) extends TimeUnit

  def convert(t1: TimeUnit, t2: TimeUnit) = if (t1.value > t2.value) t2 else t1


  case class TimeSpan(denominator: Int, timeUnit: TimeUnit) {
    val value: Seconds = ???

    def add (newTimeSpan: TimeSpan) = ???
  }

  // Write a function that adds two TimeSpan values together

  // List all values of the type `Unit`
  def allValuesUnit: Set[Unit] = Set( () )

  // List all values of the type `Nothing`
  def allValuesNothing: Set[Nothing] = Set.empty

  // List all values of the type `Boolean`
  def allValuesBoolean: Set[Boolean] = Set(true, false)

  // List all values of the type `Size`
  def allValuesSize: Set[Size] = Set(Small, Medium, Large)

  // List all values of the type `(Size, Boolean)`
  def allValuesTuple: Set[(Size, Boolean)] = Set(
    (Small, true),
    (Medium, true),
    (Large, true),
    (Small, false),
    (Medium, false),
    (Large, false)
  )

  // List all values of the type `Either[Size, Boolean]`
  def allValuesEither: Set[Either[Size, Boolean]] = Set(
    Left(Small),
    Left(Medium),
    Left(Large),
    Right(false),
    Right(true)
  )

  // List all values of the type `(Size, Unit)`
  def allValuesTupleUnit: Set[(Size, Unit)] = Set(
    (Small, ()),
    (Medium, ()),
    (Large, ())
  )

  // List all values of the type `Either[Boolean, Nothing]`
  def allValuesEitherNothing: Set[Either[Boolean, Nothing]] = Set(
    Left(true),
    Left(false)
  )

}
