import os.read

import scala.collection.mutable.ListBuffer
import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex.Match
object Day06 extends AocDay {

  def main(args: Array[String]): Unit = {
    // println(deltas)
    run()
  }

  case class Position(x: Int, y: Int) {
    def +(d: Direction) = Position(x + d.x, y + d.y)
  }
  sealed trait Direction(val x: Int, val y: Int) {
    def turnRight: Direction = this match {
      case Up    => Right
      case Right => Down
      case Down  => Left
      case Left  => Up
    }
  }
  case object Up extends Direction(-1, 0)
  case object Right extends Direction(0, 1)
  case object Down extends Direction(1, 0)
  case object Left extends Direction(0, -1)

  case class Board(rep: Array[String]) {
    val rows = rep.size
    val cols = rep.head.size
    def isInBounds(p: Position): Boolean =
      0 <= p.x && p.x < rows &&
        0 <= p.y && p.y < cols
    def get(p: Position): Option[Char] =
      if (isInBounds(p))
        Some(rep(p.x)(p.y))
      else
        None

    def apply(p: Position): Char =
      rep(p.x)(p.y)
  }
  override def part1(input: String): String = {
    val board = Board(input.split("\n"))
    val startingDir: Direction = Up
    val startingPos: Position = (for (
      row <- board.rep.indices;
      col <- board.rep.head.indices;
      pos = Position(row, col)
      if board(pos) == '^'
    ) yield Position(row, col)).ensuring(_.size == 1).head

    explore(
      startingPos,
      startingDir,
      board
    ).size.toString
  }

  def explore(
      p: Position,
      d: Direction,
      board: Board
  ): Map[Position, Boolean] = {
    import scala.collection.mutable
    val visited: mutable.Map[Position, Boolean] = mutable.Map.empty
    var pos = p
    var dir = d
    while (board.isInBounds(pos)) {
      visited.put(pos, true)
      val nextPos = pos + dir

      if (board.get(nextPos) == Some('#'))
        dir = dir.turnRight
      else
        pos = pos + dir
    }
    visited.toMap
  }

  override def part2(input: String): String = {
    val board = Board(input.split("\n"))
    val startingDir: Direction = Up
    val startingPos: Position = (for (
      row <- board.rep.indices;
      col <- board.rep.head.indices;
      pos = Position(row, col)
      if board(pos) == '^'
    ) yield Position(row, col)).ensuring(_.size == 1).head

    val candidatePositions = for (
      row <- board.rep.indices;
      col <- board.rep.head.indices;
      pos = Position(row, col);
      if (board(pos) != '#' && pos != startingPos)
    ) yield pos
    candidatePositions
      .count(blockPos => {
        isInfinite(
          startingPos,
          startingDir,
          blockPos,
          board
        )
      })
      .toString
  }

  def isInfinite(
      p: Position,
      d: Direction,
      blockPos: Position,
      board: Board
  ): Boolean = {
    val totalPositions = board.cols * board.rows
    import scala.collection.mutable
    val visited: Map[Direction, mutable.BitSet] = Map(
      Up -> new mutable.BitSet(),
      Down -> new mutable.BitSet(),
      Left -> new mutable.BitSet(),
      Right -> new mutable.BitSet()
    )
    def posToIndex(p: Position): Int = p.x * board.cols + p.y
    var pos = p
    var dir = d
    while (board.isInBounds(pos)) {
      if (visited(dir).contains(posToIndex(pos)))
        return true
      visited(dir).add(posToIndex(pos))
      val nextPos = pos + dir

      if (nextPos == blockPos || board.get(nextPos) == Some('#')) {
        dir = dir.turnRight
      } else
        pos = pos + dir
    }
    false
  }

}
