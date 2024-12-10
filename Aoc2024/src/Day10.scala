import os.read

import scala.annotation.tailrec
import scala.annotation.varargs
import scala.collection.mutable.ListBuffer
import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex.Match
object Day10 extends AocDay {

  def main(args: Array[String]): Unit = {
    run()
//     println(part2("""89010123
// 78121874
// 87430965
// 96549874
// 45678903
// 32019012
// 01329801
// 10456732"""))
  }
  def part1(input: String): String = {
    type Pos = (Int, Int)
    type Board = Array[Array[Int]]
    extension (board: Board) {
      def apply(p: Pos): Int = board(p._1)(p._2)
    }
    val board: Board = input.split("\n").map(_.toCharArray().map(_.asDigit))
    val rows = board.length
    val cols = board.head.length
    extension (p: Pos) {
      def +(op: Pos): Pos =
        (p._1 + op._1, p._2 + op._2)
      def isInBounds: Boolean =
        0 <= p._1 && p._1 < rows &&
          0 <= p._2 && p._2 < cols
    }
    val scores = for (
      r <- board.indices;
      c <- board.head.indices;
      pos = (r, c)
      if board(pos) == 0
    ) yield {
      @tailrec
      def getScore(positions: Set[Pos], level: Int): Int = {
        if (level == 9)
          positions.size
        else {
          val nextLevelPositions = for (
            p <- positions;
            direction <- List(
              (0, 1),
              (0, -1),
              (1, 0),
              (-1, 0)
            );
            positionToCheck = p + direction;
            if positionToCheck.isInBounds;
            if board(positionToCheck) == level + 1
          ) yield positionToCheck
          getScore(nextLevelPositions, level + 1)
        }
      }
      getScore(Set(pos), 0)
    }
    scores.sum.toString
  }

  def part2(input: String): String = {
    type Board = Array[Array[Int]]
    extension (board: Board) {
      def apply(p: Pos): Int = board(p._1)(p._2)
    }
    val board: Board = input.split("\n").map(_.toCharArray().map(_.asDigit))
    val rows = board.length
    val cols = board.head.length
    val positions = for (
      r <- board.indices;
      c <- board.head.indices
    ) yield (r,c)
    type Pos = (Int, Int)
    extension (p: Pos) {
      def +(op: Pos): Pos =
        (p._1 + op._1, p._2 + op._2)
      def isInBounds: Boolean =
        0 <= p._1 && p._1 < rows &&
          0 <= p._2 && p._2 < cols
      def r: Int = p._1
      def c: Int = p._2
    }
    def getRatingGrid(level: Int = 9): Array[Array[Int]] = {
      import scala.collection.mutable.ArrayBuffer
      extension (ab: Array[ArrayBuffer[Int]]) {
        def update(p: Pos, v: Int): Unit = 
          ab(p._1).update(p._2, v)
      }
      val ratings: Array[ArrayBuffer[Int]] =
        Array.fill(rows)(ArrayBuffer.fill(cols)(0))
      for (
        pos <- positions
        if board(pos) == 9
      ) {
        ratings.update(pos, 1)
      }
      for (
        level <- (0 to 8).reverse;
        pos <- positions;
        if board(pos) == level
      ) {
        val nextLevelUpRatings = for (
          direction <- List(
            (0, 1),
            (0, -1),
            (1, 0),
            (-1, 0)
          );
          positionToCheck = pos + direction;
          if positionToCheck.isInBounds
          if board(positionToCheck) == level + 1
        ) yield ratings(positionToCheck._1)(positionToCheck._2)
        ratings.update(pos, nextLevelUpRatings.sum)
      }
      ratings.map(_.toArray)
    }
    val ratings = getRatingGrid()
    // ratings.map(_.map(r => f"$r%2d").mkString(" | ")).mkString("\n").pipe(println)
    (for (
      pos <- positions;
      if board(pos) == 0
    ) yield {
      // println(s"Rating at $r $c is ${ratings(r)(c)}")
      ratings(pos._1)(pos._2)
    }).sum.toString
  }

}
