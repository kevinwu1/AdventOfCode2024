import os.read

import scala.annotation.tailrec
import scala.annotation.varargs
import scala.collection.mutable.ListBuffer
import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex.Match
object Day12 extends AocDay {

  def main(args: Array[String]): Unit = {
    run()
//     part2("""AAAAAA
// AAABBA
// AAABBA
// ABBAAA
// ABBAAA
// AAAAAA""").pipe(println)
  }
  def part1(input: String): String = {
    type Pos = (Int, Int)
    val board: Array[Array[Char]] = input.split("\n").map(_.toCharArray())
    val rows = board.length
    val cols = board.head.length
    extension (b: Array[Array[Char]]) {
      def apply(p: Pos): Char = b(p._1)(p._2)
    }
    extension (p: Pos) {
      def +(op: Pos): Pos = (p._1 + op._1, p._2 + op._2)
      def isInBounds: Boolean =
        0 <= p._1 && p._1 < rows &&
          0 <= p._2 && p._2 < cols
    }
    val allPos = for (r <- board.indices; c <- board.head.indices) yield (r, c)

    case class Region(rep: Char, positions: Set[Pos])

    def drawPos(positions: Set[Pos], rep: Char): String = {
      (for (
        r <- board.indices;
        c <- board.head.indices
      ) yield {
        val char = if (positions.contains((r,c))) rep.toString else "."
        val nl = if (c == board.head.size - 1) "\n" else ""
        char + nl
      }).mkString
    }
    
    val regions: List[Region] = {
      @tailrec
      def explore(
          positions: Set[Pos],
          rep: Char,
          explored: Set[Pos]
      ): Region = {
        if (positions.isEmpty)
          Region(rep, explored)
        else {
          // println("_" * cols)
          // println(drawPos(explored))
          // println("_" * cols)
          val newExplored = explored ++ positions
          val horizon: Set[Pos] = for (
            position <- positions;
            direction <- List(
              (-1, 0),
              (1, 0),
              (0, -1),
              (0, 1)
            );
            horizonPos = position + direction;
            if horizonPos.isInBounds
            if !explored.contains(horizonPos)
            if board(horizonPos) == rep
          ) yield horizonPos
  
          explore(horizon, rep, newExplored)
        }
      }
      val regions: List[Region] = allPos
        .foldLeft(
          (Set[Pos](), List[Region]())
        ) { case ((previouslyFoundPositions, knownRegions), pos) =>
          if (previouslyFoundPositions.contains(pos))
            (previouslyFoundPositions, knownRegions)
          else {
            val newRegion = explore(Set(pos), board(pos), Set())
            (
              previouslyFoundPositions ++ newRegion.positions,
              newRegion :: knownRegions
            )
          }
        }
        ._2
      regions
    }

    val posToRegion: Map[Pos, Region] = 
      regions.flatMap {
        case r @ Region(_, positions) => 
          positions.map(_ -> r)
      }.toMap
    val regionToPerimeter: Map[Region, Int] = {
      val leftRightEdges = (for (
        r <- 0 until rows
      ) yield List(posToRegion((r, 0)), posToRegion((r, cols - 1)))).flatten
      val topBotEdges = (for (
        c <- 0 until cols
      ) yield List(posToRegion((0, c)), posToRegion((rows - 1, c)))).flatten
      val downAndRights = for (
        pos <- allPos;
        direction <- List((1, 0), (0, 1));
        if (pos + direction).isInBounds
        if board(pos) != board(pos + direction)
      ) yield List(
        (posToRegion(pos)),
        (posToRegion(pos + direction)),
      )

      (downAndRights.flatten ++ leftRightEdges ++ topBotEdges).groupBy(identity).mapValues(_.size).toMap
    }
    (for (
      region <- regions;
      perim = regionToPerimeter(region);
      area = region.positions.size
    ) yield perim * area).sum.toString
  }

  def part2(input: String): String = { 
    type Pos = (Int, Int)
    val board: Array[Array[Char]] = input.split("\n").map(_.toCharArray())
    val rows = board.length
    val cols = board.head.length
    extension (b: Array[Array[Char]]) {
      def apply(p: Pos): Char = b(p._1)(p._2)
    }
    extension (p: Pos) {
      def +(op: Pos): Pos = (p._1 + op._1, p._2 + op._2)
      def r: Int = p._1
      def c: Int = p._2
      def isInBounds: Boolean =
        0 <= p._1 && p._1 < rows &&
          0 <= p._2 && p._2 < cols
    }
    val allPos = for (r <- board.indices; c <- board.head.indices) yield (r, c)

    case class Region(rep: Char, positions: Set[Pos])

    def drawPos(positions: Set[Pos], rep: Char): String = {
      (for (
        r <- board.indices;
        c <- board.head.indices
      ) yield {
        val char = if (positions.contains((r,c))) rep.toString else "."
        val nl = if (c == board.head.size - 1) "\n" else ""
        char + nl
      }).mkString
    }
    
    val regions: List[Region] = {
      @tailrec
      def explore(
          positions: Set[Pos],
          rep: Char,
          explored: Set[Pos]
      ): Region = {
        if (positions.isEmpty)
          Region(rep, explored)
        else {
          // println("_" * cols)
          // println(drawPos(explored))
          // println("_" * cols)
          val newExplored = explored ++ positions
          val horizon: Set[Pos] = for (
            position <- positions;
            direction <- List(
              (-1, 0),
              (1, 0),
              (0, -1),
              (0, 1)
            );
            horizonPos = position + direction;
            if horizonPos.isInBounds
            if !explored.contains(horizonPos)
            if board(horizonPos) == rep
          ) yield horizonPos
  
          explore(horizon, rep, newExplored)
        }
      }
      val regions: List[Region] = allPos
        .foldLeft(
          (Set[Pos](), List[Region]())
        ) { case ((previouslyFoundPositions, knownRegions), pos) =>
          if (previouslyFoundPositions.contains(pos))
            (previouslyFoundPositions, knownRegions)
          else {
            val newRegion = explore(Set(pos), board(pos), Set())
            (
              previouslyFoundPositions ++ newRegion.positions,
              newRegion :: knownRegions
            )
          }
        }
        ._2
      regions
    }

    val posToRegion: Map[Pos, Region] = 
      regions.flatMap {
        case r @ Region(_, positions) => 
          positions.map(_ -> r)
      }.toMap


    def getSides(region: Region): Int = {
      // println(region)
      case class Side(pos: Pos, direction: Pos)
      val rawSides = for (
        pos <- region.positions;
        direction <- List(
          (-1, 0),
          (1, 0),
          (0, -1),
          (0, 1)
        );
        neighborPos = pos + direction;
        neighbor = if (neighborPos.isInBounds) Some(board(neighborPos)) else None;
        if (neighbor == None || neighbor.get != board(pos)) //outer edge, or neighboring plot is different
      ) yield Side(pos, direction)

      val (verticalSides, horizontalSides) = rawSides.partition(_.direction._1 == 0)
      // println("V: " + verticalSides)
      // println("H: " + horizontalSides)
      val numVerticalSides = verticalSides.groupBy(s => (s.pos.c, s.direction)).values.map(sameColSides => {
        sameColSides.map(_.pos.r).toVector.sorted //sort by row
        .foldLeft((0, -2)) {
          case ((total, prevRow), row) =>
            if (prevRow + 1 == row)
              (total, row)
            else
              (total + 1, row)
        }._1
      }).sum
      val numHorizonalSides = horizontalSides.groupBy(s => (s.pos.r, s.direction)).values.map(sameRowSides => {
        sameRowSides.map(_.pos.c).toVector.sorted //sort by row
        .foldLeft((0, -2)) {
          case ((total, prevCol), col) =>
            if (prevCol + 1 == col)
              (total, col)
            else
              (total + 1, col)
        }._1
      }).sum
      val sides = numVerticalSides + numHorizonalSides
      // println(s"$region has $sides sides")
      sides
    }

    (for (
      region <- regions;
      sides = getSides(region);
      area = region.positions.size
    ) yield sides * area).sum.toString
  }

}
