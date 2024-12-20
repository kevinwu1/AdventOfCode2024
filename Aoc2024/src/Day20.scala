import os.read

import scala.annotation.tailrec
import scala.annotation.varargs
import scala.collection.IndexedSeqView
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue
import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex.Match
object Day20 extends AocDay {

  def main(args: Array[String]): Unit = {
    run()
//     part2("""###############
// #...#...#.....#
// #.#.#.#.#.###.#
// #S#...#.#.#...#
// #######.#.#.###
// #######.#.#...#
// #######.#.###.#
// ###..E#...#...#
// ###.#######.###
// #...###...#...#
// #.#####.#.###.#
// #.#...#.#.#...#
// #.#.#.#.#.#.###
// #...#...#...###
// ###############""")
  }
  def part1(input: String): String = {

    val start = 'S'
    val end = 'E'
    val space = '.'
    val wall = '#'

    val board: Array[Array[Char]] = input
      .split("\n")
      .map(_.toCharArray())
    val hei = board.length
    val wid = board.head.length
    type Pos = (Int, Int)
    extension (p: Pos) {
      def r: Int = p._1
      def c: Int = p._2
      def +(o: Pos): Pos = (p._1 + o._1, p._2 + o._2)
      def isInBounds: Boolean =
        0 <= p._1 && p._1 < wid &&
          0 <= p._2 && p._2 < hei
      def dist(o: Pos): Int = 
        Math.abs(o.r - p.r) + Math.abs(o.c - p.c) 
    }

    extension (b: Array[Array[Char]]) {
      def apply(p: Pos): Char = b(p.r)(p.c)
      def prettyString: String = b.map(_.mkString).mkString("\n")
    }

    val startPos = board
      .map(_.indexOf(start))
      .zipWithIndex
      .collect {
        case (c, r) if c >= 0 => (r, c)
      }
      .head
    val endPos = board
      .map(_.indexOf(end))
      .zipWithIndex
      .collect {
        case (c, r) if c >= 0 => (r, c)
      }
      .head


    def getDistances(b: Array[Array[Char]], startPos: Pos): Map[Pos, Int] = {
      import scala.collection.mutable
      val pq: PriorityQueue[(Pos, Int)] = new PriorityQueue[(Pos, Int)]()(
        Ordering.fromLessThan[(Pos, Int)]((a, b) => a._2 < b._2).reverse
      )
      pq += ((startPos, 0))
      val costs: mutable.Map[Pos, Int] = mutable.Map()
      while (pq.nonEmpty) {
        val (item, cost) = pq.dequeue()
        if (!costs.contains(item)) {
          costs(item) = cost
          for (
            dir <- List(
              (0, 1),
              (0, -1),
              (-1, 0),
              (1, 0)
            );
            newPos= item + dir;
            if (newPos.isInBounds && !costs.contains(newPos))
          ) {
            if (
              b(newPos) != wall 
            ) {
              val newCost = cost + 1
              pq += ((newPos, newCost))
            }
          }
        }
      }
      costs.toMap
    }
    val startToPoint: Map[Pos, Int] = getDistances(board, startPos)
    val endToPoint: Map[Pos, Int] = getDistances(board, endPos)

    val maxDist = 2
    val costs = for (
      r <- board.indices;
      c <- board.head.indices;
      pos = (r, c);
      xd <- (-maxDist to maxDist);
      ymax = maxDist - Math.abs(xd);
      yd <- (-ymax to ymax);
      pos2 = pos + (xd, yd);
      dst = pos.dist(pos2);
      if (board(pos) != wall)
      if (pos2.isInBounds && board(pos2) != wall)
    ) yield startToPoint(pos) + dst + endToPoint(pos2) 


    val baseCost = startToPoint(endPos)
    costs
      .view
      .map(baseCost - _)
      .filter(_ >= 100)
      .size.toString
  }

  def part2(input: String): String = {

    val start = 'S'
    val end = 'E'
    val space = '.'
    val wall = '#'

    val board: Array[Array[Char]] = input
      .split("\n")
      .map(_.toCharArray())
    val hei = board.length
    val wid = board.head.length
    type Pos = (Int, Int)
    extension (p: Pos) {
      def r: Int = p._1
      def c: Int = p._2
      def +(o: Pos): Pos = (p._1 + o._1, p._2 + o._2)
      def isInBounds: Boolean =
        0 <= p._1 && p._1 < wid &&
          0 <= p._2 && p._2 < hei
      def dist(o: Pos): Int = 
        Math.abs(o.r - p.r) + Math.abs(o.c - p.c) 
    }

    extension (b: Array[Array[Char]]) {
      def apply(p: Pos): Char = b(p.r)(p.c)
      def updatedP(p: Pos, c: Char): Array[Array[Char]] =  {
        val ic = b(p.r).clone().updated(p.c, c)
        b.clone().updated(p.r, ic)
      }
      def prettyString: String = b.map(_.mkString).mkString("\n")
    }

    val startPos = board
      .map(_.indexOf(start))
      .zipWithIndex
      .collect {
        case (c, r) if c >= 0 => (r, c)
      }
      .head
    val endPos = board
      .map(_.indexOf(end))
      .zipWithIndex
      .collect {
        case (c, r) if c >= 0 => (r, c)
      }
      .head


    def getDistances(b: Array[Array[Char]], startPos: Pos): Map[Pos, Int] = {
      import scala.collection.mutable
      val pq: PriorityQueue[(Pos, Int)] = new PriorityQueue[(Pos, Int)]()(
        Ordering.fromLessThan[(Pos, Int)]((a, b) => a._2 < b._2).reverse
      )
      pq += ((startPos, 0))
      val costs: mutable.Map[Pos, Int] = mutable.Map()
      while (pq.nonEmpty) {
        val (item, cost) = pq.dequeue()
        if (!costs.contains(item)) {
          costs(item) = cost
          for (
            dir <- List(
              (0, 1),
              (0, -1),
              (-1, 0),
              (1, 0)
            );
            newPos= item + dir;
            if (newPos.isInBounds && !costs.contains(newPos))
          ) {
            if (
              b(newPos) != wall 
            ) {
              val newCost = cost + 1
              pq += ((newPos, newCost))
            }
          }
        }
      }
      costs.toMap
    }
    val startToPoint: Map[Pos, Int] = getDistances(board, startPos)
    val endToPoint: Map[Pos, Int] = getDistances(board, endPos)

    val maxDist = 20
    val costs = for (
      r <- board.indices;
      c <- board.head.indices;
      pos = (r, c);
      xd <- (-maxDist to maxDist);
      ymax = maxDist - Math.abs(xd);
      yd <- (-ymax to ymax);
      pos2 = pos + (xd, yd);
      dst = pos.dist(pos2);
      if (board(pos) != wall)
      if (pos2.isInBounds && board(pos2) != wall)
    ) yield startToPoint(pos) + dst + endToPoint(pos2) 


    val baseCost = startToPoint(endPos)
    costs
      .view
      .map(baseCost - _)
      .filter(_ >= 100)
      .size.toString
  }

}
