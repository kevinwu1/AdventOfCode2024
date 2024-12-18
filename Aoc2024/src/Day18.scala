import os.read

import scala.annotation.tailrec
import scala.annotation.varargs
import scala.collection.IndexedSeqView
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue
import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex.Match
object Day18 extends AocDay {

  def main(args: Array[String]): Unit = {
    run()
//     part1("""Register A: 117440
// Register B: 0
// Register C: 0

// Program: 0,3,5,4,3,0""").pipe(
//       println
//     )
//     part1("""Register A: 117440
// Register B: 0
// Register C: 0

// Program: 0,3,5,4,3,0""").pipe(
//       println
//     )
  }
  def part1(input: String): String = {
    type Pos = (Int, Int)
    val wid = 71
    val hei = 71
    extension (p: Pos) {
      def x: Int = p._1
      def y: Int = p._2
      def +(o: Pos): Pos = (p._1 + o._1, p._2 + o._2)
      def isInBounds: Boolean =
        0 <= p._1 && p._1 < wid &&
          0 <= p._2 && p._2 < hei
    }

    val spl = input
      .split("\n")
    val blockers: Set[Pos] = (spl
      .slice(0, 1024)
      .map {
        case s"$x,$y" => {
          (x.toInt, y.toInt)
        }
      })
      .toSet

    import scala.collection.mutable
    val pq: PriorityQueue[(Pos, Int)] = new PriorityQueue[(Pos, Int)]()(
      Ordering.fromLessThan[(Pos, Int)]((a, b) => a._2 < b._2).reverse
    )
    val costs: mutable.Map[Pos, Int] = mutable.Map()
    val start = (0, 0)
    val end = (wid - 1, hei - 1)
    pq += ((start, 0))
    while (!costs.contains(end) && pq.nonEmpty) {
      val (item, cost) = pq.dequeue()
      if (!costs.contains(item)) {
        costs(item) = cost
        for (
          dir <- List(
            (0, 1),
            (0, -1),
            (-1, 0),
            (1, 0)
          )
        ) {
          val newPos: Pos = item + dir;
          if (
            newPos.isInBounds && !blockers.contains(newPos) && !costs
              .contains(
                newPos
              )
          ) {
            val newCost = cost + 1
            pq += ((newPos, newCost))
          }
        }
      }
    }
    costs(end).toString

  }

  def part2(input: String): String = {
    type Pos = (Int, Int)
    val wid = 71
    val hei = 71
    extension (p: Pos) {
      def x: Int = p._1
      def y: Int = p._2
      def +(o: Pos): Pos = (p._1 + o._1, p._2 + o._2)
      def isInBounds: Boolean =
        0 <= p._1 && p._1 < wid &&
          0 <= p._2 && p._2 < hei
    }

    val spl = input
      .split("\n")
    val blockers: Set[Pos] = (spl
      .slice(0, 1024)
      .map {
        case s"$x,$y" => {
          (x.toInt, y.toInt)
        }
      })
      .toSet

    spl
      .slice(1024, spl.length)
      .foldLeft[Either[Set[Pos], Pos]](Left(blockers)) {
        case (Right(p), _) => Right(p)
        case (Left(blockers), s"$x,$y") =>
          val newBlocker = (x.toInt, y.toInt)
          val newBlockers = blockers + newBlocker
          import scala.collection.mutable
          val pq: PriorityQueue[(Pos, Int)] = new PriorityQueue[(Pos, Int)]()(
            Ordering.fromLessThan[(Pos, Int)]((a, b) => a._2 < b._2).reverse
          )
          val costs: mutable.Map[Pos, Int] = mutable.Map()
          val start = (0, 0)
          val end = (wid - 1, hei - 1)
          pq += ((start, 0))
          while (!costs.contains(end) && pq.nonEmpty) {
            val (item, cost) = pq.dequeue()
            if (!costs.contains(item)) {
              costs(item) = cost
              for (
                dir <- List(
                  (0, 1),
                  (0, -1),
                  (-1, 0),
                  (1, 0)
                )
              ) {
                val newPos: Pos = item + dir;
                if (
                  newPos.isInBounds && !newBlockers.contains(newPos) && !costs
                    .contains(
                      newPos
                    )
                ) {
                  val newCost = cost + 1
                  pq += ((newPos, newCost))
                }
              }
            }
          }
          if (!costs.contains(end))
            Right(newBlocker)
          else {
            Left(newBlockers)
          }

      } match {
      case Right(blocker) =>
        blocker._1 + "," + blocker._2
    }

  }

}
