import os.read

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex.Match
object Day08 extends AocDay {

  def main(args: Array[String]): Unit = {
    run()
  }

  type Pos = (Int, Int)
  extension (p: Pos) {
    inline def +(o: Pos): Pos = (p._1 + o._1, p._2 + o._2)
    def negate: Pos = (-p._1, -p._2)
    inline def -(o: Pos): Pos = p + o.negate
  }
  case class Antenna(char: Char, pos: Pos)

  override def part1(input: String): String = {
    val board = input.split("\n")
    val rows = board.size
    val cols = board.head.size

    extension (p: Pos) {
      def isInBounds: Boolean =
        0 <= p._1 && p._1 < rows &&
          0 <= p._2 && p._2 < cols
    }

    val ants = for (
      r <- board.indices;
      c <- board.head.indices;
      char = board(r)(c);
      if char != '.'
    ) yield Antenna(char, (r, c))

    ants
      .groupBy(_.char)
      .mapValues(_.combinations(2).map { (l => (l.head, l(1))) })
      .values
      .flatten
      .foldLeft(Set[(Int, Int)]()) {
        case (set, (ant1, ant2)) => {
          val a1Toa2 = ant2.pos - ant1.pos

          val anode1 = ant1.pos - a1Toa2
          val anode2 = ant2.pos + a1Toa2

          val toAdd = Set(anode1, anode2).filter(_.isInBounds)
          set ++ toAdd
        }
      }
      .size
      .toString
  }

  override def part2(input: String): String = {
    val board = input.split("\n")
    val rows = board.size
    val cols = board.head.size

    extension (p: Pos) {
      def isInBounds: Boolean =
        0 <= p._1 && p._1 < rows &&
          0 <= p._2 && p._2 < cols
    }

    val ants = for (
      r <- board.indices;
      c <- board.head.indices;
      char = board(r)(c);
      if char != '.'
    ) yield Antenna(char, (r, c))

    ants
      .groupBy(_.char)
      .mapValues(_.combinations(2).map(l => (l.head, l(1))))
      .values
      .flatten
      .foldLeft(Set[(Int, Int)]()) {
        case (set, (ant1, ant2)) => {
          val a1Toa2 = ant2.pos - ant1.pos

          def getPoints(p: Pos, delta: Pos): LazyList[Pos] =
            p #:: (
              if ((p + delta).isInBounds)
                getPoints(p + delta, delta)
              else
                LazyList.empty
            )
          val pointsOne: LazyList[Pos] = getPoints(ant1.pos, a1Toa2)
          val pointsTwo: LazyList[Pos] = getPoints(ant2.pos, a1Toa2.negate)

          set ++ pointsOne.toSet ++ pointsTwo.toSet
        }
      }
      .size
      .toString
  }

}
