import os.read

import scala.annotation.tailrec
import scala.annotation.varargs
import scala.collection.mutable.ListBuffer
import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex.Match
object Day11 extends AocDay {

  def main(args: Array[String]): Unit = {
    run()
    // part1("125 17").pipe(println)
  }
  def part1(input: String): String = {
    def transition(l: Long): Vector[Long] = {
      l match {
        case 0 => Vector(1)
        case _ => {
          val lstr = l.toString
          val llen = lstr.length()
          if (llen % 2 == 0) {
            Vector(
              lstr.slice(0, llen / 2).toLong,
              lstr.slice(llen / 2, llen).toLong
            )
          } else {
            Vector(l * 2024)
          }
        }
      }
    }
    val initial = input.stripSuffix("\n").split(" ").map(_.toLong)
    (0 until 25)
      .foldLeft(initial) { case (state, _) =>
        state.flatMap(transition(_))
      }
      .length
      .toString()
  }

  def part2(input: String): String = {
    def transition(l: Long): Vector[Long] = {
      l match {
        case 0 => Vector(1)
        case _ => {
          val lstr = l.toString
          val llen = lstr.length()
          if (llen % 2 == 0) {
            Vector(
              lstr.slice(0, llen / 2).toLong,
              lstr.slice(llen / 2, llen).toLong
            )
          } else {
            Vector(l * 2024)
          }
        }
      }
    }

    case class StoneGroup(value: Long, count: Long) {
      def transitionStoneGroup: Vector[StoneGroup] =
        transition(value).map(StoneGroup(_, count))
      def +(sg: StoneGroup): StoneGroup = {
        assert(sg.value == value)
        StoneGroup(value, count + sg.count)
      }
    }
    val initial =
      input
        .stripSuffix("\n")
        .split(" ")
        .map(l => StoneGroup(l.toLong, 1))
        .toVector
    (0 until 75)
      .foldLeft(initial) { case (state, _) =>
        state
          .flatMap(_.transitionStoneGroup)
          .groupBy(_.value)
          .values
          .map(_.reduce(_ + _))
          .toVector
      }
      .map(_.count)
      .sum
      .toString()
  }

}
