import os.read

import scala.annotation.tailrec
import scala.annotation.varargs
import scala.collection.mutable.ListBuffer
import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex.Match
object Day14 extends AocDay {

  extension (i: Long) {
    def %%(o: Long): Long = {
      ((i % o) + o) % o
    }
  }

  def main(args: Array[String]): Unit = {
    run()
//     part1("""p=0,4 v=3,-3
// p=6,3 v=-1,-3
// p=10,3 v=-1,2
// p=2,0 v=2,-1
// p=0,0 v=1,3
// p=3,0 v=-2,-2
// p=7,6 v=-1,-3
// p=3,0 v=-1,-2
// p=9,3 v=2,3
// p=7,3 v=-1,2
// p=2,4 v=2,-3
// p=9,5 v=-3,-3""").pipe(println)
  }
  def part1(input: String): String = {
    val wid = 101
    val hei = 103
    val steps = 100
    val quads = input.linesIterator
      .map { case s"p=$x,$y v=$vx,$vy" =>
        val finalX = (x.toLong + vx.toLong * steps) %% wid
        val finalY = (y.toLong + vy.toLong * steps) %% hei
        // println((finalX, finalY))
        (finalX, finalY)
      }
      .toList
      .foldLeft((0, 0, 0, 0)) { case ((tl, tr, bl, br), (fx, fy)) =>
        if (fx == wid / 2 || fy == hei / 2)
          (tl, tr, bl, br)
        else
          (fx < wid / 2, fy < hei / 2) match {
            case (true, true)   => (tl + 1, tr, bl, br)
            case (true, false)  => (tl, tr, bl + 1, br)
            case (false, true)  => (tl, tr + 1, bl, br)
            case (false, false) => (tl, tr, bl, br + 1)
          }
      }
    (quads._1 *
      quads._2 *
      quads._3 *
      quads._4).toString
  }

  def part2(input: String): String = {
    val wid = 101
    val hei = 103
    case class Robot(x: Long, y: Long, vx: Long, vy: Long) {
      def atIteration(i: Int): (Long, Long) = {
        val finalX = (x + vx * i) %% wid
        val finalY = (y + vy * i) %% hei
        (finalX, finalY)
      }
    }
    val robots = input.linesIterator.map { case s"p=$x,$y v=$vx,$vy" =>
      Robot(x.toLong, y.toLong, vx.toLong, vy.toLong)
    }.toVector

    val N = robots.length
    (0 to wid * hei) // robots would go back to their starting point after wid*hei iterations
      .foldLeft((99999.0, "")) { // start with some arbitrarily chosen std deviation that should be higher than any naturally occuring one
        case ((stdDevSum, prevImage), iteration) =>
          val coordsL = robots.map(_.atIteration(iteration)).toVector

          val aveX = coordsL.map(_._1).sum / N
          val aveY = coordsL.map(_._2).sum / N
          val stddevX =
            Math.sqrt(coordsL.map(_._1 - aveX).map(x => x * x).sum.toDouble / N)
          val stddevY =
            Math.sqrt(coordsL.map(_._2 - aveY).map(x => x * x).sum.toDouble / N)

          val coords = coordsL.toSet
          val tree = (for (
            y <- 0 until hei;
            x <- 0 until wid
          ) yield {
            val char = if coords.contains((x, y)) then "#" else " "
            val nl = if x == wid - 1 then "\n" else ""
            char + nl
          }).mkString

          val image = iteration + s" : $stddevX, $stddevY \n" + tree
          if (stddevX + stddevY < stdDevSum)
            (stddevX + stddevY, image)
          else
            (stdDevSum, prevImage)
      }
      ._2
  }

}
