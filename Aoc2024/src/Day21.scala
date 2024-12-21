import os.read

import scala.annotation.tailrec
import scala.annotation.varargs
import scala.collection.IndexedSeqView
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue
import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex.Match
object Day21 extends AocDay {

  def main(args: Array[String]): Unit = {
    run()
//     part2("""029A
// 980A
// 179A
// 456A
// 379A""").pipe(println)
    // part1("""179A""").pipe(println)
  }

  def part1(input: String): String = {
    type Pos = (Int, Int)
    extension (p: Pos) {
      def x: Int = p._1
      def y: Int = p._2
      def +(o: Pos): Pos = (p._1 + o._1, p._2 + o._2)
      def negate: Pos = (-p._1, -p._2)
      def -(o: Pos): Pos = p + o.negate
    }
    def charToPos: Map[Char, Pos] = Map(
      '0' -> (1, 0),
      'X' -> (2, 0),
      '1' -> (0, 1),
      '2' -> (1, 1),
      '3' -> (2, 1),
      '4' -> (0, 2),
      '5' -> (1, 2),
      '6' -> (2, 2),
      '7' -> (0, 3),
      '8' -> (1, 3),
      '9' -> (2, 3),
      '<' -> (0, 0),
      'v' -> (1, 0),
      '>' -> (2, 0),
      '^' -> (1, 1),
      'A' -> (2, 1)
    )
    def posToSeq(p: Pos): Set[String] = {
      val xdir = if (p.x > 0) ">" else "<"
      val ydir = if (p.y > 0) "^" else "v"

      val xPart = xdir * Math.abs(p.x)
      val yPart = ydir * Math.abs(p.y)
      Set(xPart + yPart, yPart + xPart)
    }
    def move(c1: Char, c2: Char): Set[String] = {
      val bannedPos =
        if (Set('<', 'v', '>', '^', 'A').contains(c1)) (0, 1) else (0, 0)
      def isValid(s: String): Boolean = {
        s.foldLeft(Option(charToPos(c1))) {
          case (None, _) => None
          case (Some(p), c) => {
            val dir = c match {
              case '^' => (0, 1)
              case '>' => (1, 0)
              case 'v' => (0, -1)
              case '<' => (-1, 0)
            }
            if (p + dir == bannedPos)
              None
            else Some(p + dir)
          }
        }.nonEmpty
      }
      posToSeq(charToPos(c2) - charToPos(c1)).filter(isValid).map(_ + "A")
    }

    val chars2 = List('<', 'v', '>', '^', 'A')
    val baseOptimals = (for (
      c1 <- chars2;
      c2 <- chars2
    ) yield ((c1, c2, 0) -> 1L)).toMap
    val optimals = (1 to 2).foldLeft(baseOptimals) { case (optimals, layer) =>
      val addingOptimals = (for (
        c1 <- chars2;
        c2 <- chars2
      )
        yield {
          val movestrs = move(c1, c2).map("A" + _)

          val cost =
            movestrs
              .map(s =>
                if (s == "A") 1
                else
                  s.sliding(2)
                    .map(w => {
                      val Array(sc1, sc2) = w.toCharArray()
                      optimals((sc1, sc2, layer - 1))
                    })
                    .sum
              )
              .min
          (c1, c2, layer) -> cost
        }).toMap
      addingOptimals ++ optimals
    }

    input
      .split("\n")
      .map(s => {
        val numer = s.stripSuffix("A").toInt
        val strPart =
          ("X" + s.replace("A", "X"))
            .sliding(2)
            .map(w => {
              val Array(c1, c2) = w.toCharArray()
              val cost = move(c1, c2)
                .map("A" + _)
                .map(s =>
                  s.sliding(2)
                    .map(w2 => {
                      val Array(sc1, sc2) = w2.toCharArray()
                      optimals((sc1, sc2, 2))
                    })
                    .sum
                )
                .min
              cost
            })
            .sum
        strPart * numer
      })
      .sum
      .toString
  }

  def part2(input: String): String = {
    type Pos = (Int, Int)
    extension (p: Pos) {
      def x: Int = p._1
      def y: Int = p._2
      def +(o: Pos): Pos = (p._1 + o._1, p._2 + o._2)
      def negate: Pos = (-p._1, -p._2)
      def -(o: Pos): Pos = p + o.negate
    }
    def charToPos: Map[Char, Pos] = Map(
      '0' -> (1, 0),
      'X' -> (2, 0),
      '1' -> (0, 1),
      '2' -> (1, 1),
      '3' -> (2, 1),
      '4' -> (0, 2),
      '5' -> (1, 2),
      '6' -> (2, 2),
      '7' -> (0, 3),
      '8' -> (1, 3),
      '9' -> (2, 3),
      '<' -> (0, 0),
      'v' -> (1, 0),
      '>' -> (2, 0),
      '^' -> (1, 1),
      'A' -> (2, 1)
    )
    def posToSeq(p: Pos): Set[String] = {
      val xdir = if (p.x > 0) ">" else "<"
      val ydir = if (p.y > 0) "^" else "v"

      val xPart = xdir * Math.abs(p.x)
      val yPart = ydir * Math.abs(p.y)
      Set(xPart + yPart, yPart + xPart)
    }
    def move(c1: Char, c2: Char): Set[String] = {
      val bannedPos =
        if (Set('<', 'v', '>', '^', 'A').contains(c1)) (0, 1) else (0, 0)
      def isValid(s: String): Boolean = {
        s.foldLeft(Option(charToPos(c1))) {
          case (None, _) => None
          case (Some(p), c) => {
            val dir = c match {
              case '^' => (0, 1)
              case '>' => (1, 0)
              case 'v' => (0, -1)
              case '<' => (-1, 0)
            }
            if (p + dir == bannedPos)
              None
            else Some(p + dir)
          }
        }.nonEmpty
      }
      posToSeq(charToPos(c2) - charToPos(c1)).filter(isValid).map(_ + "A")
    }

    val chars2 = List('<', 'v', '>', '^', 'A')
    val baseOptimals = (for (
      c1 <- chars2;
      c2 <- chars2
    ) yield ((c1, c2, 0) -> 1L)).toMap
    val optimals = (1 to 25).foldLeft(baseOptimals) { case (optimals, layer) =>
      val addingOptimals = (for (
        c1 <- chars2;
        c2 <- chars2
      )
        yield {
          val movestrs = move(c1, c2).map("A" + _)

          val cost =
            movestrs
              .map(s =>
                if (s == "A") 1
                else
                  s.sliding(2)
                    .map(w => {
                      val Array(sc1, sc2) = w.toCharArray()
                      optimals((sc1, sc2, layer - 1))
                    })
                    .sum
              )
              .min
          (c1, c2, layer) -> cost
        }).toMap
      addingOptimals ++ optimals
    }

    input
      .split("\n")
      .map(s => {
        val numer = s.stripSuffix("A").toInt
        val strPart =
          ("X" + s.replace("A", "X"))
            .sliding(2)
            .map(w => {
              val Array(c1, c2) = w.toCharArray()
              val cost = move(c1, c2)
                .map("A" + _)
                .map(s =>
                  s.sliding(2)
                    .map(w2 => {
                      val Array(sc1, sc2) = w2.toCharArray()
                      optimals((sc1, sc2, 25))
                    })
                    .sum
                )
                .min
              cost
            })
            .sum
        strPart * numer
      })
      .sum
      .toString
  }

}
