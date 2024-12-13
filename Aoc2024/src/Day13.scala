import os.read

import scala.annotation.tailrec
import scala.annotation.varargs
import scala.collection.mutable.ListBuffer
import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex.Match
object Day13 extends AocDay {

  def main(args: Array[String]): Unit = {
    run()
//     part1("""Button A: X+94, Y+34
// Button B: X+22, Y+67
// Prize: X=8400, Y=5400

// Button A: X+26, Y+66
// Button B: X+67, Y+21
// Prize: X=12748, Y=12176

// Button A: X+17, Y+86
// Button B: X+84, Y+37
// Prize: X=7870, Y=6450

// Button A: X+69, Y+23
// Button B: X+27, Y+71
// Prize: X=18641, Y=10279""").pipe(println)
  }
  def part1(input: String): String = {
    input
      .split("\n\n")
      .map(machine => {
        val lines = machine.split("\n")
        println(lines.toList)
        val (ax, ay) = lines(0) match
          case s"Button A: X+$ax, Y+$ay" => (ax.toInt, ay.toInt)
        val (bx, by) = lines(1) match
          case s"Button B: X+$bx, Y+$by" => (bx.toInt, by.toInt)
        val (px, py) = lines(2) match
          case s"Prize: X=$px, Y=$py" => (px.toInt, py.toInt)
        // A * ax * ay + B * bx * ay = px * ay
        // A * ay * ax + B * by * ax = py * ax
        val B = ((px * ay - py * ax) / (bx * ay - by * ax)).toInt
        val A = ((px - B * bx) / ax).toInt
        if (
          A * ax + B * bx == px &&
          A * ay + B * by == py
        ) Some(A * 3 + B)
        else
          None
      })
      .map(_.getOrElse(0))
      .sum
      .toString()
  }

  def part2(input: String): String = {
    input
      .split("\n\n")
      .map(machine => {
        val lines = machine.split("\n")
        println(lines.toList)
        val (ax, ay) = lines(0) match
          case s"Button A: X+$ax, Y+$ay" => (ax.toLong, ay.toLong)
        val (bx, by) = lines(1) match
          case s"Button B: X+$bx, Y+$by" => (bx.toLong, by.toLong)
        val (px, py) = lines(2) match
          case s"Prize: X=$px, Y=$py" =>
            (px.toLong + 10000000000000L, py.toLong + 10000000000000L)
        // A * ax * ay + B * bx * ay = px * ay
        // A * ay * ax + B * by * ax = py * ax
        val B = ((px * ay - py * ax) / (bx * ay - by * ax)).toLong
        val A = ((px - B * bx) / ax).toLong
        if (
          A * ax + B * bx == px &&
          A * ay + B * by == py
        ) Some(A * 3 + B)
        else
          None
      })
      .map(_.getOrElse(0L))
      .sum
      .toString()
  }

}
