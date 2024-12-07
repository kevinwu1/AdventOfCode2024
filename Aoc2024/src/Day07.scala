import os.read

import scala.collection.mutable.ListBuffer
import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex.Match
object Day07 extends AocDay {

  def main(args: Array[String]): Unit = {
    run()
  }

  override def part1(input: String): String = {
    input
      .split("\n")
      .map(line => {
        val spl = line.split(": ")
        val target = spl.head.toLong
        val sequence = spl(1).split(" ").map(_.toLong)
        val nOps = sequence.length - 1
        val isPossible = (0 until 1 << (nOps)).exists(opBit => {
          sequence.tail
            .foldLeft((sequence.head, 0)) { case ((res, shift), item) =>
              val next =
                if ((opBit & (1 << shift)) != 0) res * item else res + item
              (next, shift + 1)
            }
            ._1 == target
        })
        if (isPossible)
          target
        else 0
      })
      .sum
      .toString
  }

  override def part2(input: String): String = {
    input
      .split("\n")
      .map(line => {
        val spl = line.split(": ")
        val target = spl.head.toLong
        val sequence = spl(1).split(" ").map(_.toLong)
        val nOps = sequence.length - 1
        val isPossible1 = isPossible(
          target,
          sequence.head,
          sequence.tail
        )
        if (isPossible1)
          target
        else 0
      })
      .sum
      .toString
  }

  def isPossible(
      target: Long,
      startingValue: Long,
      remaining: Seq[Long]
  ): Boolean = {
    if (startingValue == target && remaining.isEmpty)
      true
    else if (startingValue > target || remaining.isEmpty)
      return false
    else
      isPossible(target, startingValue * remaining.head, remaining.tail) ||
      isPossible(target, startingValue + remaining.head, remaining.tail) ||
      isPossible(
        target,
        (startingValue.toString + remaining.head.toString).toLong,
        remaining.tail
      )
  }

}
