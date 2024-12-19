import os.read

import scala.annotation.tailrec
import scala.annotation.varargs
import scala.collection.IndexedSeqView
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue
import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex.Match
object Day19 extends AocDay {

  def main(args: Array[String]): Unit = {
    run()
  }
  def part1(input: String): String = {
    val spl = input.split("\n\n")
    val towels = spl(0).split(", ").toVector
    val targets = spl(1).split("\n")
    def isPossible(target: String): Boolean = {
      if (target.size == 0)
        true
      else
        towels
          .flatMap(t =>
            if (target.startsWith(t)) {
              Some(target.slice(t.length, target.length))
            } else None
          )
          .exists(isPossible(_))
    }
    targets.count(isPossible).toString
  }

  def part2(input: String): String = {
    val spl = input.split("\n\n")
    val towels = spl(0).split(", ").toVector
    val targets = spl(1).split("\n")
    import scala.collection.mutable
    def countWays(target: String, cache: mutable.Map[String, Long]): Long = {
      if (cache.contains(target))
        cache(target)
      else if (target.size == 0)
        1L
      else {
        val r =
          towels
            .flatMap(t =>
              if (target.startsWith(t)) {
                Some(target.slice(t.length, target.length))
              } else None
            )
            .map(countWays(_, cache))
            .sum
        cache.update(target, r)
        r
      }
    }
    targets
      .map(targ => {
        val cache = mutable.Map[String, Long]()
        countWays(targ, cache)
      })
      .sum
      .toString
  }

}
