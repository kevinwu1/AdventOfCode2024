import scala.util.Random
object Day25 extends AocDay {

  import scala.util.chaining.scalaUtilChainingOps

  import scala.collection.parallel.CollectionConverters._
  def main(args: Array[String]): Unit = {
    run()
  }
  def part1(input: String): String = {
    type Key = (Int, Int, Int, Int, Int)
    extension (k: Key) {
      def arr: Array[Int] = Array(
        k._1,
        k._2,
        k._3,
        k._4,
        k._5
      )
    }
    val (locksR, keysR) = input
      .split("\n\n")
      .map(s => {
        val spl = s.split("\n")
        if (spl.head == "#" * 5) {
          // lock
          val Array(q, w, e, r, t) = (0 until 5)
            .map(col => {
              spl.map(_(col)).zipWithIndex.filter(_._1 == '#').map(_._2).max
            })
            .toArray
          (true, (q, w, e, r, t))
        } else {
          // key
          val Array(q, w, e, r, t) = (0 until 5)
            .map(col => {
              6 - spl
                .map(_(col))
                .zipWithIndex
                .filter(_._1 == '#')
                .map(_._2)
                .min
            })
            .toArray
          (false, (q, w, e, r, t))
        }
      })
      .partition(_._1)
    val locks = locksR.map(_._2)
    val keys = keysR.map(_._2)
    println()
    println(locks.size)
    println(keys.size)
    val total = (for (
      l <- locks;
      k <- keys
      if (l.arr.zip(k.arr).forall((a, b) => a + b <= 5))
    ) yield 1).sum
    total.toString()
  }

  def part2(input: String): String = {
    ???
  }

}
