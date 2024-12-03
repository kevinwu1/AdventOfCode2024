import scala.util.chaining.scalaUtilChainingOps
object Day01 extends AocDay {
  def main(args: Array[String]): Unit = {
    run()
  }

  override def part1(input: String): String = {
    val (left, right) = input
      .split("\n")
      .toList
      .map(_ match {
        case s"$li   $ri" => (li.toInt, ri.toInt)
      })
      .unzip
    val l1 = left.sorted
    val l2 = right.sorted
    l1.zip(l2)
      .map { case (a, b) =>
        Math.abs(a - b)
      }
      .reduce(_ + _)
      .toString
  }

  override def part2(input: String): String = {
    val (left, right) = input
      .split("\n")
      .toList
      .map(_ match {
        case s"$li   $ri" => (li.toInt, ri.toInt)
      })
      .unzip
    val rightCount =
      right.foldLeft(Map[Int, Int]()) { case (map, item) =>
        map + (item -> (map.getOrElse(item, 0) + 1))
      }
    left
      .map(n => n * rightCount.getOrElse(n, 0))
      .sum
      .toString
  }
}
