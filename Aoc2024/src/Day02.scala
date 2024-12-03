import scala.util.chaining.scalaUtilChainingOps
object Day02 extends AocDay {
  def main(args: Array[String]): Unit = {
    run()
  }

  override def part1(input: String): String = {
    val rows: List[List[Int]] = input
      .split("\n")
      .toList
      .map(_.split("\\s+").map(_.toInt).toList)
    rows
      .count(isSafe)
      .toString
  }

  def isSafe(row: List[Int]): Boolean = {
    val diffs: List[Int] = row.sliding(2).map(l => l(1) - l(0)).toList
    val shouldDiffsBePositive = diffs.head > 0
    diffs.forall(d => {
      val absDiff = Math.abs(d)
      val diffIsPositive = d > 0
      1 <= absDiff && absDiff <= 3 && diffIsPositive == shouldDiffsBePositive
    })
  }

  override def part2(input: String): String = {
    val rows: List[List[Int]] = input
      .split("\n")
      .toList
      .map(_.split("\\s+").map(_.toInt).toList)
    rows
      .count(row => {
        row.indices.exists(ind =>
          val newRow = row.patch(ind, List(), 1)
          isSafe(newRow)
        )
      })
      .toString()
  }
}
