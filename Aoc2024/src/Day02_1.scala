import scala.util.chaining.scalaUtilChainingOps

object Day2_1 {
  def main(args: Array[String]): Unit = {
    val input = os.read(os.pwd / "input" / "day2input.txt")
    val rows: List[List[Int]] = input
      .split("\n")
      .toList
      .map(_.split("\\s+").map(_.toInt).toList)
    rows
      .count(row => {
        val diffs: List[Int] = row.sliding(2).map(l => l(1) - l(0)).toList
        val shouldDiffsBePositive = diffs.head > 0
        diffs.forall(d => {
          val absDiff = Math.abs(d)
          val diffIsPositive = d > 0
          1 <= absDiff && absDiff <= 3 && diffIsPositive == shouldDiffsBePositive
        })
      })
      .pipe(println)
  }
}
