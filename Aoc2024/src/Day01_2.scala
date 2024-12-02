import scala.util.chaining.scalaUtilChainingOps
object Day1_2 {
  def main(args: Array[String]): Unit = {
    val input = os.read(os.pwd / "input" / "day1input.txt")
    val rows = input
      .split("\n")
      .toList
      .map(_.split("\\s+").map(_.toInt))
    val left = rows.map(_.apply(0))
    val right = rows.map(_.apply(1))
    val rightCount =
      right.foldLeft(Map[Int, Int]()) { case (map, item) =>
        map + (item -> (map.getOrElse(item, 0) + 1))
      }
    left
      .map(n => n * rightCount.getOrElse(n, 0))
      .sum
      .pipe(println)

  }
}
