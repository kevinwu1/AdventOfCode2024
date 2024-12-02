import scala.util.chaining.scalaUtilChainingOps
object Day1_1 {
  def main(args: Array[String]): Unit = {
    val input = os.read(os.pwd / "input" / "day1input.txt")
    val rows = input
      .split("\n")
      .toList
      .map(_.split("\\s+").map(_.toInt))
    val l1 = rows.map(_.apply(0)).sorted
    val l2 = rows.map(_.apply(1)).sorted
    l1.zip(l2)
      .map { case (a, b) =>
        Math.abs(a - b)
      }
      .reduce(_ + _)
      .pipe(println)

  }
}
