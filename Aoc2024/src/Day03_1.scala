import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex.Match

object Day3_1 {
  def main(args: Array[String]): Unit = {
    val input = os.read(os.pwd / "input" / "day3input.txt")

    val regex = raw"mul\((\d{1,3}),(\d{1,3})\)".r
    (
      for (thematch <- regex.findAllIn(input).matchData)
        yield (1 to thematch.groupCount)
          .map(thematch.group)
          .map(_.toInt)
          .ensuring(_.length == 2)
          .reduce(_ * _)
    ).sum
      .pipe(println)
  }

}
