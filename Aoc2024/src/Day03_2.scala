import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex.Match

object Day3_2 {
  def main(args: Array[String]): Unit = {
    val input = os.read(os.pwd / "input" / "day3input.txt")

    input
      .split(raw"do\(\)")
      .map(section => {
        section.split(raw"don't\(\)").head
      })
      .toList
      .map(section => {
        val regex = raw"mul\((\d{1,3}),(\d{1,3})\)".r
        val matches: List[Match] = regex.findAllIn(section).matchData.toList
        matches
          .map(thematch => {
            (1 to thematch.groupCount)
              .map(thematch.group)
              .map(_.toInt)
              .ensuring(_.length == 2)
              .reduce(_ * _)
          })
          .sum
      })
      .sum
      .pipe(println)
  }

}
