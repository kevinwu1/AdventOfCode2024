import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex.Match
object Day03 extends AocDay {
  def main(args: Array[String]): Unit = {
    run()
  }

  override def part1(input: String): String = {
    val regex = raw"mul\((\d{1,3}),(\d{1,3})\)".r
    (
      for (thematch <- regex.findAllIn(input).matchData)
        yield (1 to thematch.groupCount)
          .map(thematch.group)
          .map(_.toInt)
          .ensuring(_.length == 2)
          .reduce(_ * _)
    ).sum.toString
  }

  override def part2(input: String): String = {
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
      .toString()
  }
}
