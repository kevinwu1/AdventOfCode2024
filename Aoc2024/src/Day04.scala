import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex.Match
object Day04 extends AocDay {
  val searchTerm = "XMAS"
  val searchLen = searchTerm.length
  val deltas = for (
    xd <- (-1 to 1);
    yd <- (-1 to 1);
    (x, y) = (xd, yd) if xd != 0 || yd != 0
  ) yield (x, y)
  def main(args: Array[String]): Unit = {
    // println(deltas)
    run()
//     part2(""".M.S......
// ..A..MSMS.
// .M.S.MAA..
// ..A.ASMSM.
// .M.S.M....
// ..........
// S.S.S.S.S.
// .A.A.A.A..
// M.M.M.M.M.
// ..........""").pipe(println)
  }

  extension (pos: (Int, Int)) {
    def *(n: Int): (Int, Int) = (pos._1 * n, pos._2 * n)
    def +(delta: (Int, Int)): (Int, Int) =
      (pos._1 + delta._1, pos._2 + delta._2)
    def <(other: (Int, Int)): Boolean =
      pos._1 < other._1 && pos._2 < other._2
    def <=(other: (Int, Int)): Boolean =
      pos._1 <= other._1 && pos._2 <= other._2
  }

  override def part1(input: String): String = {
    val board = input.split("\n")
    val height = board.size
    val width = board.head.size
    def charAt(pos: (Int, Int)): Char = board(pos._1)(pos._2)
    (for (
      row <- (0 until height);
      col <- (0 until width);
      startPos = (row, col);
      delta <- deltas;
      endPos = startPos + delta * (searchLen - 1);
      if ((0, 0) <= endPos && endPos < (height, width))
      if (
        (0 until searchLen)
          .map(delta * _ + startPos)
          .map(charAt)
          .mkString == searchTerm
      )
    ) yield {
        1
    }).sum.toString
  }

  override def part2(input: String): String = {
    val board = input.split("\n")
    val height = board.size
    val width = board.head.size
    def charAt(pos: (Int, Int)): Char = board(pos._1)(pos._2)
    val deltasPart2 = for (xd <- List(-1, 1); yd <- List(-1, 1)) yield (xd, yd)
    (for (
      row <- (0 until height);
      col <- (0 until width);
      startPos = (row, col);
      if (deltasPart2.forall(delta => {
        val endPos = startPos + delta
        (0, 0) <= endPos && endPos < (height, width)
      }));
      if ({
        val letters = deltasPart2.map(startPos + _).map(charAt).sorted
        (charAt(startPos) == 'A') &&
        (letters ==
          List('M', 'M', 'S', 'S')) && // two Ms and two Xs
        (charAt(startPos + (1, 1)) != charAt(
          startPos + (-1, -1)
        )) // diagonal chars are different
      })
    ) yield {
        1
    }).sum.toString
  }
}
