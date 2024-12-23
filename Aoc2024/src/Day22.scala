object Day22 extends AocDay {

  def main(args: Array[String]): Unit = {
    run()
  }
  def part1(input: String): String = {
    val M = 16777216
    def a(i: Long): Long =
      ((i * 64) ^ i) % M
    def b(i: Long): Long =
      ((i / 32) ^ i) % M
    def c(i: Long): Long =
      ((i * 2048) ^ i) % M
    def n(i: Long): Long =
      c(b(a(i)))
    input
      .split("\n")
      .map(_.toLong)
      .map(x =>
        (0 until 2000)
          .foldLeft(x) { case (a, _) =>
            n(a)
          }
      )
      .sum
      .toString
  }

  def part2(input: String): String = {
    val M = 16777216
    def a(i: Long): Long =
      ((i * 64) ^ i) % M
    def b(i: Long): Long =
      ((i / 32) ^ i) % M
    def c(i: Long): Long =
      ((i * 2048) ^ i) % M
    def n(i: Long): Long =
      c(b(a(i)))
    import scala.collection.parallel.CollectionConverters._
    input
      .split("\n")
      .map(_.toLong)
      .par
      .map(x =>
        val n1 = x
        val n2 = n(n1)
        val n3 = n(n2)
        val n4 = n(n3)
        val n5 = n(n4)

        val d1 = (n2 % 10 - n1 % 10)
        val d2 = (n3 % 10 - n2 % 10)
        val d3 = (n4 % 10 - n3 % 10)
        val d4 = (n5 % 10 - n4 % 10)
        val inittm = Map() + ((d1, d2, d3, d4) -> (n5 % 10).toInt)

        val tm =
          (0 until 2000 - 5).foldLeft(
            (n1, n2, n3, n4, n5, inittm)
          ) { case ((n1, n2, n3, n4, n5, tm), _) =>
            val n6 = n(n5)
            val d1 = (n3 % 10 - n2 % 10)
            val d2 = (n4 % 10 - n3 % 10)
            val d3 = (n5 % 10 - n4 % 10)
            val d4 = (n6 % 10 - n5 % 10)

            val value = (n6 % 10).toInt
            val newm =
              if (tm.contains((d1, d2, d3, d4)))
                tm
              else tm + ((d1, d2, d3, d4) -> value)
            (n2, n3, n4, n5, n6, newm)
          }
        tm._6
      )
      .foldLeft(Map[(Long, Long, Long, Long), Long]()) { case (ac, m) =>
        m.foldLeft(ac) { case (a, (k, v)) =>
          a.updated(k, a.getOrElse(k, 0L) + v)
        }
      }
      .values
      .max
      .toString
  }

}
