trait AocDay {
  def run(): Unit = {
    val day =
      this.getClass().getSimpleName().stripSuffix("$").stripPrefix("Day")
    val input = os.read(os.pwd / "input" / s"$day.txt")
    val t1 = System.nanoTime()
    println(s"Part1: ${part1(input)}")
    val t2 = System.nanoTime()
    println(s"Part2: ${part2(input)}")
    val t3 = System.nanoTime()
    println(s"Time 1: ${(t2 - t1) / 1e9}")
    println(s"Time 2: ${(t3 - t2) / 1e9}")
  }
  def part1(input: String): String
  def part2(input: String): String
}
