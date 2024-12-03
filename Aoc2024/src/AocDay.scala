trait AocDay {
  def run(): Unit = {
    val day =
      this.getClass().getSimpleName().stripSuffix("$").stripPrefix("Day")
    val input = os.read(os.pwd / "input" / s"$day.txt")
    println(s"Part1: ${part1(input)}")
    println(s"Part2: ${part2(input)}")
  }
  def part1(input: String): String
  def part2(input: String): String
}
