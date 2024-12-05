import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex.Match
object Day05 extends AocDay {
  def main(args: Array[String]): Unit = {
    // println(deltas)
    run()
//     part2("""47|53
// 97|13
// 97|61
// 97|47
// 75|29
// 61|13
// 75|53
// 29|13
// 97|29
// 53|29
// 61|53
// 97|53
// 61|29
// 47|13
// 75|47
// 97|75
// 47|61
// 75|61
// 47|29
// 75|13
// 53|13

// 75,47,61,53,29
// 97,61,53,29,13
// 75,29,13
// 75,97,47,61,53
// 61,13,29
// 97,13,75,29,47""").pipe(println)
  }

  case class Rule(before: Int, after: Int)
  override def part1(input: String): String = {
    val (rules: List[Rule], lists: List[List[Int]]) = getRulesAndLists(input)
    lists.filter(isListValid(_, rules)).map(l => l(l.size / 2)).sum.toString()
  }

  def getRulesAndLists(input: String): (List[Rule], List[List[Int]]) = {
    val spl = input.split("\n\n")
    (
      spl(0)
        .split("\n")
        .map { case s"$a|$b" =>
          Rule(a.toInt, b.toInt)
        }
        .toList,
      spl(1)
        .split("\n")
        .map(pages => pages.split(",").map(_.toInt).toList)
        .toList
    )
  }

  def isListValid(list: List[Int], rules: List[Rule]): Boolean = {
    val positions = list.zipWithIndex.map { case (item, index) =>
      item -> index
    }.toMap
    rules.forall { case Rule(before, after) =>
      positions.getOrElse(before, -1) < positions.getOrElse(after, Int.MaxValue)
    }
  }

  override def part2(input: String): String = {
    val (rules: List[Rule], lists: List[List[Int]]) = getRulesAndLists(input)
    val ruleMappings: Map[Int, List[Int]] = rules.groupMap(_.before)(_.after)
    // println(isBefore(81, 42, ruleMappings))
    // println(isBefore(42, 81, ruleMappings))
    // ???
    lists
      .filter(!isListValid(_, rules))
      .map(fixListOrdering(_, rules))
      .map(l => l(l.size / 2))
      .sum
      .toString()
  }

  def fixListOrdering(list: List[Int], rules: List[Rule]): List[Int] = {
    // println("Fixing " + list)
    val allRuledPages = rules.flatMap(r => List(r.before, r.after)).toSet
    val (affected, unaffected) = list.partition(allRuledPages.contains)
    val fixed = unaffected ++ sortAffected(affected, rules)
    // println("unaffected: " + unaffected)
    // println("Fixed: " + fixed)
    // println()
    fixed
  }

  def sortAffected(list: List[Int], rules: List[Rule]): List[Int] = {
    val knownPages = list.toSet
    val ruleMappings: Map[Int, List[Int]] = rules
      // the entirety of the rules contain cycles, but for a given list, there are no cycles
      // so we need to filter the rules down to only the elements in the list to get a correct sort
      .filter { case Rule(from, to) =>
        knownPages.contains(from) && knownPages.contains(to)
      }
      .groupMap(_.before)(_.after)
    list.sortWith(isBefore(_, _, ruleMappings))
  }

  def isBefore(a: Int, b: Int, ruleMappings: Map[Int, List[Int]]): Boolean = {
    ruleMappings.contains(a) && {
      val children = ruleMappings(a)
      val isDirectChild = children.exists(_ == b)
      // if (isDirectChild)
      //   println(a + "-->" + b)
      isDirectChild || children.exists(c =>
        val didFindChild = isBefore(c, b, ruleMappings)
        // if (didFindChild)
        //   println(a + "->" + c)
        didFindChild
      )
    }
  }

}
