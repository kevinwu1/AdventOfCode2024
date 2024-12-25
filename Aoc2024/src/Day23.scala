object Day23 extends AocDay {

  import scala.util.chaining.scalaUtilChainingOps

  import scala.collection.parallel.CollectionConverters._
  def main(args: Array[String]): Unit = {
    run()
  }
  def part1(input: String): String = {
    val connections = input.split("\n").foldLeft(Map[String, Set[String]]()) { 
      case (connections, s"$a-$b") =>
      val newA = connections.getOrElse(a, Set()) + b
      val newB = connections.getOrElse(b, Set()) + a
      connections.updated(a,newA).updated(b, newB)
    }
    val known = connections.keySet.toVector
    (for (
      a <- known.par;
      b <- known;
      c <- known;
      if (a < b && b < c)
      if (a.startsWith("t") || b.startsWith("t") || c.startsWith("t"))
    ) yield {
      if (
        connections(a).contains(b) &&
        connections(a).contains(c) &&
        connections(b).contains(a) &&
        connections(b).contains(c) &&
        connections(c).contains(b) &&
        connections(c).contains(a)
      ) {
        1
      } else
        0
    }).sum.toString
  }

  def part2(input: String): String = {
    val connections = input.split("\n").foldLeft(Map[String, Set[String]]()) { 
      case (connections, s"$a-$b") =>
      val newA = connections.getOrElse(a, Set()) + b
      val newB = connections.getOrElse(b, Set()) + a
      connections.updated(a,newA).updated(b, newB)
    }

    def canAddToClique(clique: Set[String], item: String): Boolean = {
      val ic = connections(item)
      clique.forall(ic.contains)
    }
    val nodes = connections.keySet
    val startingCliques = nodes.map(x => Set(x)).toSet

    Iterator.iterate((startingCliques, true)) {
      case (cliques, cancontinue) => {
        val newcliques = for (
          node <- nodes.par;
          clique <- cliques
          if (canAddToClique(clique, node))
        ) yield {
          clique + node
        }
        if (newcliques.size == 0) {
          (cliques, false)
        } else 
          (newcliques.seq, true)
      }
    }
    .find(!_._2)
    .get._1.head.toVector.sorted.mkString(",")
  }

}
