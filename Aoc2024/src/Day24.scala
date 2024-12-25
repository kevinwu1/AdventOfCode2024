import scala.util.Random
object Day24 extends AocDay {

  import scala.util.chaining.scalaUtilChainingOps

  import scala.collection.parallel.CollectionConverters._
  def main(args: Array[String]): Unit = {
    run()
  }
  def part1(input: String): String = {
    val spl = input.split("\n\n")
    val init = (spl(0)
      .split('\n')
      .map { case s"$name: $value" =>
        name -> (if (value.toInt == 1) true else false)
      })
      .toMap
    val gates = spl(1).split("\n").map { case s"$i1 $op $i2 -> $out" =>
      (i1, op, i2, out)
    }
    val finalMapping = Iterator
      .iterate((gates, init)) {
        case (gates, mapping) => {
          val (usable, unusable) =
            gates.partition(g =>
              mapping.contains(g._1) && mapping.contains(g._3)
            )
          val newMapping = usable.foldLeft(mapping) {
            case (mp, (s1, op, s2, out)) => {
              val v1 = mp(s1)
              val v2 = mp(s2)
              val finalVal = op match {
                case "XOR" => v1 ^ v2
                case "OR"  => v1 | v2
                case "AND" => v1 & v2
              }
              mp + (out -> finalVal)
            }
          }
          (unusable, newMapping)
        }
      }
      .find(_._1.isEmpty)
      .get
      ._2
    finalMapping.keySet
      .filter(_.startsWith("z"))
      .toList
      .sorted
      .reverse
      .map(k => if (finalMapping(k)) 1 else 0)
      .reverse
      .zipWithIndex
      .map((isOne, ind) => if (isOne == 1) 1L << ind else 0)
      .sum
      .toString
  }

  def part2(input: String): String = {
    val spl = input.split("\n\n")
    val init = {
      val init = (spl(0)
        .split('\n')
        .map { case s"$name: $value" =>
          name -> (if (value.toInt == 1) true else false)
        })
        .toMap
      init.keySet
        .filter(k => k.startsWith("x") || k.startsWith("y"))
        .foldLeft(init) { case (m, k) =>
          m + (k -> false)
        }
    }
    val and = "AND"
    val or = "OR"
    val xor = "XOR"
    type Gate = (String, String, String, String)

    extension (g: Gate) {
      def in1: String = g._1
      def op: String = g._2
      def in2: String = g._3
      def out: String = g._4
      def inputs: Set[String] = Set(in1, in2)
      def isOn(a: String, b: String): Boolean = {
        g.in1 == a && g.in2 == b ||
        g.in1 == b && g.in2 == a
      }
    }
    type Gates = Array[Gate]
    def xWire(i: Int): String = i.gateName("x")
    def yWire(i: Int): String = i.gateName("y")
    def zWire(i: Int): String = i.gateName("z")
    def carryWire(i: Int): String = s"_car$i"
    def xorWire(i: Int): String = s"_xor$i"

    extension (gates: Gates) {
      def swapped(i1: Int, i2: Int): Gates = {
        val (q1, w1, e1, o1) = gates(i1)
        val (q2, w2, e2, o2) = gates(i2)
        gates
          .updated(i1, (q1, w1, e1, o2))
          .updated(i2, (q2, w2, e2, o1))
      }
      def swappedName(n1: String, n2: String): Gates = {
        // println("!" * 70)
        // println(s"Swapping $n1 with $n2")
        val i1 = gates.indexWhere(_._4 == n1)
        val i2 = gates.indexWhere(_._4 == n2)
        val (q1, w1, e1, o1) = gates(i1)
        val (q2, w2, e2, o2) = gates(i2)
        gates
          .updated(i1, (q1, w1, e1, n2))
          .updated(i2, (q2, w2, e2, n1))
      }
      def renamed(f: String, t: String): Gates = {
        // println(s"Renaming $f -> $t")
        val replacements = gates.zipWithIndex.map {
          case ((q, w, e, r), i) if r == f => Some(((q, w, e, t), i))
          case ((q, w, e, r), i) if q == f => Some(((t, w, e, r), i))
          case ((q, w, e, r), i) if e == f => Some(((q, w, t, r), i))
          case _                           => None
        }.flatten
        replacements.foldLeft(gates) { case (gts, (newgate, ind)) =>
          gts
            .updated(ind, newgate)
        }

      }
    }
    extension (i: Int) {
      def gateName(p: String): String =
        f"$p$i%02d"
    }
    val gates1: Gates = spl(1)
      .split("\n")
      .map { case s"$i1 $op $i2 -> $out" =>
        (i1, op, i2, out)
      }

    val carry1 = gates1.find(g => g.isOn(xWire(0), yWire(0)) && g.op == and).get
    val xor1 = gates1.find(g => g.isOn(xWire(1), yWire(1)) && g.op == xor).get
    val gates = gates1
      .renamed(carry1.out, carryWire(1))
      .renamed(xor1.out, xorWire(1))

    extension (a: Long) {
      def toBool: Boolean = if (a == 0) false else true
      def toBinStr: String = {
        val as = a.toBinaryString
        val asl = as.length()
        val cs = if (asl > 45) { as.slice(asl - 45, asl) }
        else as
        cs.reverse.padTo(45, '0').reverse
      }
      def flipped: Long =
        a.toBinStr.zipWithIndex.map {
          case ('1', ind) => 1L << ind
          case _          => 0
        }.sum
      def bitAt(i: Int): Long = {
        if ((a & (1L << i)) == 0) 0 else 1
      }
    }

    val gatesByOut = gates.map(g => g._4 -> g).toMap

    def expandGate(e: List[String]): List[String] = {
      e.flatMap({
        case op if Set("XOR", "AND", "OR").contains(op) => List(op)
        case input if input.startsWith("x") || input.startsWith("y") =>
          List(input)
        case s if !s.startsWith("_") => {
          val g = gatesByOut(s)
          val items = List(g._1, g._2, g._3)
          List(s + "[" + expandGate(items).mkString(" ") + "]")
        }
        case x => List(x)

      })
    }
    // for (i <- 0 until 3) {
    //   val expanded = expandGate(List(i.gateName("z")))
    //   if (
    //     expanded
    //       .filter(t =>
    //         t.startsWith("x") || t
    //           .startsWith("y")
    //       )
    //       .exists(t => t.slice(1, t.size).toInt > i)
    //   ) {
    //     println("BAD: " + expanded)
    //   } else {}
    //   println(expanded)
    // }

    val (_, swaps) = (2 until 45).foldLeft((gates, Vector[String]()))({
      case ((gts, swapped), i) => {
        val xi = i.gateName("x")
        val yi = i.gateName("y")
        val zi = i.gateName("z")
        val px = (i - 1).gateName("x")
        val py = (i - 1).gateName("y")
        extension (g: Gate) {
          def isxy: Boolean = g.isOn(xi, yi)
          def isPxy: Boolean = g.isOn(px, py)
        }
        // println("On iteration: " + i)
        def expandGate(e: List[String]): List[String] = {
          e.flatMap({
            case op if Set("XOR", "AND", "OR").contains(op) => List(op)
            case input if input.startsWith("x") || input.startsWith("y") =>
              List(input)
            case s if !s.startsWith("_") => {
              val g = gts.map(g => g._4 -> g).toMap.apply(s)
              val items = List(g._1, g._2, g._3)
              List(s + "[" + expandGate(items).mkString(" ") + "]")
            }
            case x => List(x)

          })
        }
        val expanded = expandGate(List(i.gateName("z")))
        // println(expanded)
        val xori = gts.find(g => g.isxy && g._2 == "XOR").get
        val carPrev = gts.find(g => g._4 == "_car" + (i - 1)).get
        val xorPrev = gts.find(g => g._4 == "_xor" + (i - 1)).get
        val andPrev = gts
          .find(g =>
            g.isOn(
              (i - 1).gateName("x"),
              (i - 1).gateName("y")
            ) && g._2 == "AND"
          )
          .get
        val carComp1 =
          gts.find(g => g.isOn(carPrev._4, xorPrev._4) && g._2 == "AND").get

        val cari =
          gts.find(g => g.isOn(andPrev._4, carComp1._4) && g._2 == "OR").get

        val (swapZ, swaps) = {
          val zout = gts.find(g => g.out == zi).get
          if (!(zout.isOn(cari.out, xori.out) && zout.op == xor)) {
            gts.find(g => g.isOn(cari.out, xori.out) && g.op == xor) match {
              case Some(actualZout) => {
                (
                  gts.swappedName(zout.out, actualZout.out),
                  Map(zout.out -> actualZout.out, actualZout.out -> zout.out)
                )
              }
              case None => {
                val expectedInputs = Set(xori.out, cari.out)
                val partialGate = gts
                  .find(g =>
                    g.inputs
                      .intersect(expectedInputs)
                      .nonEmpty &&
                      g.op == xor &&
                      g.out == zi
                  )
                  .get
                val weirdInput = (partialGate.inputs -- expectedInputs)
                  .ensuring(_.size == 1)
                  .head
                val wrongOutput = (expectedInputs -- partialGate.inputs)
                  .ensuring(_.size == 1)
                  .head
                (
                  gts.swappedName(weirdInput, wrongOutput),
                  Map(weirdInput -> wrongOutput, wrongOutput -> weirdInput)
                )
              }
            }
          } else (gts, Map())
        }
        // println(s"CAR $i: " + cari)
        // println(s"xor $i: " + xori)
        // println(swapZ.find(g => g.isOn(cari._4, xori._4) && g._2 == "XOR"))
        // println(swapZ.find(g => g._4 == i.gateName("z") && g._2 == "XOR"))
        // println(swapZ.find(g => g._4 == i.gateName("z") && g._2 == "XOR"))
        // println()
        val newGates = swapZ
          .renamed(swaps.getOrElse(andPrev._4, andPrev._4), "_and" + (i - 1))
          .renamed(swaps.getOrElse(xori._4, xori._4), "_xor" + i)
          .renamed(swaps.getOrElse(cari._4, cari._4), "_car" + i)

        val gatesByOut = newGates.map(g => g._4 -> g).toMap

        (newGates, swapped ++ swaps.keySet.toVector)
      }
    })
    swaps.sorted.mkString(",")
  }

}
