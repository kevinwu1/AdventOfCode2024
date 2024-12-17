import os.read

import scala.annotation.tailrec
import scala.annotation.varargs
import scala.collection.IndexedSeqView
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue
import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex.Match
object Day17 extends AocDay {

  def main(args: Array[String]): Unit = {
    run()
//     part1("""Register A: 117440
// Register B: 0
// Register C: 0

// Program: 0,3,5,4,3,0""").pipe(
//       println
//     )
  }
  def part1(input: String): String = {
    // ops
    //

    case class Reg(A: Int, B: Int, C: Int, ip: Int, output: Vector[Int]) {
      def incr: Reg =
        this.copy(ip = ip + 2)
    }
    def comboOp(o: Int, reg: Reg): Int = {
      o match {
        case 0 | 1 | 2 | 3 => o
        case 4             => reg.A
        case 5             => reg.B
        case 6             => reg.C
      }
    }

    var counter = 0
    def instr(i: Int, op: Int, reg: Reg): Reg = {
      // println(s"Executing $i, $op, $reg")
      counter += 1
      if (counter == 100)
        throw new IllegalArgumentException()
      i match {
        case 0 =>
          reg.copy(A = reg.A / (1 << comboOp(op, reg))).incr
        case 1 =>
          reg.copy(B = op ^ reg.B).incr
        case 2 =>
          reg.copy(B = comboOp(op, reg) % 8).incr
        case 3 =>
          if (reg.A == 0)
            reg.incr
          else
            reg.copy(ip = op)
        case 4 =>
          reg.copy(B = reg.B ^ reg.C).incr
        case 5 =>
          reg.copy(output = reg.output :+ (comboOp(op, reg) % 8)).incr
        case 6 =>
          reg.copy(B = reg.A / (1 << comboOp(op, reg))).incr
        case 7 =>
          reg.copy(C = reg.A / (1 << comboOp(op, reg))).incr
      }
    }

    val spl = input.split("\n\n")
    val abc = spl(0)
      .split("\n")
      .map(line =>
        line match {
          case s"Register $x: $v" =>
            v.toInt
        }
      )
    val state = Reg(abc(0), abc(1), abc(2), 0, Vector())
    val instructions = spl(1).stripSuffix("\n") match {
      case s"Program: $c" => c.split(",").map(_.toInt)
    }
    // println("Instructions: " + instructions.mkString(","))
    val execution = Iterator.iterate(state) { case state =>
      val i = instructions(state.ip)
      val op = instructions(state.ip + 1)
      instr(i, op, state)
    }

    execution.find(_.ip >= instructions.length).get.output.mkString(",")
  }

  def part2(input: String): String = {

    case class Reg(A: Long, B: Long, C: Long, ip: Int, output: Vector[Long]) {
      def incr: Reg =
        this.copy(ip = ip + 2)

      override def toString(): String = {
        f"\nA=${A.toBinaryString}%64s\nB=${B.toBinaryString}%64s\nC=${C.toBinaryString}%64s\nip=$ip @ $output"
      }
    }
    def comboOp(o: Int, reg: Reg): Long = {
      o match {
        case 0 | 1 | 2 | 3 => o
        case 4             => reg.A
        case 5             => reg.B
        case 6             => reg.C
      }
    }

    def instr(i: Int, op: Int, reg: Reg): Reg = {
      i match {
        case 0 =>
          reg.copy(A = reg.A / (1 << comboOp(op, reg))).incr
        case 1 =>
          reg.copy(B = op ^ reg.B).incr
        case 2 =>
          reg.copy(B = comboOp(op, reg) % 8).incr
        case 3 =>
          if (reg.A == 0)
            reg.incr
          else
            reg.copy(ip = op)
        case 4 =>
          reg.copy(B = reg.B ^ reg.C).incr
        case 5 =>
          reg.copy(output = reg.output :+ (comboOp(op, reg) % 8)).incr
        case 6 =>
          reg.copy(B = reg.A / (1 << comboOp(op, reg))).incr
        case 7 =>
          reg.copy(C = reg.A / (1 << comboOp(op, reg))).incr
      }
    }

    val spl = input.split("\n\n")
    val abc = spl(0)
      .split("\n")
      .map(line =>
        line match {
          case s"Register $x: $v" =>
            v.toInt
        }
      )

    val instructions = (spl(1).stripSuffix("\n") match {
      case s"Program: $c" => c.split(",").map(_.toInt)
    }).toList
    val len = instructions.length
    def explore(targets: List[Int], depth: Int, seed: Long): Vector[Long] = {
      if (targets.isEmpty)
        Vector(seed)
      else {

        val target = targets.head
        // println(s"Exploring $targets $depth ${seed.toBinaryString}")
        val shift: Long = (len - 1 - depth) * 3
        val possibilities = (for (bits <- 0L until (1L << 3)) yield {
          val B = bits
          val A = (seed >>> shift) | B
          // println(s"${A.toBinaryString}")
          val B1 = B ^ 1
          val C = A / (1 << B1)
          val A2 = A / (1 << 3)
          val B2 = B1 ^ 4
          val B3 = B2 ^ C
          val result = B3 % 8
          /*
          2,4, //B = A%8
          1,1, //B = B ^ 1
          7,5, //C = A / (1<<B)
          0,3, //A = A / (1<<3)
          1,4, //B = B ^ 4
          4,0, //B = B ^ C
          5,5, //out B%*
          3,0  //loop
           */
          // println(s"Attemp ${bits.toBinaryString} got $result")
          // println()
          if (result == target)
            Some(bits)
          else
            None
        }).flatten

        possibilities
          .map(p => (p << shift) | seed)
          .flatMap(newSeed => explore(targets.tail, depth + 1, newSeed))
          .toVector

      }
    }
    val results = explore(instructions.reverse, 0, 0)
    // println(results)
    // println("Lowest: " + results.min)
    val state = Reg(results.head, abc(1), abc(2), 0, Vector())

    // println("Instructions: " + instructions.mkString(","))
    val execution = Iterator.iterate(state) { case state =>
      val i = instructions(state.ip)
      val op = instructions(state.ip + 1)
      val r = instr(i, op, state)
      // println(s"i: $i, op $op, r: $r")
      r
    }

    assert(
      execution.find(_.ip >= instructions.length).get.output == instructions
    )
    results.min.toString

  }

}
