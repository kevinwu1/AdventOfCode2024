import os.read

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex.Match
object Day09 extends AocDay {

  def main(args: Array[String]): Unit = {
    run()
    // println(part2("2333133121414131402"))
  }
  def part1(input: String): String = {
    val diskMap = input.stripSuffix("\n").map(c => c.toString.toInt)
    val expanded = diskMap.zipWithIndex.flatMap { case (item, ind) =>
      val rep = if (ind % 2 == 0) Some((ind / 2).toLong) else None
      List.fill(item)(rep)
    }.toArray
    val compLen = diskMap.zipWithIndex.collect {
      case (item, ind) if ind % 2 == 0 => {
        item
      }
    }.sum
    // expanded
    //   .map {
    //     case Some(d) => d.toString
    //     case _       => "."
    //   }
    //   .mkString
    //   .pipe(println)
    val backhalf = expanded.slice(compLen, expanded.length).filter(_.nonEmpty)
    expanded
      .slice(0, compLen)
      .zipWithIndex
      .foldLeft((0L, 0)) {
        case ((sum, dotsSeenPrior), (c, index)) => {
          val (digit, newDotsSeenPrior) = c match {
            case Some(d) =>
              (c.get, dotsSeenPrior)
            case None =>
              (
                backhalf(backhalf.length - 1 - dotsSeenPrior).get,
                dotsSeenPrior + 1
              )
          }
          (sum + digit * index, newDotsSeenPrior)
        }
      }
      ._1
      .toString
  }
  case class File(rep: Option[Long], len: Int)

  extension (af: Array[File]) {
    def pretty: String =
      af.map(f => f.rep.map(_.toString).getOrElse(".") * f.len).mkString
  }
  def part2(input: String): String = {
    val diskMap = input.stripSuffix("\n").map(c => c.toString.toInt)
    val expanded: Array[File] = diskMap.zipWithIndex.map { case (item, ind) =>
      val rep = if (ind % 2 == 0) Some((ind / 2).toLong) else None
      File(rep, item)
    }.toArray
    compact(expanded, expanded.length - 1)
      // .tap(c => {
      //   c.pretty
      //     .pipe(println)
      // })
      .foldLeft((0L, 0)) { case ((sum, itemsBefore), File(rep, len)) =>
        if (rep == None)
          (sum, itemsBefore + len)
        else {
          val newSum =
            sum + (itemsBefore until (itemsBefore + len)).sum * rep.get
          (newSum, itemsBefore + len)
        }
      }
      ._1
      .toString()
  }

  @tailrec
  def compact(expanded: Array[File], fromIndex: Int): Array[File] = {
    // println(s"Compacting ${expanded.pretty} @ $fromIndex")
    // println(s"           ${" " * expanded.slice(0, fromIndex).map(_.len).sum}^")
    if (fromIndex == 0) {
      expanded
    } else if (expanded(fromIndex).rep == None)
      compact(expanded, fromIndex - 1)
    else {
      val lastItem = expanded(fromIndex)
      val insertionPoint =
        expanded
          .slice(0, fromIndex)
          .indexWhere(f => f.rep == None && f.len >= lastItem.len)
      if (insertionPoint == -1) {
        compact(expanded, fromIndex - 1)
      } else {
        val insertionHost = expanded(insertionPoint)
        compact(
          expanded
            .patch(
              fromIndex,
              Array(File(None, lastItem.len)),
              1
            )
            .patch(
              insertionPoint,
              Array(lastItem, File(None, insertionHost.len - lastItem.len)),
              1
            ),
          fromIndex // don't -1 here because we inserted an extra element
        )
      }
    }
  }

}
