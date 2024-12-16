import os.read

import scala.annotation.tailrec
import scala.annotation.varargs
import scala.collection.IndexedSeqView
import scala.collection.mutable.ListBuffer
import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex.Match
object Day15 extends AocDay {

  def main(args: Array[String]): Unit = {
    run()
//     part2("""##########
// #..O..O.O#
// #......O.#
// #.OO..O.O#
// #..O@..O.#
// #O#..O...#
// #O..O..O.#
// #.OO.O.OO#
// #....O...#
// ##########

// <vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
// vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
// ><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
// <<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
// ^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
// ^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
// >^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
// <><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
// ^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
// v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^""").pipe(
//       println
//     )
  }
  def part1(input: String): String = {
    val spl = input.split("\n\n")

    val boardRep = spl(0).split("\n").map(_.toCharArray())
    val hei = boardRep.length
    val wid = boardRep.head.length
    type Pos = (Int, Int)
    extension (p: Pos) {
      def r: Int = p._1
      def c: Int = p._2
      def +(o: Pos): Pos = (p.r + o.r, p.c + o.c)
    }

    type RawBoard = Array[Array[Char]]
    val space = '.'
    val box = 'O'
    val wall = '#'
    val robot = '@'
    case class Board(
        rawBoard: RawBoard,
        robotPos: Pos
    ) {
      def bo(p: Pos): Char = rawBoard(p.r)(p.c)
      def move(command: Char): Board = {
        val direction = command match {
          case '^' => (-1, 0)
          case '>' => (0, 1)
          case 'v' => (1, 0)
          case '<' => (0, -1)
        }
        val positions: LazyList[Pos] = {
          def nextPos(p: Pos, dir: Pos): LazyList[Pos] =
            p #:: nextPos(p + dir, dir)
          nextPos(robotPos, direction)
        }
        val firstNonbox =
          positions.find(p => bo(p) != box && bo(p) != robot).get
        bo(firstNonbox) match {
          case nonBoxChar if nonBoxChar == wall => this
          case nonBoxChar if nonBoxChar == space => {
            val newRobotPos = robotPos + direction

            extension (rb: RawBoard) {
              def updateSpot(
                  pos: Pos,
                  newVal: Char
              ): RawBoard = {
                val p1 = rb(pos.r).updated(pos.c, newVal)
                rb.updated(pos.r, p1)
              }
            }
            Board(
              rawBoard
                .updateSpot(firstNonbox, bo(newRobotPos))
                .updateSpot(newRobotPos, robot)
                .updateSpot(robotPos, space),
              newRobotPos
            )
          }
        }
      }
    }

    val board = Board(
      boardRep,
      boardRep
        .map(_.indexOf('@'))
        .zipWithIndex
        .map(_.swap)
        .find(_.c != -1)
        .get
    )
    val endBoard =
      spl(1).replaceAll("\n", "").foldLeft(board) { case (board, command) =>
        board.move(command)
      }
    (for (
      r <- 0 until hei;
      c <- 0 until wid;
      pos = (r, c)
      if endBoard.bo(pos) == box
    ) yield r * 100 + c).sum.toString
  }

  def part2(input: String): String = {
    val spl = input.split("\n\n")

    val boardRep = spl(0)
      .split("\n")
      .map(_.toCharArray().flatMap {
        case '#' => Array('#', '#')
        case 'O' => Array('[', ']')
        case '.' => Array('.', '.')
        case '@' => Array('@', '.')
      })

    def printRaw(board: Array[Array[Char]]): Unit = {
      println(board.map(_.mkString).mkString("\n"))
    }

    val hei = boardRep.length
    val wid = boardRep.head.length
    type Pos = (Int, Int)
    extension (p: Pos) {
      def r: Int = p._1
      def c: Int = p._2
      def +(o: Pos): Pos = (p.r + o.r, p.c + o.c)
    }

    type RawBoard = Array[Array[Char]]
    val space = '.'
    val boxL = '['
    val boxR = ']'
    val wall = '#'
    val robot = '@'

    case class Board(
        rawBoard: RawBoard,
        robotPos: Pos
    ) {
      val up = (-1, 0)
      val right = (0, 1)
      val down = (1, 0)
      val left = (0, -1)

      def bo(p: Pos): Char = rawBoard(p.r)(p.c)
      def isBox(p: Pos): Boolean =
        bo(p) == boxL || bo(p) == boxR
      def normalizeBoxPosition(p: Pos): Pos = {
        require(isBox(p))
        if (bo(p) == boxL) p else p + left
      }

      def trymove(command: Char): Board = {
        val (direction, isVertical) = command match {
          case '^' => (up, true)
          case '>' => (right, false)
          case 'v' => (down, true)
          case '<' => (left, false)
        }

        def getPushToLocation(normalizedBoxPos: Pos): Set[Pos] = {
          direction match {
            case d if d == up || d == down =>
              Set(
                normalizedBoxPos + direction,
                normalizedBoxPos + right + direction
              )
            case d if d == left  => Set(normalizedBoxPos + left)
            case d if d == right => Set(normalizedBoxPos + right + right)
          }
        }

        def getAffectedBoxes(
            pushTos: Set[Pos]
        ): Option[Set[Pos]] = {
          if (pushTos.exists(bo(_) == wall))
            None
          else
            pushTos
              .map(pt => {
                if (isBox(pt)) {
                  val normalized = normalizeBoxPosition(pt)
                  val newPushTos = getPushToLocation(normalized)
                  getAffectedBoxes(
                    newPushTos
                  ).map(_ + normalized)
                } else {
                  Some(Set())
                }
              })
              .foldLeft(Option(Set[Pos]())) {
                case (None, _) | (_, None) => None
                case (Some(accum), Some(pushed)) =>
                  Some(accum ++ pushed)
              }
        }
        val affectedBoxesOpt: Option[Set[Pos]] = getAffectedBoxes(
          Set(robotPos + direction)
        )
        // println(direction)
        affectedBoxesOpt
          .map(affectedBoxes => {
            // println("Affected " + affectedBoxes)
            val sorted =
              (affectedBoxes.flatMap(b =>
                List(b, b + right)
              ) + robotPos).toVector
                .sortBy(p => {
                  (p.r * direction.r, p.c * direction.c)
                })
                .reverse
            // println("sorted: " + sorted)

            extension (rb: RawBoard) {
              def updateSpot(
                  pos: Pos,
                  newVal: Char
              ): RawBoard = {
                val p1 = rb(pos.r).updated(pos.c, newVal)
                rb.updated(pos.r, p1)
              }
            }
            val newRawBoard = sorted
              .foldLeft(rawBoard) {
                case (rb, pos) => {

                  val after = rb
                    .updateSpot(pos + direction, bo(pos))
                    .updateSpot(pos, space)
                  after
                }
              }
            // printRaw(newRawBoard)
            Board(
              newRawBoard,
              robotPos + direction
            )
          })
          .getOrElse(this)
      }
    }

    val board = Board(
      boardRep,
      boardRep
        .map(_.indexOf('@'))
        .zipWithIndex
        .map(_.swap)
        .find(_.c != -1)
        .get
    )
    val endBoard =
      spl(1)
        .replaceAll("\n", "")
        .foldLeft(board) { case (board, command) =>
          board.trymove(command)
        }

    (for (
      r <- 0 until hei;
      c <- 0 until wid;
      pos = (r, c)
      if endBoard.bo(pos) == boxL
    ) yield r * 100 + c).sum.toString
  }

}
