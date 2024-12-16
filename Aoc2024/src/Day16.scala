import os.read

import scala.annotation.tailrec
import scala.annotation.varargs
import scala.collection.IndexedSeqView
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue
import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex.Match
object Day16 extends AocDay {

  def main(args: Array[String]): Unit = {
    run()
//     part2("""#################
// #...#...#...#..E#
// #.#.#.#.#.#.#.#.#
// #.#.#.#...#...#.#
// #.#.#.#.###.#.#.#
// #...#.#.#.....#.#
// #.#.#.#.#.#####.#
// #.#...#.#.#.....#
// #.#.#####.#.###.#
// #.#.#.......#...#
// #.#.###.#####.###
// #.#.#...#.....#.#
// #.#.#.#####.###.#
// #.#.#.........#.#
// #.#.#.#########.#
// #S#.............#
// #################""").pipe(
//       println
//     )
  }
  def part1(input: String): String = {
    val board =
      input
        .split("\n")
        .map(_.toCharArray())

    def printRaw(board: Array[Array[Char]]): Unit = {
      println(board.map(_.mkString).mkString("\n"))
    }

    val hei = board.length
    val wid = board.head.length
    type Pos = (Int, Int)
    extension (p: Pos) {
      def r: Int = p._1
      def c: Int = p._2
      def +(o: Pos): Pos = (p.r + o.r, p.c + o.c)
    }

    val space = '.'
    val wall = '#'
    val start = 'S'
    val end = 'E'

    // printRaw(board)

    enum Direction(val rep: Pos) {
      case Up extends Direction((-1, 0))
      case Right extends Direction((0, 1))
      case Down extends Direction((1, 0))
      case Left extends Direction((0, -1))

      def turnRight: Direction = this match {
        case Up    => Right
        case Right => Down
        case Down  => Left
        case Left  => Up
      }
      def turnLeft: Direction = this.turnRight.turnRight.turnRight
    }
    import Direction._

    given Conversion[Direction, Pos] = (d: Direction) => d.rep
    case class State(pos: Pos, dir: Direction) {
      def turnLeft: State = State(pos, dir.turnLeft)
      def turnRight: State = State(pos, dir.turnRight)
    }

    def findInBoard(c: Char): Pos =
      board.zipWithIndex
        .collect { case (ar, ind) =>
          (ind, ar.indexOf(c))
        }
        .filter(_._2 != -1)
        .ensuring(_.length == 1)
        .head

    val startingState = State(findInBoard(start), Right)

    val endingStates: Set[State] = {
      val endingPos = findInBoard(end)
      Direction.values.map(State(endingPos, _)).toSet
    }

    // println(endingStates)

    import scala.collection.mutable
    val costs: mutable.Map[State, Int] = mutable.Map()
    val pq = PriorityQueue[(State, Int)]()(
      Ordering
        .fromLessThan[(State, Int)] { case ((_, c1), (_, c2)) =>
          c1 < c2
        }
        .reverse
    )
    pq += ((startingState, 0))
    while (!(endingStates.exists(s => costs.contains(s)))) { // loop until an ending state is reached
      val (state, cost) = pq.dequeue()
      // println(s"got $state, $cost")
      if (!costs.contains(state)) { // if we already saw this state, then skip it
        costs += state -> cost
        val State(pos, dir) = state

        def bo(p: Pos): Char = board(p.r)(p.c)

        if (bo(pos + dir) != wall)
          pq += ((State(pos + dir, dir), cost + 1))
        pq += ((state.turnLeft, cost + 1000))
        pq += ((state.turnRight, cost + 1000))
      }
    }
    endingStates.flatMap(es => costs.get(es)).min.toString
  }

  def part2(input: String): String = {
    val board =
      input
        .split("\n")
        .map(_.toCharArray())

    def printRaw(board: Array[Array[Char]]): Unit = {
      println(board.map(_.mkString).mkString("\n"))
    }

    val hei = board.length
    val wid = board.head.length
    type Pos = (Int, Int)
    extension (p: Pos) {
      def r: Int = p._1
      def c: Int = p._2
      def +(o: Pos): Pos = (p.r + o.r, p.c + o.c)
    }

    val space = '.'
    val wall = '#'
    val start = 'S'
    val end = 'E'

    // printRaw(board)

    enum Direction(val rep: Pos) {
      case Up extends Direction((-1, 0))
      case Right extends Direction((0, 1))
      case Down extends Direction((1, 0))
      case Left extends Direction((0, -1))

      def turnRight: Direction = this match {
        case Up    => Right
        case Right => Down
        case Down  => Left
        case Left  => Up
      }
      def turnLeft: Direction = this.turnRight.turnRight.turnRight
    }
    import Direction._
    given Conversion[Direction, Pos] = (d: Direction) => d.rep

    case class State(pos: Pos, dir: Direction) {
      def turnLeft: State = State(pos, dir.turnLeft)
      def turnRight: State = State(pos, dir.turnRight)
    }

    def findInBoard(c: Char): Pos =
      board.zipWithIndex
        .collect { case (ar, ind) =>
          (ind, ar.indexOf(c))
        }
        .filter(_._2 != -1)
        .ensuring(_.length == 1)
        .head

    val startingState = State(findInBoard(start), Right)

    val endingStates: Set[State] = {
      val endingPos = findInBoard(end)
      Direction.values.map(State(endingPos, _)).toSet
    }

    // println(endingStates)

    import scala.collection.mutable
    val costs: mutable.Map[State, Int] = mutable.Map()
    val optimalPrePath: mutable.Map[State, Set[State]] = mutable.Map()
    val pq = PriorityQueue[(State, State, Int)]()(
      Ordering
        .fromLessThan[(State, State, Int)] { case ((_, _, c1), (_, _, c2)) =>
          c1 < c2
        }
        .reverse
    )
    pq += ((startingState, startingState, 0))
    while (costs.keySet.intersect(endingStates).isEmpty) { // loop until an ending state is reached
      val (state, prevState, cost) = pq.dequeue()
      // println(s"got $state, $cost")
      if (!costs.contains(state)) { // if we already saw this state, then skip it
        costs += state -> cost
        val State(pos, dir) = state

        def bo(p: Pos): Char = board(p.r)(p.c)

        if (bo(pos + dir) != wall) {
          pq += ((State(pos + dir, dir), state, cost + 1))
        }
        pq += ((state.turnLeft, state, cost + 1000))
        pq += ((state.turnRight, state, cost + 1000))
        optimalPrePath += state -> Set(prevState)
      } else if (cost == costs(state)) {
        optimalPrePath.update(
          state,
          optimalPrePath.getOrElse(state, Set()) + prevState
        )
      }
    }
    optimalPrePath.update(startingState, Set()) // important!

    Iterator
      .iterate((endingStates, Set(endingStates.head.pos))) {
        case (states, known) =>
          val oneStepBack: Set[State] =
            states.flatMap(s => optimalPrePath.getOrElse(s, Set()))
          (oneStepBack, known ++ oneStepBack.map(_.pos))
      }
      .find(
        _._1.isEmpty
      ) // step through iterator until we've exhausted all backtracks
      .get
      ._2
      .size
      .toString
  }

}
