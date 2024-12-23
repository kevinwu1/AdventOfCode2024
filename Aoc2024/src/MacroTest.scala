import Aoc2024.macros.Macros.*

import scala.annotation.experimental
import scala.language.experimental
@experimental
object MacroTest {

  val hi = 9
  @logged
  def mymethod(a: Int): Int = a + 2

  def main(args: Array[String]): Unit = {
    println(mymethod(4))
  }
}
