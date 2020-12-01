package aoc2020.day1

import aoc2020.Util
import aoc2020.Util.readLines

object Day1 {

  def main(args: Array[String]): Unit = {
    val in = readLines("input/day1.txt").map(_.toInt)

    val p1 =
      (for (e1 <- in; e2 <- in if e1 + e2 == 2020)
        yield e1 * e2).head

    val p2 =
      (for (e1 <- in; e2 <- in; e3 <- in if e1 + e2 + e3 == 2020)
        yield e1 * e2 * e3).head

    println(s"part1: $p1")
    println(s"part2: $p2")
  }
}
