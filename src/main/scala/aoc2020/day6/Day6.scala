package aoc2020.day6

import aoc2020.Util._

object Day6 {

  def main(args: Array[String]): Unit = {
    val answers = readLines("input/day6.txt")
      .partitionWhere(_.isEmpty)
      .map(_.toSet)

    val p1 = answers.map(_.flatten.size).sum
    val p2 = answers.map(_.reduce(_ intersect _).size).sum

    println(p1)
    println(p2)
  }
}
