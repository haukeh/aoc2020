package aoc2020.day5

import aoc2020.Util._

object Day5 {

  val parseBinaryString: String => String = _.map {
    case 'F' | 'L' => '0'
    case 'B' | 'R' => '1'
  }

  val parseRowAndCol: String => (Int, Int) = str => {
    val (row, col) = str.splitAt(7)
    Integer.parseInt(row, 2) -> Integer.parseInt(col, 2)
  }

  val calcSeatID: (Int, Int) => Int = (r, c) => r * 8 + c

  def main(args: Array[String]): Unit = {
    val ids = readLines("input/day5.txt")
      .map(parseBinaryString)
      .map(parseRowAndCol)
      .map(calcSeatID.tupled)

    val p1 = ids.max
    val p2 = ids
      .find { num => ids.contains(num + 2) && !ids.contains(num + 1) }
      .map(_ + 1)
      .get

    println(s"p1: $p1")
    println(s"p2: $p2")
  }
}
