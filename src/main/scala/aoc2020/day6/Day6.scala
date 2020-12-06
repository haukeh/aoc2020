package aoc2020.day6

import aoc2020.Util._

object Day6 {

  case class Acc(
      val idx: Int = 0,
      val accu: Map[Int, Set[Set[Char]]] = Map(0 -> Set())
  )
  
  def main(args: Array[String]): Unit = {
    val Acc(_, answers) = readLines("input/day6.txt").foldLeft(Acc()) {
      (acc, line) =>
        if (line.trim().isEmpty) acc.copy(idx = acc.idx + 1)
        else
          acc.copy(accu = acc.accu.updatedWith(acc.idx) {
            case Some(value) => Some(value + line.toCharArray.toSet)
            case None        => Some(Set(line.toCharArray.toSet))
          })
    }
    
    val p1 = answers
      .map {
        case (_, s) => s.flatten.size
      }.sum

    val p2 = answers
      .map {
        case (_, s) => s.reduce(_ intersect _).size
      }.sum
    
    println(p1)
    println(p2)
  }
}
