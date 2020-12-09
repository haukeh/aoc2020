package aoc2020.day9

import aoc2020.Util._
import cats.Foldable

import scala.annotation.tailrec

object Day9 {

  def isValid(preamble: List[Long], next: Long): Boolean =
    (for (i <- preamble; j <- preamble if i != j && i + j == next)
      yield (i, j)).size > 0

  @tailrec
  def validate(numbers: List[Long]): Long = {
    val preamble :+ next = numbers.take(26)
    if (isValid(preamble, next)) validate(numbers.tail) else next
  }

  @tailrec
  def sumUp(
      numbers: List[Long],
      target: Long,
      acc: Long = 0,
      items: List[Long] = List()
  ): Option[List[Long]] =
    numbers match {
      case head :: tail =>
        if (acc + head > target) None
        else if (acc + head == target) Some(items :+ head)
        else sumUp(tail, target, acc + head, items :+ head)
      case Nil => None
    }

  def main(args: Array[String]): Unit = {
    val nums = readLines("input/day9.txt").map(_.toLong)
    val p1 = validate(nums)
    val range = (for (i <- 0 until nums.size) yield sumUp(nums.drop(i), p1)).flatten.filter(_.size > 1).head
    val p2 = range.min + range.max
    
    println(p1)
    println(p2)
  }
}
