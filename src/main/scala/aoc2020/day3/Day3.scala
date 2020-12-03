package aoc2020.day3

import aoc2020.Util._

import scala.annotation.tailrec

enum Point {
  case Tree
  case Square
}

object Day3 {
  
  import aoc2020.day3.Point._
  
  def traverse(grid: Seq[Seq[Point]], x: Int, y: Int)(steps: (Int, Int)): Long = {
    val (stepX, stepY) = steps
    
    def recurse(grid: Seq[Seq[Point]], x: Int, y: Int)(sum: Long): Long = {
      if y > grid.length - 1 then sum
      else
        val next = recurse(grid, (x + stepX) % grid(y).length, y + stepY)
        grid(y)(x) match
          case Square => next(sum)
          case Tree => next(sum + 1)
    }
    
    recurse(grid, x, y)(0)
  }
  
  def main(args: Array[String]): Unit = {
    val grid: Seq[Seq[Point]] =
      readLines("input/day3.txt")
        .map(_.map(x => if (x == '#') Tree else Square))

    val p1 = traverse(grid, 0, 0)(3, 1)
    val p2 = Seq((1,1), (3,1), (5,1), (7,1), (1,2)).map(traverse(grid, 0, 0)).reduce(_ * _)
    
    println(p1)
    println(p2)
  }

}
