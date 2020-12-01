package aoc2020

import scala.io.Source

object Util {

  def readLines(file: String): List[String] =
    Source.fromFile(file).getLines.toList

}
