package aoc2020.day2
import aoc2020.Util._

object Day2 {

  def main(args: Array[String]): Unit = {
    val passwordRules = readLines("input/day2.txt").map(parseInput)

    val p1 = passwordRules
      .filter(_.isValidPart1())
      .length

    val p2 = passwordRules
      .filter(_.isValidPart2())
      .length

    println(p1)
    println(p2)
  }

  def parseInput(input: String): PasswordRule =
    input match {
      case s"$min-$max $char: $password" =>
        PasswordRule(min.toInt, max.toInt, char.charAt(0), password)
    }
}

case class PasswordRule(
    min: Int,
    max: Int,
    neededChar: Char,
    password: String
) {

  def isValidPart1(): Boolean = {
    val occs = password.foldLeft(0) { (acc, curr) =>
      if (curr == neededChar) acc + 1 else acc
    }

    min <= occs && occs <= max
  }

  def isValidPart2(): Boolean = {
    val charA = password.charAt(min - 1)
    val charB = password.charAt(max - 1)
    
    charA != charB && (charA == neededChar || charB == neededChar)
  }
}
