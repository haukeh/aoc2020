package aoc2020.day7

import aoc2020.Util._

object Day7 {

  val SingleBagPattern = raw"(\d) (\w+ \w+) bags?".r
  val Pattern =
    raw"(\w+ \w+) bags contain (\d \w+ \w+ bags?|no other bags)((?:, \d \w+ \w+ bags?)*)\.".r

  private def parseRow(
      bagName: String,
      maybeFirst: String,
      maybeRest: String
  ): (String, List[(Int, String)]) = {

    val firstChild = maybeFirst.trim match {
      case "no other bags"             => None
      case SingleBagPattern(num, name) => Some(num.toInt -> name)
    }

    val otherChildren = Option(maybeRest).map {
      _.split(',').tail
        .map(_.replace(',', ' ').trim)
        .map {
          case SingleBagPattern(num, name) => num.toInt -> name
        }
        .toList
    }

    bagName ->
      firstChild.fold(List()) {
        List(_) ++ otherChildren.getOrElse(Nil)
      }
  }

  def main(args: Array[String]): Unit = {
    val bagHierarchy: Map[String, List[(Int, String)]] =
      readLines("input/day7.txt").map {
        case Pattern(bagName, maybeFirst, maybeRest) =>
          parseRow(bagName, maybeFirst, maybeRest)
      }.toMap

    def canContainGoldBag(bag: String): Boolean = {
      val children = bagHierarchy.getOrElse(bag, Nil).map(_._2)
      children.contains("shiny gold") || children.exists(canContainGoldBag(_))
    }

    def coundNested(bag: String)(id: Int): Int =
      bagHierarchy.getOrElse(bag, Nil).foldLeft(id) {
        case (acc, (num, children)) => acc + num * coundNested(children)(1)
      }

    val p1 = bagHierarchy.keys.count(canContainGoldBag)
    val p2 = coundNested("shiny gold")(0)

    println(p1)
    println(p2)
  }
}
