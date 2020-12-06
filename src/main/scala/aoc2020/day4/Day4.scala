package aoc2020.day4

import aoc2020.Util._

import scala.util.Try
import scala.util.matching.Regex

enum PassportFieldKey {
  case Iyr
  case Byr
  case Eyr
  case Ecl
  case Hgt
  case Hcl
  case Pid
  case Cid
}

object Day4 {

  import aoc2020.day4.PassportFieldKey._

  val PassportPattern = "(\\S+):(\\S+)".r
  val ColorPattern = "^#([a-f0-9]{6})$".r
  val PidPattern = "^[0-9]{9}$".r

  case class Acc(
      val idx: Int = 0,
      val passports: Map[Int, List[String]] = Map(0 -> List())
  )

  case class RawPassportField(val key: String, val value: String)

  case class PassportField(val key: PassportFieldKey, val value: String) {
    def isValid = key match {
      case Byr => Try(value.toInt).filter(byr => 1920 <= byr && byr <= 2002).isSuccess
      case Iyr => Try(value.toInt).filter(iyr => 2010 <= iyr && iyr <= 2020).isSuccess
      case Eyr => Try(value.toInt).filter(eyr => 2020 <= eyr && eyr <= 2030).isSuccess
      case Hgt =>
        value match {
          case s"${hgt}cm" => Try(hgt.toInt).filter(h => 150 <= h && h <= 193).isSuccess
          case s"${hgt}in" => Try(hgt.toInt).filter(h => 59 <= h && h <= 76).isSuccess
          case _ => false
        }
      case Hcl => ColorPattern.matches(value)
      case Ecl => value match {
        case "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" => true
        case _ => false
      }
      case Pid => PidPattern.matches(value)
      case Cid => true
    }
  }

  object PassportField {
    def fromRaw(raw: RawPassportField): Option[PassportField] = {
      raw match {
        case RawPassportField("iyr", value)  => Some(PassportField(Iyr, value))
        case RawPassportField("byr", value) => Some(PassportField(Byr, value))
        case RawPassportField("eyr", value) => Some(PassportField(Eyr, value))
        case RawPassportField("ecl", value) => Some(PassportField(Ecl, value))
        case RawPassportField("hgt", value) => Some(PassportField(Hgt, value))
        case RawPassportField("pid", value) => Some(PassportField(Pid, value))
        case RawPassportField("hcl", value) => Some(PassportField(Hcl, value))
        case RawPassportField("cid", value) => Some(PassportField(Cid, value))
        case _ => None
      }
    }
  }

  case class Passport(val fields: Seq[PassportField]) {
    def isValidPart1 = {
      val diff = (Passport.allFields diff fields.map(_.key).toSet)
      diff.isEmpty || diff == Set(Cid)
    }

    def isValidPart2 = isValidPart1 && fields.forall(_.isValid)
  }

  object  Passport {
    val allFields = Set(PassportFieldKey.values: _*)
  }

  def parseRaw: Regex.MatchIterator => Seq[RawPassportField] =
    iter =>
      iter.matchData
        .map(matcher => RawPassportField(matcher.group(1), matcher.group(2)))
        .toList

  def parse: Seq[RawPassportField] => Passport = raw =>
      Passport(raw.map(PassportField.fromRaw).flatten)


  def main(args: Array[String]): Unit = {
    val Acc(_, passportStrings) = readLines("input/day4.txt").foldLeft(Acc()) {
      (acc, line) =>
        if (line.trim().isEmpty) acc.copy(idx = acc.idx + 1)
        else
          acc.copy(passports = acc.passports.updatedWith(acc.idx) {
            case Some(value) => Some(value :+ line)
            case None        => Some(List(line))
          })
    }

    val passports = passportStrings
      .map {
        case (_, l) => l.map(_.trim).mkString(" ")
      }
      .map(PassportPattern.findAllIn)
      .map(parseRaw andThen parse)

    val p1 = passports.filter(_.isValidPart1)
    val p2 = passports.filter(_.isValidPart2)

    println(s"p1: ${p1.size}")
    println(s"p2: ${p2.size}")
  }

}
