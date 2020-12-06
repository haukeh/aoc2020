package aoc2020

import scala.io.Source

object Util {

  def readLines(file: String): List[String] =
    Source.fromFile(file).getLines.toList

  extension[T](xs: List[T]) {
    def partitionWhere(f: T => Boolean): List[List[T]] =
      xs.foldRight(List[List[T]]()) {
        case (elem, acc) if f(elem) => List(Nil) ++ acc
        case (elem, head :: tail)   => List(elem :: head) ++ tail
        case (elem, Nil)            => List(List(elem))
      }
  }
}
