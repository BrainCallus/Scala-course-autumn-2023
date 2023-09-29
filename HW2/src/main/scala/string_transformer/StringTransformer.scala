package string_transformer

import scala.annotation.tailrec

object StringTransformer {
  def transform(function: String => String)(s: String): String = function(s)

  def duplicate(s: String): String = s + s

  def duplicateConcat(s: String): String = s.concat(s)

  def cutHalf(s: String): String = s.substring(0, s.length / 2)

  def reverseNaive(s: String): String =
    if (s.length == 1) s
    else {
      reverseNaive(s.substring(s.length / 2)) ++ reverseNaive(cutHalf(s))
    }

  def reverseTail(s: String): String = reverseTailHelper(s)

  @tailrec
  private def reverseTailHelper(s: String, newS: StringBuffer = new StringBuffer()): String = {
    if (s.isEmpty) newS.toString
    else reverseTailHelper(s.init, newS append s.charAt(s.length - 1))
  }

  def reverseActual(s: String): String = reverseActualHelper(s)

  @tailrec
  private def reverseActualHelper(s: String, newS: StringBuffer = new StringBuffer(), index: Int = 0): String = {
    if (index == s.length) newS.toString
    else reverseActualHelper(s, newS append s.charAt(s.length - 1 - index), index + 1)
  }

}
