package collections

import scala.annotation.tailrec

object Collections {

  /*
    In a sorted list find all pairs of two neighbor numbers which have a gap between them
    None for Seq(1, 2, 3, 4)
    Some(Seq((2, 8))) for Seq(1, 2, 8)
    Some(Seq((3, 5), (5, 7))) for Seq(3, 5, 7)
   */
  def findGaps(seq: Seq[Int]): Option[Seq[(Int, Int)]] = {
    val nSeq = seq
      .dropRight(1)
      .zip(seq.tail)
      .foldRight(Seq.empty[(Int, Int)])((pair, s) =>
        if (math.abs(pair._2 - pair._1) > 1) pair +: s // sorting order not specified
        else s
      )
    if (nSeq.isEmpty) None
    else Some(nSeq)
  }

  /*
    Find key-value pair with the minimum value in the map
    try to implement min in different ways (fold, reduce, recursion)
   */
  def minFold(map: Map[String, Int]): Option[(String, Int)] = {
    if (map.isEmpty) None
    Some(
      map.fold(("", Int.MaxValue))(
        minByValue
      )
    )

  }

  def minReduce(map: Map[String, Int]): Option[(String, Int)] = {
    map.reduceOption(minByValue)
  }

  def minRecursion(map: Map[String, Int]): Option[(String, Int)] =
    if (map.isEmpty) None
    else {
      Some(
        minRecursionHelper(map.tail, map.head)
      )
    }

  @tailrec
  private def minRecursionHelper(map: Map[String, Int], current: (String, Int)): (String, Int) =
    if (map.isEmpty)
      current
    else {
      minRecursionHelper(map.tail, minByValue(map.head, current))
    }

  private def minByValue(tuple1: (String, Int), tuple2: (String, Int)) =
    if (tuple1._2 < tuple2._2) tuple1 else tuple2

  // Implement scanLeft - running total, applying [f] to elements of [list] (not using scans ofc)
  def scanLeft[T](zero: T)(list: Seq[T])(f: (T, T) => T): Seq[T] = {
    if (list.isEmpty)
      Seq(zero)
    else {
      zero +: scanLeft(f(zero, list.head))(list.tail)(f)
    }
  }

  // Count the consistent occurences of each character in the string
  def count(s: String): List[(Char, Int)] = {
    s.groupBy(ch => ch).view.mapValues(v => v.length).toList
  }
}
