package collections

import collections.Collections._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CollectionsSpec extends AnyFlatSpec with Matchers {
  "findGaps" should "find all pairs with gaps" in {
    findGaps(Seq(1, 2, 3)) shouldEqual None
    findGaps(Seq(3, 5, 7)) shouldEqual Some(Seq((3, 5), (5, 7)))
    findGaps(Seq(1, 2, 8)) shouldEqual Some(Seq((2, 8)))
  }

  "minFold" should "return pair with minimum value" in {
    val map: Map[String, Int] = Map(("abc", 0), ("d19dj", 6), ("abracadabra", -1))
    minFold(map) shouldEqual Some("abracadabra", -1)
  }

  "minReduce" should "return pair with minimum value" in {
    val map: Map[String, Int] = Map(("abc", 0), ("d19dj", 6), ("abracadabra", -1))
    minReduce(map) shouldEqual Some("abracadabra", -1)
  }

  "minRecursion" should "return pair with minimum value" in {
    val map: Map[String, Int] = Map(("abc", 0), ("d19dj", 6), ("abracadabra", -1))
    minRecursion(map) shouldEqual Some("abracadabra", -1)
  }

  "scanLeft" should "return pair vit minimum value" in {
    scanLeft[Int](2)(Seq[Int](1, 2, 3))(_ + _) shouldEqual Seq[Int](2, 3, 5, 8)
  }

  "count" should "count the consistent occurences of each character in the string" in {
    count("look for area") shouldEqual List(
      ('e', 1),
      ('f', 1),
      ('a', 2),
      (' ', 2),
      ('l', 1),
      ('r', 2),
      ('k', 1),
      ('o', 3)
    )
  }
}
