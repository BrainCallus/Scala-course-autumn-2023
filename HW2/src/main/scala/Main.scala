import house.house_exception
import fibonacci.Fibonacci.fibonacci
import string_transformer.StringTransformer.{duplicateConcat, duplicate, reverseNaive, reverseTail, reverseActual}

import scala.io.Source
import scala.util.Using

object Main {
  def sum3: Int => Int => Int => Long = x => y => z => x + y + z
  def main(args: Array[String]): Unit = {
    // println(fibonacci(1))
    // println(fibonacci(0))
    // println(duplicate("aaa"))
    // println(fibonacci(10000))
    // Using (Source.fromFile("theGift_chapter1.txt")) { file =>
    //   val longText = file.getLines().mkString
    //   val start: Long = System.currentTimeMillis()
    //   duplicate(longText)
    //   println("\"reverse\" took " + (System.currentTimeMillis() - start) + " ms")
    //   val start2 = System.currentTimeMillis()
    //   dup(longText)
    //   println("\"reverse\" took " + (System.currentTimeMillis() - start2) + " ms")
//
    //   val sysStart = System.currentTimeMillis()
    //   longText.reverse
    //   println("\"system reverse\" took " + (System.currentTimeMillis() - sysStart) + " ms")
    // }
    println(sum3(1)(1)(1))
    println(sum3(1).toString())

    println(reverseTail("abcde"))
    println(reverseActual("abcde"))


  }
}
