

import complex.ComplexBuilder._
import complex.ComplexNumber
import complex.ComplexNumberExtension.ComplexNumericOperations._
import complex.ComplexNumberExtension._

import scala.language.postfixOps

object Main {
  def main(args: Array[String]): Unit = {
    val builded: ComplexNumber = 2.i
    val bb=(2.3\+9.i).i \+ 9.i

    println("  f    +  - 0     + 2i  ".replaceAll("( )+", "").split("[+]").mkString("Array(", ", ", ")"))
  }
}
