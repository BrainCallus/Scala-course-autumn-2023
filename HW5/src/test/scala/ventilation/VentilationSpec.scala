package ventilation

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ventilation.Ventilation.{ventilationList, ventilationONk}

class VentilationSpec extends AnyFlatSpec with Matchers {
  "ventilationList" should "return list of (N-k) maximal degrees" in {
    assertResult(List(2, 3, 4))(ventilationList(List(1, 2, 3, 4), 2))
    assertResult(List.empty[Int])(ventilationList(List.empty[Int], 2))
    assertResult(List(9, 9, 8))(ventilationList(List(2, 9, 7, 8), 2))
  }

  "ventilationONk" should "return list of (N-k) maximal degrees" in {
    assertResult(List(2, 3, 4))(ventilationONk(List(1, 2, 3, 4), 2))
    assertResult(List.empty[Int])(ventilationONk(List.empty[Int], 2))
    assertResult(List(9, 9, 8))(ventilationONk(List(2, 9, 7, 8), 2))
  }

}
