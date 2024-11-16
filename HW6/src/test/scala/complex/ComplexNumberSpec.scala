package complex

import complex.ComplexBuilder._
import complex.ComplexNumberExtension.ComplexNumberAdditionalOperationsImpl
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class ComplexNumberSpec extends AnyFlatSpec with Matchers {

  "toPolarForm" should "return polar form of the complex number" in {
    val cn1 = ComplexNumber(0, 0)
    val polar1 = cn1.toPolarForm
    assert(polar1.angle == 0.0)
    assert(polar1.modul == 0.0)
    assert(polar1.modul - cn1.modul < 1e-6)
    assert(polar1.angle - cn1.angle < 1e-6)
    val cn2 = ComplexNumber(math.sqrt(2), -1.0)
    val polar2 = cn2.toPolarForm
    assert(polar2.modul - math.sqrt(3) < 1e-6)
    assert(polar2.angle - math.Pi / 3 < 1e-6)
    assert(polar2.modul - cn2.modul < 1e-6)
    assert(polar2.angle - cn2.angle < 1e-6)
  }

  "ComplexPolar +" should "be equal to ComplexNumber +" in {
    val polar1 = ComplexPolar(3.0, math.Pi / 2)
    val polar2 = ComplexPolar(2.5, -math.Pi / 4)
    val mulPolar = polar1.+(polar2)
    val cn1 = ComplexNumber(0, 3.0)
    val cn2 = ComplexNumber(1.25 * math.sqrt(2), -1.25 * math.sqrt(2))
    val mulNorm = cn1.+(cn2)
    assert(mulPolar.toNormalForm.real - mulNorm.real < 1e-6)
    assert(mulPolar.toNormalForm.imaginary - mulNorm.imaginary < 1e-6)
  }

  "ComplexPolar *" should "be equal to ComplexNumber *" in {
    val polar1 = ComplexPolar(3.0, math.Pi / 2)
    val polar2 = ComplexPolar(2.5, -math.Pi / 4)
    val mulPolar = polar1.*(polar2)
    val cn1 = ComplexNumber(0, 3.0)
    val cn2 = ComplexNumber(1.25 * math.sqrt(2), -1.25 * math.sqrt(2))
    val mulNorm = cn1.*(cn2)
    assert(mulPolar.toNormalForm.real - mulNorm.real < 1e-6)
    assert(mulPolar.toNormalForm.imaginary - mulNorm.imaginary < 1e-6)
  }

  "ComplexPolar --" should "be equal to ComplexNumber --" in {
    val polar1 = ComplexPolar(3.0, math.Pi / 2)
    val polar2 = ComplexPolar(2.5, -math.Pi / 4)
    val mulPolar = polar1.--(polar2)
    val cn1 = ComplexNumber(0, 3.0)
    val cn2 = ComplexNumber(1.25 * math.sqrt(2), -1.25 * math.sqrt(2))
    val mulNorm = cn1.--(cn2)
    assert(mulPolar.toNormalForm.real - mulNorm.real < 1e-6)
    assert(mulPolar.toNormalForm.imaginary - mulNorm.imaginary < 1e-6)
  }

  "ComplexPolar /" should "be equal to ComplexNumber /" in {
    val polar1 = ComplexPolar(3.0, math.Pi / 2)
    val polar2 = ComplexPolar(2.5, -math.Pi / 4)
    val mulPolar = polar1./(polar2)
    val cn1 = ComplexNumber(0, 3.0)
    val cn2 = ComplexNumber(1.25 * math.sqrt(2), -1.25 * math.sqrt(2))
    val mulNorm = cn1./(cn2)
    assert(mulPolar.toOption.get.toNormalForm.real - mulNorm.toOption.get.real < 1e-6)
    assert(mulPolar.toOption.get.toNormalForm.imaginary - mulNorm.toOption.get.imaginary < 1e-6)
  }

  "/" should "return EvaluateException on division by zero" in {
    val cn1 = ComplexNumber(99.3, 1999.2456)
    val cn2 = ComplexPolar(0.0, math.Pi / 65)
    assert(cn2.toNormalForm.isZero)
    assert(cn1.toPolarForm./(cn2) match {
      case Left(_)  => true
      case Right(_) => false
    })
  }

  "ComplexPolar negate" should "be equal to ComplexNumber negate" in {
    val complexNumber = ComplexNumber(1.8, -99)
    val polar = complexNumber.toPolarForm
    assertResult(complexNumber.negate.toPolarForm)(polar.negate)
  }

  "ComplexBuilder" should "construct ComplexNumber in normal form with syntax .i .\\+" in {
    val cn = 2.3 \+ 9.i
    assert(cn.real == 2.3)
    assert(cn.imaginary == 9)
    val cn2 = (2.3 \+ 9.i).i \+ 1.5.i
    assert(cn2.real == -9)
    assert(cn2.imaginary == 3.8)
    val realOnly = 10.0 \+ 22.1
    assert(realOnly.real == 32.1)
    assert(realOnly.imaginary == 0.0)
    val imOnly = (4.i.i \+ -20).i
    assert(imOnly.real == 0.0)
    assert(imOnly.imaginary == -24.0)
  }
}
