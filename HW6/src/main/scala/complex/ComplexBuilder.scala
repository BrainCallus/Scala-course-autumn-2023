package complex

import complex.ComplexNumberExtension.ComplexNumericOperations.plus

sealed trait ComplexBuilder[T] {
  def \+(other: T): ComplexNumber
  def \+(real: Double): ComplexNumber
  def i: ComplexNumber
}

object ComplexBuilder {
  implicit class FromDoubleBuilder(number: Double) extends ComplexBuilder[Double] {
    def \+(real: Double): ComplexNumber = plus(ComplexNumber(number, 0.0), ComplexNumber(real, 0.0))

    def \+(other: ComplexNumber): ComplexNumber = plus(ComplexNumber(number, 0.0), other)

    def i: ComplexNumber = ComplexNumber(0.0, number)
  }

  implicit class FromComplexBuilder(number: ComplexNumber) extends ComplexBuilder[ComplexNumber] {
    override def \+(real: Double): ComplexNumber = number.+(ComplexNumber(real, 0.0))

    override def \+(other: ComplexNumber): ComplexNumber = number.+(other)

    override def i: ComplexNumber = number.*(ComplexNumber(0.0, 1.0))
  }
}
