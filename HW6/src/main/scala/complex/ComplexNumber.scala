package complex

// DO NOT CHANGE ANYTHING BELOW
final case class ComplexNumber(real: Double, imaginary: Double) {
  def *(other: ComplexNumber): ComplexNumber = // sorry, but there are warnings without type annotations
    ComplexNumber(
      // (a+bi)*(c+di) = (ac-bd)+(ad+bc)i <- i*i=-1
      // (real * other.real) - (imaginary * other.imaginary) is false
      (real * other.real) - (imaginary * other.imaginary),
      (real * other.imaginary) + (imaginary * other.real)
    )
  def +(other: ComplexNumber): ComplexNumber =
    ComplexNumber(real + other.real, imaginary + other.imaginary)
  def ~=(o: ComplexNumber): Boolean =
    (real - o.real).abs < 1e-6 && (imaginary - o.imaginary).abs < 1e-6
}

object ComplexNumber
// DO NOT CHANGE ANYTHING ABOVE
