package complex
import complex.ComplexNumberExtension.ComplexNumericOperations._
import complex.ComplexNumberExtension._

final case class ComplexPolar(modul: Double, angle: Double) extends ComplexNumberAdditionalOperations[ComplexPolar] {
  override def toNormalForm: ComplexNumber = ComplexNumber(modul * math.cos(angle), modul * math.sin(angle))

  override def toPolarForm: ComplexPolar = this

  override def +(other: ComplexPolar): ComplexPolar = applyOnNormal(plus, other)
  def *(other: ComplexPolar): ComplexPolar = applyOnNormal((t, o) => t * o, other)

  override def --(other: ComplexPolar): ComplexPolar =
    applyOnNormal(minus, other)

  override def /(other: ComplexPolar): Either[EvaluateException, ComplexPolar] =
    this.toNormalForm./(other.toNormalForm) match {
      case Left(err)    => Left(err)
      case Right(value) => Right(value.toPolarForm)
    }

  override def negate: ComplexPolar = applyOnNormal(c => c.negate)

  override def conjugate: ComplexPolar = applyOnNormal(c => c.conjugate)

  override def power(exp: Double): ComplexPolar = applyOnNormal(c => c.power(exp))

  /** Same with [[ComplexNumber.real]].toInt()
    *
    * @return
    *   real part of ComplexNumber as Int
    */
  override def asInt(): Int = this.toNormalForm.asInt()

  override def asLong(): Long = this.toNormalForm.asLong()

  override def asFloat(): Float = this.toNormalForm.asFloat()

  override def asDouble(): Double = this.toNormalForm.asDouble()

  override def compareTo(other: ComplexPolar): Int = this.toNormalForm.compareTo(other.toNormalForm)

  private def applyOnNormal(f: (ComplexNumber, ComplexNumber) => ComplexNumber, other: ComplexPolar): ComplexPolar =
    f(this.toNormalForm, other.toNormalForm).toPolarForm

  private def applyOnNormal(f: ComplexNumber => ComplexNumber): ComplexPolar = f(this.toNormalForm).toPolarForm
}
