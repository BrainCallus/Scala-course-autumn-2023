package complex

import scala.Double.NaN
import scala.annotation.tailrec
import scala.language.postfixOps

object ComplexNumberExtension {

  private[this] implicit class PositivePower(val x: Double) {
    def ^(deg: Int): Double = binPow(deg, x)

    @tailrec
    private def binPow(n: Int, init: Double, accum: Double = 1): Double =
      n match {
        case 0               => accum
        case 1               => accum * init
        case p if p % 2 == 0 => binPow(n / 2, init * init, accum)
        case _               => binPow((n - 1) / 2, init * init, accum * init)
      }
  }

  object ComplexNumericOperations extends Numeric[ComplexNumber] {
    override def plus(x: ComplexNumber, y: ComplexNumber): ComplexNumber = x.+(y)

    override def minus(x: ComplexNumber, y: ComplexNumber): ComplexNumber = x.--(y)

    override def times(x: ComplexNumber, y: ComplexNumber): ComplexNumber = x.*(y)

    override def negate(x: ComplexNumber): ComplexNumber = x negate

    override def fromInt(x: Int): ComplexNumber = ComplexNumber(x.doubleValue, 0.0)

    override def parseString(str: String): Option[ComplexNumber] = fromString(str)

    override def toInt(x: ComplexNumber): Int = x.asInt()

    override def toLong(x: ComplexNumber): Long = x.asLong()

    override def toFloat(x: ComplexNumber): Float = x.asFloat()

    override def toDouble(x: ComplexNumber): Double = x.asDouble()

    // throw is forbidden but we have no way to adequate compare to complex, beside comparing modules or angle
    // so comparing as ordered pairs (module, angle)
    override def compare(x: ComplexNumber, y: ComplexNumber): Int = x.compareTo(y)

    private def fromString(str: String): Option[ComplexNumber] = {
      def getRealOrIm(
        parts: Array[String],
        onContainI: String => Option[Double],
        onElse: String => Option[Double]
      ): Double =
        parts.foldLeft(0.0)((sum, str) =>
          (if (str.contains("i"))
             onContainI(str)
           else
             onElse(str)) match {
            case Some(d) => sum + d
            case None    => NaN
          }
        )

      val parts = str.replaceAll("( )+", "").split("[+]")
      val realPart = getRealOrIm(parts, _ => Some(0.0), str => str.toDoubleOption)
      val imPart = getRealOrIm(
        parts,
        str => str.replaceAll("i", "").toDoubleOption,
        _ => Some(0.0)
      )
      if (realPart == NaN || imPart == NaN)
        None
      else
        Some(ComplexNumber(realPart, imPart))
    }
  }

  trait ComplexNumberAdditionalOperations[Form] {
    def +(other: Form): Form
    def *(other: Form): Form
    def --(other: Form): Form

    def /(other: Form): Either[EvaluateException, Form]

    def negate: Form

    def conjugate: Form

    def modul: Double

    def angle: Double

    /** */

    /** n -th root is, accordingly, is power(1/n), n: Double note that result can be Nan if base and exp are both
      * negative and exp not integer
      *
      * @param exp
      *   exponent
      * @return
      */
    def power(exp: Double): Form

    /** Same with [[ComplexNumber.real]].toInt()
      * @return
      *   real part of ComplexNumber as Int
      */
    def asInt(): Int

    def asLong(): Long

    def asFloat(): Float

    def asDouble(): Double

    def compareTo(other: Form): Int

    def toPolarForm: ComplexPolar

    def toNormalForm: ComplexNumber
  }

  implicit class ComplexNumberAdditionalOperationsImpl(val selfNumber: ComplexNumber)
    extends ComplexNumberAdditionalOperations[ComplexNumber] {

    override def +(other: ComplexNumber): ComplexNumber = selfNumber.+(other)

    override def *(other: ComplexNumber): ComplexNumber = selfNumber.*(other)

    override def --(other: ComplexNumber): ComplexNumber =
      ComplexNumber(selfNumber.real - other.real, selfNumber.imaginary - other.imaginary)

    override def /(other: ComplexNumber): Either[EvaluateException, ComplexNumber] = {
      if (other isZero)
        Left(EvaluateException("Division by zero"))
      else {
        val num = selfNumber.*(other.conjugate)
        val denom = other.modul.^(2)
        Right(ComplexNumber(num.real / denom, num.imaginary / denom))
      }
    }

    override def negate: ComplexNumber = ComplexNumber(-selfNumber.real, -selfNumber.imaginary)

    override def conjugate: ComplexNumber = ComplexNumber(selfNumber.real, -selfNumber.imaginary)

    override def modul: Double = math.sqrt(selfNumber.real.^(2) + selfNumber.imaginary.^(2))

    override def angle: Double = {
      if (selfNumber.isZero) 0.0
      else math.acos(selfNumber.real / selfNumber.modul)
    }

    override def power(exp: Double): ComplexNumber = {
      val newMod = math.pow(selfNumber.modul, exp)
      ComplexNumber(newMod * math.cos(selfNumber.angle * exp), newMod * math.sin(selfNumber.angle * exp))
    }

    /** Same with [[ComplexNumber.real]].toInt()
      *
      * @return
      *   real part of ComplexNumber as Int
      */
    override def asInt(): Int = selfNumber.real.intValue

    override def asLong(): Long = selfNumber.real.longValue

    override def asFloat(): Float = selfNumber.real.floatValue

    override def asDouble(): Double = selfNumber.real.doubleValue

    override def compareTo(other: ComplexNumber): Int =
      if ((selfNumber modul) == (other modul))
        selfNumber.angle.compare(other.angle)
      else
        selfNumber.modul.compare(other.modul)

    override def toPolarForm: ComplexPolar = ComplexPolar(selfNumber modul, selfNumber angle)

    override def toNormalForm: ComplexNumber = selfNumber

    def isZero: Boolean = selfNumber.real.abs < 1e-6 && selfNumber.imaginary.abs < 1e-6
  }
}
