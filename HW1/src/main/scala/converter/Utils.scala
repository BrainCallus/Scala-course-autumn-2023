package converter

/** object contains common used methods
  */
object Utils {

  /** class for ternary operator
    *
    * @param condition
    *   boolean statement defines [[converter.Utils.Ternary#??]] result
    * @tparam T
    *   type of [[converter.Utils.Ternary#??]] return value
    */
  implicit class Ternary[T](condition: Boolean) {

    /** ternary operator
      *
      * @param a
      *   value returned if [[converter.Utils.Ternary#condition]] was met
      * @param b
      *   value returned if [[converter.Utils.Ternary#condition]] wasn't met
      * @return
      *   depending of [[converter.Utils.Ternary#condition]] value
      */
    def ??(a: => T, b: => T): T = if (condition) a else b
  }

  /** Transforms given collection of strings delimiting them with `delimiter`
    *
    * {{{
    *   stringJoin(List("abc", "dog", "apple"), "::") -> "abc::dog::apple"
    * }}}
    *
    * @param strings
    *   collection of strings extends [[scala.collection.Iterable]][String]
    * @param delimiter
    *   string delimiter
    * @return
    *   string consists of elements from given collection delimited with `delimiter`
    */
  def stringJoin(strings: Iterable[String], delimiter: String): String =
    strings.mkString(delimiter)

  /** Computes levenshtein distance between two strings
    *
    * Given strings can't be null
    *
    * @param fst
    *   first string
    * @param snd
    *   second string
    * @return
    *   levenshtein distance between strings
    */
  def levenshteinDistance(fst: String, snd: String): Long = {
    val a: String = " " + fst
    val b: String = " " + snd
    val lengthA = a.length
    val lengthB = b.length
    val changes = Array.fill(lengthA + 1) {
      Array.fill(lengthB + 1) {
        0L
      }
    }
    for (i <- 0 to lengthA) {
      changes(i)(0) = i
    }

    for (i <- 1 to lengthB) {
      changes(0)(i) = i
    }

    for (i <- 1 until lengthA) {
      for (j <- 1 until lengthB) {
        changes(i)(j) = (a.charAt(i) == b.charAt(j)) ??
          (changes(i - 1)(j - 1), minTriple(changes(i - 1)(j - 1), changes(i)(j - 1), changes(i - 1)(j)) + 1)
      }
    }
    changes(lengthA - 1)(lengthB - 1)
  }

  /** inverts returnable Boolean value of given method
    *
    * @param f
    *   function which result type is Boolean
    * @tparam T
    *   type of arguments applied by 'f
    * @return
    *   same to given function with inverted result
    */
  def not[T](f: T => Boolean): T => Boolean = !f.apply(_)

  /** Replaces map keys from String to another types with specified `mapper` function
    *
    * @param stringKeyMap
    *   initial map which keys are strings
    * @param mapper
    *   function transforms String to NewKeyType
    * @tparam NewKeyType
    *   type of new keys
    * @tparam ValueType
    *   type of map values
    * @return
    *   map with replaced keys
    */
  def mapStringKeyMapByKey[NewKeyType, ValueType](
    stringKeyMap: Map[String, ValueType],
    mapper: String => NewKeyType
  ): Map[NewKeyType, ValueType] =
    stringKeyMap map (entry => (mapper.apply(entry._1), entry._2))

  /** calls given method and throws given exception if returned value is true
    *
    * @param exceptionCondition
    *   method called to check
    * @param returnIfSuccess
    *   value returned if condition wasn't met
    * @param exception
    *   throws if condition was met
    * @tparam T
    *   type of exceptionCondition
    * @return
    */
  def testOrThrow[T](exceptionCondition: T => Boolean, returnIfSuccess: T, exception: Exception): T = {
    if (exceptionCondition(returnIfSuccess)) throw exception
    returnIfSuccess
  }

  private def minTriple(a: Long, b: Long, c: Long): Long = a.min(b).min(c)
}
