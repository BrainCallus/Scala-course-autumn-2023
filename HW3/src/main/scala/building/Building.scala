package building

/** Здание должно иметь:
  *   - строковый адрес
  *   - этажи (сходящиеся к первому этажу) Этаж может быть жилым, коммерческим, либо чердаком (который сам может быть
  *     коммерческим). На каждом жилом этаже живет 2 человека и есть лестница(ссылка) ведущая на следующий этаж У
  *     каждого человека есть возраст (>0) и пол На коммерческом этаже может быть несколько заведений (используйте
  *     Array), но не меньше 1. Здание всегда должно заканчиваться чердаком На чердаке никто не живет, но это может быть
  *     и коммерческое помещение (но только 1).
  */
trait Commercial // rewrite me

trait Building // rewrite me

trait Floor // rewrite me

trait Attic // rewrite me

trait ResidentialFloor // rewrite me

object Building {

  /** Проходится по зданию снизу в вверх, применяя функцию [[f]] на каждом жилом этаже с начальным аккумулятором
    * [[accumulator]]
    */
  def fold(building: Building, accumulator: Int)(f: (Int, ResidentialFloor) => Int): Int = ???

  /** Подсчитывает количество этаже, на которых живет хотя бы один мужчина старше [[olderThan]]. Используйте [[fold]]
    */
  def countOldManFloors(building: Building, olderThan: Int): Int = ???

  /** Находит наибольший возраст женьщины, проживающей в здании. Используйте [[fold]] */
  def womanMaxAge(building: Building): Int = ???

  /** Находит кол-во коммерческих заведений в здании. Используйте [[fold]] */
  def countCommercial(building: Building): Int = ???

  /* Находит среднее кол-во коммерческих заведений зданиях. Реализуйте свою функцию, похожую на [[fold]] для прохода по зданию */
  def countCommercialAvg(building: Array[Building]): Double = ???

  /* Находит среднее кол-во мужчин на четных этажах. Реализуйте свою функцию, похожую на [[fold]] для прохода по зданию */
  def evenFloorsMenAvg(building: Building): Double = ???
}
