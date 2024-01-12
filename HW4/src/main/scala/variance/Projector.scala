package variance

import scala.annotation.tailrec

class Projector[-R](converter: Converter[R]) {

  @tailrec
  private def projectHelper(screen: Slide[R], accum: String): String = {
    screen.read match {
      case (Some(text), nextSlide) => projectHelper(nextSlide, accum + converter.convert(text))
      case (None, _)               => accum
    }
  }
  def project(screen: Slide[R]): String =
    projectHelper(screen, "")
}
