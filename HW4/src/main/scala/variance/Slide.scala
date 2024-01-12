package variance

trait Slide[+R] {
  def read: (Option[R], Slide[R])
}

class HelloSlide[R <: WordLine](val lines: Seq[R]) extends Slide[R] {
  override def read: (Option[R], HelloSlide[R]) = lines match {
    case x :: xs => (Some(x), new HelloSlide(xs))
    case _       => (None, new HelloSlide(Nil))
  }
}
