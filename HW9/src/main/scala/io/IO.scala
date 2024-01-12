package io

import io.IO.{FlatMap, Pure, Suspended}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

sealed trait IO[+T] { self =>
  def map[E](f: T => E): IO[E] = flatMap(a => IO.pure(f(a)))
  def flatMap[E](f: T => IO[E]): IO[E] = FlatMap(self, f)
  def *>[E](another: IO[E]): IO[E] = flatMap(_ => another)
  def as[E](newValue: => E): IO[E] = IO(newValue)
  def void: IO[Unit] = map(_ => ())
  def attempt: IO[Either[Throwable, T]] = Try(run) match {
    case Success(value) => IO(Right(value))
    case Failure(fail)  => IO(Left(fail))
  }
  def option: IO[Option[T]] = redeem(_ => None, Some(_))
  def handleErrorWith[R >: T](handler: Throwable => IO[R]): IO[R] = attempt.flatMap {
    case Left(ex)     => handler(ex)
    case Right(value) => IO(value)
  }
  def redeem[E](recover: Throwable => E, map: T => E): IO[E] = attempt.map {
    case Left(ex)     => recover(ex)
    case Right(value) => map(value)
  }
  def redeemWith[E](recover: Throwable => IO[E], bind: T => IO[E]): IO[E] = attempt.flatMap {
    case Left(ex)     => recover(ex)
    case Right(value) => bind(value)
  }
  def unsafeRunSync(): T = run

  @tailrec
  private def run: T = runResult match {
    case Right(value) => value
    case Left(nested) => nested().run
  }
  private def runResult: Either[() => IO[T], T] = self match {
    case Pure(value)         => Right(value)
    case Suspended(function) => Left(function)
    case FlatMap(init, fun) =>
      init match {
        case Pure(t)                        => Left(() => fun(t))
        case Suspended(function)            => Left(() => FlatMap(function(), fun))
        case FlatMap(other_init, other_fun) => Left(() => other_init.flatMap(x => other_fun(x).flatMap(fun)))
      }
  }
}

object IO {
  final case class Pure[T](t: T) extends IO[T]

  final case class Suspended[T](function: () => IO[T]) extends IO[T]

  final case class FlatMap[T, E](init: IO[T], fun: T => IO[E]) extends IO[E]
  def apply[T](body: => T): IO[T] = Suspended(() => pure(body))
  def suspend[T](thunk: => IO[T]): IO[T] = Suspended(() => thunk)
  def delay[T](body: => T): IO[T] = apply(body)
  def pure[T](a: T): IO[T] = Pure(a)
  def fromEither[T](e: Either[Throwable, T]): IO[T] = e match {
    case Right(value)    => IO(value)
    case Left(exception) => IO.raiseError(exception)
  }
  def fromOption[T](option: Option[T])(orElse: => Throwable): IO[T] = option match {
    case Some(value) => IO(value)
    case None        => IO(throw orElse)
  }
  def fromTry[T](t: Try[T]): IO[T] = t match {
    case Success(value)     => IO(value)
    case Failure(exception) => IO.raiseError(exception)
  }
  def none[T]: IO[Option[T]] = IO(None)
  def raiseError[T](e: Throwable): IO[T] = IO(throw e)
  def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = if (cond) unit else IO(throw e)
  def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = raiseUnless(!cond)(e)
  def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) unit else action
  def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = unlessA(!cond)(action)
  val unit: IO[Unit] = IO(())
}
