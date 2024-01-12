package task2
import cats.Eval

import scala.annotation.tailrec

object EvalTricks {
  def fibonacci(limit: Long): BigInt = {
    @tailrec
    def getFibonacciNumber(limit: Long, n: Long, fib_n_prev: BigInt, result: BigInt): BigInt =
      n match {
        case x if x == limit => result + fib_n_prev
        case 1               => getFibonacciNumber(limit, 2, 0, 1)
        case _               => getFibonacciNumber(limit, n + 1, result, result + fib_n_prev)
      }
    if (limit == 1) {
      1
    } else {
      getFibonacciNumber(limit, 0, 0, 0)
    }
  }

  def fib(n: Int): BigInt = {
    def internalFib(n: Int): Eval[(BigInt, BigInt)] = {
      n match {
        case 0 => Eval.now((0, 0))
        case 1 => Eval.now((0, 1))
        case _ => Eval.defer(internalFib(n - 1)).map { case (x, y) => (y, x + y) }
      }
    }
    internalFib(n).value._2
  }

  def foldRight[A, B](list: List[A], accum: B)(fun: (A, B) => B): B = {
    def internalFoldr(lst: List[A], acc: Eval[B]): Eval[B] = {
      lst match {
        case Nil     => acc
        case x :: xs => Eval.defer(internalFoldr(xs, acc.map(fun(x, _))))
      }
    }
    internalFoldr(list, Eval.now(accum)).value
  }
}
