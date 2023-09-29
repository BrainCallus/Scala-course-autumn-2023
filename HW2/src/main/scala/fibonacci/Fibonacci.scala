package fibonacci

import scala.annotation.tailrec

object Fibonacci {

  def fibonacci(limit: Long, acc: BigInt = 0): BigInt = {
    getFibonacciNumber(limit, 0, 0, 0)
  }

  @tailrec
  private def getFibonacciNumber(limit: Long, n: Long, fib_n_prev: BigInt, result: BigInt): BigInt = n match {
    case x if x == limit => result + fib_n_prev
    case 1               => getFibonacciNumber(limit, 2, 0, 1)
    case _               => getFibonacciNumber(limit, n + 1, result, result + fib_n_prev)
  }
}
