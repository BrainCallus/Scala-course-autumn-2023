package io

import scala.util.Random

object TestUtil {
  type IntOp = Int => Int

  val partOps: List[Int => IntOp] =
    List(
      (x: Int) => _ * x,
      (x: Int) => _ + x,
      (x: Int) => _ - x,
      (x: Int) => x - _,
      (x: Int) => _ / x
    )

  def randomSign: Int = if (Random.nextFloat() < 0.5) -1 else 1

  def randomIntFunction: IntOp =
    partOps(Random.nextInt(partOps.length))(Random.nextInt(100000) match {
      case 0 => -1 // escape DBZ
      case x => x * randomSign
    })

  def generateInt(constraint: Int = Int.MaxValue): Int = Random.nextInt(constraint) * randomSign

  def generateRandomIntList(n: Int): List[Int] =
    (0 until n map (_ => Random.nextInt(100000))).toList

  def generateRandomIntFunctionList(n: Int): List[IntOp] =
    (0 until n map { _ => randomIntFunction }).toList

  def generateIntIOFunctions(n: Int): List[Int => IO[Int]] =
    generateRandomIntFunctionList(n).map(f => (x: Int) => IO(f.apply(x)))

  private def genRandString = Random.nextString(Random.nextInt(64))

  def generateFail: IO[Nothing] = IO.raiseError(new RuntimeException(genRandString))

  def generateFailList(n: Int): List[IO[Nothing]] = (0 until n map { _ => generateFail }).toList
}
