package io

import io.TestUtil._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import java.io.{ByteArrayOutputStream, PrintStream}
import scala.util.{Success, Try}

class IOSpec extends AnyFlatSpec with Matchers {
  val DEFAULT_TEST_SIZE = 100
  val stressConst: BigInt = BigInt(DEFAULT_TEST_SIZE ^ 10)

  "unsaveRunSync" should "return value correspond IO" in {
    val listInt = List(0, -10, 92, 10101, 2000, -923374764)
    for (i <- listInt) {
      assertResult(i)(IO(i).unsafeRunSync())
    }
    assertResult(listInt)(IO(listInt).unsafeRunSync())
  }

  it should "return given function" in {
    val listString = List("abc", "123", "lolkek", "ыаыаыаыаыаыаыа")
    for (pair <- listString map (s => (s, () => s))) {
      val io = IO(pair._2)
      assertResult(pair._1)(io.unsafeRunSync().apply())
    }
  }

  it should "returns result of the suspended call" in {
    val listString = List("abc", "123", "lolkek", "ыаыаыаыаыаыаыа")
    for (pair <- listString map (s => (s, IO(s), () => IO(s)))) {
      val io = IO(pair._2)
      val ioFn = IO(pair._3)
      assertResult(pair._1)(io.unsafeRunSync().unsafeRunSync())
      assertResult(pair._1)(ioFn.unsafeRunSync().apply().unsafeRunSync())
    }
  }

  "map" should "apply given function to IO value" in {
    val ints = generateRandomIntList(DEFAULT_TEST_SIZE)
    val ios = ints map (x => IO(x))
    val functions = generateRandomIntFunctionList(DEFAULT_TEST_SIZE)
    for (i <- 0 until DEFAULT_TEST_SIZE) {
      for (j <- 0 until DEFAULT_TEST_SIZE) {
        assertResult(functions(j)(ints(i)))(ios(i).map(functions(j)).unsafeRunSync())
      }
    }
  }

  it should "not fail on large computation" in {
    val io = Range.BigInt(1, stressConst, 1).foldLeft(IO(BigInt(0)))((acc, i) => acc.map(x => x + i))
    assertResult(stressConst * (stressConst - 1) / 2)(io.unsafeRunSync())
  }

  "flatMap" should "apply given function to IO value" in {
    val ints = generateRandomIntList(DEFAULT_TEST_SIZE)
    val ios = ints map (x => IO(x))
    val fMapFuncs = generateIntIOFunctions(DEFAULT_TEST_SIZE)
    for (i <- 0 until DEFAULT_TEST_SIZE) {
      for (j <- 0 until DEFAULT_TEST_SIZE) {
        assertResult(fMapFuncs(j)(ints(i)).unsafeRunSync())(ios(i).flatMap(fMapFuncs(j)).unsafeRunSync())
      }
    }
  }

  it should "not fail on large computation" in {
    val io = Range.BigInt(1, stressConst, 1).foldLeft(IO(BigInt(0)))((acc, i) => acc.flatMap(x => IO(x + i)))
    assertResult(stressConst * (stressConst - 1) / 2)(io.unsafeRunSync())
  }

  "*>" should "skip initial IO and return given" in {
    val initIO = IO("dlkfldklf")
    for (intIOFun <- generateRandomIntList(DEFAULT_TEST_SIZE).zip(generateIntIOFunctions(DEFAULT_TEST_SIZE))) {
      assertResult(intIOFun._1)((initIO *> IO(intIOFun._1)).unsafeRunSync())
      assertResult(intIOFun._2(intIOFun._1).unsafeRunSync())(
        initIO.*>(IO(intIOFun._1).flatMap(intIOFun._2)).unsafeRunSync()
      )
    }
  }

  "as" should "skip initial IO value and return IO with given value" in {
    val initIO = IO("dlkfldklf")
    for (intIOFun <- generateRandomIntList(DEFAULT_TEST_SIZE).zip(generateIntIOFunctions(DEFAULT_TEST_SIZE))) {
      assertResult(intIOFun._1)((initIO as intIOFun._1).unsafeRunSync())
      assertResult(intIOFun._2(intIOFun._1).unsafeRunSync())(
        initIO.as(intIOFun._1).flatMap(intIOFun._2).unsafeRunSync()
      )
    }
  }

  "void" should "return IO[Unit]" in {
    for (ioFun <- generateIntIOFunctions(DEFAULT_TEST_SIZE)) {
      assertResult(())(ioFun(generateInt()).void.unsafeRunSync())
    }
  }

  "attempt" should "wrap fail success or fail of the run" in {
    for (int <- generateRandomIntList(DEFAULT_TEST_SIZE)) {
      assert(
        IO(int).attempt.unsafeRunSync() match {
          case Right(res) => res == int
          case _          => false
        }
      )
    }

    for (exceptIO <- generateFailList(DEFAULT_TEST_SIZE)) {
      assert(
        exceptIO.attempt.unsafeRunSync() match {
          case Left(_) => true
          case _       => false
        }
      )
    }
  }

  "option" should "wrap value to Option" in {
    for (int <- generateRandomIntList(DEFAULT_TEST_SIZE)) {
      assert(
        IO(int).option.unsafeRunSync() match {
          case Some(res) => res == int
          case _         => false
        }
      )
    }

    for (exceptIO <- generateFailList(DEFAULT_TEST_SIZE)) {
      assert(
        exceptIO.option.unsafeRunSync() match {
          case None => true
          case _    => false
        }
      )
    }
  }

  "handleErrorWith" should "handle error replacing with result of function-handler" in {
    for (fail <- generateFailList(DEFAULT_TEST_SIZE)) {
      val handleValue = generateInt()
      assertResult(handleValue)(fail.handleErrorWith(_ => IO(handleValue)).unsafeRunSync())
    }
  }

  "redeem" should "recover result in case failure or map with function in case success" in {
    for (int <- generateRandomIntList(DEFAULT_TEST_SIZE)) {
      for (mapper <- generateRandomIntFunctionList(DEFAULT_TEST_SIZE)) {
        assertResult(mapper(int))(IO(int).redeem(_ => "fail", mapper).unsafeRunSync())
      }
    }

    for (exceptIO <- generateFailList(DEFAULT_TEST_SIZE)) {
      assertResult("success")(exceptIO.redeem(_ => "success", _ => "fail").unsafeRunSync())
    }
  }

  "redeemWith" should "recover result in case failure or map with function to IO in case success" in {
    for (int <- generateRandomIntList(DEFAULT_TEST_SIZE)) {
      for (mapper <- generateIntIOFunctions(DEFAULT_TEST_SIZE)) {
        assertResult(mapper(int).unsafeRunSync())(IO(int).redeemWith(_ => IO("fail"), mapper).unsafeRunSync())
      }
    }

    for (exceptIO <- generateFailList(DEFAULT_TEST_SIZE)) {
      assertResult("success")(exceptIO.redeemWith(_ => IO("success"), _ => IO("fail")).unsafeRunSync())
    }
  }

  "suspend" should "delay computation" in {
    for (int <- generateRandomIntList(DEFAULT_TEST_SIZE)) {
      val io = IO.suspend(IO(int))
      assertResult(int)(io.unsafeRunSync())
    }
  }

  "delay" should "delay computation" in {
    for (int <- generateRandomIntList(DEFAULT_TEST_SIZE)) {
      for (func <- generateRandomIntFunctionList(DEFAULT_TEST_SIZE)) {
        val outStream = new ByteArrayOutputStream()
        val printStream = new PrintStream(outStream)
        val io = IO.delay { Console.withOut(printStream) { print(func(int)) } }
        assertResult("")(outStream.toString)
        io.unsafeRunSync()
        assertResult(func(int).toString)(outStream.toString)
      }
    }
  }

  "pure" should "return given value" in {
    for (int <- generateRandomIntList(DEFAULT_TEST_SIZE)) {
      assertResult(int)(IO.pure(int).unsafeRunSync())
    }
  }

  "fromEither" should "unwrap Either value" in {
    for (int <- generateRandomIntList(DEFAULT_TEST_SIZE)) {
      val io = IO.fromEither(Right(int))
      assertResult(int)(io.unsafeRunSync())
    }

    assertResult("success")(
      IO.fromEither(Left(new Exception("exception"))).handleErrorWith(_ => IO("success")).unsafeRunSync()
    )

  }

  "fromOption" should "unwrap Option value" in {
    for (int <- generateRandomIntList(DEFAULT_TEST_SIZE)) {
      val io = IO.fromOption(Some(int))(new Exception("fail"))
      assertResult(int)(io.unsafeRunSync())
    }

    assertResult("exception")(
      IO.fromOption(None)(new Exception("exception")).handleErrorWith(e => IO(e.getMessage)).unsafeRunSync()
    )
  }

  "fromTry" should "return result if try was success ot raise error with which it failed" in {
    for (int <- generateRandomIntList(DEFAULT_TEST_SIZE)) {
      assertResult(int)(IO.fromTry(Success(int)).unsafeRunSync())
      assertResult("DBZ")(IO.fromTry(Try(int / 0)).handleErrorWith(_ => IO("DBZ")).unsafeRunSync())
    }
  }

  "none" should "return None after run" in {
    assertResult(None)(IO.none.unsafeRunSync())
  }

  "raiseError" should "raise given exception" in {
    for (int <- generateRandomIntList(DEFAULT_TEST_SIZE)) {
      assertResult(int.toString)(
        IO.raiseError(new Exception(int.toString)).handleErrorWith(e => IO(e.getMessage)).unsafeRunSync()
      )
    }
  }

  "raiseUnless" should "raise exception if condition not met" in {
    for (i <- 0 until DEFAULT_TEST_SIZE) {
      if (i < 50) {
        assertResult(())(IO.raiseUnless(i < 50)(new Exception("fail")).unsafeRunSync())
      } else {
        assertResult(i.toString)(
          IO.raiseUnless(i < 50)(new Exception(i.toString)).handleErrorWith(e => IO(e.getMessage)).unsafeRunSync()
        )
      }
    }
  }

  "raiseWhen" should "raise exception if condition met" in {
    for (i <- 0 until DEFAULT_TEST_SIZE) {
      if (i >= 50) {
        assertResult(())(IO.raiseWhen(i < 50)(new Exception("fail")).unsafeRunSync())
      } else {
        assertResult(i.toString)(
          IO.raiseWhen(i < 50)(new Exception(i.toString)).handleErrorWith(e => IO(e.getMessage)).unsafeRunSync()
        )
      }
    }
  }

  "unlessA" should "do action if condition not met" in {

    for (i <- 0 until DEFAULT_TEST_SIZE) {
      val outStream = new ByteArrayOutputStream()
      val printStream = new PrintStream(outStream)
      IO.unlessA(i < 50)(IO(Console.withOut(printStream) { print(i) })).unsafeRunSync()
      if (i < 50) {
        assertResult("")(outStream.toString)
      } else {
        assertResult(i.toString)(outStream.toString)
      }
    }
  }

  "whenA" should "do action if condition met" in {
    for (i <- 0 until DEFAULT_TEST_SIZE) {
      val outStream = new ByteArrayOutputStream()
      val printStream = new PrintStream(outStream)
      IO.whenA(i < 50)(IO(Console.withOut(printStream) {
        print(i)
      }))
        .unsafeRunSync()
      if (i < 50) {
        assertResult(i.toString)(outStream.toString)
      } else {
        assertResult("")(outStream.toString)
      }
    }
  }

  "unit" should "return IO[Unit]" in {
    assertResult(())(IO.unit.unsafeRunSync())
  }

}
