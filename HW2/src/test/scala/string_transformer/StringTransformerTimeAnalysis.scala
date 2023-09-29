package string_transformer

import java.io.File
import java.nio.file.{FileSystems, Files}
import java.util.concurrent.{ConcurrentHashMap, Executors, Phaser}
import string_transformer.StringTransformer._

import java.nio.charset.StandardCharsets
import scala.collection.immutable.TreeMap
import scala.concurrent.duration._
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.Using

class StringTransformerTimeAnalysis(methodTimeoutMills: Long = 30000) {
  val PATH_PREFIX: String = "src\\test\\scala\\string_transformer\\examples".replace("\\", File.separator)
  val files: List[File] =
    Files.list(FileSystems.getDefault getPath PATH_PREFIX).map(path => path.toFile).iterator().asScala.toList

  val fileExceptions: Map[String, RuntimeException] =
    files.map(file => (file.getName, new RuntimeException(file.getName + " produce errors: "))).toMap

  val methods: Map[String, MethodStatistic] = TreeMap(
    "reverseNaive" -> MethodStatistic("reverseNaive", reverseNaive, (res, initial) => initial.reverse.equals(res)),
    "reverseTail" -> MethodStatistic("reverseTail", reverseTail, (res, initial) => initial.reverse.equals(res)),
    "reverseActual" -> MethodStatistic("reverseActual", reverseActual, (res, initial) => initial.reverse.equals(res)),
    "reverseSystem" -> MethodStatistic("reverseSystem", (_: String).reverse),
    "duplicate" -> MethodStatistic("duplicate", duplicate),
    "duplicateConcat" -> MethodStatistic("duplicateConcat", duplicateConcat)
  )

  case class MethodStatistic(
    name: String,
    method: String => String,
    resultChecker: (String, String) => Boolean = (_, _) => true
  ) {

    val statistic: ConcurrentHashMap[String, ListBuffer[invocationResult]] =
      new ConcurrentHashMap[String, ListBuffer[invocationResult]]()
    for (file <- files) {
      statistic put (file.getName, ListBuffer())
    }

    def writeForFile(initial: String, result: (String, Long))(
      fileName: String = "no file"
    ): ListBuffer[invocationResult] = statistic.get(fileName) += invocationResult(fileName, initial, result)

    def totalTime: Long = getTotal(res => res.timeMillis)

    def totalLength: Long = getTotal(res => res.contentLength)

    private def getTotal(mapper: invocationResult => Long): Long =
      statistic.values.stream().filter(list => list.nonEmpty) map (list => totalForFile(list, mapper)) reduce (0, (
        a: Long,
        b: Long
      ) => a + b)

    def getAverageForFile(fileName: String): Long = {
      val listForName = statistic get fileName
      totalForFile(listForName, res => res.timeMillis) / math.max(1, listForName.length)
    }

    private def totalForFile(results: ListBuffer[invocationResult], mapper: invocationResult => Long): Long = {
      if (results.isEmpty) 0
      else
        results map mapper reduce ((a: Long, b: Long) => a + b)
    }

    def printResults(): Unit = {
      println()

      def getRowString(content: String, rowLength: Int, needSticks: Boolean = false) = {
        if (needSticks) s"| $content" + " ".repeat(math.max(0, rowLength - content.length)) + "|"
        else
          s"$content" + " ".repeat(math.max(0, rowLength - content.length))
      }

      def markFailed(results: ListBuffer[invocationResult]) = {
        if (results.isEmpty || (results exists (invocation => !invocation.valid))) "   !failed" else ""

      }

      println(getRowString(name, 80))
      statistic.entrySet.forEach(entry => {
        print(getRowString(entry.getKey, 32, needSticks = true))
        println(
          getRowString(getAverageForFile(entry.getKey) + " ms" + markFailed(entry.getValue), 43, needSticks = true)
        )
      })
      println(getRowString(s"Total time: $totalTime ms", 75))
      println(
        getRowString(
          "Average per 10^5 symbols: " + f"${(totalTime * 100000).doubleValue / math
            .max(totalLength, 1)
            .doubleValue}%1.3f" + " ms",
          75
        )
      )
      println()
      println("-" repeat 80)
      println()
    }

    case class invocationResult private (fileName: String, timeMillis: Long, contentLength: Long, valid: Boolean) {}

    object invocationResult {
      def apply(fileName: String, initial: String, result: (String, Long)) =
        new invocationResult(fileName, result._2, initial.length, isValidResult(result, initial))

      private def isValidResult(result: (String, Long), initial: String): Boolean =
        result._1 != null && result._2 != -1 && resultChecker.apply(result._1, initial)
    }

  }

  def computeReverseTime(runCount: Int): Unit = {
    val executors = Executors.newFixedThreadPool(files.length)
    val phaser = new Phaser(1)
    for (_ <- 0 until runCount) {
      for (file <- files) {
        phaser.register()
        executors.execute(() => executorTask(file, phaser))
      }
    }

    phaser.arriveAndAwaitAdvance()
    executors.shutdown()
    for (methodStatistic <- methods.values) {
      methodStatistic.printResults()
    }

    println("\"reverseTail\" probably fail with StackOverflowError")

    for (exception <- fileExceptions.values) {
      if (!exception.getSuppressed.isEmpty) {
        println(exception.getMessage)
        for (suppressed <- exception.getSuppressed) {
          System.err.println(suppressed.toString)
        }
      }
    }
  }

  def executorTask(file: File, phaser: Phaser): Unit = {
    try {
      Using(Source.fromFile(file)(StandardCharsets.UTF_8)) { source =>
        try {

          val line = source.getLines().mkString

          def limitedExecution[T](method: => T)(duration: Duration): Option[T] = {
            Some(Await.result(Future(method), duration))
          }

          def runWithTimeLimit(method: => (String, Long), methodName: String)(timeOutMs: Long): (String, Long) = {
            try {
              limitedExecution(method)(Duration(timeOutMs, MILLISECONDS)).getOrElse((null, -1))
            } catch {
              case e: Exception =>
                System.err.println(s"Execution $methodName on ${file.getName} failed: " + e.toString)
                (null, -1)
            }
          }

          def getMethodTime(method: String => String): (String, Long) = {
            val start = System.currentTimeMillis()
            val result = method(line)
            (result, System.currentTimeMillis() - start)

          }

          for (methodInfo <- methods.values) {
            println(methodInfo.name + " started on " + file.getName)
            methodInfo.writeForFile(
              line,
              runWithTimeLimit(getMethodTime(methodInfo.method), methodInfo.name)(methodTimeoutMills)
            )(
              file.getName
            )
            println(methodInfo.name + " end on " + file.getName)
          }
        } catch {
          case e: Exception => fileExceptions.get(file.getName).asInstanceOf[RuntimeException].addSuppressed(e)
        }
      }
    } finally {
      phaser.arriveAndDeregister()
    }
  }

}

object Main {
  def main(args: Array[String]): Unit = {
    new StringTransformerTimeAnalysis().computeReverseTime(3)
  }
}
