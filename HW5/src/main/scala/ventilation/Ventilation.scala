package ventilation

import scala.annotation.tailrec
import scala.collection.AbstractIterable
import scala.collection.immutable.{Queue}

object Ventilation {

  class IntPriorityQueue private (val st1: List[(Int, Int)], val st2: List[(Int, Int)]) extends AbstractIterable[Int] {

    override def iterator: Iterator[Int] = st1.map(x => x._1).iterator

    def getMax: Option[Int] = {
      if (st1.isEmpty && st2.isEmpty) {
        None
      } else if (st2.isEmpty) {
        Some(st1.head._2)
      } else if (st1.isEmpty) {
        Some(st2.head._2)
      } else {
        Some(if (st1.head._2 > st2.head._2) st1.head._2 else st2.head._2)
      }
    }

    def enqueue(element: Int): IntPriorityQueue = {
      val mx = if (st1.isEmpty || element > st1.head._2) element else st1.head._2
      new IntPriorityQueue((element, mx) +: st1, st2)
    }

    def dequeue(): (Some[Int], IntPriorityQueue) = {
      if (st2.isEmpty) {
        val pair = dequeInternal(st1, st2)
        (Some(pair._2.head._1), new IntPriorityQueue(pair._1, pair._2.tail))
      } else (Some(st2.head._1), new IntPriorityQueue(st1, st2.tail))
    }

    def enqueueAll(list: List[Int]): IntPriorityQueue =
      enqueueAllHelper(list, this)

    @tailrec
    private def enqueueAllHelper(list: List[Int], quque: IntPriorityQueue): IntPriorityQueue =
      if (list.isEmpty)
        quque
      else {
        enqueueAllHelper(list.tail, quque.enqueue(list.head))
      }

    @tailrec
    private def dequeInternal(s1: List[(Int, Int)], s2: List[(Int, Int)]): (List[(Int, Int)], List[(Int, Int)]) =
      if (s1.isEmpty)
        (s1, s2)
      else {
        val elem = s1.head._1
        val mx = if (s2.isEmpty || s2.head._2 < elem) elem else s2.head._2
        dequeInternal(s1.tail, (elem, mx) +: s2)
      }

  }

  private object IntPriorityQueue {
    def apply(): IntPriorityQueue = {
      new IntPriorityQueue(List.empty[(Int, Int)], List.empty[(Int, Int)])
    }
  }

  def ventilationList(degrees: List[Int], k: Int): List[Int] =
    degrees.sliding(k).map(l => l reduce math.max).toList

  def ventilationONk(degrees: List[Int], k: Int): List[Int] = {
    if (degrees.isEmpty)
      List.empty[Int]
    else if (degrees.length <= k)
      List(degrees.reduce(math.max))
    else {
      val firstK = degrees.take(k)
      val queue = IntPriorityQueue().enqueueAll(firstK)
      val res = Queue().enqueue(queue.getMax match {
        case Some(value) => value
        case None        => Int.MinValue
      })
      @tailrec
      def internal(res: Queue[Int], priorityQueue: IntPriorityQueue, idx: Int): Queue[Int] =
        if (idx >= degrees.length)
          res
        else {
          val elem = degrees.apply(idx)
          val mx = math.max(
            priorityQueue.getMax match {
              case Some(v) => v
              case None    => elem
            },
            elem
          )
          internal(res.enqueue(mx), priorityQueue.dequeue()._2.enqueue(elem), idx + 1)
        }
      internal(res, queue.dequeue()._2, k).toList
    }
  }

}
