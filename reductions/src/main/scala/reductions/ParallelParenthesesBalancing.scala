package reductions

import scala.annotation._
import org.scalameter._
import common._

import scala.reflect.macros.blackbox

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    var i = 0
    var count = 0
    while (i < chars.length) {
      chars(i) match {
        case '(' => count = count + 1
        case ')' =>
          if (count == 0) return false
          count = count - 1
        case _ =>
      }
      i = i + 1
    }
    return count == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    @tailrec
    def traverse(idx: Int, until: Int, open: Int, close: Int): (Int, Int) = {
      if (idx == until) return (open, close)
      chars(idx) match {
        case '(' => traverse(idx + 1, until, open + 1, close)
        case ')' => if (open > 0) traverse(idx + 1, until, open - 1, close) else traverse(idx + 1, until, open, close + 1)
        case _ => traverse(idx + 1, until, open, close)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if ((until - from) <= threshold)
        return traverse(from, until, 0, 0)

      val mid = (from + until) / 2
      val ((open1, close1), (open2, close2)) = parallel(reduce(from, mid), reduce(mid, until))
      val closed = if (open1 < close2) open1 else close2
      (open1 + open2 - closed, close1 + close2 - closed)
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
