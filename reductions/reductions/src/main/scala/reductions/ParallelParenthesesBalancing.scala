package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var counter = 0
    var i = 0
    while (i < chars.length) {
      val c = chars(i)
      if (c == '(') counter += 1
      else if (c == ')') counter -= 1
      if(counter<0) return false
      i += 1
    }
    counter == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx< until) {
        chars(idx) match {
          case '(' => traverse(idx + 1, until, arg1 + 1, arg2)
          case ')' =>
            if(arg1 > 0) traverse(idx+1, until, arg1-1, arg2)
            else traverse(idx+1, until, arg1, arg2+1)
          case _ => traverse(idx+1, until, arg1, arg2)
        }
      }
      else (arg1, arg2)
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if(until - from > threshold) {
        val mid = (from + until) / 2
        val ((a1, b1), (a2, b2)) = parallel(reduce(from, mid), reduce(mid, until))
        val min = math.min(a1, b2)
        a1+b1-min -> (a2+b2 - min)
      }
        traverse(from, until, 0, 0)

    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}