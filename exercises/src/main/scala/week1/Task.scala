package week1

trait Task[A] {

  def task(c: => A): Task[A]

  def join: A

  implicit def getJoin[T](x: Task[T]): T = x.join

}

object Parallel {


  def parallel[A, B](cA: => A, cB: => B): (A, B) = {
    val tB: Task[B] = task {
      cB
    }
    val tA: A = cA
    (tA, tB.join)
  }
}