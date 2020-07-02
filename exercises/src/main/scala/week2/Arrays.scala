package week2

object Arrays extends App {

  val threshold = 2
  val in = Array(2, 3, 4, 5, 6)
  val out = Array(0, 0, 0, 0, 0)

  def mapArraySeq[A, B](input: Array[A], left: Int, right: Int, f: A => B,
                        output: Array[B]) = {
    var i = left
    while (i < right) {
      output(i) = f(input(i))
      i = i + 1
    }
  }

  def mapArrayPar[A, B](input: Array[A], left: Int, right: Int, f: A => B,
                        output: Array[B]): Unit = {
    if (right - left < threshold) mapArraySeq(input, left, right, f, output)
    else {
      val mid = left + (right - left) / 2
      parallel(mapArrayPar(input, left, mid, f, output), mapArrayPar(input, mid, right, f, output))
    }
  }

  mapArrayPar(in, 1, 3, (x: Int) => x * x, out)
  println(out.mkString(", "))

}
