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
mapArraySeq(in, 1, 3, (x: Int) => x * x, out)
out