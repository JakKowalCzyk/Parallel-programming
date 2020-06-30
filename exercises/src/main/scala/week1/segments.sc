
def sumSegments(a: Array[Int], p: Double, s: Int, t: Int): Int = {
  if (s == t) 0
  else math.exp(p * math.log(math.abs(a(s)))).toInt + sumSegments(a, p, s + 1, t)
}

def sumSegments2(a: Array[Int], p: Double, s: Int, t: Int): Int = {
  var i = s;
  var sum: Int = 0;
  while (i < t) {
    sum = sum + power(a(i), p)
    i = i + 1
  }
  sum
}

def power(i: Int, p: Double): Int = math.exp(p * math.log(math.abs(i))).toInt

def pNorm(a: Array[Int], p: Double): Int = power(sumSegments(a, p, 0, a.length), 1 / p)

def pNormTwoPart(a: Array[Int], p: Double): Int = {
  val m = a.length / 2
  val (sum1, sum2) = (sumSegments(a, p, 0, m), sumSegments(a, p, m, a.length))
  power(sum1 + sum2, 1 / p)
}

def pNormTwoPartParr(a: Array[Int], p: Double): Int = {
  val m = a.length / 2
  val (sum1, sum2) = parallel(sumSegments(a, p, 0, m), sumSegments(a, p, m, a.length))
  power(sum1 + sum2, 1 / p)
}

def pNormRec(a: Array[Int], p: Double): Int = {
  power(segmentRec(a, p, 0, a.length), 1 / p)
}

def segmentRec(ints: Array[Int], p: Double, s: Int, t: Int): Int = {
  val m = s + (t - s) / 2
  val (sum1, sum2) = parallel(segmentRec(ints, p, s, m),
    segmentRec(ints, p, m, t))
  sum1 + sum2
}



sumSegments(Array(2, 3, 4, 5, 6), 2, 1, 3)
sumSegments2(Array(2, 3, 4, 5, 6), 2, 1, 3)