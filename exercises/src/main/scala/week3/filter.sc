import scala.collection.parallel.CollectionConverters._


(1 until 1000).par
  .filter(n => n % 3 == 0)
  .count(n => n.toString == n.toString.reverse)


def sum(xs: Array[Int]): Int = {
  xs.par.fold(0)(_ + _)
}

def max(xs: Array[Int]): Int = {
  xs.par.fold(Int.MinValue)(math.max)
}
max(Array(1, 2, 45, 123, 4123, 321, 321, 2))

def isVowel(c: Char) = "AEIOUaeiou".indexOf(c) != -1

Array('A', 'B', 'C', 'e', 'D', 'I')
  .par
  .aggregate(0)((s, c) => if (isVowel(c)) s + 1 else s + 0, _ + _)

