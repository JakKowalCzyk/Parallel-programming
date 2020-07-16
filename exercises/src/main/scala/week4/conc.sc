sealed trait Conc[+T]{
  def level:Int
  def size:Int
  def left : Conc[T]
  def right : Conc[T]


}

case object Empty extends Conc[Nothing] {
  override def level = 0

  override def size = 0

  override def left = ???

  override def right = ???
}

class Single[T](val x:T) extends Conc[T] {
  override def level = 0

  override def size = 1

  override def left = ???

  override def right = ???
}

case class <>[T](left:Conc[T], right:Conc[T]) extends Conc[T] {
  override def level = 1+math.max(left.level, right.level)

  override def size = left.size + right.size

  def <>(that:Conc[T]):Conc[T] = {
    if(this == Empty) that
    else if (that == Empty) this
    else concat(this, that)
  }

  def concat(xs: Conc[T], ys: Conc[T]) :Conc[T] = {
    val diff = xs.level - ys.level
    if(diff >= -1 && diff <= 1) new <>(xs, ys)
    else if (diff < -1){
      if(xs.left.level >= xs.right.level){
        val nr = concat(xs.right, ys)
        new <>(xs.left, nr)
      }else{
        val nrr = concat(xs.right.right, ys)
        if(nrr.level == xs.level -3) {
          val nl = xs.left
          val nr = new <>(xs.right.left, nl)
          new <>(nl, nr)
        }else{
          val nl = new <>(xs.left, ys.right.left)
          new <>(nl, nrr)
        }
      }
    }else{
      if(ys.right.level >= ys.left.level) {
        val nl = concat(xs, ys.left)
        new <>(nl, ys.right)
      }else{
        val nll = concat(xs, ys.left.left)
        if(nll.level == ys.level - 3) {
          val nl = new <>(nll, ys.left.left)
          new <>(nl, ys.right)
        }else{
          val nr = new <>(ys.left.right, ys.right)
          new <>(nll, nr)
        }
      }
    }
  }
}