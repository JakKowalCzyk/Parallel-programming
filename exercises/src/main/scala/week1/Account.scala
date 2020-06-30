package week1

object Account extends App {
  val a1 = new Account(100000)
  val a2 = new Account(500000)
  val t = startThread(a1, a2, 1500)
  val s = startThread(a2, a1, 1500)
  private val x = new AnyRef {}
  private var uid = 0L

  def getUid(): Long = x.synchronized {
    uid = uid + 1
    uid
  }

  def startThread(a: Account, b: Account, n: Int) = {
    val t = new Thread {
      override def run(): Unit = {
        for (i <- 0 until n) {
          a.transfer(b, n)
        }
      }
    }
    t.start()
    t
  }

  class Account(private var amount: Int = 0) {
    val uid = getUid()

    def transfer(target: Account, n: Int) =
      if (this.uid < target.uid) this.lock(target, n)
      else target.lock(this, -n)

    private def lock(target: Account, n: Int) = {
      this.synchronized {
        target.synchronized {
          this.amount -= n
          target.amount += n
        }
      }
    }

  }

  t.join()
  s.join()
}