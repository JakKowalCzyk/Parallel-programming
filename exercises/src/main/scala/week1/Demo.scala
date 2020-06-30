package week1

object Demo extends App {
  private val x = new AnyRef {}
  private var uid = 0L

  def startThread() = {
    val t = new Thread {
      override def run(): Unit = {
        val uids = for (i <- 0 until 10) yield getUid()
        println(uids)
      }
    }
    t.start()
    t
  }

  def getUid(): Long = x.synchronized {
    uid = uid + 1
    uid
  }

  startThread()
  startThread()


}


