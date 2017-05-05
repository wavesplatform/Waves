package scorex.consensus.mining

import scorex.utils.Time

/**
  * Created by ilyas on 05-May-17.
  */
class TestTime extends Time {
  var t: Long = 0L

  def setTime(tt: Long): Unit = {
    t = tt
  }

  override def correctedTime(): Long = t

  override def getTimestamp(): Long = t
}
