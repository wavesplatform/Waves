package com.wavesplatform.state2

import java.util

import org.scalatest.{FreeSpec, FunSuite, Matchers}

class StateReaderImplTest extends FreeSpec with Matchers {

  class InMemory extends JavaMapStorage {
    override val transactions = new util.HashMap[Array[Byte], (Int, Array[Byte])]
    override val portfolios = new util.HashMap[Array[Byte], (Long, Long, Map[Array[Byte], Long])]
    override val assets = new util.HashMap[Array[Byte], (Boolean, Long)]
    override val accountTransactionIds = new util.HashMap[Array[Byte], util.List[Array[Byte]]]
    override val effectiveBalanceSnapshots = new util.HashMap[(Array[Byte], Int), (Long, Long)]

    var height: Int = 0

    override def getHeight: Int = height

    override def setHeight(i: Int): Unit = {
      height = i
    }
  }


//  "A StateReader" should {
//    "effectiveBalanceAtHeightWithConfirmations" - {
//      val p = new InMemory
//      p.effectiveBalanceSnapshots
//      new StateWriterImpl(p)
//    }
//  }

}
