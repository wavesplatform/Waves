package com.wavesplatform.state2.reader

import java.util

import com.wavesplatform.state2.JavaMapStorage
import org.scalatest.{FunSuite, Matchers}
import scorex.account.Account


class StateReaderImplEffectiveBalanceTest extends FunSuite with Matchers {

  import StateReaderImplEffectiveBalanceTest._

  val acc: Account = Account.fromPublicKey(Array.emptyByteArray)
  val stateHeight = 100

  test("exposes minimum of all 'current' and  one 'previous' of oldest record") {

    val storage = new TestStorage

    storage.setHeight(stateHeight)

    storage.effectiveBalanceSnapshots.put((acc.bytes, 20), (0, 1, 0, 0))
    storage.effectiveBalanceSnapshots.put((acc.bytes, 75), (1, 200, 0, 0))
    storage.effectiveBalanceSnapshots.put((acc.bytes, 90), (200, 100, 0, 0))

    new StateReaderImpl(storage).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50) shouldBe 1
  }

  test("exposes current effective balance if no records in past N blocks are made") {

    val storage = new TestStorage

    storage.setHeight(stateHeight)
    storage.effectiveBalanceSnapshots.put((acc.bytes, 20), (0, 1, 0, 0))
    storage.portfolios.put(acc.bytes, (1, (0, 0), Map.empty))
    new StateReaderImpl(storage).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50) shouldBe 1
  }

  test("doesn't include info older than N blocks") {
    val storage = new TestStorage

    storage.setHeight(stateHeight)
    storage.effectiveBalanceSnapshots.put((acc.bytes, 20), (0, 1000, 0, 0))
    storage.effectiveBalanceSnapshots.put((acc.bytes, 50), (1000, 50000, 0, 0))
    storage.effectiveBalanceSnapshots.put((acc.bytes, 75), (50000, 100000, 0, 0))

    new StateReaderImpl(storage).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50) shouldBe 50000
  }

  test("includes most recent update") {
    val storage = new TestStorage

    storage.setHeight(stateHeight)
    storage.effectiveBalanceSnapshots.put((acc.bytes, 20), (0, 1000, 0, 0))
    storage.effectiveBalanceSnapshots.put((acc.bytes, 51), (1000, 50000, 0, 0))
    storage.effectiveBalanceSnapshots.put((acc.bytes, 100), (50000, 1, 0, 0))

    new StateReaderImpl(storage).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50) shouldBe 1
  }
}

object StateReaderImplEffectiveBalanceTest {

  class TestStorage extends JavaMapStorage {
    override val transactions = new util.HashMap[Array[Byte], (Int, Array[Byte])]
    override val portfolios = new util.HashMap[Array[Byte], (Long, (Long, Long), Map[Array[Byte], Long])]
    override val assets = new util.HashMap[Array[Byte], (Boolean, Long)]
    override val accountTransactionIds = new util.HashMap[Array[Byte], List[Array[Byte]]]
    override val effectiveBalanceSnapshots = new util.HashMap[(Array[Byte], Int), (Long, Long, Long, Long)]
    override val paymentTransactionHashes = new util.HashMap[Array[Byte], Array[Byte]]
    override val aliasToAddress = new util.HashMap[String, Array[Byte]]
    override val exchangeTransactionsByOrder = new util.HashMap[Array[Byte], List[Array[Byte]]]
    override val leaseState = new util.HashMap[Array[Byte], Boolean]

    var height: Int = 0

    override def getHeight: Int = height

    override def setHeight(i: Int): Unit = {
      height = i
    }

    override def commit(): Unit = ()
  }

}