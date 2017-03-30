package com.wavesplatform.state2.reader

import com.wavesplatform.state2.diffs.TestStorage
import org.scalatest.{FunSuite, Matchers}
import scorex.account.Account


class StateReaderImplTest extends FunSuite with Matchers {

  val acc: Account = Account.fromPublicKey(Array.emptyByteArray)
  val stateHeight = 100

  test("exposes minimum of all 'current' and  one 'previous' of oldest record") {

    val storage = new TestStorage

    storage.setHeight(stateHeight)

    storage.effectiveBalanceSnapshots.put((acc.bytes, 20), (0, 1))
    storage.effectiveBalanceSnapshots.put((acc.bytes, 75), (1, 200))
    storage.effectiveBalanceSnapshots.put((acc.bytes, 90), (200, 100))

    new StateReaderImpl(storage).effectiveBalanceAtHeightWithConfirmations(acc, 100, 50) shouldBe 1
  }

  test("exposes current effective balance if no records in past N blocks are made") {

    val storage = new TestStorage

    storage.setHeight(stateHeight)
    storage.effectiveBalanceSnapshots.put((acc.bytes, 20), (0, 1))
    storage.portfolios.put(acc.bytes, (1, 1, Map.empty))
    new StateReaderImpl(storage).effectiveBalanceAtHeightWithConfirmations(acc, 100, 50) shouldBe 1
  }

  test("doesn't include info older than N blocks") {
    val storage = new TestStorage

    storage.setHeight(stateHeight)
    storage.effectiveBalanceSnapshots.put((acc.bytes, 20), (0, 1000))
    storage.effectiveBalanceSnapshots.put((acc.bytes, 50), (1000, 50000))
    storage.effectiveBalanceSnapshots.put((acc.bytes, 75), (50000, 100000))

    new StateReaderImpl(storage).effectiveBalanceAtHeightWithConfirmations(acc, 100, 50) shouldBe 50000
  }

  test("includes most recent update") {
    val storage = new TestStorage

    storage.setHeight(stateHeight)
    storage.effectiveBalanceSnapshots.put((acc.bytes, 20), (0, 1000))
    storage.effectiveBalanceSnapshots.put((acc.bytes, 51), (1000, 50000))
    storage.effectiveBalanceSnapshots.put((acc.bytes, 100), (50000, 1))

    new StateReaderImpl(storage).effectiveBalanceAtHeightWithConfirmations(acc, 100, 50) shouldBe 1
  }
}
