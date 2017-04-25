package com.wavesplatform.state2.reader

import com.wavesplatform.state2.MVStorePrimitiveImpl
import org.h2.mvstore.MVStore
import org.scalatest.{FunSuite, Matchers}
import scorex.account.Account


class StateReaderImplEffectiveBalanceTest extends FunSuite with Matchers {

  val acc: Account = Account.fromPublicKey(Array.emptyByteArray)
  val stateHeight = 100

  test("exposes minimum of all 'current' and  one 'previous' of oldest record") {

    val storage = new MVStorePrimitiveImpl(new MVStore.Builder().open())

    storage.setHeight(stateHeight)

    storage.effectiveBalanceSnapshots.put((acc.bytes, 20), (0, 0, 1))
    storage.effectiveBalanceSnapshots.put((acc.bytes, 75), (20, 0, 200))
    storage.effectiveBalanceSnapshots.put((acc.bytes, 90), (75, 0, 100))
    storage.lastUpdateHeight.put(acc.bytes, 90)

    new StateReaderImpl(storage).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50) shouldBe 1
  }

  test("exposes current effective balance if no records in past N blocks are made") {

    val storage = new MVStorePrimitiveImpl(new MVStore.Builder().open())

    storage.setHeight(stateHeight)
    storage.effectiveBalanceSnapshots.put((acc.bytes, 20), (0, 0, 1))
    storage.portfolios.put(acc.bytes, (1, (0, 0), Map.empty))
    storage.lastUpdateHeight.put(acc.bytes, 20)

    new StateReaderImpl(storage).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50) shouldBe 1
  }

  test("doesn't include info older than N blocks") {
    val storage = new MVStorePrimitiveImpl(new MVStore.Builder().open())

    storage.setHeight(stateHeight)
    storage.effectiveBalanceSnapshots.put((acc.bytes, 20), (0, 0, 1000))
    storage.effectiveBalanceSnapshots.put((acc.bytes, 50), (20, 0, 50000))
    storage.effectiveBalanceSnapshots.put((acc.bytes, 75), (50, 0, 100000))
    storage.lastUpdateHeight.put(acc.bytes, 75)


    new StateReaderImpl(storage).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50) shouldBe 50000
  }

  test("includes most recent update") {
    val storage = new MVStorePrimitiveImpl(new MVStore.Builder().open())

    storage.setHeight(stateHeight)
    storage.effectiveBalanceSnapshots.put((acc.bytes, 20), (0, 0, 1000))
    storage.effectiveBalanceSnapshots.put((acc.bytes, 51), (20, 0, 50000))
    storage.effectiveBalanceSnapshots.put((acc.bytes, 100), (51, 0, 1))
    storage.lastUpdateHeight.put(acc.bytes, 100)

    new StateReaderImpl(storage).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50) shouldBe 1
  }
}