package com.wavesplatform.state2.reader

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.state2.StateStorage
import com.wavesplatform.state2.StateStorage._
import org.scalatest.{Matchers, Outcome, fixture}
import scorex.account.Address


class StateReaderEffectiveBalanceTest extends fixture.FunSuite with Matchers {

  val acc: Address = Address.fromPublicKey(Array.emptyByteArray)
  val stateHeight = 100

  override type FixtureParam = StateStorage

  override protected def withFixture(test: OneArgTest): Outcome = {
    val storage = StateStorage(None, dropExisting = false).get
    storage.setHeight(stateHeight)
    test(storage)
  }

  test("exposes minimum of all 'current' and  one 'previous' of oldest record") { storage =>
    storage.balanceSnapshots.put(accountIntKey(acc, 20), (0, 0, 1))
    storage.balanceSnapshots.put(accountIntKey(acc, 75), (20, 0, 200))
    storage.balanceSnapshots.put(accountIntKey(acc, 90), (75, 0, 100))
    storage.lastBalanceSnapshotHeight.put(acc.bytes, 90)

    new StateReaderImpl(storage, new ReentrantReadWriteLock()).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50).get shouldBe 1
  }

  test("exposes current effective balance if no records in past N blocks are made") { storage =>
    storage.balanceSnapshots.put(accountIntKey(acc, 20), (0, 0, 1))
    storage.wavesBalance.put(acc.bytes, (1, 0, 0))
    storage.lastBalanceSnapshotHeight.put(acc.bytes, 20)

    new StateReaderImpl(storage, new ReentrantReadWriteLock()).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50).get shouldBe 1
  }

  test("doesn't include info older than N blocks") { storage =>
    storage.balanceSnapshots.put(accountIntKey(acc, 20), (0, 0, 1000))
    storage.balanceSnapshots.put(accountIntKey(acc, 50), (20, 0, 50000))
    storage.balanceSnapshots.put(accountIntKey(acc, 75), (50, 0, 100000))
    storage.lastBalanceSnapshotHeight.put(acc.bytes, 75)

    new StateReaderImpl(storage, new ReentrantReadWriteLock()).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50).get shouldBe 50000
  }

  test("includes most recent update") { storage =>
    storage.balanceSnapshots.put(accountIntKey(acc, 20), (0, 0, 1000))
    storage.balanceSnapshots.put(accountIntKey(acc, 51), (20, 0, 50000))
    storage.balanceSnapshots.put(accountIntKey(acc, 100), (51, 0, 1))
    storage.lastBalanceSnapshotHeight.put(acc.bytes, 100)

    new StateReaderImpl(storage, new ReentrantReadWriteLock()).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50).get shouldBe 1
  }

  test("exposes zero if record was made in past N blocks") { storage =>
    storage.balanceSnapshots.put(accountIntKey(acc, 70), (0, 0, 1000))
    storage.lastBalanceSnapshotHeight.put(acc.bytes, 70)
    new StateReaderImpl(storage, new ReentrantReadWriteLock()).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50).get shouldBe 0
  }

  test("exposes zero if no records was made at all") { storage =>
    new StateReaderImpl(storage, new ReentrantReadWriteLock()).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50).get shouldBe 0
  }
}