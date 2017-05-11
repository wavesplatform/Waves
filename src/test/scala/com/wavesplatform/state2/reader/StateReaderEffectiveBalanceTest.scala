package com.wavesplatform.state2.reader

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.state2.StateStorage
import org.h2.mvstore.MVStore
import org.scalatest.{Matchers, Outcome, fixture}
import scorex.account.Account


class StateReaderEffectiveBalanceTest extends fixture.FunSuite with Matchers {

  val acc: Account = Account.fromPublicKey(Array.emptyByteArray)
  val stateHeight = 100

  override type FixtureParam = StateStorage

  override protected def withFixture(test: OneArgTest): Outcome = {
    val storage = new StateStorage(new MVStore.Builder().open())
    storage.setHeight(stateHeight)
    test(storage)
  }

  test("exposes minimum of all 'current' and  one 'previous' of oldest record") { storage =>
    storage.balanceSnapshots.put(StateStorage.snapshotKey(acc, 20), (0, 0, 1))
    storage.balanceSnapshots.put(StateStorage.snapshotKey(acc, 75), (20, 0, 200))
    storage.balanceSnapshots.put(StateStorage.snapshotKey(acc, 90), (75, 0, 100))
    storage.lastUpdateHeight.put(acc.bytes, 90)

    new StateReaderImpl(storage, new ReentrantReadWriteLock()).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50) shouldBe 1
  }

  test("exposes current effective balance if no records in past N blocks are made") { storage =>
    storage.balanceSnapshots.put(StateStorage.snapshotKey(acc, 20), (0, 0, 1))
    storage.portfolios.put(acc.bytes, (1, (0, 0), Map.empty))
    storage.lastUpdateHeight.put(acc.bytes, 20)

    new StateReaderImpl(storage, new ReentrantReadWriteLock()).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50) shouldBe 1
  }

  test("doesn't include info older than N blocks") { storage =>
    storage.balanceSnapshots.put(StateStorage.snapshotKey(acc, 20), (0, 0, 1000))
    storage.balanceSnapshots.put(StateStorage.snapshotKey(acc, 50), (20, 0, 50000))
    storage.balanceSnapshots.put(StateStorage.snapshotKey(acc, 75), (50, 0, 100000))
    storage.lastUpdateHeight.put(acc.bytes, 75)

    new StateReaderImpl(storage, new ReentrantReadWriteLock()).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50) shouldBe 50000
  }

  test("includes most recent update") { storage =>
    storage.balanceSnapshots.put(StateStorage.snapshotKey(acc, 20), (0, 0, 1000))
    storage.balanceSnapshots.put(StateStorage.snapshotKey(acc, 51), (20, 0, 50000))
    storage.balanceSnapshots.put(StateStorage.snapshotKey(acc, 100), (51, 0, 1))
    storage.lastUpdateHeight.put(acc.bytes, 100)

    new StateReaderImpl(storage, new ReentrantReadWriteLock()).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50) shouldBe 1
  }
}