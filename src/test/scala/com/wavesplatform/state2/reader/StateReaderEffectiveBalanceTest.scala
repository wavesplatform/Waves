package com.wavesplatform.state2.reader

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.TestDB
import com.wavesplatform.state2.StateStorage
import org.scalatest.{Matchers, Outcome, fixture}
import scorex.account.Address


class StateReaderEffectiveBalanceTest extends fixture.FunSuite with Matchers with TestDB {

  val acc: Address = Address.fromPublicKey(Array.emptyByteArray)
  val stateHeight = 100
  val db = open()

  override type FixtureParam = StateStorage

  override protected def withFixture(test: OneArgTest): Outcome = {
    val storage = StateStorage(db, dropExisting = false).get
    storage.setHeight(None, stateHeight)
    test(storage)
  }

  test("exposes minimum of all 'current' and  one 'previous' of oldest record") { storage =>
    storage.putBalanceSnapshots(None, acc, 20, (0, 0, 1))
    storage.putBalanceSnapshots(None, acc, 75, (20, 0, 200))
    storage.putBalanceSnapshots(None, acc, 90, (75, 0, 100))
    storage.putLastBalanceSnapshotHeight(None, acc, 90)

    new StateReaderImpl(storage, new ReentrantReadWriteLock()).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50).get shouldBe 1
  }

  test("exposes current effective balance if no records in past N blocks are made") { storage =>
    storage.putBalanceSnapshots(None, acc, 20, (0, 0, 1))
    storage.putWavesBalance(None, acc, (1, 0, 0))
    storage.putLastBalanceSnapshotHeight(None, acc, 20)

    new StateReaderImpl(storage, new ReentrantReadWriteLock()).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50).get shouldBe 1
  }

  test("doesn't include info older than N blocks") { storage =>
    storage.putBalanceSnapshots(None, acc, 20, (0, 0, 1000))
    storage.putBalanceSnapshots(None, acc, 50, (20, 0, 50000))
    storage.putBalanceSnapshots(None, acc, 75, (50, 0, 100000))
    storage.putLastBalanceSnapshotHeight(None, acc, 75)

    new StateReaderImpl(storage, new ReentrantReadWriteLock()).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50).get shouldBe 50000
  }

  test("includes most recent update") { storage =>
    storage.putBalanceSnapshots(None, acc, 20, (0, 0, 1000))
    storage.putBalanceSnapshots(None, acc, 51, (20, 0, 50000))
    storage.putBalanceSnapshots(None, acc, 100, (51, 0, 1))
    storage.putLastBalanceSnapshotHeight(None, acc, 100)

    new StateReaderImpl(storage, new ReentrantReadWriteLock()).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50).get shouldBe 1
  }

  test("exposes zero if record was made in past N blocks") { storage =>
    storage.putBalanceSnapshots(None, acc, 70, (0, 0, 1000))
    storage.putLastBalanceSnapshotHeight(None, acc, 70)
    new StateReaderImpl(storage, new ReentrantReadWriteLock()).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50).get shouldBe 0
  }

  test("exposes zero if no records was made at all") { storage =>
    new StateReaderImpl(storage, new ReentrantReadWriteLock()).effectiveBalanceAtHeightWithConfirmations(acc, stateHeight, 50).get shouldBe 0
  }
}