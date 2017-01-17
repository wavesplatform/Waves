package scorex.transaction.state.database.blockchain

import com.google.common.primitives.Longs
import org.h2.mvstore.MVStore
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Assertions, Matchers, PropSpec}
import scorex.transaction.TransactionGen
import scorex.transaction.state.database.state.storage.{MVStoreAssetsExtendedStateStorage, MVStoreOrderMatchStorage, MVStoreStateStorage}

class AssetsExtendedStateSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks
  with Matchers with TransactionGen with Assertions {

  property("Assets quantity and issueability should work on one update") {
    val mvStore = new MVStore.Builder().open()
    val storage = new MVStoreStateStorage with MVStoreAssetsExtendedStateStorage {
      override val db: MVStore = mvStore
    }
    val state = new AssetsExtendedState(storage)

    forAll(bytes32gen, bytes32gen, positiveLongGen) { (assetId, transactionId, quantity) =>
      state.addAsset(assetId, 1, transactionId, quantity, reissuable = true)
      state.getAssetQuantity(assetId) shouldBe quantity
      state.isReissuable(assetId) shouldBe true
    }
  }

  property("Assets quantity should work on huge sequential updates") {
    val mvStore = new MVStore.Builder().open()
    val storage = new MVStoreStateStorage with MVStoreAssetsExtendedStateStorage {
      override val db: MVStore = mvStore
    }
    val state = new AssetsExtendedState(storage)
    forAll(bytes32gen) { assetId =>
      var i = 0
      var q: Long = 0L
      forAll(bytes32gen, smallFeeGen) { (transactionId, quantity) =>
        i = i + 1
        q = q + quantity
        state.addAsset(assetId, i, transactionId, quantity, reissuable = true)
        state.getAssetQuantity(assetId) shouldBe q
      }
    }
  }

  property("Reissuable should work in simple case") {
    val mvStore = new MVStore.Builder().open()
    val storage = new MVStoreStateStorage with MVStoreAssetsExtendedStateStorage {
      override val db: MVStore = mvStore
    }
    val state = new AssetsExtendedState(storage)
    val assetId = getId(0)

    state.addAsset(assetId, 1, getId(1), 10, reissuable = true)
    state.isReissuable(assetId) shouldBe true
    state.addAsset(assetId, 2, getId(2), 10, reissuable = false)
    state.isReissuable(assetId) shouldBe false
  }

  property("Reissuable should work correctly in case of few updates per block") {
    val mvStore = new MVStore.Builder().open()
    val storage = new MVStoreStateStorage with MVStoreAssetsExtendedStateStorage {
      override val db: MVStore = mvStore
    }
    val state = new AssetsExtendedState(storage)
    val assetId = getId(0)

    state.addAsset(assetId, 1, getId(1), 10, reissuable = true)
    state.addAsset(assetId, 1, getId(2), 20, reissuable = true)

    state.getAssetQuantity(assetId) shouldBe 30
    state.isReissuable(assetId) shouldBe true

    state.addAsset(assetId, 3, getId(3), 30, reissuable = true)
    state.addAsset(assetId, 3, getId(4), 40, reissuable = false)

    state.getAssetQuantity(assetId) shouldBe 100
    state.isReissuable(assetId) shouldBe false

    state.rollbackTo(assetId, 2)

    state.getAssetQuantity(assetId) shouldBe 30
    state.isReissuable(assetId) shouldBe true
  }

  property("Rollback should work after simple sequence of updates") {
    val mvStore = new MVStore.Builder().open()
    val storage = new MVStoreStateStorage with MVStoreAssetsExtendedStateStorage {
      override val db: MVStore = mvStore
    }
    val state = new AssetsExtendedState(storage)
    val assetId = getId(0)

    state.addAsset(assetId, 1, getId(1), 10, reissuable = true)
    state.addAsset(assetId, 2, getId(2), 10, reissuable = true)
    state.addAsset(assetId, 3, getId(3), 10, reissuable = true)

    state.getAssetQuantity(assetId) shouldBe 30
    state.isReissuable(assetId) shouldBe true

    state.addAsset(assetId, 4, getId(4), 10, reissuable = false)

    state.getAssetQuantity(assetId) shouldBe 40
    state.isReissuable(assetId) shouldBe false

    state.rollbackTo(assetId, 2)

    state.getAssetQuantity(assetId) shouldBe 20
    state.isReissuable(assetId) shouldBe true
  }

  property("Rollback should work after simple sequence of updates with gaps") {
    val mvStore = new MVStore.Builder().open()
    val storage = new MVStoreStateStorage with MVStoreAssetsExtendedStateStorage {
      override val db: MVStore = mvStore
    }
    val state = new AssetsExtendedState(storage)
    val assetId = getId(0)

    state.addAsset(assetId, 10, getId(1), 10, reissuable = true)
    state.addAsset(assetId, 20, getId(2), 10, reissuable = true)
    state.addAsset(assetId, 30, getId(3), 10, reissuable = true)

    state.getAssetQuantity(assetId) shouldBe 30
    state.isReissuable(assetId) shouldBe true

    state.addAsset(assetId, 40, getId(4), 10, reissuable = false)

    state.getAssetQuantity(assetId) shouldBe 40
    state.isReissuable(assetId) shouldBe false

    state.rollbackTo(assetId, 25)

    state.getAssetQuantity(assetId) shouldBe 20
    state.isReissuable(assetId) shouldBe true
  }

  property("Duplicated calls should work correctly") {
    val mvStore = new MVStore.Builder().open()
    val storage = new MVStoreStateStorage with MVStoreAssetsExtendedStateStorage {
      override val db: MVStore = mvStore
    }
    val state = new AssetsExtendedState(storage)
    val assetId = getId(0)

    state.addAsset(assetId, 10, getId(1), 10, reissuable = true)
    state.addAsset(assetId, 10, getId(1), 10, reissuable = true)
    state.addAsset(assetId, 10, getId(1), 10, reissuable = true)

    state.getAssetQuantity(assetId) shouldBe 10
    state.isReissuable(assetId) shouldBe true

    state.addAsset(assetId, 20, getId(2), 20, reissuable = false)
    state.addAsset(assetId, 20, getId(2), 20, reissuable = false)
    state.addAsset(assetId, 20, getId(2), 20, reissuable = false)

    state.getAssetQuantity(assetId) shouldBe 30
    state.isReissuable(assetId) shouldBe false

    state.rollbackTo(assetId, 18)

    state.getAssetQuantity(assetId) shouldBe 10
    state.isReissuable(assetId) shouldBe true
  }

  property("Burn should work after simple sequence of updates and rollback") {
    val mvStore = new MVStore.Builder().open()
    val storage = new MVStoreStateStorage with MVStoreAssetsExtendedStateStorage {
      override val db: MVStore = mvStore
    }
    val state = new AssetsExtendedState(storage)
    val assetId = getId(0)

    state.addAsset(assetId, 10, getId(1), 10, reissuable = true)
    state.addAsset(assetId, 10, getId(2), 10, reissuable = true)

    state.addAsset(assetId, 20, getId(3), 20, reissuable = true)
    state.addAsset(assetId, 20, getId(4), 20, reissuable = true)

    state.addAsset(assetId, 30, getId(5), 30, reissuable = true)

    state.getAssetQuantity(assetId) shouldBe 90
    state.isReissuable(assetId) shouldBe true

    state.burnAsset(assetId, 40, getId(6), -50)
    state.addAsset(assetId, 40, getId(7), 10, reissuable = false)

    state.getAssetQuantity(assetId) shouldBe 50
    state.isReissuable(assetId) shouldBe false

    state.rollbackTo(assetId, 15)

    state.getAssetQuantity(assetId) shouldBe 20
    state.isReissuable(assetId) shouldBe true
  }

  private def getId(i: Int): Array[Byte] = {
    Longs.toByteArray(0) ++ Longs.toByteArray(0) ++ Longs.toByteArray(0) ++ Longs.toByteArray(i)
  }

}
