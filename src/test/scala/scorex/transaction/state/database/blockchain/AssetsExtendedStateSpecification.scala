package scorex.transaction.state.database.blockchain

import com.google.common.primitives.Longs
import com.wavesplatform.TransactionGen
import org.h2.mvstore.MVStore
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Assertions, Matchers, PropSpec}
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.state.database.state.storage._
import scorex.transaction.State

class AssetsExtendedStateSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks
  with Matchers with TransactionGen with Assertions with MockFactory {

  def newAssetExtendedState(): State = {
    val mvStore = new MVStore.Builder().open()
    val storage = new MVStoreStateStorage
      with MVStoreOrderMatchStorage
      with MVStoreAssetsExtendedStateStorage
      with MVStoreLeaseExtendedStateStorage
      with MVStoreAliasExtendedStorage {
      override val db: MVStore = mvStore
    }
    new StoredState(storage, TestFunctionalitySettings.Enabled)
  }

  property("Assets quantity and issueability should work on one update") {
    val state = newAssetExtendedState()
    forAll(bytes32gen, bytes32gen, positiveLongGen) { (assetId, transactionId, quantity) =>
      state.addAsset(assetId, 1, transactionId, quantity, reissuable = true)
      state.totalAssetQuantity(assetId) shouldBe quantity
      state.isReissuable(assetId) shouldBe true
    }
  }

  property("Assets quantity should work on huge sequential updates") {
    val state = newAssetExtendedState()
    forAll(bytes32gen) { assetId =>
      var i = 0
      var q: Long = 0L
      forAll(bytes32gen, smallFeeGen) { (transactionId, quantity) =>
        i = i + 1
        q = q + quantity
        state.addAsset(assetId, i, transactionId, quantity, reissuable = true)
        state.totalAssetQuantity(assetId) shouldBe q
      }
    }
  }

  property("Reissuable should work in simple case") {
    val state = newAssetExtendedState()
    val assetId = getId(0)

    state.addAsset(assetId, 1, getId(1), 10, reissuable = true)
    state.isReissuable(assetId) shouldBe true
    state.addAsset(assetId, 2, getId(2), 10, reissuable = false)
    state.isReissuable(assetId) shouldBe false
  }

  property("Reissuable should work correctly in case of few updates per block") {
    val state = newAssetExtendedState()
    val assetId = getId(0)

    state.addAsset(assetId, 1, getId(1), 10, reissuable = true)
    state.addAsset(assetId, 1, getId(2), 20, reissuable = true)

    state.totalAssetQuantity(assetId) shouldBe 30
    state.isReissuable(assetId) shouldBe true

    state.addAsset(assetId, 3, getId(3), 30, reissuable = true)
    state.addAsset(assetId, 3, getId(4), 40, reissuable = false)

    state.totalAssetQuantity(assetId) shouldBe 100
    state.isReissuable(assetId) shouldBe false

    state.assetRollbackTo(assetId, 2, Some(true))

    state.totalAssetQuantity(assetId) shouldBe 30
    state.isReissuable(assetId) shouldBe true
  }

  property("Reissuable should work in case of few updates") {
    val state = newAssetExtendedState()
    val assetId = getId(0)

    state.addAsset(assetId, 1, getId(1), 10, reissuable = true)

    state.totalAssetQuantity(assetId) shouldBe 10
    state.isReissuable(assetId) shouldBe true

    state.addAsset(assetId, 2, getId(2), 20, reissuable = false)

    state.totalAssetQuantity(assetId) shouldBe 30
    state.isReissuable(assetId) shouldBe false

    state.addAsset(assetId, 3, getId(3), 20, reissuable = true)

    state.totalAssetQuantity(assetId) shouldBe 50
    state.isReissuable(assetId) shouldBe true

    state.assetRollbackTo(assetId, 2, Some(false))

    state.totalAssetQuantity(assetId) shouldBe 30
    state.isReissuable(assetId) shouldBe false
  }

  property("Rollback should work after simple sequence of updates") {
    val state = newAssetExtendedState()
    val assetId = getId(0)

    state.addAsset(assetId, 1, getId(1), 10, reissuable = true)
    state.addAsset(assetId, 2, getId(2), 10, reissuable = true)
    state.addAsset(assetId, 3, getId(3), 10, reissuable = true)

    state.totalAssetQuantity(assetId) shouldBe 30
    state.isReissuable(assetId) shouldBe true

    state.addAsset(assetId, 4, getId(4), 10, reissuable = false)

    state.totalAssetQuantity(assetId) shouldBe 40
    state.isReissuable(assetId) shouldBe false

    state.assetRollbackTo(assetId, 2, Some(true))

    state.totalAssetQuantity(assetId) shouldBe 20
    state.isReissuable(assetId) shouldBe true
  }

  property("Rollback should work after simple sequence of updates with gaps") {
    val state = newAssetExtendedState()
    val assetId = getId(0)

    state.addAsset(assetId, 10, getId(1), 10, reissuable = true)
    state.addAsset(assetId, 20, getId(2), 10, reissuable = true)
    state.addAsset(assetId, 30, getId(3), 10, reissuable = true)

    state.totalAssetQuantity(assetId) shouldBe 30
    state.isReissuable(assetId) shouldBe true

    state.addAsset(assetId, 40, getId(4), 10, reissuable = false)

    state.totalAssetQuantity(assetId) shouldBe 40
    state.isReissuable(assetId) shouldBe false

    state.assetRollbackTo(assetId, 25, Some(true))

    state.totalAssetQuantity(assetId) shouldBe 20
    state.isReissuable(assetId) shouldBe true
  }

  property("Duplicated reissue = true calls should work correctly") {
    val state = newAssetExtendedState()
    val assetId = getId(0)

    state.addAsset(assetId, 10, getId(1), 10, reissuable = true)
    state.addAsset(assetId, 10, getId(1), 10, reissuable = true)
    state.addAsset(assetId, 10, getId(1), 10, reissuable = true)

    state.totalAssetQuantity(assetId) shouldBe 10
    state.isReissuable(assetId) shouldBe true

    state.addAsset(assetId, 20, getId(2), 20, reissuable = false)

    state.assetRollbackTo(assetId, 18, Some(true))

    state.totalAssetQuantity(assetId) shouldBe 10
    state.isReissuable(assetId) shouldBe true
  }

  property("Duplicated reissue = false calls should not work correctly") {
    val state = newAssetExtendedState()
    val assetId = getId(0)

    state.addAsset(assetId, 10, getId(1), 10, reissuable = true)

    state.totalAssetQuantity(assetId) shouldBe 10
    state.isReissuable(assetId) shouldBe true

    state.addAsset(assetId, 20, getId(2), 20, reissuable = false)

    state.totalAssetQuantity(assetId) shouldBe 30
    state.isReissuable(assetId) shouldBe false

    state.assetRollbackTo(assetId, 18, Some(true))

    state.totalAssetQuantity(assetId) shouldBe 10
    state.isReissuable(assetId) shouldBe true
  }

  property("Burn should work after simple sequence of updates and rollback") {
    val state = newAssetExtendedState()
    val assetId = getId(0)

    state.addAsset(assetId, 10, getId(1), 10, reissuable = true)
    state.addAsset(assetId, 10, getId(2), 10, reissuable = true)

    state.addAsset(assetId, 20, getId(3), 20, reissuable = true)
    state.addAsset(assetId, 20, getId(4), 20, reissuable = true)

    state.addAsset(assetId, 30, getId(5), 30, reissuable = true)

    state.totalAssetQuantity(assetId) shouldBe 90
    state.isReissuable(assetId) shouldBe true

    state.burnAsset(assetId, 40, getId(6), -50)
    state.addAsset(assetId, 40, getId(7), 10, reissuable = false)

    state.totalAssetQuantity(assetId) shouldBe 50
    state.isReissuable(assetId) shouldBe false

    state.assetRollbackTo(assetId, 15, Some(true))

    state.totalAssetQuantity(assetId) shouldBe 20
    state.isReissuable(assetId) shouldBe true
  }

  property("Burn should not changes asset reissue flag") {

    val state = newAssetExtendedState()
    val assetId0 = getId(0)
    val assetId1 = getId(1)

    state.addAsset(assetId0, 10, getId(1), 10, reissuable = true)
    state.addAsset(assetId1, 10, getId(2), 10, reissuable = false)

    state.burnAsset(assetId0, 40, getId(3), -1)
    state.burnAsset(assetId1, 40, getId(4), -1)

    state.totalAssetQuantity(assetId0) shouldBe 9
    state.isReissuable(assetId0) shouldBe true

    state.totalAssetQuantity(assetId1) shouldBe 9
    state.isReissuable(assetId1) shouldBe false
  }


  private def getId(i: Int): Array[Byte] = {
    Longs.toByteArray(0) ++ Longs.toByteArray(0) ++ Longs.toByteArray(0) ++ Longs.toByteArray(i)
  }

}
