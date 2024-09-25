package com.wavesplatform.state

import com.google.common.primitives.Longs
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.state.StateHash.SectionId
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class StateHashSpec extends FreeSpec {
  "state hash" - {
    val stateHash = new StateHashBuilder
    val address   = Address.fromString("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8").explicitGet()
    val address1  = Address.fromString("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh").explicitGet()
    val assetId   = IssuedAsset(ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get)
    val testScript = ScriptCompiler
      .compile(
        """
          |{-# STDLIB_VERSION 2 #-}
          |{-# CONTENT_TYPE EXPRESSION #-}
          |{-# SCRIPT_TYPE ACCOUNT #-}
          |true
          |""".stripMargin,
        ScriptEstimatorV1
      )
      .explicitGet()
      ._1
    val dataEntry = StringDataEntry("test", "test")

    stateHash.addLeaseBalance(address, 10000L, 10000L)
    stateHash.addAccountScript(address, Some(testScript))
    stateHash.addAssetScript(assetId, Some(testScript))
    stateHash.addAlias(address, "test")
    stateHash.addAlias(address, "test1")
    stateHash.addAlias(address1, "test2")
    stateHash.addDataEntry(address, dataEntry)
    stateHash.addLeaseStatus(TransactionId @@ assetId.id, isActive = true)
    stateHash.addSponsorship(assetId, 1000)
    stateHash.addAssetBalance(address, assetId, 2000)
    stateHash.addAssetBalance(address1, assetId, 2000)
    stateHash.addWavesBalance(address, 1000)
    val result = stateHash.result()

    def hash(bs: Array[Byte]*): ByteStr    = ByteStr(com.wavesplatform.crypto.fastHash(bs.reduce(_ ++ _)))
    def sect(id: SectionId.Value): ByteStr = result.hashes(id)
    import SectionId._

    "sections" - {
      "lease balance" in {
        sect(LeaseBalance) shouldBe hash(
          address.bytes,
          Longs.toByteArray(10000L),
          Longs.toByteArray(10000L)
        )
      }

      "asset balance" in {
        sect(AssetBalance) shouldBe hash(
          address.bytes,
          assetId.id.arr,
          Longs.toByteArray(2000),
          address1.bytes,
          assetId.id.arr,
          Longs.toByteArray(2000)
        )
      }

      "waves balance" in {
        sect(WavesBalance) shouldBe hash(
          address.bytes,
          Longs.toByteArray(1000)
        )
      }

      "account script" in {
        sect(AccountScript) shouldBe hash(
          address.bytes,
          testScript.bytes().arr
        )
      }

      "asset script" in {
        sect(AssetScript) shouldBe hash(
          assetId.id.arr,
          testScript.bytes().arr
        )
      }

      "alias" in {
        sect(Alias) shouldBe hash(
          address.bytes,
          "test".getBytes(),
          address.bytes,
          "test1".getBytes(),
          address1.bytes,
          "test2".getBytes()
        )
      }

      "data entry" in {
        sect(DataEntry) shouldBe hash(
          address.bytes,
          "test".getBytes(),
          dataEntry.valueBytes
        )
      }

      "lease status" in {
        sect(LeaseStatus) shouldBe hash(
          assetId.id.arr,
          Array(1.toByte)
        )
      }

      "sponsor" in {
        sect(Sponsorship) shouldBe hash(
          assetId.id.arr,
          Longs.toByteArray(1000)
        )
      }
    }

    "total" in {
      val allHashes = SectionId.values.toSeq.map(id => result.hashes(id))
      allHashes shouldBe Seq(WavesBalance, AssetBalance, DataEntry, AccountScript, AssetScript, LeaseBalance, LeaseStatus, Sponsorship, Alias)
        .map(sect)

      val testPrevHash = sect(SectionId.Alias)
      result.createStateHash(testPrevHash).totalHash shouldBe hash((testPrevHash.arr +: allHashes.map(_.arr))*)
      result.copy(hashes = result.hashes - SectionId.WavesBalance).createStateHash(ByteStr.empty).totalHash shouldBe hash(
        (StateHashBuilder.EmptySectionHash.arr +: allHashes.tail.map(_.arr))*
      )
    }
  }
}
