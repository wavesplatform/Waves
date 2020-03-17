package com.wavesplatform.state

import com.google.common.primitives.Longs
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.state.StateHash.SectionId
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.{FreeSpec, Matchers}

class StateHashSpec extends FreeSpec with Matchers {
  "state hash" - {
    val stateHash = new StateHashBuilder
    val address   = Address.fromString("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8").explicitGet()
    val assetId   = IssuedAsset(Base58.decode("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz"))
    val Right((testScript, _)) = ScriptCompiler.compile(
      """
        |{-# STDLIB_VERSION 2 #-}
        |{-# CONTENT_TYPE EXPRESSION #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |true
        |""".stripMargin,
      ScriptEstimatorV1
    )
    val dataEntry = StringDataEntry("test", "test")

    stateHash.addLeaseBalance(address, 10000L, 10000L)
    stateHash.addAccountScript(address, Some(testScript))
    stateHash.addAssetScript(assetId, Some(testScript))
    stateHash.addAlias(address, "test")
    stateHash.addDataEntry(address, dataEntry)
    stateHash.addLeaseStatus(TransactionId @@ assetId.id, status = true)
    stateHash.addSponsorship(assetId, 1000)
    stateHash.addAssetBalance(address, assetId, 2000)
    stateHash.addWavesBalance(address, 1000)
    val result = stateHash.result()

    def hash(bs: ByteStr*): ByteStr        = com.wavesplatform.crypto.fastHash(bs.reduce(_ ++ _))
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
          assetId.id,
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
          testScript.bytes()
        )
      }

      "asset script" in {
        sect(AssetScript) shouldBe hash(
          assetId.id,
          testScript.bytes()
        )
      }

      "alias" in {
        sect(Alias) shouldBe hash(
          address.bytes,
          "test".getBytes()
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
          assetId.id,
          Array(1.toByte)
        )
      }

      "sponsor" in {
        sect(Sponsorship) shouldBe hash(
          assetId.id,
          Longs.toByteArray(1000)
        )
      }
    }

    "total" in {
      val allHashes = SectionId.values.toSeq.map(id => result.hashes(id))
      allHashes shouldBe Seq(WavesBalance, AssetBalance, DataEntry, AccountScript, AssetScript, LeaseBalance, LeaseStatus, Sponsorship, Alias)
        .map(sect)

      val testPrevHash = sect(SectionId.Alias)
      result.createStateHash(testPrevHash).totalHash shouldBe hash(testPrevHash +: allHashes: _*)
      result.copy(hashes = result.hashes - SectionId.WavesBalance).createStateHash(ByteStr.empty).totalHash shouldBe hash(
        StateHashBuilder.EmptySectionHash +: allHashes.tail: _*
      )
    }
  }
}
