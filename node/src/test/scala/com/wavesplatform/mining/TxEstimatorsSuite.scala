package com.wavesplatform.mining

import com.wavesplatform.TransactionGen
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.V1
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.TransactionDiffer
import com.wavesplatform.state.{AssetDescription, Blockchain}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.transfer.TransferTransactionV1
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers}

class TxEstimatorsSuite extends FreeSpec with Matchers with MockFactory with TransactionGen {
  def differ = TransactionDiffer(None, System.currentTimeMillis(), 1, false)() _
  val preActivatedFeatures = TestFunctionalitySettings.Enabled.preActivatedFeatures
  "scriptRunNumber" - {
    "smart account" - {
      "should not count transactions going from a regular account" in {
        val blockchain = mock[Blockchain]
        (blockchain.hasScript _).expects(*).returning(false).anyNumberOfTimes()
        (blockchain.activatedFeatures _).expects().returning(preActivatedFeatures).anyNumberOfTimes()
        (blockchain.height _).expects().returning(1).anyNumberOfTimes()

        TxEstimators.scriptRunNumber(blockchain, transferWavesTx, differ(blockchain, transferWavesTx).resultE.right.get) shouldBe 0
      }

      "should count transactions going from a smart account" in {
        val blockchain = mock[Blockchain]
        (blockchain.hasScript _).expects(*).returning(true).anyNumberOfTimes()
        (blockchain.activatedFeatures _).expects().returning(preActivatedFeatures).anyNumberOfTimes()
        (blockchain.height _).expects().returning(1).anyNumberOfTimes()

        TxEstimators.scriptRunNumber(blockchain, transferWavesTx, differ(blockchain, transferWavesTx).resultE.right.get) shouldBe 1
      }
    }

    "smart tokens" - {
      "should not count transactions working with a regular tokens" in {
        val blockchain = mock[Blockchain]
        (blockchain.hasScript _).expects(*).returning(false).anyNumberOfTimes()
        (blockchain.assetDescription _).expects(*).returning(None).anyNumberOfTimes()
        (blockchain.activatedFeatures _).expects().returning(preActivatedFeatures).anyNumberOfTimes()
        (blockchain.height _).expects().returning(1).anyNumberOfTimes()

        TxEstimators.scriptRunNumber(blockchain, transferAssetsTx, differ(blockchain, transferWavesTx).resultE.right.get) shouldBe 0
      }

      "should count transactions working with smart tokens" in {
        val blockchain = mock[Blockchain]
        (blockchain.hasScript _).expects(*).returning(false).anyNumberOfTimes()
        (blockchain.hasAssetScript _).expects(*).returning(true).anyNumberOfTimes()
        (blockchain.assetDescription _).expects(*).returning(Some(assetDescription)).anyNumberOfTimes()
        (blockchain.activatedFeatures _).expects().returning(preActivatedFeatures).anyNumberOfTimes()
        (blockchain.height _).expects().returning(1).anyNumberOfTimes()

        TxEstimators.scriptRunNumber(blockchain, transferAssetsTx, differ(blockchain, transferAssetsTx).resultE.right.get) shouldBe 1
      }
    }

    "both - should double count transactions working with smart tokens from samrt account" in {
      val blockchain = mock[Blockchain]
      (blockchain.hasScript _).expects(*).returning(true).anyNumberOfTimes()
      (blockchain.hasAssetScript _).expects(*).returning(true).anyNumberOfTimes()
      (blockchain.assetDescription _).expects(*).returning(Some(assetDescription)).anyNumberOfTimes()
      (blockchain.activatedFeatures _).expects().returning(preActivatedFeatures).anyNumberOfTimes()
      (blockchain.height _).expects().returning(1).anyNumberOfTimes()

      TxEstimators.scriptRunNumber(blockchain, transferAssetsTx, differ(blockchain, transferAssetsTx).resultE.right.get) shouldBe 2
    }
  }

  private val assetId = ByteStr("coin_id".getBytes())
  private val script  = ExprScript(V1, Terms.TRUE, checkSize = false).explicitGet()

  private val transferWavesTx = TransferTransactionV1
    .selfSigned(
      assetId = Waves,
      sender = KeyPair("sender".getBytes()),
      recipient = KeyPair("recipient".getBytes()),
      amount = 1,
      timestamp = System.currentTimeMillis(),
      feeAssetId = Waves,
      feeAmount = 100000,
      attachment = Array.emptyByteArray
    )
    .explicitGet()

  private val transferAssetsTx = TransferTransactionV1
    .selfSigned(
      assetId = IssuedAsset(assetId),
      sender = KeyPair("sender".getBytes()),
      recipient = KeyPair("recipient".getBytes()),
      amount = 1,
      timestamp = System.currentTimeMillis(),
      feeAssetId = Waves,
      feeAmount = 100000,
      attachment = Array.emptyByteArray
    )
    .explicitGet()

  private val assetDescription = AssetDescription(
    issuer = KeyPair("sender".getBytes()),
    name = "coin".getBytes(),
    description = "description".getBytes(),
    decimals = 2,
    reissuable = false,
    totalVolume = Long.MaxValue,
    script = Some(script),
    sponsorship = 0
  )
}
