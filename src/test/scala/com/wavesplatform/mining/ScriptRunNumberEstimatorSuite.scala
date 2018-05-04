package com.wavesplatform.mining

import com.wavesplatform.TransactionGen
import com.wavesplatform.lang.v1.Terms.Typed
import com.wavesplatform.state.{AssetDescription, Blockchain, ByteStr, EitherExt2}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{FreeSpec, Matchers}
import scorex.account.{Address, PrivateKeyAccount}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.smart.script.v1.ScriptV1
import scorex.transaction.transfer.TransferTransactionV1

class ScriptRunNumberEstimatorSuite extends FreeSpec with Matchers with PathMockFactory with TransactionGen {
  "ScriptRunNumberEstimator" - {
    "smart account" - {
      "should not count transactions going from a regular account" in {
        val blockchain = stub[Blockchain]
        (blockchain.accountScript _).when(*).onCall((_: Address) => None).anyNumberOfTimes()

        ScriptRunNumberEstimator.estimate(blockchain, transferWavesTx) shouldBe 0
      }

      "should count transactions going from a smart account" in {
        val blockchain = stub[Blockchain]
        (blockchain.accountScript _).when(*).onCall((_: Address) => Some(script)).anyNumberOfTimes()

        ScriptRunNumberEstimator.estimate(blockchain, transferWavesTx) shouldBe 1
      }
    }

    "smart tokens" - {
      "should not count transactions working with a regular tokens" in {
        val blockchain = stub[Blockchain]
        (blockchain.accountScript _).when(*).onCall((_: Address) => None).anyNumberOfTimes()
        (blockchain.assetDescription _).when(*).onCall((_: ByteStr) => None).anyNumberOfTimes()

        ScriptRunNumberEstimator.estimate(blockchain, transferAssetsTx) shouldBe 0
      }

      "should count transactions working with smart tokens" in {
        val blockchain = stub[Blockchain]
        (blockchain.accountScript _).when(*).onCall((_: Address) => None).anyNumberOfTimes()
        (blockchain.assetDescription _).when(*).onCall((_: ByteStr) => Some(assetDescription)).anyNumberOfTimes()

        ScriptRunNumberEstimator.estimate(blockchain, transferAssetsTx) shouldBe 1
      }
    }

    "should sum both types of transactions" in {
      val blockchain = stub[Blockchain]
      (blockchain.accountScript _).when(*).onCall((_: Address) => Some(script)).anyNumberOfTimes()
      (blockchain.assetDescription _).when(*).onCall((_: ByteStr) => Some(assetDescription)).anyNumberOfTimes()

      // 3 because from one account, so transferAssetsTx is validated by both scripts
      ScriptRunNumberEstimator.estimate(blockchain, TestBlock.create(Seq(transferWavesTx, transferAssetsTx))) shouldBe 3
    }
  }

  private val assetId = ByteStr("coin_id".getBytes())
  private val script  = ScriptV1(Typed.TRUE, checkSize = false).explicitGet()

  private val transferWavesTx = TransferTransactionV1
    .create(
      assetId = None,
      sender = PrivateKeyAccount("sender".getBytes()),
      recipient = PrivateKeyAccount("recipient".getBytes()),
      amount = 1,
      timestamp = System.currentTimeMillis(),
      feeAssetId = None,
      feeAmount = 100000,
      attachment = Array.emptyByteArray
    )
    .explicitGet()

  private val transferAssetsTx = TransferTransactionV1
    .create(
      assetId = Some(assetId),
      sender = PrivateKeyAccount("sender".getBytes()),
      recipient = PrivateKeyAccount("recipient".getBytes()),
      amount = 1,
      timestamp = System.currentTimeMillis(),
      feeAssetId = None,
      feeAmount = 100000,
      attachment = Array.emptyByteArray
    )
    .explicitGet()

  private val assetDescription = AssetDescription(
    issuer = PrivateKeyAccount("sender".getBytes()),
    name = "coin".getBytes(),
    description = "description".getBytes(),
    decimals = 2,
    reissuable = false,
    totalVolume = Long.MaxValue,
    script = Some(script),
    sponsorship = 0
  )
}
