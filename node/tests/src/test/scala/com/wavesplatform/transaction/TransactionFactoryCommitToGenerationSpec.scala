package com.wavesplatform.transaction

import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.test.*
import play.api.libs.json.Json

class TransactionFactoryCommitToGenerationSpec extends FreeSpec with WithDomain {
  private val sender = TxHelpers.signer(1)

  private def sign(senderBound: Boolean): CommitToGenerationTransaction =
    TransactionFactory
      .parseRequestAndSign(Json.obj("type" -> TransactionType.CommitToGeneration.id), sender, Some(3001), senderBound)
      .explicitGet()
      .asInstanceOf[CommitToGenerationTransaction]

  private def popVerifies(tx: CommitToGenerationTransaction, senderBound: Boolean): Boolean =
    tx.commitmentSignature.verifyBasic(tx.popMessage(senderBound), tx.endorserPublicKey).isRight

  "a signed commitment's proof of possession" - {
    "is not bound to the sender before Finality Improvements" in
      withDomain(DomainPresets.DeterministicFinality, AddrWithBalance.enoughBalances(sender)) { d =>
        val tx = sign(senderBound = false)
        popVerifies(tx, senderBound = false) shouldBe true
        popVerifies(tx, senderBound = true) shouldBe false
        d.appendAndAssertSucceed(tx)
      }

    "is bound to the sender since Finality Improvements" in
      withDomain(DomainPresets.FinalityImprovements, AddrWithBalance.enoughBalances(sender)) { d =>
        d.blockchain.isFeatureActivated(BlockchainFeatures.FinalityImprovements, d.blockchain.height + 1) shouldBe true
        val tx = sign(senderBound = true)
        popVerifies(tx, senderBound = true) shouldBe true
        popVerifies(tx, senderBound = false) shouldBe false
        d.appendAndAssertSucceed(tx)
      }

    "names the request's sender when it differs from the signer" in {
      val other = TxHelpers.signer(2)
      val tx = TransactionFactory
        .parseRequestAndSign(
          Json.obj("type" -> TransactionType.CommitToGeneration.id, "senderPublicKey" -> other.publicKey.toString),
          sender,
          Some(3001),
          popSenderBound = true
        )
        .explicitGet()
        .asInstanceOf[CommitToGenerationTransaction]
      tx.sender shouldBe other.publicKey
      popVerifies(tx, senderBound = true) shouldBe true
    }
  }
}
