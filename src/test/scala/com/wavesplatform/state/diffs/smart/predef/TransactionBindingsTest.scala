package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.state._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import scorex.account.{Address, Alias}

class TransactionBindingsTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  property("TransferTransaction binding") {
    forAll(transferV1Gen) { t =>
      // `version`  is not properly bound yet
      // `proofs`   can be up to 8, gens generate only 1
      // attachment is too big to be encoded in base58
      // bodyBytes  is too big to be encoded in base58

      val result = runScript[Boolean](
        s"""
          |match tx {
          | case t : TransferTransaction  => 
          |   let id = t.id == base58'${t.id().base58}'
          |   let fee = t.fee == ${t.fee}
          |   let timestamp = t.timestamp == ${t.timestamp}
          |   let senderPk = t.senderPk == base58'${ByteStr(t.sender.publicKey).base58}'
          |   let proof0 = t.proofs[0] == base58'${t.signature.base58}'
          |   let proof1 = t.proofs[1] == base58''
          |   let amount = t.amount == ${t.amount}
          |   let feeAssetId = if (${t.feeAssetId.isDefined})
          |      then extract(t.feeAssetId) == base58'${t.feeAssetId.getOrElse(ByteStr.empty).base58}'
          |      else isDefined(t.feeAssetId) == false
          |   let transferAssetId = if (${t.assetId.isDefined})
          |      then extract(t.transferAssetId) == base58'${t.assetId.getOrElse(ByteStr.empty).base58}'
          |      else isDefined(t.transferAssetId) == false
          |   let recipient = match (t.recipient) {
          |       case a: Address => a.bytes == base58'${t.recipient.cast[Address].map(_.bytes.base58).getOrElse("")}'
          |       case a: Alias => a.alias == "${t.recipient.cast[Alias].map(_.name).getOrElse("")}"
          |      }
          |   id && fee && timestamp && senderPk && proof0 && proof1 && amount && feeAssetId && transferAssetId && recipient
          | case other => throw
          | }
          |""".stripMargin,
        t
      )
      result shouldBe Right(true)
    }
  }

}
