package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.state._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import scorex.account.{Address, Alias}
import scorex.transaction.ProvenTransaction

class TransactionBindingsTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  def provenPart(t: ProvenTransaction): String = {
    def pg(i: Int) = s"let proof$i = t.proofs[$i] == base58'${t.proofs.proofs.applyOrElse(i, (_: Int) => ByteStr.empty).base58}'"
    s"""
       |   let id = t.id == base58'${t.id().base58}'
       |   let fee = t.fee == ${t.assetFee._2}
       |   let timestamp = t.timestamp == ${t.timestamp}
       |   let senderPk = t.senderPk == base58'${ByteStr(t.sender.publicKey).base58}'
       |   ${Range(0, 8).map(pg).mkString("\n")}
     """.stripMargin
  }

  val assertProvenPart = "id && fee && timestamp && senderPk && proof0 && proof1 && proof2 && proof3 && proof4 && proof5 && proof6 && proof7"

  property("TransferTransaction binding") {
    forAll(Gen.oneOf(transferV1Gen, transferV2Gen)) { t =>
      // `version`  is not properly bound yet
      // `proofs`   can be up to 8, gens generate only 1
      // attachment is too big to be encoded in base58
      // bodyBytes  is too big to be encoded in base58

      val result = runScript[Boolean](
        s"""
          |match tx {
          | case t : TransferTransaction  => 
          |   ${provenPart(t)}
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
          |   $assertProvenPart && amount && feeAssetId && transferAssetId && recipient
          | case other => throw
          | }
          |""".stripMargin,
        t
      )
      result shouldBe Right(true)
    }
  }

}
