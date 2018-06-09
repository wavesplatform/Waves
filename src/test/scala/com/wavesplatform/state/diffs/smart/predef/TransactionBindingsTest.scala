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
          |    let attachment = t.attachment == base58'${ByteStr(t.attachment).base58}'
          |   $assertProvenPart && amount && feeAssetId && transferAssetId && recipient && attachment
          | case other => throw
          | }
          |""".stripMargin,
        t
      )
      result shouldBe Right(true)
    }
  }

  property("IssueTransaction binding") {
    forAll(issueGen) { t =>
      val s = s"""
                 |match tx {
                 | case t : IssueTransaction =>
                 |   ${provenPart(t)}
                 |   let quantity = t.quantity == ${t.quantity}
                 |   let decimals = t.decimals == ${t.decimals}
                 |   let reissuable = t.reissuable == ${t.reissuable}
                 |   let name = t.name == base58'${ByteStr(t.name).base58}'
                 |   let description = t.description == base58'${ByteStr(t.description).base58}'
                 |   let script = if (${t.script.isDefined}) then extract(t.script) == base64'${t.script
                   .map(_.bytes().base64)
                   .getOrElse("")}' else isDefined(t.script) == false
                 |   $assertProvenPart && quantity && decimals && reissuable && script && name && description
                 | case other => throw
                 | }
                 |""".stripMargin

      val result = runScript[Boolean](
        s,
        t
      )
      result shouldBe Right(true)
    }
  }

  property("BurnTransaction binding") {
    forAll(burnGen) { t =>
      val result = runScript[Boolean](
        s"""
          |match tx {
          | case t : BurnTransaction =>
          |   ${provenPart(t)}
          |   let quantity = t.quantity == ${t.quantity}
          |   let assetId = t.assetId == base58'${t.assetId.base58}'
          |   $assertProvenPart && quantity && assetId
          | case other => throw
          | }
          |""".stripMargin,
        t
      )
      result shouldBe Right(true)
    }
  }

  property("ReissueTransaction binding") {
    forAll(reissueGen) { t =>
      val result = runScript[Boolean](
        s"""
          |match tx {
          | case t : ReissueTransaction =>
          |   ${provenPart(t)}
          |   let quantity = t.quantity == ${t.quantity}
          |   let assetId = t.assetId == base58'${t.assetId.base58}'
          |   let reissuable = t.reissuable == ${t.reissuable}
          |   $assertProvenPart && quantity && assetId && reissuable
          | case other => throw
          | }
          |""".stripMargin,
        t
      )
      result shouldBe Right(true)
    }
  }

  property("CreateAliasTransaction binding") {
    forAll(createAliasGen) { t =>
      val result = runScript[Boolean](
        s"""
          |match tx {
          | case t : CreateAliasTransaction =>
          |   ${provenPart(t)}
          |   let alias = t.alias == "${t.alias.name}"
          |   $assertProvenPart && alias
          | case other => throw
          | }
          |""".stripMargin,
        t
      )
      result shouldBe Right(true)
    }
  }

  property("LeaseTransaction binding") {
    forAll(leaseGen) { t =>
      val result = runScript[Boolean](
        s"""
          |match tx {
          | case t : LeaseTransaction =>
          |   ${provenPart(t)}
          |   let amount = t.amount == ${t.amount}
          |   let recipient = match (t.recipient) {
          |       case a: Address => a.bytes == base58'${t.recipient.cast[Address].map(_.bytes.base58).getOrElse("")}'
          |       case a: Alias => a.alias == "${t.recipient.cast[Alias].map(_.name).getOrElse("")}"
          |      }
          |   $assertProvenPart && amount && recipient
          | case other => throw
          | }
          |""".stripMargin,
        t
      )
      result shouldBe Right(true)
    }
  }

  property("LeaseCancelTransaction binding") {
    forAll(leaseCancelGen) { t =>
      val result = runScript[Boolean](
        s"""
          |match tx {
          | case t : LeaseCancelTransaction =>
          |   ${provenPart(t)}
          |   let leaseId = t.leaseId == base58'${t.leaseId.base58}'
          |   $assertProvenPart && leaseId
          | case other => throw
          | }
          |""".stripMargin,
        t
      )
      result shouldBe Right(true)
    }
  }

  property("SponsorFeeTransaction binding (+ cancel sponsorship transaction)") {
    forAll(Gen.oneOf(sponsorFeeGen, cancelFeeSponsorshipGen)) { t =>
      val result = runScript[Boolean](
        s"""
          |match tx {
          | case t : SponsorFeeTransaction =>
          |   ${provenPart(t)}
          |   let assetId = t.assetId == base58'${t.assetId.base58}'
          |   let minSponsoredAssetFee = if (${t.minSponsoredAssetFee.isDefined}) then extract(t.minSponsoredAssetFee) == ${t.minSponsoredAssetFee
             .getOrElse(0)} else isDefined(t.minSponsoredAssetFee) == false
          |   $assertProvenPart && assetId && minSponsoredAssetFee
          | case other => throw
          | }
          |""".stripMargin,
        t
      )
      result shouldBe Right(true)
    }
  }

  property("SetScriptTransaction binding") {
    forAll(setScriptTransactionGen) { t =>
      val result = runScript[Boolean](
        s"""
           |match tx {
           | case t : SetScriptTransaction =>
           |   ${provenPart(t)}
           |   let script = if (${t.script.isDefined}) then extract(t.script) == base64'${t.script
             .map(_.bytes().base64)
             .getOrElse("")}' else isDefined(t.script) == false
           |   $assertProvenPart && script
           | case other => throw
           | }
           |""".stripMargin,
        t
      )
      result shouldBe Right(true)
    }
  }

  property("DataTransaction binding") {
    forAll(dataTransactionGen(5, true)) { t =>
      def pg(i: Int) = {
        val v = t.data(i) match {
          case x: LongDataEntry    => s"case a: LongDataEntry => a.value == ${x.value}"
          case x: BooleanDataEntry => s"case a: BoolDataEntry => a.value == ${x.value}"
          case x: BinaryDataEntry  => s"case a: ByteVectorDataEntry => a.value == base64'${x.value.base64}'"
          case x: StringDataEntry  => s"""case a: StrDataEntry => a.value == "${x.value}""""
        }

        s"""let key$i = t.data[$i].key == "${t.data(i).key}"
           |let value$i = match (t.data[$i]) {
           | $v
           | case other => true
           |}
         """.stripMargin
      }

      val resString =
        if (t.data.isEmpty) assertProvenPart else assertProvenPart + s" && ${Range(0, t.data.length).map(i => s"key$i && value$i").mkString(" && ")}"

      val s = s"""
                 |match tx {
                 | case t : DataTransaction =>
                 |   ${provenPart(t)}
                 |   ${Range(0, t.data.length).map(pg).mkString("\n")}
                 |   $resString
                 | case other => throw
                 | }
                 |""".stripMargin

      val result = runScript[Boolean](
        s,
        t
      )
      result shouldBe Right(true)
    }
  }

  property("MassTransferTransaction binding") {
    forAll(massTransferGen(10)) { t =>
      def pg(i: Int) =
        s"""let recipient$i = match (t.transfers[$i].recipient) {
           |case a: Address => a.bytes == base58'${t.transfers(i).address.cast[Address].map(_.bytes.base58).getOrElse("")}'
           |case a: Alias => a.alias == "${t.transfers(i).address.cast[Alias].map(_.name).getOrElse("")}"
           |}
           |let amount$i = t.transfers[$i].amount == ${t.transfers(i).amount}
         """.stripMargin

      val resString =
        if (t.transfers.isEmpty) assertProvenPart
        else
          assertProvenPart + s" &&" + {
            Range(0, t.transfers.length)
              .map(i => s"recipient$i && amount$i")
              .mkString(" && ")
          }

      val script = s"""
                      |match tx {
                      | case t : MassTransferTransaction =>
                      |    let assetId = if (${t.assetId.isDefined}) then extract(t.assetId) == base58'${t.assetId.getOrElse(ByteStr.empty).base58}'
                      |      else isDefined(t.assetId) == false
                      |     let transferCount = t.transferCount == ${t.transfers.length}
                      |     let totalAmount = t.totalAmount == ${t.transfers.map(_.amount).sum}
                      |     let attachment = t.attachment == base58'${ByteStr(t.attachment).base58}'
                      |     ${Range(0, t.transfers.length).map(pg).mkString("\n")}
                      |   ${provenPart(t)}
                      |   $resString && assetId && transferCount && totalAmount && attachment
                      | case other => throw
                      | }
                      |""".stripMargin

      val result = runScript[Boolean](
        script,
        t
      )
      result shouldBe Right(true)
    }
  }

}
