package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.state._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.transaction.{ProvenTransaction, VersionedTransaction}
import com.wavesplatform.transaction.assets.exchange.Order
import play.api.libs.json.Json // For string escapes.

class TransactionBindingsTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {
  def provenPart(t: ProvenTransaction): String = {
    def pg(i: Int) = s"let proof$i = t.proofs[$i] == base58'${t.proofs.proofs.applyOrElse(i, (_: Int) => ByteStr.empty).base58}'"
    s"""
       |   let id = t.id == base58'${t.id().base58}'
       |   let fee = t.fee == ${t.assetFee._2}
       |   let timestamp = t.timestamp == ${t.timestamp}
       |   let bodyBytes = t.bodyBytes == base64'${ByteStr(t.bodyBytes.apply()).base64}'
       |   let sender = t.sender == addressFromPublicKey(base58'${ByteStr(t.sender.publicKey).base58}')
       |   let senderPublicKey = t.senderPublicKey == base58'${ByteStr(t.sender.publicKey).base58}'
       |   let version = t.version == ${t match {
         case v: VersionedTransaction => v.version
         case _                       => 1
       }}
       |   ${Range(0, 8).map(pg).mkString("\n")}
     """.stripMargin
  }

  val assertProvenPart =
    "id && fee && timestamp && sender && senderPublicKey && proof0 && proof1 && proof2 && proof3 && proof4 && proof5 && proof6 && proof7 && bodyBytes && version"

  property("TransferTransaction binding") {
    forAll(Gen.oneOf(transferV1Gen, transferV2Gen)) { t =>
      // `version`  is not properly bound yet

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
           |       case a: Alias => a.alias == ${Json.toJson(t.recipient.cast[Alias].map(_.name).getOrElse(""))}
           |      }
           |    let attachment = t.attachment == base58'${ByteStr(t.attachment).base58}'
           |   $assertProvenPart && amount && feeAssetId && transferAssetId && recipient && attachment
           | case other => throw
           | }
           |""".stripMargin,
        t,
        'T'
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
        t,
        'T'
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
        t,
        'T'
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
        t,
        'T'
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
          |   let alias = t.alias == ${Json.toJson(t.alias.name)}
          |   $assertProvenPart && alias
          | case other => throw
          | }
          |""".stripMargin,
        t,
        'T'
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
          |       case a: Alias => a.alias == ${Json.toJson(t.recipient.cast[Alias].map(_.name).getOrElse(""))}
          |      }
          |   $assertProvenPart && amount && recipient
          | case other => throw
          | }
          |""".stripMargin,
        t,
        'T'
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
        t,
        'T'
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
        t,
        'T'
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
        t,
        'T'
      )
      result shouldBe Right(true)
    }
  }

  property("DataTransaction binding") {
    forAll(dataTransactionGen(10)) { t =>
      def pg(i: Int) = {
        val v = t.data(i) match {
          case e: IntegerDataEntry => e.value.toString
          case e: BooleanDataEntry => e.value.toString
          case e: BinaryDataEntry  => s"base64'${e.value.base64}'"
          case e: StringDataEntry  => Json.toJson(e.value)
        }

        s"""let key$i = t.data[$i].key == ${Json.toJson(t.data(i).key)}
           |let value$i = t.data[$i].value == $v
         """.stripMargin
      }

      val resString =
        if (t.data.isEmpty) assertProvenPart else assertProvenPart + s" && ${t.data.indices.map(i => s"key$i && value$i").mkString(" && ")}"

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
        t,
        'T'
      )
      result shouldBe Right(true)
    }
  }

  property("MassTransferTransaction binding") {
    forAll(massTransferGen) { t =>
      def pg(i: Int) =
        s"""let recipient$i = match (t.transfers[$i].recipient) {
           |case a: Address => a.bytes == base58'${t.transfers(i).address.cast[Address].map(_.bytes.base58).getOrElse("")}'
           |case a: Alias => a.alias == ${Json.toJson(t.transfers(i).address.cast[Alias].map(_.name).getOrElse(""))}
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
                      |    let assetId = if (${t.assetId.isDefined}) then extract(t.transferAssetId) == base58'${t.assetId
                        .getOrElse(ByteStr.empty)
                        .base58}'
                      |      else isDefined(t.transferAssetId) == false
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
        t,
        'T'
      )
      result shouldBe Right(true)
    }
  }

  property("ExchangeTransaction binding") {
    forAll(exchangeTransactionGen) { t =>
      def pg(ord: Order) = {
        val oType = ord.orderType.toString
        val script = s"""
           |   let ${oType}Id = t.${oType}Order.id == base58'${ByteStr(ord.id.value).base58}'
           |   let ${oType}Sender = t.${oType}Order.sender == addressFromPublicKey(base58'${ByteStr(ord.sender.publicKey).base58}')
           |   let ${oType}SenderPk = t.${oType}Order.senderPublicKey == base58'${ByteStr(ord.sender.publicKey).base58}'
           |   let ${oType}MatcherPk = t.${oType}Order.matcherPublicKey == base58'${ByteStr(ord.matcherPublicKey.publicKey).base58}'
           |   let ${oType}Price = t.${oType}Order.price == ${ord.price}
           |   let ${oType}Amount = t.${oType}Order.amount == ${ord.amount}
           |   let ${oType}Timestamp = t.${oType}Order.timestamp == ${ord.timestamp}
           |   let ${oType}Expiration = t.${oType}Order.expiration == ${ord.expiration}
           |   let ${oType}OrderMatcherFee = t.${oType}Order.matcherFee == ${ord.matcherFee}
           |   let ${oType}Signature = t.${oType}Order.signature == base58'${ByteStr(ord.signature).base58}'
           |   let ${oType}AssetPairAmount = if (${ord.assetPair.amountAsset.isDefined}) then extract(t.${oType}Order.assetPair.amountAsset) == base58'${ord.assetPair.amountAsset
                          .getOrElse(ByteStr.empty)
                          .base58}'
           |   else isDefined(t.${oType}Order.assetPair.amountAsset) == false
           |   let ${oType}AssetPairPrice = if (${ord.assetPair.priceAsset.isDefined}) then extract(t.${oType}Order.assetPair.priceAsset) == base58'${ord.assetPair.priceAsset
                          .getOrElse(ByteStr.empty)
                          .base58}'
           |   else isDefined(t.${oType}Order.assetPair.priceAsset) == false
           |   # let ${oType}OrderType = t.${oType}Order.orderType ==
         """.stripMargin

        val lets = List("Id",
                        "Sender",
                        "SenderPk",
                        "MatcherPk",
                        "Price",
                        "Amount",
                        "Timestamp",
                        "Expiration",
                        "OrderMatcherFee",
                        "Signature",
                        "AssetPairAmount",
                        "AssetPairPrice")
          .map(i => s"${oType}$i")
          .mkString(" && ")

        (script, lets)
      }

      val s = s"""|match tx {
                | case t : ExchangeTransaction =>
                |   ${provenPart(t)}
                |   let price = t.price == ${t.price}
                |   let amount = t.amount == ${t.amount}
                |   let buyMatcherFee = t.buyMatcherFee == ${t.buyMatcherFee}
                |   let sellMatcherFee = t.sellMatcherFee == ${t.sellMatcherFee} 
                |   ${pg(t.buyOrder)._1}
                |   ${pg(t.sellOrder)._1}
                |   $assertProvenPart && price && amount && buyMatcherFee && sellMatcherFee && ${pg(t.buyOrder)._2} && ${pg(t.sellOrder)._2}
                | case other => throw
                | }
                |""".stripMargin

      val result = runScript[Boolean](
        s,
        t,
        'T'
      )
      result shouldBe Right(true)
    }
  }

}
