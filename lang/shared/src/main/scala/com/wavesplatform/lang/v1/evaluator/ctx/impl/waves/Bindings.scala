package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import com.wavesplatform.lang.v1.evaluator.ctx.CaseObj
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext.fromOption
import com.wavesplatform.lang.v1.traits.Tx._
import com.wavesplatform.lang.v1.traits._
import scodec.bits.ByteVector

object Bindings {
  import Types._

  private def headerPart(tx: Header): Map[String, Any] = Map(
    "id"        -> tx.id,
    "fee"       -> tx.fee,
    "timestamp" -> tx.timestamp,
    "version"   -> tx.version,
  )

  private def provenTxPart(tx: Proven): Map[String, Any] =
    Map(
      "sender"          -> senderObject(tx.sender),
      "senderPublicKey" -> tx.senderPk,
      "bodyBytes"       -> tx.bodyBytes,
      "proofs" -> {
        val existingProofs = tx.proofs
        (existingProofs ++ Seq.fill(8 - existingProofs.size)(ByteVector.empty)).toIndexedSeq
      }
    ) ++ headerPart(tx.h)

  private def mapRecipient(r: Recipient) =
    "recipient" -> (r match {
      case Recipient.Alias(name) => CaseObj(aliasType.typeRef, Map("alias" -> name))
      case x: Recipient.Address  => senderObject(x)
    })

  def assetPair(ap: APair): CaseObj =
    CaseObj(
      assetPairType.typeRef,
      Map(
        "amountAsset" -> fromOption(ap.amountAsset),
        "priceAsset"  -> fromOption(ap.priceAsset)
      )
    )

  def ordType(o: OrdType): CaseObj =
    CaseObj((o match {
      case OrdType.Buy  => buyType
      case OrdType.Sell => sellType
    }).typeRef, Map.empty)

  def orderObject(ord: Ord): CaseObj =
    CaseObj(
      orderType.typeRef,
      Map(
        "id"               -> ord.id,
        "sender"           -> senderObject(ord.sender),
        "senderPublicKey"  -> ord.senderPublicKey,
        "matcherPublicKey" -> ord.matcherPublicKey,
        "assetPair"        -> assetPair(ord.assetPair),
        "orderType"        -> ordType(ord.orderType),
        "price"            -> ord.price,
        "amount"           -> ord.amount,
        "timestamp"        -> ord.timestamp,
        "expiration"       -> ord.expiration,
        "matcherFee"       -> ord.matcherFee,
        "signature"        -> ord.signature
      )
    )

  def senderObject(sender: Recipient.Address): CaseObj = CaseObj(addressType.typeRef, Map("bytes" -> sender.bytes))

  def transactionObject(tx: Tx): CaseObj =
    tx match {
      case Tx.Genesis(h, amount, recipient) =>
        CaseObj(genesisTransactionType.typeRef, Map("amount" -> amount) ++ headerPart(h) + mapRecipient(recipient))
      case Tx.Payment(p, amount, recipient) =>
        CaseObj(paymentTransactionType.typeRef, Map("amount" -> amount) ++ provenTxPart(p) + mapRecipient(recipient))
      case Tx.Transfer(p, feeAssetId, assetId, amount, recipient, attachment) =>
        CaseObj(
          transferTransactionType.typeRef,
          Map(
            "amount"     -> amount,
            "feeAssetId" -> fromOption(feeAssetId),
            "assetId"    -> fromOption(assetId),
            "attachment" -> attachment
          ) ++ provenTxPart(p) + mapRecipient(recipient)
        )
      case Issue(p, quantity, name, description, reissuable, decimals, scriptOpt) =>
        CaseObj(
          issueTransactionType.typeRef,
          Map(
            "quantity"    -> quantity,
            "name"        -> name,
            "description" -> description,
            "reissuable"  -> reissuable,
            "decimals"    -> decimals,
            "script"      -> fromOption(scriptOpt)
          ) ++ provenTxPart(p)
        )
      case ReIssue(p, quantity, assetId, reissuable) =>
        CaseObj(
          reissueTransactionType.typeRef,
          Map(
            "quantity"   -> quantity,
            "assetId"    -> assetId,
            "reissuable" -> reissuable,
          ) ++ provenTxPart(p)
        )
      case Burn(p, quantity, assetId) =>
        CaseObj(burnTransactionType.typeRef,
                Map(
                  "quantity" -> quantity,
                  "assetId"  -> assetId
                ) ++ provenTxPart(p))
      case Lease(p, amount, recipient) =>
        CaseObj(
          leaseTransactionType.typeRef,
          Map(
            "amount" -> amount,
          ) ++ provenTxPart(p) + mapRecipient(recipient)
        )
      case LeaseCancel(p, leaseId) =>
        CaseObj(
          leaseCancelTransactionType.typeRef,
          Map(
            "leaseId" -> leaseId,
          ) ++ provenTxPart(p)
        )
      case CreateAlias(p, alias) =>
        CaseObj(
          createAliasTransactionType.typeRef,
          Map(
            "alias" -> alias,
          ) ++ provenTxPart(p)
        )
      case MassTransfer(p, assetId, transferCount, totalAmount, transfers, attachment) =>
        CaseObj(
          massTransferTransactionType.typeRef,
          Map(
            "transfers" -> transfers
              .map(bv => CaseObj(transfer.typeRef, Map(mapRecipient(bv.recipient), "amount" -> bv.amount))),
            "assetId"       -> fromOption(assetId),
            "transferCount" -> transferCount,
            "totalAmount"   -> totalAmount,
            "attachment"    -> attachment
          ) ++ provenTxPart(p)
        )
      case SetScript(p, scriptOpt) =>
        CaseObj(setScriptTransactionType.typeRef, Map("script" -> fromOption(scriptOpt)) ++ provenTxPart(p))
      case Sponsorship(p, assetId, minSponsoredAssetFee) =>
        CaseObj(
          sponsorFeeTransactionType.typeRef,
          Map("assetId" -> assetId, "minSponsoredAssetFee" -> fromOption(minSponsoredAssetFee)) ++ provenTxPart(p)
        )
      case Data(p, data) =>
        CaseObj(
          dataTransactionType.typeRef,
          Map("data" -> data.map(e => CaseObj(dataEntryType.typeRef, Map("key" -> e.key, "value" -> e.value)))) ++ provenTxPart(p)
        )
      case Exchange(p, price, amount, buyMatcherFee, sellMatcherFee, buyOrder, sellOrder) =>
        CaseObj(
          exchangeTransactionType.typeRef,
          Map(
            "buyOrder"       -> orderObject(buyOrder),
            "sellOrder"      -> orderObject(sellOrder),
            "price"          -> price,
            "amount"         -> amount,
            "buyMatcherFee"  -> buyMatcherFee,
            "sellMatcherFee" -> sellMatcherFee,
          ) ++ provenTxPart(p)
        )
    }

}
