package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import com.wavesplatform.lang.v1.compiler.Terms.{BOOLEAN, BYTEVECTOR, LONG, STRING}
import com.wavesplatform.lang.v1.evaluator.ctx.{CaseObj, Val}
import com.wavesplatform.lang.v1.traits.DataItem.{Bin, Bool, Lng, Str}
import com.wavesplatform.lang.v1.traits.Tx._
import com.wavesplatform.lang.v1.traits._
import scodec.bits.ByteVector

object Bindings {
  import Types._

  private def headerPart(tx: Header): Map[String, Val] = Map(
    "id"        -> Val(BYTEVECTOR)(tx.id),
    "fee"       -> Val(LONG)(tx.fee),
    "timestamp" -> Val(LONG)(tx.timestamp),
    "version"   -> Val(LONG)(tx.version),
  )

  private def provenTxPart(tx: Proven): Map[String, Val] =
    Map(
      "senderPk"  -> Val(BYTEVECTOR)(tx.senderPk),
      "bodyBytes" -> Val(BYTEVECTOR)(tx.bodyBytes),
      "proofs" -> Val(listByteVector) {
        val existingProofs = tx.proofs
        val allProofs      = existingProofs ++ Seq.fill(8 - existingProofs.size)(ByteVector.empty)
        allProofs.toIndexedSeq.asInstanceOf[listByteVector.Underlying]
      }
    ) ++ headerPart(tx.h)

  private def mapRecipient(r: Recipient) =
    "recipient" -> Val(addressOrAliasType)(r match {
      case Recipient.Alias(name)    => CaseObj(aliasType.typeRef, Map("alias"   -> Val(STRING)(name)))
      case Recipient.Address(bytes) => CaseObj(addressType.typeRef, Map("bytes" -> Val(BYTEVECTOR)(bytes)))
    })

  def assetPair(ap: APair): CaseObj =
    CaseObj(
      assetPairType.typeRef,
      Map(
        "amountAsset" -> Val(optionByteVector)(ap.amountAsset.asInstanceOf[optionByteVector.Underlying]),
        "priceAsset"  -> Val(optionByteVector)(ap.priceAsset.asInstanceOf[optionByteVector.Underlying])
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
        "senderPublicKey"  -> Val(BYTEVECTOR)(ord.senderPublicKey),
        "matcherPublicKey" -> Val(BYTEVECTOR)(ord.matcherPublicKey),
        "assetPair"        -> Val(assetPairType.typeRef)(assetPair(ord.assetPair)),
        "orderType"        -> Val(ordTypeType)(ordType(ord.orderType)),
        "price"            -> Val(LONG)(ord.price),
        "amount"           -> Val(LONG)(ord.amount),
        "timestamp"        -> Val(LONG)(ord.timestamp),
        "expiration"       -> Val(LONG)(ord.expiration),
        "matcherFee"       -> Val(LONG)(ord.matcherFee),
        "signature"        -> Val(BYTEVECTOR)(ord.signature)
      )
    )

  def transactionObject(tx: Tx): CaseObj =
    tx match {
      case Tx.Genesis(h, amount, recipient) =>
        CaseObj(genesisTransactionType.typeRef, Map("amount" -> Val(LONG)(amount)) ++ headerPart(h) + mapRecipient(recipient))
      case Tx.Payment(p, amount, recipient) =>
        CaseObj(genesisTransactionType.typeRef, Map("amount" -> Val(LONG)(amount)) ++ provenTxPart(p) + mapRecipient(recipient))
      case Tx.Transfer(p, feeAssetId, transferAssetId, amount, recipient, attachment) =>
        CaseObj(
          transferTransactionType.typeRef,
          Map(
            "amount"          -> Val(LONG)(amount),
            "feeAssetId"      -> Val(optionByteVector)(feeAssetId.asInstanceOf[optionByteVector.Underlying]),
            "transferAssetId" -> Val(optionByteVector)(transferAssetId.asInstanceOf[optionByteVector.Underlying]),
            "attachment"      -> Val(BYTEVECTOR)(attachment)
          ) ++ provenTxPart(p) + mapRecipient(recipient)
        )
      case Issue(p, amount, assetName, assetDescription, reissuable) =>
        CaseObj(
          issueTransactionType.typeRef,
          Map(
            "amount"           -> Val(LONG)(amount),
            "assetName"        -> Val(BYTEVECTOR)(assetName),
            "assetDescription" -> Val(BYTEVECTOR)(assetDescription),
            "reissuable"       -> Val(BOOLEAN)(reissuable)
          ) ++ provenTxPart(p)
        )
      case ReIssue(p, amount, reissuable) =>
        CaseObj(reissueTransactionType.typeRef,
                Map(
                  "amount"     -> Val(LONG)(amount),
                  "reissuable" -> Val(BOOLEAN)(reissuable),
                ) ++ provenTxPart(p))
      case Burn(p, amount) =>
        CaseObj(burnTransactionType.typeRef,
                Map(
                  "amount" -> Val(LONG)(amount),
                ) ++ provenTxPart(p))
      case Lease(p, amount, recipient) =>
        CaseObj(
          leaseTransactionType.typeRef,
          Map(
            "amount" -> Val(LONG)(amount),
          ) ++ provenTxPart(p) + mapRecipient(recipient)
        )
      case LeaseCancel(p, leaseId) =>
        CaseObj(
          leaseCancelTransactionType.typeRef,
          Map(
            "leaseId" -> Val(BYTEVECTOR)(leaseId),
          ) ++ provenTxPart(p)
        )
      case CreateAlias(p, alias) =>
        CaseObj(
          leaseCancelTransactionType.typeRef,
          Map(
            "alias" -> Val(STRING)(alias),
          ) ++ provenTxPart(p)
        )
      case MassTransfer(p, transferAssetId, transfers, attachment) =>
        CaseObj(
          massTransferTransactionType.typeRef,
          Map(
            "transfers" -> Val(listTransfers)(
              transfers
                .map(bv => CaseObj(transfer.typeRef, Map(mapRecipient(bv.recipient), "amount" -> Val(LONG)(bv.amount))))
                .asInstanceOf[listTransfers.Underlying]),
            "transferAssetId" -> Val(optionByteVector)(transferAssetId.asInstanceOf[optionByteVector.Underlying]),
            "attachment"      -> Val(BYTEVECTOR)(attachment)
          ) ++ provenTxPart(p)
        )
      case SetScript(p, scriptOpt) =>
        CaseObj(setScriptTransactionType.typeRef,
                Map("script" -> Val(optionByteVector)(scriptOpt.asInstanceOf[optionByteVector.Underlying])) ++ provenTxPart(p))
      case Sponsorship(p, minFee) =>
        CaseObj(sponsorFeeTransactionType.typeRef, Map("minFee" -> Val(optionLong)(minFee.asInstanceOf[optionLong.Underlying])) ++ provenTxPart(p))
      case Data(p, dataItems) =>
        CaseObj(
          dataTransactionType.typeRef,
          Map(
            "dataEntries" -> Val(listOfDataEntriesType)(dataItems
              .map {
                case Lng(k, v)  => CaseObj(longDataEntryType.typeRef, Map("key" -> Val(STRING)(k), "value" -> Val(LONG)(v)))
                case Str(k, v)  => CaseObj(longDataEntryType.typeRef, Map("key" -> Val(STRING)(k), "value" -> Val(STRING)(v)))
                case Bool(k, v) => CaseObj(longDataEntryType.typeRef, Map("key" -> Val(STRING)(k), "value" -> Val(BOOLEAN)(v)))
                case Bin(k, v)  => CaseObj(longDataEntryType.typeRef, Map("key" -> Val(STRING)(k), "value" -> Val(BYTEVECTOR)(v)))
              }
              .asInstanceOf[listOfDataEntriesType.Underlying])) ++
            provenTxPart(p)
        )
      case Exchange(p, price, amount, buyMatcherFee, sellMatcherFee, buyOrder, sellOrder) =>
        CaseObj(
          exchangeTransactionType.typeRef,
          Map(
            "buyOrder"       -> Val(orderType.typeRef)(orderObject(buyOrder)),
            "sellOrder"      -> Val(orderType.typeRef)(orderObject(sellOrder)),
            "price"          -> Val(LONG)(price),
            "amount"         -> Val(LONG)(amount),
            "buyMatcherFee"  -> Val(LONG)(buyMatcherFee),
            "sellMatcherFee" -> Val(LONG)(sellMatcherFee),
          ) ++ provenTxPart(p)
        )
    }

}
