package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.{ARR, CaseObj}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.converters
import com.wavesplatform.lang.v1.traits.domain.Tx._
import com.wavesplatform.lang.v1.traits.domain._
import scodec.bits.ByteVector

object Bindings {

  import converters._

  private def combine(m0: Map[String, EVALUATED], m1: Map[String, EVALUATED]*) = m1.toList.fold(m0)(_ ++ _)

  import Types._

  private def headerPart(tx: Header): Map[String, EVALUATED] = Map(
    "id"        -> tx.id,
    "fee"       -> tx.fee,
    "timestamp" -> tx.timestamp,
    "version"   -> tx.version,
  )

  private def proofsPart(existingProofs: IndexedSeq[ByteVector]) =
    "proofs" -> ARR((existingProofs ++ Seq.fill(8 - existingProofs.size)(ByteVector.empty)).map(CONST_BYTEVECTOR).toIndexedSeq)

  private def provenTxPart(tx: Proven): Map[String, EVALUATED] =
    combine(Map(
              "sender"          -> senderObject(tx.sender),
              "senderPublicKey" -> tx.senderPk,
              "bodyBytes"       -> tx.bodyBytes,
              proofsPart(tx.proofs)
            ),
            headerPart(tx.h))

  private def mapRecipient(r: Recipient) =
    "recipient" -> (r match {
      case Recipient.Alias(name) => CaseObj(aliasType.typeRef, Map("alias" -> name))
      case x: Recipient.Address  => senderObject(x)
    })

  def assetPair(ap: APair): CaseObj =
    CaseObj(
      assetPairType.typeRef,
      Map(
        "amountAsset" -> fromOptionBV(ap.amountAsset),
        "priceAsset"  -> ap.priceAsset
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
        "amount"           -> ord.amount,
        "price"            -> ord.price,
        "timestamp"        -> ord.timestamp,
        "expiration"       -> ord.expiration,
        "matcherFee"       -> ord.matcherFee,
        "bodyBytes"        -> ord.bodyBytes,
        proofsPart(ord.proofs)
      )
    )

  def senderObject(sender: Recipient.Address): CaseObj = CaseObj(addressType.typeRef, Map("bytes" -> sender.bytes))

  def transactionObject(tx: Tx): CaseObj =
    tx match {
      case Tx.Genesis(h, amount, recipient) =>
        CaseObj(genesisTransactionType.typeRef, Map("amount" -> CONST_LONG(amount)) ++ headerPart(h) + mapRecipient(recipient))
      case Tx.Payment(p, amount, recipient) =>
        CaseObj(paymentTransactionType.typeRef, Map("amount" -> CONST_LONG(amount)) ++ provenTxPart(p) + mapRecipient(recipient))
      case Tx.Transfer(p, feeAssetId, assetId, amount, recipient, attachment) =>
        CaseObj(
          transferTransactionType.typeRef,
          combine(
            Map(
              "amount"     -> amount,
              "feeAssetId" -> feeAssetId,
              "assetId"    -> assetId,
              "attachment" -> attachment
            ),
            provenTxPart(p) + mapRecipient(recipient)
          )
        )
      case Issue(p, quantity, name, description, reissuable, decimals, scriptOpt) =>
        CaseObj(
          issueTransactionType.typeRef,
          combine(
            Map(
              "quantity"    -> quantity,
              "name"        -> name,
              "description" -> description,
              "reissuable"  -> reissuable,
              "decimals"    -> decimals,
              "script"      -> scriptOpt
            ),
            provenTxPart(p)
          )
        )
      case ReIssue(p, quantity, assetId, reissuable) =>
        CaseObj(reissueTransactionType.typeRef,
                combine(Map(
                          "quantity"   -> quantity,
                          "assetId"    -> assetId,
                          "reissuable" -> reissuable,
                        ),
                        provenTxPart(p)))
      case Burn(p, quantity, assetId) =>
        CaseObj(burnTransactionType.typeRef,
                combine(Map(
                          "quantity" -> quantity,
                          "assetId"  -> assetId
                        ),
                        provenTxPart(p)))
      case Lease(p, amount, recipient) =>
        CaseObj(leaseTransactionType.typeRef,
                combine(Map(
                          "amount" -> amount,
                        ),
                        provenTxPart(p) + mapRecipient(recipient)))
      case LeaseCancel(p, leaseId) =>
        CaseObj(
          leaseCancelTransactionType.typeRef,
          combine(Map(
                    "leaseId" -> leaseId,
                  ),
                  provenTxPart(p))
        )
      case CreateAlias(p, alias) =>
        CaseObj(createAliasTransactionType.typeRef,
                combine(Map(
                          "alias" -> alias,
                        ),
                        provenTxPart(p)))
      case MassTransfer(p, assetId, transferCount, totalAmount, transfers, attachment) =>
        CaseObj(
          massTransferTransactionType.typeRef,
          combine(
            Map(
              "transfers" -> transfers
                .map(bv => CaseObj(transfer.typeRef, Map(mapRecipient(bv.recipient), "amount" -> bv.amount))),
              "assetId"       -> assetId,
              "transferCount" -> transferCount,
              "totalAmount"   -> totalAmount,
              "attachment"    -> attachment
            ),
            provenTxPart(p)
          )
        )
      case SetScript(p, scriptOpt) =>
        CaseObj(setScriptTransactionType.typeRef, Map("script" -> fromOptionBV(scriptOpt)) ++ provenTxPart(p))
      case SetAssetScript(p, assetId, scriptOpt) =>
        CaseObj(setScriptTransactionType.typeRef,combine( Map("script" -> fromOptionBV(scriptOpt), "assetId" -> assetId) , provenTxPart(p)))
      case Sponsorship(p, assetId, minSponsoredAssetFee) =>
        CaseObj(
          sponsorFeeTransactionType.typeRef,
          combine(Map("assetId" -> assetId, "minSponsoredAssetFee" -> minSponsoredAssetFee), provenTxPart(p))
        )
      case Data(p, data) =>
        def mapValue(e: Any): EVALUATED = e match {
          case s: String     => c(s)
          case s: Boolean    => c(s)
          case s: Long       => c(s)
          case s: ByteVector => c(s)
          case _             => ???
        }

        CaseObj(
          dataTransactionType.typeRef,
          combine(Map("data" -> data.map(e => CaseObj(dataEntryType.typeRef, Map("key" -> CONST_STRING(e.key), "value" -> mapValue(e.value))))),
                  provenTxPart(p))
        )
      case Exchange(p, amount, price, buyMatcherFee, sellMatcherFee, buyOrder, sellOrder) =>
        CaseObj(
          exchangeTransactionType.typeRef,
          combine(
            Map(
              "buyOrder"       -> orderObject(buyOrder),
              "sellOrder"      -> orderObject(sellOrder),
              "amount"         -> amount,
              "price"          -> price,
              "buyMatcherFee"  -> buyMatcherFee,
              "sellMatcherFee" -> sellMatcherFee,
            ),
            provenTxPart(p)
          )
        )
    }

}
