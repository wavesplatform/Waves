package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.converters
import com.wavesplatform.lang.v1.traits.domain.Tx._
import com.wavesplatform.lang.v1.traits.domain._

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

  private def proofsPart(existingProofs: IndexedSeq[ByteStr]) =
    "proofs" ->  ARR((existingProofs ++ IndexedSeq.fill(8 - existingProofs.size)(ByteStr.empty)).map(b => CONST_BYTESTR(b).explicitGet())).explicitGet()

  private def provenTxPart(tx: Proven, proofsEnabled: Boolean): Map[String, EVALUATED] = {
    val commonPart = combine(Map(
                               "sender"          -> senderObject(tx.sender),
                               "senderPublicKey" -> tx.senderPk,
                               "bodyBytes"       -> tx.bodyBytes
                             ),
                             headerPart(tx.h))

    if (proofsEnabled) combine(commonPart, Map(proofsPart(tx.proofs)))
    else commonPart
  }
  def mapRecipient(r: Recipient) =
    "recipient" -> (r match {
      case Recipient.Alias(name) => CaseObj(aliasType, Map("alias" -> name))
      case x: Recipient.Address  => senderObject(x)
    })

  def assetPair(ap: APair): CaseObj =
    CaseObj(
      assetPairType,
      Map(
        "amountAsset" -> fromOptionBV(ap.amountAsset),
        "priceAsset"  -> ap.priceAsset
      )
    )

  def ordType(o: OrdType): CaseObj =
    CaseObj((o match {
      case OrdType.Buy  => buyType
      case OrdType.Sell => sellType
    }), Map.empty)

  def buildPayment(mpmt: Option[Tx.Pmt]): CaseObj = mpmt match {
    case None => com.wavesplatform.lang.v1.evaluator.ctx.impl.unit
    case Some(pmt) =>
      CaseObj(
        paymentType,
        Map(
          "amount" -> CONST_LONG(pmt.amount),
          "assetId" -> (pmt.asset match {
            case None                 => com.wavesplatform.lang.v1.evaluator.ctx.impl.unit
            case Some(asset: ByteStr) => CONST_BYTESTR(asset).explicitGet()
          })
        )
      )

  }

  def orderObject(ord: Ord, proofsEnabled: Boolean, version: StdLibVersion = V3): CaseObj =
    CaseObj(
      buildOrderType(proofsEnabled),
      combine(
        Map(
          "id"                -> ord.id,
          "sender"            -> senderObject(ord.sender),
          "senderPublicKey"   -> ord.senderPublicKey,
          "matcherPublicKey"  -> ord.matcherPublicKey,
          "assetPair"         -> assetPair(ord.assetPair),
          "orderType"         -> ordType(ord.orderType),
          "amount"            -> ord.amount,
          "price"             -> ord.price,
          "timestamp"         -> ord.timestamp,
          "expiration"        -> ord.expiration,
          "matcherFee"        -> ord.matcherFee,
          "bodyBytes"         -> ord.bodyBytes,
          "matcherFeeAssetId" -> ord.matcherFeeAssetId
        ),
        if (proofsEnabled || version != V3) Map(proofsPart(ord.proofs)) else Map.empty
      )
    )

  def buildInvocation(
      caller: Recipient.Address,
      callerPk: ByteStr,
      payment: Option[Pmt],
      dappAddress: Recipient.Address,
      transactionId: ByteStr,
      fee: Long,
      feeAssetId: Option[ByteStr]
  ) =
    CaseObj(
      invocationType,
      Map(
        "caller"          -> mapRecipient(caller)._2,
        "callerPublicKey" -> callerPk,
        "payment"         -> buildPayment(payment),
        "transactionId"   -> transactionId,
        "fee"             -> fee,
        "feeAssetId"      -> feeAssetId
      )
    )

  def senderObject(sender: Recipient.Address): CaseObj = CaseObj(addressType, Map("bytes" -> sender.bytes))

  def scriptTransfer(ct: ScriptTransfer): CaseObj =
    transactionObject(
      Transfer(
        Proven(h = Header(id = ct.id, fee = 0, timestamp = ct.timestamp, version = 0),
               sender = ct.sender,
               bodyBytes = ByteStr.empty,
               senderPk = ByteStr.empty,
               proofs = IndexedSeq.empty),
        feeAssetId = None,
        assetId = ct.assetId,
        amount = ct.amount,
        recipient = ct.recipient,
        attachment = ByteStr.empty
      ),
      false
    )

  def transactionObject(tx: Tx, proofsEnabled: Boolean, version: StdLibVersion = V3): CaseObj =
    tx match {
      case Tx.Genesis(h, amount, recipient) =>
        CaseObj(genesisTransactionType, Map("amount" -> CONST_LONG(amount)) ++ headerPart(h) + mapRecipient(recipient))
      case Tx.Payment(p, amount, recipient) =>
        CaseObj(buildPaymentTransactionType(proofsEnabled),
                Map("amount" -> CONST_LONG(amount)) ++ provenTxPart(p, proofsEnabled) + mapRecipient(recipient))
      case Tx.Transfer(p, feeAssetId, assetId, amount, recipient, attachment) =>
        CaseObj(
          buildTransferTransactionType(proofsEnabled),
          combine(
            Map(
              "amount"     -> amount,
              "feeAssetId" -> feeAssetId,
              "assetId"    -> assetId,
              "attachment" -> attachment
            ),
            provenTxPart(p, proofsEnabled) + mapRecipient(recipient)
          )
        )
      case Issue(p, quantity, name, description, reissuable, decimals, scriptOpt) =>
        CaseObj(
          buildIssueTransactionType(proofsEnabled),
          combine(
            Map(
              "quantity"    -> quantity,
              "name"        -> name,
              "description" -> description,
              "reissuable"  -> reissuable,
              "decimals"    -> decimals,
              "script"      -> scriptOpt
            ),
            provenTxPart(p, proofsEnabled)
          )
        )
      case ReIssue(p, quantity, assetId, reissuable) =>
        CaseObj(
          buildReissueTransactionType(proofsEnabled),
          combine(Map(
                    "quantity"   -> quantity,
                    "assetId"    -> assetId,
                    "reissuable" -> reissuable,
                  ),
                  provenTxPart(p, proofsEnabled))
        )
      case Burn(p, quantity, assetId) =>
        CaseObj(
          buildBurnTransactionType(proofsEnabled),
          combine(Map(
                    "quantity" -> quantity,
                    "assetId"  -> assetId
                  ),
                  provenTxPart(p, proofsEnabled))
        )
      case CI(p, addressOrAlias, maybePayment, feeAssetId, funcName, funcArgs) =>
        CaseObj(
          buildInvokeScriptTransactionType(proofsEnabled),
          combine(
            Map(
              "dApp"       -> mapRecipient(addressOrAlias)._2,
              "payment"    -> buildPayment(maybePayment),
              "feeAssetId" -> feeAssetId,
              "function"   -> funcName,
              "args"       -> funcArgs
            ),
            provenTxPart(p, proofsEnabled)
          )
        )

      case Lease(p, amount, recipient) =>
        CaseObj(
          buildLeaseTransactionType(proofsEnabled),
          combine(Map("amount" -> amount), provenTxPart(p, proofsEnabled) + mapRecipient(recipient))
        )
      case LeaseCancel(p, leaseId) =>
        CaseObj(
          buildLeaseCancelTransactionType(proofsEnabled),
          combine(Map(
                    "leaseId" -> leaseId,
                  ),
                  provenTxPart(p, proofsEnabled))
        )
      case CreateAlias(p, alias) =>
        CaseObj(buildCreateAliasTransactionType(proofsEnabled),
                combine(Map(
                          "alias" -> alias,
                        ),
                        provenTxPart(p, proofsEnabled)))
      case MassTransfer(p, assetId, transferCount, totalAmount, transfers, attachment) =>
        CaseObj(
          buildMassTransferTransactionType(proofsEnabled),
          combine(
            Map(
              "transfers" -> transfers
                .map(bv => CaseObj(transfer, Map(mapRecipient(bv.recipient), "amount" -> bv.amount))),
              "assetId"       -> assetId,
              "transferCount" -> transferCount,
              "totalAmount"   -> totalAmount,
              "attachment"    -> attachment
            ),
            provenTxPart(p, proofsEnabled)
          )
        )
      case SetScript(p, scriptOpt) =>
        CaseObj(buildSetScriptTransactionType(proofsEnabled), Map("script" -> fromOptionBV(scriptOpt)) ++ provenTxPart(p, proofsEnabled))
      case SetAssetScript(p, assetId, scriptOpt) =>
        CaseObj(
          buildSetAssetScriptTransactionType(proofsEnabled),
          combine(Map("script" -> fromOptionBV(scriptOpt), "assetId" -> assetId), provenTxPart(p, proofsEnabled))
        )
      case Sponsorship(p, assetId, minSponsoredAssetFee) =>
        CaseObj(
          buildSponsorFeeTransactionType(proofsEnabled),
          combine(Map("assetId" -> assetId, "minSponsoredAssetFee" -> minSponsoredAssetFee), provenTxPart(p, proofsEnabled))
        )
      case Data(p, data) =>
        def mapValue(e: Any): EVALUATED = e match {
          case s: String  => c(s)
          case s: Boolean => c(s)
          case s: Long    => c(s)
          case s: ByteStr => c(s)
          case _          => ???
        }

        CaseObj(
          buildDataTransactionType(proofsEnabled),
          combine(Map("data" -> data.map(e => CaseObj(
            dataEntryType,
            Map("key" -> CONST_STRING(e.key).explicitGet(), "value" -> mapValue(e.value))
          ))),
                  provenTxPart(p, proofsEnabled)
          )
        )
      case Exchange(p, amount, price, buyMatcherFee, sellMatcherFee, buyOrder, sellOrder) =>
        CaseObj(
          buildExchangeTransactionType(proofsEnabled),
          combine(
            Map(
              "buyOrder"       -> orderObject(buyOrder, proofsEnabled, version),
              "sellOrder"      -> orderObject(sellOrder, proofsEnabled, version),
              "amount"         -> amount,
              "price"          -> price,
              "buyMatcherFee"  -> buyMatcherFee,
              "sellMatcherFee" -> sellMatcherFee,
            ),
            provenTxPart(p, proofsEnabled)
          )
        )
    }

  def buildAssetInfo(sAInfo: ScriptAssetInfo) =
    CaseObj(
      assetType,
      Map(
        "id"              -> sAInfo.id,
        "quantity"        -> sAInfo.quantity,
        "decimals"        -> sAInfo.decimals.toLong,
        "issuer"          -> mapRecipient(sAInfo.issuer)._2,
        "issuerPublicKey" -> sAInfo.issuerPk,
        "reissuable"      -> sAInfo.reissuable,
        "scripted"        -> sAInfo.scripted,
        "sponsored"       -> sAInfo.sponsored
      )
    )

  def buildLastBlockInfo(blockInf: BlockInfo) =
    CaseObj(
      blockInfo,
      Map(
        "timestamp"           -> blockInf.timestamp,
        "height"              -> blockInf.height.toLong,
        "baseTarget"          -> blockInf.baseTarget,
        "generationSignature" -> blockInf.generationSignature,
        "generator"           -> CaseObj(addressType, Map("bytes" -> blockInf.generator)),
        "generatorPublicKey"  -> blockInf.generatorPublicKey
      )
    )
}
