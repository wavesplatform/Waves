package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.evaluator.ctx.impl.unit
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V4}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.Invocation
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
    "proofs" -> ARR((existingProofs ++ Seq.fill(8 - existingProofs.size)(ByteStr.empty)).map(b => CONST_BYTESTR(b).explicitGet()))

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

  private def buildPayments(payments: AttachedPayments): (String, EVALUATED) =
    payments match {
      case AttachedPayments.Single(p) => "payment"  -> fromOptionCO(p.map(mapPayment))
      case AttachedPayments.Multi(p)  => "payments" -> ARR(p.map(mapPayment).toVector)
    }

  private def mapPayment(payment: (Long, Option[ByteStr])): CaseObj = {
    val (amount, asset) = payment
    CaseObj(
      paymentType,
      Map(
        "amount" -> CONST_LONG(amount),
        "assetId" -> (asset match {
          case None                 => unit
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

  def buildInvocation(i: Invocation, version: StdLibVersion): CaseObj = {
    val fields: Map[String, EVALUATED] = Map(
      "caller"          -> mapRecipient(i.caller)._2,
      "callerPublicKey" -> i.callerPk,
      "transactionId"   -> i.transactionId,
      "fee"             -> i.fee,
      "feeAssetId"      -> i.feeAssetId
    )
    CaseObj(invocationType(version), fields + buildPayments(i.payments))
  }

  def senderObject(sender: Recipient.Address): CaseObj = CaseObj(addressType, Map("bytes" -> sender.bytes))

  def scriptTransfer(ct: ScriptTransfer, version: StdLibVersion): CaseObj =
    transferTransactionObject(
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
        attachment = ByteStrValue(ByteStr.empty)
      ),
      proofsEnabled = false,
      version
    )

  def mapReissuePseudoTx(r: ReissuePseudoTx): CaseObj =
    reissueTransactionObject(
      proofsEnabled = false,
      Proven(
        h = Header(id = r.txId, fee = 0, timestamp = r.timestamp, version = 0),
        sender = r.sender,
        bodyBytes = ByteStr.empty,
        senderPk = ByteStr.empty,
        proofs = IndexedSeq.empty
      ),
      r.reissue.quantity,
      r.reissue.assetId,
      r.reissue.isReissuable
    )

  def mapBurnPseudoTx(b: BurnPseudoTx): CaseObj =
    burnTransactionObject(
      proofsEnabled = false,
      Proven(
        h = Header(id = b.txId, fee = 0, timestamp = b.timestamp, version = 0),
        sender = b.sender,
        bodyBytes = ByteStr.empty,
        senderPk = ByteStr.empty,
        proofs = IndexedSeq.empty
      ),
      b.burn.quantity,
      b.burn.assetId
    )

  def transactionObject(tx: Tx, proofsEnabled: Boolean, version: StdLibVersion = V3): CaseObj =
    tx match {
      case Tx.Genesis(h, amount, recipient) =>
        CaseObj(genesisTransactionType, Map("amount" -> CONST_LONG(amount)) ++ headerPart(h) + mapRecipient(recipient))
      case Tx.Payment(p, amount, recipient) =>
        CaseObj(buildPaymentTransactionType(proofsEnabled),
                Map("amount" -> CONST_LONG(amount)) ++ provenTxPart(p, proofsEnabled) + mapRecipient(recipient))
      case transfer: Tx.Transfer => transferTransactionObject(transfer, proofsEnabled, version)
      case Tx.Issue(p, quantity, name, description, reissuable, decimals, scriptOpt) =>
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
        reissueTransactionObject(proofsEnabled, p, quantity, assetId, reissuable)
      case Tx.Burn(p, quantity, assetId) =>
        burnTransactionObject(proofsEnabled, p, quantity, assetId)
      case CI(p, addressOrAlias, payments, feeAssetId, funcName, funcArgs) =>
        CaseObj(
          buildInvokeScriptTransactionType(proofsEnabled, version),
          combine(
            Map(
              "dApp"       -> mapRecipient(addressOrAlias)._2,
              "feeAssetId" -> feeAssetId,
              "function"   -> funcName,
              "args"       -> funcArgs
            ),
            Map(buildPayments(payments)),
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
          buildMassTransferTransactionType(proofsEnabled, version),
          combine(
            Map(
              "transfers" -> transfers
                .map(bv => CaseObj(transfer, Map(mapRecipient(bv.recipient), "amount" -> bv.amount))),
              "assetId"       -> assetId,
              "transferCount" -> transferCount,
              "totalAmount"   -> totalAmount,
              "attachment"    -> attachment.evaluated
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
        def mapValue(e: Any): (EVALUATED, CASETYPEREF) =
          e match {
            case s: String  => (c(s), stringDataEntry)
            case s: Boolean => (c(s), booleanDataEntry)
            case s: Long    => (c(s), intDataEntry)
            case s: ByteStr => (c(s), binaryDataEntry)
            case _          => ???
         }

        def mapDataEntry(d: DataItem[_]): EVALUATED =
          d match {
            case DataItem.Delete(key) =>
              CaseObj(deleteDataEntry, Map("key" -> CONST_STRING(key).explicitGet()))
            case writeItem =>
              val (entryValue, entryType) = mapValue(writeItem.value)
              val fields = Map("key" -> CONST_STRING(writeItem.key).explicitGet(), "value" -> entryValue)
              if (version >= V4)
                CaseObj(entryType, fields)
              else
                CaseObj(genericDataEntry, fields)
          }

        CaseObj(
          buildDataTransactionType(proofsEnabled, version),
          combine(
            Map("data" -> data.map(mapDataEntry)),
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
      case UpdateAssetInfo(p, assetId, name, description) =>
        CaseObj(
          buildUpdateAssetInfoTransactionType(proofsEnabled),
          combine(
            Map(
              "assetId" -> assetId,
              "name" -> name,
              "description" -> description
            ),
            provenTxPart(p, proofsEnabled)
          )
        )
    }

  private def reissueTransactionObject(
    proofsEnabled: Boolean,
    p: Proven,
    quantity: Long,
    assetId: ByteStr,
    reissuable: Boolean
  ): CaseObj =
    CaseObj(
      buildReissueTransactionType(proofsEnabled),
      combine(
        Map(
          "quantity" -> quantity,
          "assetId" -> assetId,
          "reissuable" -> reissuable,
        ),
        provenTxPart(p, proofsEnabled)
      )
    )

  private def burnTransactionObject(
    proofsEnabled: Boolean,
    p: Proven,
    quantity: Long,
    assetId: ByteStr
  ): CaseObj =
    CaseObj(
      buildBurnTransactionType(proofsEnabled),
      combine(
        Map(
          "quantity" -> quantity,
          "assetId"  -> assetId
        ),
        provenTxPart(p, proofsEnabled)
      )
    )

  def transferTransactionObject(tx: Tx.Transfer, proofsEnabled: Boolean, version: StdLibVersion): CaseObj =
    CaseObj(
      buildTransferTransactionType(proofsEnabled, version),
      combine(
        Map(
          "amount"     -> tx.amount,
          "feeAssetId" -> tx.feeAssetId,
          "assetId"    -> tx.assetId,
          "attachment" -> tx.attachment.evaluated
        ),
        provenTxPart(tx.p, proofsEnabled) + mapRecipient(tx.recipient)
      )
    )

  def buildAssetInfo(sAInfo: ScriptAssetInfo, version: StdLibVersion) = {
    val commonFields: Map[String, EVALUATED] =
      Map(
        "id"              -> sAInfo.id,
        "quantity"        -> sAInfo.quantity,
        "decimals"        -> sAInfo.decimals.toLong,
        "issuer"          -> mapRecipient(sAInfo.issuer)._2,
        "issuerPublicKey" -> sAInfo.issuerPk,
        "reissuable"      -> sAInfo.reissuable,
        "scripted"        -> sAInfo.scripted
      )

    val sponsoredField: (String, EVALUATED) =
      if (version >= V4)
        "minSponsoredFee" -> sAInfo.minSponsoredFee
      else
        "sponsored" -> sAInfo.minSponsoredFee.isDefined

    CaseObj(
      assetType(version),
      commonFields + sponsoredField
    )
  }

  def buildBlockInfo(blockInf: BlockInfo, version: StdLibVersion) = {
    val commonFields: Map[String, EVALUATED] =
      Map(
        "timestamp"           -> blockInf.timestamp,
        "height"              -> blockInf.height.toLong,
        "baseTarget"          -> blockInf.baseTarget,
        "generationSignature" -> blockInf.generationSignature,
        "generator"           -> CaseObj(addressType, Map("bytes" -> blockInf.generator)),
        "generatorPublicKey"  -> blockInf.generatorPublicKey
      )

    val vrfFieldOpt: Map[String, EVALUATED] =
      if (version >= V4) Map[String, EVALUATED]("vrf" -> blockInf.vrf, "transactionsRoot" -> blockInf.transactionsRoot)
      else Map()

    CaseObj(blockInfo(version), commonFields ++ vrfFieldOpt)
  }
}
