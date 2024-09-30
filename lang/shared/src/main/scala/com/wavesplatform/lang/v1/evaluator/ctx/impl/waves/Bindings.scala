package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V4, V5, V7, V8}
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BYTESTR.{DataEntrySize, NoLimit}
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.Invocation
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{converters, unit}
import com.wavesplatform.lang.v1.traits.domain.Tx.*
import com.wavesplatform.lang.v1.traits.domain.*
import com.wavesplatform.common.utils.EitherExt3.*

object Bindings {

  import converters.*

  private def combine(m0: Map[String, EVALUATED], m1: Map[String, EVALUATED]*) = m1.toList.fold(m0)(_ ++ _)

  import Types.*

  private def headerPart(tx: Header): Map[String, EVALUATED] = Map(
    "id"        -> tx.id,
    "fee"       -> tx.fee,
    "timestamp" -> tx.timestamp,
    "version"   -> tx.version
  )

  private def proofsPart(existingProofs: IndexedSeq[ByteStr]) =
    "proofs" -> ARR(
      existingProofs.map(b => CONST_BYTESTR(b).explicitGet()) ++ Seq.fill(8 - existingProofs.size)(CONST_BYTESTR(ByteStr.empty).explicitGet()),
      limited = false
    ).explicitGet()

  private def provenTxPart(tx: Proven, proofsEnabled: Boolean, version: StdLibVersion): Map[String, EVALUATED] = {
    val limit =
      if (version >= V4) CONST_BYTESTR.NoLimit
      else CONST_BYTESTR.DataTxSize

    val commonPart = combine(
      Map(
        "sender"          -> senderObject(tx.sender),
        "senderPublicKey" -> tx.senderPk,
        "bodyBytes"       -> CONST_BYTESTR(tx.bodyBytes, limit).explicitGet()
      ),
      headerPart(tx.h)
    )

    if (proofsEnabled) combine(commonPart, Map(proofsPart(tx.proofs)))
    else commonPart
  }
  def mapRecipient(r: Recipient): (String, CaseObj) =
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
    CaseObj(
      o match {
        case OrdType.Buy  => buyType
        case OrdType.Sell => sellType
      },
      Map.empty
    )

  private def buildPayments(payments: AttachedPayments): (String, EVALUATED) =
    payments match {
      case AttachedPayments.Single(p) => "payment"  -> fromOptionCO(p.map(mapPayment))
      case AttachedPayments.Multi(p)  => "payments" -> ARR(p.map(mapPayment).toVector, limited = false).explicitGet()
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
      buildOrderType(proofsEnabled, version),
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
        if (proofsEnabled || version != V3) Map(proofsPart(ord.proofs)) else Map.empty,
        if (version >= V8) Map("attachment" -> ord.attachment) else Map.empty
      )
    )

  def buildInvocation(i: Invocation, version: StdLibVersion): CaseObj = {
    val fields: Map[String, EVALUATED] = Map[String, EVALUATED](
      "caller"          -> mapRecipient(i.caller)._2,
      "callerPublicKey" -> i.callerPk,
      "transactionId"   -> i.transactionId,
      "fee"             -> i.fee,
      "feeAssetId"      -> i.feeAssetId
    ) ++ (if (version >= V5)
            Map[String, EVALUATED](
              "originCaller"          -> mapRecipient(i.originCaller)._2,
              "originCallerPublicKey" -> i.originCallerPublicKey
            )
          else Map.empty)
    CaseObj(invocationType(version), fields + buildPayments(i.payments))
  }

  def senderObject(sender: Recipient.Address): CaseObj = CaseObj(addressType, Map("bytes" -> sender.bytes))

  def scriptTransfer(ct: ScriptTransfer, version: StdLibVersion): CaseObj =
    transferTransactionObject(
      Transfer(
        Proven(
          h = Header(id = ct.id, fee = 0, timestamp = ct.timestamp, version = 0),
          sender = ct.sender,
          bodyBytes = ByteStr.empty,
          senderPk = ct.senderPk,
          proofs = IndexedSeq.empty
        ),
        feeAssetId = None,
        assetId = ct.assetId,
        amount = ct.amount,
        recipient = ct.recipient,
        attachment = ByteStr.empty
      ),
      proofsEnabled = false,
      version
    )

  def mapReissuePseudoTx(r: ReissuePseudoTx, version: StdLibVersion): CaseObj =
    reissueTransactionObject(
      proofsEnabled = false,
      Proven(
        h = Header(id = r.txId, fee = 0, timestamp = r.timestamp, version = 0),
        sender = r.sender,
        bodyBytes = ByteStr.empty,
        senderPk = r.senderPk,
        proofs = IndexedSeq.empty
      ),
      r.reissue.quantity,
      r.reissue.assetId,
      r.reissue.isReissuable,
      version
    )

  def mapBurnPseudoTx(b: BurnPseudoTx, version: StdLibVersion): CaseObj =
    burnTransactionObject(
      proofsEnabled = false,
      Proven(
        h = Header(id = b.txId, fee = 0, timestamp = b.timestamp, version = 0),
        sender = b.sender,
        bodyBytes = ByteStr.empty,
        senderPk = b.senderPk,
        proofs = IndexedSeq.empty
      ),
      b.burn.quantity,
      b.burn.assetId,
      version
    )

  def mapSponsorFeePseudoTx(s: SponsorFeePseudoTx, version: StdLibVersion): CaseObj =
    sponsorshipTransactionObject(
      proofsEnabled = false,
      Proven(
        h = Header(id = s.txId, fee = 0, timestamp = s.timestamp, version = 0),
        sender = s.sender,
        bodyBytes = ByteStr.empty,
        senderPk = s.senderPk,
        proofs = IndexedSeq.empty
      ),
      s.sponsorFee.assetId,
      s.sponsorFee.minSponsoredAssetFee,
      version
    )

  def mapInvokePseudoTx(inv: InvokePseudoTx, version: StdLibVersion): CaseObj =
    invokeScriptTransactionObject(
      proofsEnabled = false,
      p = Proven(
        h = Header(id = inv.id, fee = 0, timestamp = inv.timestamp, version = 0),
        sender = inv.sender,
        bodyBytes = ByteStr.empty,
        senderPk = inv.senderPk,
        proofs = IndexedSeq.empty
      ),
      dApp = inv.dApp,
      feeAssetId = inv.feeAssetId,
      funcName = inv.funcName,
      funcArgs = inv.funcArgs,
      payments = inv.payments,
      version = version
    )

  def transactionObject(tx: Tx, proofsEnabled: Boolean, version: StdLibVersion, fixBigScriptField: Boolean): CaseObj =
    tx match {
      case Tx.Genesis(h, amount, recipient) =>
        CaseObj(genesisTransactionType, Map("amount" -> CONST_LONG(amount)) ++ headerPart(h) + mapRecipient(recipient))
      case Tx.Payment(p, amount, recipient) =>
        CaseObj(
          buildPaymentTransactionType(proofsEnabled),
          Map("amount" -> CONST_LONG(amount)) ++ provenTxPart(p, proofsEnabled, version) + mapRecipient(recipient)
        )
      case ttx: Tx.Transfer => transferTransactionObject(ttx, proofsEnabled, version)
      case Tx.Issue(p, quantity, name, description, reissuable, decimals, scriptOpt) =>
        CaseObj(
          buildIssueTransactionType(proofsEnabled, version),
          combine(
            Map(
              "quantity"    -> quantity,
              "name"        -> (if (version >= V4) name.toUTF8String else name),
              "description" -> (if (version >= V4) description.toUTF8String else description),
              "reissuable"  -> reissuable,
              "decimals"    -> decimals,
              "script"      -> mapScript(scriptOpt, fixBigScriptField)
            ),
            provenTxPart(p, proofsEnabled, version)
          )
        )
      case ReIssue(p, quantity, assetId, reissuable) =>
        reissueTransactionObject(proofsEnabled, p, quantity, assetId, reissuable, version)
      case Tx.Burn(p, quantity, assetId) =>
        burnTransactionObject(proofsEnabled, p, quantity, assetId, version)
      case CI(p, addressOrAlias, payments, feeAssetId, funcName, funcArgs) =>
        invokeScriptTransactionObject(proofsEnabled, p, addressOrAlias, feeAssetId, funcName, funcArgs, payments, version)
      case Tx.Lease(p, amount, recipient) =>
        CaseObj(
          buildLeaseTransactionType(proofsEnabled),
          combine(Map("amount" -> amount), provenTxPart(p, proofsEnabled, version) + mapRecipient(recipient))
        )
      case Tx.LeaseCancel(p, leaseId) =>
        CaseObj(
          buildLeaseCancelTransactionType(proofsEnabled),
          combine(
            Map(
              "leaseId" -> leaseId
            ),
            provenTxPart(p, proofsEnabled, version)
          )
        )
      case CreateAlias(p, alias) =>
        CaseObj(
          buildCreateAliasTransactionType(proofsEnabled),
          combine(
            Map(
              "alias" -> alias
            ),
            provenTxPart(p, proofsEnabled, version)
          )
        )
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
              "attachment"    -> attachment
            ),
            provenTxPart(p, proofsEnabled, version)
          )
        )
      case SetScript(p, scriptOpt) =>
        CaseObj(
          buildSetScriptTransactionType(proofsEnabled),
          Map("script" -> mapScript(scriptOpt, fixBigScriptField)) ++ provenTxPart(p, proofsEnabled, version)
        )
      case SetAssetScript(p, assetId, scriptOpt) =>
        CaseObj(
          buildSetAssetScriptTransactionType(proofsEnabled),
          combine(Map("script" -> mapScript(scriptOpt, fixBigScriptField), "assetId" -> assetId), provenTxPart(p, proofsEnabled, version))
        )
      case Sponsorship(p, assetId, minSponsoredAssetFee) =>
        sponsorshipTransactionObject(proofsEnabled, p, assetId, minSponsoredAssetFee, version)
      case Data(p, data) =>
        def mapValue(e: DataItem[?]): (EVALUATED, CASETYPEREF) =
          e match {
            case DataItem.Str(_, s)  => (c(s), stringDataEntry)
            case DataItem.Bool(_, s) => (c(s), booleanDataEntry)
            case DataItem.Lng(_, s)  => (c(s), intDataEntry)
            case DataItem.Bin(_, s)  => (c(s), binaryDataEntry)
          }

        def mapDataEntry(d: DataOp): EVALUATED =
          d match {
            case DataItem.Delete(key) =>
              CaseObj(deleteDataEntry, Map("key" -> CONST_STRING(key).explicitGet()))
            case writeItem: DataItem[?] =>
              val (entryValue, entryType) = mapValue(writeItem)
              val fields                  = Map("key" -> CONST_STRING(writeItem.key).explicitGet(), "value" -> entryValue)
              if (version >= V4)
                CaseObj(entryType, fields)
              else
                CaseObj(genericDataEntry, fields)
          }

        CaseObj(
          buildDataTransactionType(proofsEnabled, version),
          combine(
            Map("data" -> data.map(mapDataEntry)),
            provenTxPart(p, proofsEnabled, version)
          )
        )
      case Exchange(p, amount, price, buyMatcherFee, sellMatcherFee, buyOrder, sellOrder) =>
        CaseObj(
          buildExchangeTransactionType(proofsEnabled, version),
          combine(
            Map(
              "buyOrder"       -> orderObject(buyOrder, proofsEnabled, version),
              "sellOrder"      -> orderObject(sellOrder, proofsEnabled, version),
              "amount"         -> amount,
              "price"          -> price,
              "buyMatcherFee"  -> buyMatcherFee,
              "sellMatcherFee" -> sellMatcherFee
            ),
            provenTxPart(p, proofsEnabled, version)
          )
        )
      case UpdateAssetInfo(p, assetId, name, description) =>
        CaseObj(
          buildUpdateAssetInfoTransactionType(proofsEnabled),
          combine(
            Map(
              "assetId"     -> assetId,
              "name"        -> name,
              "description" -> description
            ),
            provenTxPart(p, proofsEnabled, version)
          )
        )
      case InvokeExpression(p, expression, feeAssetId) =>
        CaseObj(
          buildInvokeExpressionTransactionType(proofsEnabled),
          combine(
            Map(
              "expression" -> expression,
              "feeAssetId" -> feeAssetId
            ),
            provenTxPart(p, proofsEnabled, version)
          )
        )
    }

  private def mapScript(script: Option[ByteStr], fixBigScriptField: Boolean): EVALUATED = {
    val limit = if (fixBigScriptField) NoLimit else DataEntrySize
    script.flatMap(CONST_BYTESTR(_, limit).toOption).getOrElse(unit)
  }

  private def reissueTransactionObject(
      proofsEnabled: Boolean,
      p: Proven,
      quantity: Long,
      assetId: ByteStr,
      reissuable: Boolean,
      version: StdLibVersion
  ): CaseObj =
    CaseObj(
      buildReissueTransactionType(proofsEnabled),
      combine(
        Map(
          "quantity"   -> quantity,
          "assetId"    -> assetId,
          "reissuable" -> reissuable
        ),
        provenTxPart(p, proofsEnabled, version)
      )
    )

  private def burnTransactionObject(
      proofsEnabled: Boolean,
      p: Proven,
      quantity: Long,
      assetId: ByteStr,
      version: StdLibVersion
  ): CaseObj =
    CaseObj(
      buildBurnTransactionType(proofsEnabled),
      combine(
        Map(
          "quantity" -> quantity,
          "assetId"  -> assetId
        ),
        provenTxPart(p, proofsEnabled, version)
      )
    )

  private def sponsorshipTransactionObject(
      proofsEnabled: Boolean,
      p: Proven,
      assetId: ByteStr,
      minSponsoredAssetFee: Option[Long],
      version: StdLibVersion
  ) =
    CaseObj(
      buildSponsorFeeTransactionType(proofsEnabled),
      combine(Map("assetId" -> assetId, "minSponsoredAssetFee" -> minSponsoredAssetFee), provenTxPart(p, proofsEnabled, version))
    )

  def transferTransactionObject(tx: Tx.Transfer, proofsEnabled: Boolean, version: StdLibVersion): CaseObj =
    CaseObj(
      buildTransferTransactionType(proofsEnabled),
      combine(
        Map(
          "amount"     -> tx.amount,
          "feeAssetId" -> tx.feeAssetId,
          "assetId"    -> tx.assetId,
          "attachment" -> tx.attachment
        ),
        provenTxPart(tx.p, proofsEnabled, version) + mapRecipient(tx.recipient)
      )
    )

  def invokeScriptTransactionObject(
      proofsEnabled: Boolean,
      p: Proven,
      dApp: Recipient,
      feeAssetId: Option[ByteStr],
      funcName: Option[String],
      funcArgs: List[EVALUATED],
      payments: AttachedPayments,
      version: StdLibVersion
  ): CaseObj =
    CaseObj(
      buildInvokeScriptTransactionType(proofsEnabled, version),
      combine(
        Map(
          "dApp"       -> mapRecipient(dApp)._2,
          "feeAssetId" -> feeAssetId,
          "function"   -> funcName,
          "args"       -> funcArgs
        ),
        Map(buildPayments(payments)),
        provenTxPart(p, proofsEnabled, version)
      )
    )

  def buildAssetInfo(sAInfo: ScriptAssetInfo, version: StdLibVersion): CaseObj = {
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

    val versionDepField: Map[String, EVALUATED] =
      if (version >= V4)
        Map("minSponsoredFee" -> sAInfo.minSponsoredFee, "name" -> sAInfo.name, "description" -> sAInfo.description)
      else
        Map("sponsored" -> sAInfo.minSponsoredFee.isDefined)

    CaseObj(
      assetType(version),
      commonFields ++ versionDepField
    )
  }

  def buildBlockInfo(blockInf: BlockInfo, version: StdLibVersion): CaseObj = {
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
      if (version >= V4) Map[String, EVALUATED]("vrf" -> blockInf.vrf)
      else Map()

    val rewardsFieldOpt: Map[String, EVALUATED] =
      if (version >= V7) {
        val arrOpt = ARR(
          blockInf.rewards.map { case (addr, reward) =>
            CaseObj(runtimeTupleType, Map("_1" -> CaseObj(addressType, Map("bytes" -> addr.bytes)), "_2" -> reward))
          }.toIndexedSeq,
          limited = false
        ).toOption

        arrOpt.map("rewards" -> _).toMap
      } else Map()

    CaseObj(blockInfo(version), commonFields ++ vrfFieldOpt ++ rewardsFieldOpt)
  }
}
