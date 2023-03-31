package com.wavesplatform.api.http

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.wavesplatform.account.{Address, AddressOrAlias}
import com.wavesplatform.api.common.{CommonTransactionsApi, TransactionMeta}
import com.wavesplatform.api.http.StreamSerializerUtils.*
import com.wavesplatform.api.http.TransactionJsonSerializer.*
import com.wavesplatform.api.http.TransactionsApiRoute.{ApplicationStatus, LeaseStatus, TxMetaEnriched}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.database.protobuf.EthereumTransactionMeta.Payload
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{
  ARR,
  CONST_BOOLEAN,
  CONST_BYTESTR,
  CONST_LONG,
  CONST_STRING,
  CaseObj,
  EVALUATED,
  EXPR,
  FAIL,
  FUNCTION_CALL
}
import com.wavesplatform.lang.v1.serialization.SerdeV1
import com.wavesplatform.protobuf.transaction.PBAmounts
import com.wavesplatform.state.InvokeScriptResult.{
  AttachedPayment,
  Burn,
  Call,
  ErrorMessage,
  Invocation,
  Issue,
  Lease,
  LeaseCancel,
  Reissue,
  SponsorFee
}
import com.wavesplatform.state.{Blockchain, DataEntry, InvokeScriptResult, TxMeta}
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.{Asset, PBSince, Transaction}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.serialization.impl.InvokeScriptTxSerializer
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.utils.EthEncoding
import play.api.libs.json.{JsObject, Json, JsonConfiguration, OWrites, OptionHandlers}

final case class TransactionJsonSerializer(blockchain: Blockchain, commonApi: CommonTransactionsApi) {

  implicit val assetCodec: JsonValueCodec[Asset] = new OnlyEncodeJsonValueCodec[Asset] {
    override def encodeValue(x: Asset, out: JsonWriter): Unit =
      x match {
        case Waves           => out.writeNull()
        case IssuedAsset(id) => out.writeVal(id.toString)
      }
  }

  def evaluatedCodec(numbersAsString: Boolean): JsonValueCodec[EVALUATED] = new OnlyEncodeJsonValueCodec[EVALUATED] {
    override def encodeValue(x: EVALUATED, out: JsonWriter): Unit = {
      out.writeObjectStart()
      x match {
        case CONST_LONG(num) =>
          out.writeKeyValue("type", "Int")
          out.writeKeyValue("value", num, numbersAsString)
        case CONST_BYTESTR(bs) =>
          out.writeKeyValue("type", "ByteVector")
          out.writeKeyValue("value", bs.toString)
        case CONST_STRING(str) =>
          out.writeKeyValue("type", "String")
          out.writeKeyValue("value", str)
        case CONST_BOOLEAN(b) =>
          out.writeKeyValue("type", "Boolean")
          out.writeKeyValue("value", b)
        case CaseObj(caseType, fields) =>
          out.writeKeyValue("type", caseType.name)
          out.writeKeyValue(
            "value",
            out => {
              out.writeObjectStart()
              fields.foreach { case (key, value) =>
                out.writeKeyValue(key, encodeValue(value, _))
              }
              out.writeObjectEnd()
            }
          )
        case ARR(xs) =>
          out.writeKeyValue("type", "Array")
          out.writeKeyValue(
            "value",
            out => {
              out.writeArrayStart()
              xs.foreach(elem => encodeValue(elem, _))
              out.writeArrayEnd()
            }
          )
        case FAIL(reason) =>
          out.writeKeyValue("error", reason)
        case _ =>
      }
      out.writeObjectEnd()
    }
  }

  def funcCallCodec(numbersAsString: Boolean): JsonValueCodec[FUNCTION_CALL] = new OnlyEncodeJsonValueCodec[FUNCTION_CALL] {
    def writeSingleArg(arg: EXPR, out: JsonWriter): Unit = {
      out.writeObjectStart()
      arg match {
        case CONST_LONG(num) =>
          out.writeKeyValue("type", "integer")
          out.writeKeyValue("value", num, numbersAsString)
        case CONST_BOOLEAN(bool) =>
          out.writeKeyValue("type", "boolean")
          out.writeKeyValue("value", bool)
        case CONST_BYTESTR(bytes) =>
          out.writeKeyValue("type", "binary")
          out.writeKeyValue("value", bytes.base64)
        case CONST_STRING(str) =>
          out.writeKeyValue("type", "string")
          out.writeKeyValue("value", str)
        case ARR(_) =>
          out.writeKeyValue("type", "list")
          out.writeKeyValue("value", "unsupported")
        case arg => throw new NotImplementedError(s"Not supported: $arg")
      }
      out.writeObjectEnd()
    }

    override def encodeValue(x: FUNCTION_CALL, out: JsonWriter): Unit = {
      out.writeObjectStart()
      out.writeKeyValue("function", x.function.funcName)
      out.writeKeyValueArray(
        "args",
        out => {
          x.args.foreach {
            case Terms.ARR(elements) =>
              out.writeObjectStart()
              out.writeKeyValue("type", "list")
              out.writeKeyValueArray("value", out => elements.foreach(e => writeSingleArg(e, out)))
              out.writeObjectEnd()
            case other => writeSingleArg(other, out)
          }
        }
      )
      out.writeObjectEnd()
    }
  }

  implicit val leaseStatusCodec: JsonValueCodec[LeaseStatus] = new OnlyEncodeJsonValueCodec[LeaseStatus] {
    override def encodeValue(x: LeaseStatus, out: JsonWriter): Unit =
      if (x == LeaseStatus.active) out.writeVal("active") else out.writeVal("canceled")
  }
  def leaseRefCodec(numbersAsString: Boolean): JsonValueCodec[LeaseRef] = new OnlyEncodeJsonValueCodec[LeaseRef] {
    override def encodeValue(l: LeaseRef, out: JsonWriter): Unit = {
      out.writeObjectStart()
      out.writeKeyValue("id", l.id.toString)
      l.originTransactionId.fold(out.writeKeyNull("originTransactionId"))(txId => out.writeKeyValue("originTransactionId", txId.toString))
      l.sender.fold(out.writeKeyNull("sender"))(sender => out.writeKeyValue("sender", sender.toString))
      l.recipient.fold(out.writeKeyNull("recipient"))(recipient => out.writeKeyValue("recipient", recipient.toString))
      l.amount.fold(out.writeKeyNull("amount"))(amount => out.writeKeyValue("amount", amount, numbersAsString))
      l.height.fold(out.writeKeyNull("height"))(height => out.writeKeyValue("height", height, numbersAsString))
      out.writeKeyValue("status", if (l.status == LeaseStatus.active) "active" else "canceled")
      l.cancelHeight.fold(out.writeKeyNull("cancelHeight"))(ch => out.writeKeyValue("cancelHeight", ch, numbersAsString))
      l.cancelTransactionId.fold(out.writeKeyNull("cancelTransactionId"))(cti => out.writeKeyValue("cancelTransactionId", cti.toString))
      out.writeObjectEnd()
    }
  }

  def leaseCodec(numbersAsString: Boolean): JsonValueCodec[Lease] = new OnlyEncodeJsonValueCodec[Lease] {
    override def encodeValue(x: Lease, out: JsonWriter): Unit =
      leaseRefCodec(numbersAsString).encodeValue(leaseIdToLeaseRef(x.id, Some(x.recipient), Some(x.amount)), out)
  }

  def leaseCancelCodec(numbersAsString: Boolean): JsonValueCodec[LeaseCancel] = new OnlyEncodeJsonValueCodec[LeaseCancel] {
    override def encodeValue(x: LeaseCancel, out: JsonWriter): Unit =
      leaseRefCodec(numbersAsString).encodeValue(leaseIdToLeaseRef(x.id), out)
  }

  def paymentCodec(numbersAsString: Boolean): JsonValueCodec[Payment] = new OnlyEncodeJsonValueCodec[Payment] {
    override def encodeValue(p: Payment, out: JsonWriter): Unit = {
      out.writeObjectStart()
      out.writeKeyValue("amount", p.amount, numbersAsString)
      out.writeKeyValue("assetId", assetCodec.encodeValue(p.assetId, _))
      out.writeObjectEnd()
    }
  }

  def attachedPaymentCodec(numbersAsString: Boolean): JsonValueCodec[AttachedPayment] = new OnlyEncodeJsonValueCodec[AttachedPayment] {
    override def encodeValue(p: AttachedPayment, out: JsonWriter): Unit = {
      out.writeObjectStart()
      out.writeKeyValue("assetId", assetCodec.encodeValue(p.assetId, _))
      out.writeKeyValue("amount", p.amount, numbersAsString)
      out.writeObjectEnd()
    }
  }

  def isrPaymentCodec(numbersAsString: Boolean): JsonValueCodec[InvokeScriptResult.Payment] =
    new OnlyEncodeJsonValueCodec[InvokeScriptResult.Payment] {
      override def encodeValue(p: InvokeScriptResult.Payment, out: JsonWriter): Unit = {
        out.writeObjectStart()
        out.writeKeyValue("address", p.address.toString)
        out.writeKeyValue("asset", out => assetCodec.encodeValue(p.asset, out))
        out.writeKeyValue("amount", p.amount, numbersAsString)
        out.writeObjectEnd()
      }
    }

  def issueCodec(numbersAsString: Boolean): JsonValueCodec[Issue] = new OnlyEncodeJsonValueCodec[Issue] {
    override def encodeValue(issue: Issue, out: JsonWriter): Unit = {
      out.writeObjectStart()
      out.writeKeyValue("id", issue.id.toString)
      issue.compiledScript.foreach(sc => out.writeKeyValue("compiledScript", sc.toString))
      out.writeKeyValue("decimals", issue.decimals, numbersAsString)
      out.writeKeyValue("description", issue.description)
      out.writeKeyValue("isReissuable", issue.isReissuable)
      out.writeKeyValue("name", issue.name)
      out.writeKeyValue("quantity", issue.quantity, numbersAsString)
      out.writeKeyValue("nonce", issue.nonce, numbersAsString)
      out.writeObjectEnd()
    }
  }

  def reissueCodec(numbersAsString: Boolean): JsonValueCodec[Reissue] = new OnlyEncodeJsonValueCodec[Reissue] {
    override def encodeValue(r: Reissue, out: JsonWriter): Unit = {
      out.writeObjectStart()
      out.writeKeyValue("assetId", r.assetId.toString)
      out.writeKeyValue("isReissuable", r.isReissuable)
      out.writeKeyValue("quantity", r.quantity, numbersAsString)
      out.writeObjectEnd()
    }
  }

  def burnCodec(numbersAsString: Boolean): JsonValueCodec[Burn] = new OnlyEncodeJsonValueCodec[Burn] {
    override def encodeValue(b: Burn, out: JsonWriter): Unit = {
      out.writeObjectStart()
      out.writeKeyValue("assetId", b.assetId.toString)
      out.writeKeyValue("quantity", b.quantity, numbersAsString)
      out.writeObjectEnd()
    }
  }

  def sponsorFeeCodec(numbersAsString: Boolean): JsonValueCodec[SponsorFee] = new OnlyEncodeJsonValueCodec[SponsorFee] {
    override def encodeValue(s: SponsorFee, out: JsonWriter): Unit = {
      out.writeObjectStart()
      out.writeKeyValue("assetId", s.assetId.toString)
      s.minSponsoredAssetFee.foreach(fee => out.writeKeyValue("minSponsoredAssetFee", fee, numbersAsString))
      out.writeObjectEnd()
    }
  }

  def callCodec(numbersAsString: Boolean): JsonValueCodec[Call] = new OnlyEncodeJsonValueCodec[Call] {
    override def encodeValue(c: Call, out: JsonWriter): Unit = {
      out.writeObjectStart()
      out.writeKeyValue("function", c.function)
      out.writeKeyValueArray("args", c.args)(evaluatedCodec(numbersAsString))
      out.writeObjectEnd()
    }
  }

  def invocationCodec(numbersAsString: Boolean): JsonValueCodec[Invocation] = new OnlyEncodeJsonValueCodec[Invocation] {
    override def encodeValue(inv: Invocation, out: JsonWriter): Unit = {
      out.writeObjectStart()
      out.writeKeyValue("dApp", inv.dApp.toString)
      out.writeKeyValue("call", out => callCodec(numbersAsString).encodeValue(inv.call, out))
      out.writeKeyValueArray("payments", inv.payments)(attachedPaymentCodec(numbersAsString))
      out.writeKeyValue("stateChanges", out => invokeScriptResultCodec(numbersAsString).encodeValue(inv.stateChanges, out))
      out.writeObjectEnd()
    }
  }

  val errorMessageCodec: JsonValueCodec[ErrorMessage] = new OnlyEncodeJsonValueCodec[ErrorMessage] {
    override def encodeValue(err: ErrorMessage, out: JsonWriter): Unit = {
      out.writeObjectStart()
      out.writeKeyValue("code", err.code, numberAsString = false)
      out.writeKeyValue("text", err.text)
      out.writeObjectEnd()
    }
  }

  def invokeScriptResultCodec(numbersAsString: Boolean): JsonValueCodec[InvokeScriptResult] =
    new OnlyEncodeJsonValueCodec[InvokeScriptResult] {
      override def encodeValue(isr: InvokeScriptResult, out: JsonWriter): Unit = {
        out.writeObjectStart()
        out.writeKeyValueArray("data", isr.data)(DataEntry.dataEntryCodec(numbersAsString))
        out.writeKeyValueArray("transfers", isr.transfers)(isrPaymentCodec(numbersAsString))
        out.writeKeyValueArray("issues", isr.issues)(issueCodec(numbersAsString))
        out.writeKeyValueArray("reissues", isr.reissues)(reissueCodec(numbersAsString))
        out.writeKeyValueArray("burns", isr.burns)(burnCodec(numbersAsString))
        out.writeKeyValueArray("sponsorFees", isr.sponsorFees)(sponsorFeeCodec(numbersAsString))
        out.writeKeyValueArray("leases", isr.leases)(leaseCodec(numbersAsString))
        out.writeKeyValueArray("leaseCancels", isr.leaseCancels)(leaseCancelCodec(numbersAsString))
        out.writeKeyValueArray("invokes", isr.invokes)(invocationCodec(numbersAsString))
        isr.error.foreach(err => out.writeKeyValue("error", errorMessageCodec.encodeValue(err, _)))
        out.writeObjectEnd()
      }
    }

  def txMetaJsonCodec(address: Address, isBlockV5: Int => Boolean, numbersAsString: Boolean): JsonValueCodec[TxMetaEnriched] =
    new OnlyEncodeJsonValueCodec[TxMetaEnriched] {
      override def encodeValue(txMeta: TxMetaEnriched, out: JsonWriter): Unit = {
        txMeta.meta match {
          case TransactionMeta.Invoke(height, tx: InvokeScriptTransaction, succeeded, spentComplexity, invokeScriptResult) =>
            out.writeObjectStart()
            out.writeKeyValue("type", tx.tpe.id, numbersAsString)
            out.writeKeyValue("id", tx.id().toString)
            out.writeKeyValue("fee", tx.assetFee._2, numbersAsString)
            tx.assetFee._1.maybeBase58Repr.foreach(out.writeKeyValue("feeAssetId", _))
            out.writeKeyValue("timestamp", tx.timestamp, numbersAsString)
            out.writeKeyValue("version", tx.version, numbersAsString)
            if (tx.asInstanceOf[PBSince].isProtobufVersion) out.writeKeyValue("chainId", tx.chainId, numbersAsString)
            out.writeKeyValue("sender", tx.sender.toAddress(tx.chainId).toString)
            out.writeKeyValue("senderPublicKey", tx.sender.toString)
            out.writeKeyValueArray("proofs", out => tx.proofs.proofs.foreach(p => out.writeVal(p.toString)))
            out.writeKeyValue("dApp", tx.dApp.toString)
            out.writeKeyValueArray("payment", tx.payments)(paymentCodec(numbersAsString))
            out.writeKeyValue("call", funcCallCodec(numbersAsString).encodeValue(tx.funcCall, _))
            out.writeKeyValue("height", height.toInt, numbersAsString)
            val appStatus =
              if (isBlockV5(height))
                if (succeeded) Some(ApplicationStatus.Succeeded) else Some(ApplicationStatus.ScriptExecutionFailed)
              else
                None
            appStatus.foreach(s => out.writeKeyValue("applicationStatus", s))
            out.writeKeyValue("spentComplexity", spentComplexity, numbersAsString)
            invokeScriptResult.fold(out.writeKeyNull("stateChanges"))(isr =>
              out.writeKeyValue("stateChanges", out => invokeScriptResultCodec(numbersAsString).encodeValue(isr, out))
            )
            out.writeObjectEnd()
          case TransactionMeta.Ethereum(height, tx, succeeded, spentComplexity, Some(EthereumTransactionMeta(Payload.Invocation(i), _)), isr) =>
            val functionCallEi = SerdeV1.deserializeFunctionCall(i.functionCall.toByteArray).toOption
            val payments       = i.payments.map(p => InvokeScriptTransaction.Payment(p.amount, PBAmounts.toVanillaAssetId(p.assetId)))

            out.writeObjectStart()
            out.writeKeyValue("id", tx.id().toString)
            out.writeKeyValue("fee", tx.assetFee._2, numbersAsString)
            tx.assetFee._1.maybeBase58Repr.foreach(out.writeKeyValue("feeAssetId", _))
            out.writeKeyValue("timestamp", tx.timestamp, numbersAsString)
            out.writeKeyValue("version", tx.version, numbersAsString)
            if (tx.isProtobufVersion) out.writeKeyValue("chainId", tx.chainId, numbersAsString)
            out.writeKeyValue("bytes", EthEncoding.toHexString(tx.bytes()))
            out.writeKeyValue("sender", tx.senderAddress().toString)
            out.writeKeyValue("senderPublicKey", tx.signerPublicKey().toString)
            out.writeKeyValue("height", height.toInt, numbersAsString)
            val appStatus =
              if (isBlockV5(height))
                if (succeeded) Some(ApplicationStatus.Succeeded) else Some(ApplicationStatus.ScriptExecutionFailed)
              else
                None
            appStatus.foreach(s => out.writeKeyValue("applicationStatus", s))
            out.writeKeyValue("spentComplexity", spentComplexity, numbersAsString)
            out.writeKeyValue("type", "invocation")
            out.writeKeyValue("dApp", Address(EthEncoding.toBytes(tx.underlying.getTo)).toString)
            functionCallEi.fold(out.writeKeyNull("call"))(fc => out.writeKeyValue("call", out => funcCallCodec(numbersAsString).encodeValue(fc, out)))
            out.writeKeyValueArray("payment", payments)(paymentCodec(numbersAsString))
            isr.fold(out.writeKeyNull("stateChanges"))(isr =>
              out.writeKeyValue("stateChanges", out => invokeScriptResultCodec(numbersAsString).encodeValue(isr, out))
            )
            out.writeObjectEnd()
          case meta @ TransactionMeta.Default(height, mtt: MassTransferTransaction, succeeded, spentComplexity) if mtt.sender.toAddress != address =>
            /** Produces compact representation for large transactions by stripping unnecessary data. Currently implemented for MassTransfer
              * transaction only.
              */
            jsValueCodec(numbersAsString).encodeValue(
              mtt.compactJson(address, txMeta.aliases.getOrElse(Set.empty)) ++ transactionMetaJson(meta),
              out
            )
          case other =>
            jsValueCodec(numbersAsString).encodeValue(other.transaction.json() ++ transactionMetaJson(other), out)
        }
      }
    }

  def transactionMetaJson(meta: TransactionMeta): JsObject = {
    val specificInfo = meta.transaction match {
      case lease: LeaseTransaction =>
        import com.wavesplatform.api.http.TransactionsApiRoute.LeaseStatus.*
        Json.obj("status" -> (if (blockchain.leaseDetails(lease.id()).exists(_.isActive)) active else canceled))

      case leaseCancel: LeaseCancelTransaction =>
        Json.obj("lease" -> leaseIdToLeaseRef(leaseCancel.leaseId))

      case _ => JsObject.empty
    }

    val stateChanges = meta match {
      case i: TransactionMeta.Invoke =>
        Json.obj("stateChanges" -> i.invokeScriptResult)

      case e: TransactionMeta.Ethereum =>
        val payloadJson: JsObject = e.meta
          .map(_.payload)
          .collect {
            case Payload.Invocation(i) =>
              val functionCallEi = SerdeV1.deserializeFunctionCall(i.functionCall.toByteArray).map(InvokeScriptTxSerializer.functionCallToJson)
              val payments       = i.payments.map(p => InvokeScriptTransaction.Payment(p.amount, PBAmounts.toVanillaAssetId(p.assetId)))
              Json.obj(
                "type"         -> "invocation",
                "dApp"         -> Address(EthEncoding.toBytes(e.transaction.underlying.getTo)),
                "call"         -> functionCallEi.toOption,
                "payment"      -> payments,
                "stateChanges" -> e.invokeScriptResult
              )

            case Payload.Transfer(t) =>
              val (asset, amount) = PBAmounts.toAssetAndAmount(t.getAmount)
              Json.obj(
                "type"      -> "transfer",
                "recipient" -> Address(t.publicKeyHash.toByteArray),
                "asset"     -> asset,
                "amount"    -> amount
              )
          }
          .getOrElse(JsObject.empty)
        Json.obj("payload" -> payloadJson)

      case _ => JsObject.empty
    }

    Seq(
      TransactionJsonSerializer.height(meta.height),
      metaJson(TxMeta(meta.height, meta.succeeded, meta.spentComplexity)),
      stateChanges,
      specificInfo
    ).reduce(_ ++ _)
  }

  def transactionWithMetaJson(meta: TransactionMeta): JsObject =
    meta.transaction.json() ++ transactionMetaJson(meta)

  def unconfirmedTxExtendedJson(tx: Transaction): JsObject = tx match {
    case leaseCancel: LeaseCancelTransaction =>
      leaseCancel.json() ++ Json.obj("lease" -> leaseIdToLeaseRef(leaseCancel.leaseId))

    case t => t.json()
  }

  def metaJson(m: TxMeta): JsObject =
    TransactionJsonSerializer.applicationStatus(isBlockV5(m.height), m.succeeded) ++ Json.obj("spentComplexity" -> m.spentComplexity)

  private[this] def isBlockV5(height: Int): Boolean = blockchain.isFeatureActivated(BlockchainFeatures.BlockV5, height)

  // Extended lease format. Overrides default
  private[this] def leaseIdToLeaseRef(
      leaseId: ByteStr,
      recipientParamOpt: Option[AddressOrAlias] = None,
      amountOpt: Option[Long] = None
  ): LeaseRef = {
    val detailsOpt           = blockchain.leaseDetails(leaseId)
    val txMetaOpt            = detailsOpt.flatMap(d => blockchain.transactionMeta(d.sourceId))
    val recipientOpt         = recipientParamOpt.orElse(detailsOpt.map(_.recipient))
    val resolvedRecipientOpt = recipientOpt.flatMap(r => blockchain.resolveAlias(r).toOption)

    val statusOpt = detailsOpt.map(_.status)
    val status    = LeaseStatus(statusOpt.contains(LeaseDetails.Status.Active))
    val statusDataOpt = statusOpt.map {
      case LeaseDetails.Status.Active                  => (None, None)
      case LeaseDetails.Status.Cancelled(height, txId) => (Some(height), txId)
      case LeaseDetails.Status.Expired(height)         => (Some(height), None)
    }

    LeaseRef(
      leaseId,
      detailsOpt.map(_.sourceId),
      detailsOpt.map(_.sender.toAddress),
      resolvedRecipientOpt,
      amountOpt orElse detailsOpt.map(_.amount),
      txMetaOpt.map(_.height),
      status,
      statusDataOpt.flatMap(_._1),
      statusDataOpt.flatMap(_._2)
    )
  }

  private[http] implicit val leaseWrites: OWrites[InvokeScriptResult.Lease] =
    LeaseRef.jsonWrites.contramap((l: InvokeScriptResult.Lease) => leaseIdToLeaseRef(l.id, Some(l.recipient), Some(l.amount)))

  private[http] implicit val leaseCancelWrites: OWrites[InvokeScriptResult.LeaseCancel] =
    LeaseRef.jsonWrites.contramap((l: InvokeScriptResult.LeaseCancel) => leaseIdToLeaseRef(l.id))

  // To override nested InvokeScriptResult writes
  private[http] implicit val invocationWrites: OWrites[InvokeScriptResult.Invocation] = (i: InvokeScriptResult.Invocation) =>
    Json.obj(
      "dApp"         -> i.dApp,
      "call"         -> i.call,
      "payment"      -> i.payments,
      "stateChanges" -> invokeScriptResultWrites.writes(i.stateChanges)
    )

  private[http] implicit val invokeScriptResultWrites: OWrites[InvokeScriptResult] = {
    import InvokeScriptResult.{issueFormat, reissueFormat, burnFormat, sponsorFeeFormat}
    Json.writes[InvokeScriptResult]
  }
}

object TransactionJsonSerializer {
  def applicationStatus(isBlockV5: Boolean, succeeded: Boolean): JsObject =
    if (isBlockV5)
      Json.obj("applicationStatus" -> (if (succeeded) ApplicationStatus.Succeeded else ApplicationStatus.ScriptExecutionFailed))
    else
      JsObject.empty

  def height(height: Int): JsObject =
    Json.obj("height" -> height)

  final case class LeaseRef(
      id: ByteStr,
      originTransactionId: Option[ByteStr],
      sender: Option[Address],
      recipient: Option[Address],
      amount: Option[Long],
      height: Option[Int],
      status: LeaseStatus,
      cancelHeight: Option[Int],
      cancelTransactionId: Option[ByteStr]
  )

  object LeaseRef {
    import com.wavesplatform.utils.byteStrFormat
    implicit val config                        = JsonConfiguration(optionHandlers = OptionHandlers.WritesNull)
    implicit val jsonWrites: OWrites[LeaseRef] = Json.writes[LeaseRef]
  }
}
