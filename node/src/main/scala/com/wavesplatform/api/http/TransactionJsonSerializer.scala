package com.wavesplatform.api.http

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}
import com.wavesplatform.account.{Address, AddressOrAlias}
import com.wavesplatform.api.common.TransactionMeta
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
import com.wavesplatform.state.{Blockchain, DataEntry, InvokeScriptResult, LeaseDetails, TxMeta}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.serialization.impl.InvokeScriptTxSerializer
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.{Asset, PBSince, Transaction}
import com.wavesplatform.utils.EthEncoding
import play.api.libs.json.*
import play.api.libs.json.JsonConfiguration.Aux

final case class TransactionJsonSerializer(blockchain: Blockchain) {

  val assetSerializer: JsonSerializer[Asset] =
    (value: Asset, gen: JsonGenerator, serializers: SerializerProvider) => {
      value match {
        case Waves           => gen.writeNull()
        case IssuedAsset(id) => gen.writeString(id.toString)
      }
    }

  def evaluatedSerializer(numbersAsString: Boolean): JsonSerializer[EVALUATED] =
    (value: EVALUATED, gen: JsonGenerator, serializers: SerializerProvider) => {
      gen.writeStartObject()
      value match {
        case CONST_LONG(num) =>
          gen.writeStringField("type", "Int")
          gen.writeNumberField("value", num, numbersAsString)
        case CONST_BYTESTR(bs) =>
          gen.writeStringField("type", "ByteVector")
          gen.writeStringField("value", bs.toString)
        case CONST_STRING(str) =>
          gen.writeStringField("type", "String")
          gen.writeStringField("value", str)
        case CONST_BOOLEAN(b) =>
          gen.writeStringField("type", "Boolean")
          gen.writeBooleanField("value", b)
        case CaseObj(caseType, fields) =>
          gen.writeStringField("type", caseType.name)
          gen.writeValueField("value") { gen =>
            gen.writeStartObject()
            fields.foreach { case (key, value) =>
              gen.writeValueField(key, value)(evaluatedSerializer(numbersAsString), serializers)
            }
            gen.writeEndObject()
          }
        case ARR(xs) =>
          gen.writeStringField("type", "Array")
          gen.writeArrayField("value", xs)(evaluatedSerializer(numbersAsString), serializers)
        case FAIL(reason) =>
          gen.writeStringField("error", reason)
        case _ =>
      }
      gen.writeEndObject()
    }

  def funcCallSerializer(numbersAsString: Boolean): JsonSerializer[FUNCTION_CALL] = new JsonSerializer[FUNCTION_CALL] {
    override def serialize(funcCall: FUNCTION_CALL, gen: JsonGenerator, serializers: SerializerProvider): Unit = {
      gen.writeStartObject()
      gen.writeStringField("function", funcCall.function.funcName)
      gen.writeArrayField("args") { out =>
        funcCall.args.foreach {
          case Terms.ARR(elements) =>
            gen.writeStartObject()
            gen.writeStringField("type", "list")
            out.writeArrayField("value")(out => elements.foreach(e => writeSingleArg(e, out)))
            gen.writeEndObject()
          case other => writeSingleArg(other, out)
        }
      }
      gen.writeEndObject()
    }

    def writeSingleArg(arg: EXPR, gen: JsonGenerator): Unit = {
      gen.writeStartObject()
      arg match {
        case CONST_LONG(num) =>
          gen.writeStringField("type", "integer")
          gen.writeNumberField("value", num, numbersAsString)
        case CONST_BOOLEAN(bool) =>
          gen.writeStringField("type", "boolean")
          gen.writeBooleanField("value", bool)
        case CONST_BYTESTR(bytes) =>
          gen.writeStringField("type", "binary")
          gen.writeStringField("value", bytes.base64)
        case CONST_STRING(str) =>
          gen.writeStringField("type", "string")
          gen.writeStringField("value", str)
        case ARR(_) =>
          gen.writeStringField("type", "list")
          gen.writeStringField("value", "unsupported")
        case arg => throw new NotImplementedError(s"Not supported: $arg")
      }
      gen.writeEndObject()
    }
  }

  val leaseStatusSerializer: JsonSerializer[LeaseStatus] =
    (status: LeaseStatus, gen: JsonGenerator, serializers: SerializerProvider) => {
      if (status == LeaseStatus.active) gen.writeString("active") else gen.writeString("canceled")
    }

  def leaseRefSerializer(numbersAsString: Boolean): JsonSerializer[LeaseRef] =
    (l: LeaseRef, gen: JsonGenerator, serializers: SerializerProvider) => {
      gen.writeStartObject()
      gen.writeStringField("id", l.id.toString)
      l.originTransactionId.fold(gen.writeNullField("originTransactionId"))(txId => gen.writeStringField("originTransactionId", txId.toString))
      l.sender.fold(gen.writeNullField("sender"))(sender => gen.writeStringField("sender", sender.toString))
      l.recipient.fold(gen.writeNullField("recipient"))(recipient => gen.writeStringField("recipient", recipient.toString))
      l.amount.fold(gen.writeNullField("amount"))(amount => gen.writeNumberField("amount", amount, numbersAsString))
      l.height.fold(gen.writeNullField("height"))(height => gen.writeNumberField("height", height, numbersAsString))
      gen.writeStringField("status", if (l.status == LeaseStatus.active) "active" else "canceled")
      l.cancelHeight.fold(gen.writeNullField("cancelHeight"))(ch => gen.writeNumberField("cancelHeight", ch, numbersAsString))
      l.cancelTransactionId.fold(gen.writeNullField("cancelTransactionId"))(cti => gen.writeStringField("cancelTransactionId", cti.toString))
      gen.writeEndObject()
    }

  def leaseSerializer(numbersAsString: Boolean): JsonSerializer[Lease] =
    (l: Lease, gen: JsonGenerator, serializers: SerializerProvider) => {
      leaseRefSerializer(numbersAsString).serialize(leaseIdToLeaseRef(l.id, Some(l.recipient), Some(l.amount)), gen, serializers)
    }

  def leaseCancelSerializer(numbersAsString: Boolean): JsonSerializer[LeaseCancel] =
    (lc: LeaseCancel, gen: JsonGenerator, serializers: SerializerProvider) => {
      leaseRefSerializer(numbersAsString).serialize(leaseIdToLeaseRef(lc.id), gen, serializers)
    }

  def paymentSerializer(numbersAsString: Boolean): JsonSerializer[Payment] =
    (p: Payment, gen: JsonGenerator, serializers: SerializerProvider) => {
      gen.writeStartObject()
      gen.writeNumberField("amount", p.amount, numbersAsString)
      gen.writeValueField("assetId")(assetSerializer.serialize(p.assetId, _, serializers))
      gen.writeEndObject()
    }

  def attachedPaymentSerializer(numbersAsString: Boolean): JsonSerializer[AttachedPayment] =
    (p: AttachedPayment, gen: JsonGenerator, serializers: SerializerProvider) => {
      gen.writeStartObject()
      gen.writeValueField("assetId")(assetSerializer.serialize(p.assetId, _, serializers))
      gen.writeNumberField("amount", p.amount, numbersAsString)
      gen.writeEndObject()
    }

  def isrPaymentSerializer(numbersAsString: Boolean): JsonSerializer[InvokeScriptResult.Payment] =
    (p: InvokeScriptResult.Payment, gen: JsonGenerator, serializers: SerializerProvider) => {
      gen.writeStartObject()
      gen.writeStringField("address", p.address.toString)
      gen.writeValueField("asset")(assetSerializer.serialize(p.asset, _, serializers))
      gen.writeNumberField("amount", p.amount, numbersAsString)
      gen.writeEndObject()
    }

  def issueSerializer(numbersAsString: Boolean): JsonSerializer[Issue] =
    (issue: Issue, gen: JsonGenerator, serializers: SerializerProvider) => {
      gen.writeStartObject()
      gen.writeStringField("assetId", issue.id.toString)
      issue.compiledScript.foreach(sc => gen.writeStringField("compiledScript", sc.toString))
      gen.writeNumberField("decimals", issue.decimals, numbersAsString)
      gen.writeStringField("description", issue.description)
      gen.writeBooleanField("isReissuable", issue.isReissuable)
      gen.writeStringField("name", issue.name)
      gen.writeNumberField("quantity", issue.quantity, numbersAsString)
      gen.writeNumberField("nonce", issue.nonce, numbersAsString)
      gen.writeEndObject()
    }

  def reissueSerializer(numbersAsString: Boolean): JsonSerializer[Reissue] =
    (r: Reissue, gen: JsonGenerator, serializers: SerializerProvider) => {
      gen.writeStartObject()
      gen.writeStringField("assetId", r.assetId.toString)
      gen.writeBooleanField("isReissuable", r.isReissuable)
      gen.writeNumberField("quantity", r.quantity, numbersAsString)
      gen.writeEndObject()
    }

  def burnSerializer(numbersAsString: Boolean): JsonSerializer[Burn] =
    (b: Burn, gen: JsonGenerator, serializers: SerializerProvider) => {
      gen.writeStartObject()
      gen.writeStringField("assetId", b.assetId.toString)
      gen.writeNumberField("quantity", b.quantity, numbersAsString)
      gen.writeEndObject()
    }

  def sponsorFeeSerializer(numbersAsString: Boolean): JsonSerializer[SponsorFee] =
    (s: SponsorFee, gen: JsonGenerator, serializers: SerializerProvider) => {
      gen.writeStartObject()
      gen.writeStringField("assetId", s.assetId.toString)
      s.minSponsoredAssetFee.foreach(fee => gen.writeNumberField("minSponsoredAssetFee", fee, numbersAsString))
      gen.writeEndObject()
    }

  def callSerializer(numbersAsString: Boolean): JsonSerializer[Call] =
    (c: Call, gen: JsonGenerator, serializers: SerializerProvider) => {
      gen.writeStartObject()
      gen.writeStringField("function", c.function)
      gen.writeArrayField("args", c.args)(evaluatedSerializer(numbersAsString), serializers)
      gen.writeEndObject()
    }

  def invocationSerializer(numbersAsString: Boolean): JsonSerializer[Invocation] =
    (inv: Invocation, gen: JsonGenerator, serializers: SerializerProvider) => {
      gen.writeStartObject()
      gen.writeStringField("dApp", inv.dApp.toString)
      gen.writeValueField("call")(callSerializer(numbersAsString).serialize(inv.call, _, serializers))
      gen.writeArrayField("payment", inv.payments)(attachedPaymentSerializer(numbersAsString), serializers)
      gen.writeValueField("stateChanges")(invokeScriptResultSerializer(numbersAsString).serialize(inv.stateChanges, _, serializers))
      gen.writeEndObject()
    }

  val errorMessageSerializer: JsonSerializer[ErrorMessage] =
    (err: ErrorMessage, gen: JsonGenerator, serializers: SerializerProvider) => {
      gen.writeStartObject()
      gen.writeNumberField("code", err.code, false)
      gen.writeStringField("text", err.text)
      gen.writeEndObject()
    }

  def invokeScriptResultSerializer(numbersAsString: Boolean): JsonSerializer[InvokeScriptResult] =
    (isr: InvokeScriptResult, gen: JsonGenerator, serializers: SerializerProvider) => {
      gen.writeStartObject()
      gen.writeArrayField("data", isr.data)(DataEntry.dataEntrySerializer(numbersAsString), serializers)
      gen.writeArrayField("transfers", isr.transfers)(isrPaymentSerializer(numbersAsString), serializers)
      gen.writeArrayField("issues", isr.issues)(issueSerializer(numbersAsString), serializers)
      gen.writeArrayField("reissues", isr.reissues)(reissueSerializer(numbersAsString), serializers)
      gen.writeArrayField("burns", isr.burns)(burnSerializer(numbersAsString), serializers)
      gen.writeArrayField("sponsorFees", isr.sponsorFees)(sponsorFeeSerializer(numbersAsString), serializers)
      gen.writeArrayField("leases", isr.leases)(leaseSerializer(numbersAsString), serializers)
      gen.writeArrayField("leaseCancels", isr.leaseCancels)(leaseCancelSerializer(numbersAsString), serializers)
      gen.writeArrayField("invokes", isr.invokes)(invocationSerializer(numbersAsString), serializers)
      isr.error.foreach(err => gen.writeValueField("error")(errorMessageSerializer.serialize(err, _, serializers)))
      gen.writeEndObject()
    }

  def txMetaJsonSerializer(address: Address, isBlockV5: Int => Boolean, numbersAsString: Boolean): JsonSerializer[TxMetaEnriched] =
    (txMeta: TxMetaEnriched, gen: JsonGenerator, serializers: SerializerProvider) => {
      txMeta.meta match {
        case TransactionMeta.Invoke(height, tx: InvokeScriptTransaction, status, spentComplexity, invokeScriptResult) =>
          gen.writeStartObject()
          gen.writeNumberField("type", tx.tpe.id, numbersAsString)
          gen.writeStringField("id", tx.id().toString)
          gen.writeNumberField("fee", tx.assetFee._2, numbersAsString)
          tx.feeAssetId match {
            case IssuedAsset(id) => gen.writeStringField("feeAssetId", id.toString)
            case Asset.Waves     => gen.writeNullField("feeAssetId")
          }
          gen.writeNumberField("timestamp", tx.timestamp, numbersAsString)
          gen.writeNumberField("version", tx.version, numbersAsString)
          if (PBSince.affects(tx)) gen.writeNumberField("chainId", tx.chainId, numbersAsString)
          gen.writeStringField("sender", tx.sender.toAddress(tx.chainId).toString)
          gen.writeStringField("senderPublicKey", tx.sender.toString)
          gen.writeArrayField("proofs")(gen => tx.proofs.proofs.foreach(p => gen.writeString(p.toString)))
          gen.writeStringField("dApp", tx.dApp.toString)
          gen.writeArrayField("payment", tx.payments)(paymentSerializer(numbersAsString), serializers)
          gen.writeValueField("call")(funcCallSerializer(numbersAsString).serialize(tx.funcCall, _, serializers))
          gen.writeNumberField("height", height.toInt, numbersAsString)
          val appStatus =
            if (isBlockV5(height)) {
              Some(applicationStatusFromTxStatus(status))
            } else
              None
          appStatus.foreach(s => gen.writeStringField("applicationStatus", s))
          gen.writeNumberField("spentComplexity", spentComplexity, numbersAsString)
          invokeScriptResult.fold(gen.writeNullField("stateChanges"))(isr =>
            gen.writeValueField("stateChanges")(invokeScriptResultSerializer(numbersAsString).serialize(isr, _, serializers))
          )
          gen.writeEndObject()
        case TransactionMeta.Ethereum(height, tx, status, spentComplexity, Some(EthereumTransactionMeta(Payload.Invocation(i), _)), isr) =>
          val functionCallEi = SerdeV1.deserializeFunctionCall(i.functionCall.toByteArray).toOption
          val payments       = i.payments.map(p => InvokeScriptTransaction.Payment(p.amount, PBAmounts.toVanillaAssetId(p.assetId)))

          gen.writeStartObject()
          gen.writeNumberField("type", tx.tpe.id, numbersAsString)
          gen.writeStringField("id", tx.id().toString)
          gen.writeNumberField("fee", tx.assetFee._2, numbersAsString)
          gen.writeStringField("feeAssetId", null)
          gen.writeNumberField("timestamp", tx.timestamp, numbersAsString)
          gen.writeNumberField("version", 1, numbersAsString)
          gen.writeNumberField("chainId", tx.chainId, numbersAsString)
          gen.writeStringField("bytes", EthEncoding.toHexString(tx.bytes()))
          gen.writeStringField("sender", tx.senderAddress().toString)
          gen.writeStringField("senderPublicKey", tx.signerPublicKey().toString)
          gen.writeNumberField("height", height.toInt, numbersAsString)
          val appStatus =
            if (isBlockV5(height)) {
              Some(applicationStatusFromTxStatus(status))
            } else
              None
          appStatus.foreach(s => gen.writeStringField("applicationStatus", s))
          gen.writeNumberField("spentComplexity", spentComplexity, numbersAsString)
          gen.writeObjectFieldStart("payload")
          gen.writeStringField("type", "invocation")
          gen.writeStringField("dApp", Address(EthEncoding.toBytes(tx.underlying.getTo)).toString)
          functionCallEi.fold(gen.writeNullField("call"))(fc =>
            gen.writeValueField("call")(funcCallSerializer(numbersAsString).serialize(fc, _, serializers))
          )
          gen.writeArrayField("payment", payments)(paymentSerializer(numbersAsString), serializers)
          isr.fold(gen.writeNullField("stateChanges"))(isr =>
            gen.writeValueField("stateChanges")(invokeScriptResultSerializer(numbersAsString).serialize(isr, _, serializers))
          )
          gen.writeEndObject()
          gen.writeEndObject()
        case meta @ TransactionMeta.Default(_, mtt: MassTransferTransaction, _, _) if mtt.sender.toAddress != address =>
          /** Produces compact representation for large transactions by stripping unnecessary data. Currently implemented for MassTransfer transaction
            * only.
            */
          jsObjectSerializer(numbersAsString).serialize(
            mtt.compactJson(address, txMeta.aliases.getOrElse(Set.empty)) ++ transactionMetaJson(meta),
            gen,
            serializers
          )
        case other =>
          jsObjectSerializer(numbersAsString).serialize(other.transaction.json() ++ transactionMetaJson(other), gen, serializers)
      }
    }

  def jsObjectSerializer(numbersAsString: Boolean): JsonSerializer[JsObject] = new JsonSerializer[JsObject] {
    override def serialize(jsObj: JsObject, gen: JsonGenerator, serializers: SerializerProvider): Unit = {
      gen.writeStartObject()
      jsObj.fields.foreach { case (key, value) => encodeField(key, value, gen, serializers) }
      gen.writeEndObject()
    }

    private def encodeField(key: String, jsValue: JsValue, gen: JsonGenerator, serializers: SerializerProvider): Unit = {
      jsValue match {
        case n: JsNumber =>
          gen.writeNumberField(key, n.value, numbersAsString)
        case b: JsBoolean =>
          gen.writeBooleanField(key, b.value)
        case s: JsString =>
          gen.writeStringField(key, s.value)
        case a: JsArray =>
          gen.writeArrayField(key)(out => a.value.foreach(encodeArrayElem(_, out, serializers)))
        case o: JsObject =>
          gen.writeValueField(key)(serialize(o, _, serializers))
        case _ =>
          gen.writeNullField(key)
      }
    }

    private def encodeArrayElem(jsValue: JsValue, gen: JsonGenerator, serializers: SerializerProvider): Unit = {
      jsValue match {
        case n: JsNumber =>
          gen.writeNumber(n.value.bigDecimal)
        case b: JsBoolean =>
          gen.writeBoolean(b.value)
        case s: JsString =>
          gen.writeString(s.value)
        case a: JsArray =>
          gen.writeStartArray()
          a.value.foreach(encodeArrayElem(_, gen, serializers))
          gen.writeEndArray()
        case o: JsObject =>
          serialize(o, gen, serializers)
        case _ =>
          gen.writeNull()
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
      metaJson(TxMeta(meta.height, meta.status, meta.spentComplexity)),
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
    TransactionJsonSerializer.applicationStatus(isBlockV5(m.height), m.status) ++ Json.obj("spentComplexity" -> m.spentComplexity)

  private def isBlockV5(height: Int): Boolean = blockchain.isFeatureActivated(BlockchainFeatures.BlockV5, height)

  // Extended lease format. Overrides default
  private def leaseIdToLeaseRef(
      leaseId: ByteStr,
      recipientParamOpt: Option[AddressOrAlias] = None,
      amountOpt: Option[Long] = None
  ): LeaseRef = {
    val detailsOpt           = blockchain.leaseDetails(leaseId)
    val txMetaOpt            = detailsOpt.flatMap(d => blockchain.transactionMeta(d.sourceId))
    val recipientOpt         = recipientParamOpt.orElse(detailsOpt.map(_.recipientAddress))
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
      amountOpt orElse detailsOpt.map(_.amount.value),
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
  def applicationStatus(isBlockV5: Boolean, status: TxMeta.Status): JsObject =
    if (isBlockV5)
      Json.obj("applicationStatus" -> applicationStatusFromTxStatus(status))
    else
      JsObject.empty

  def applicationStatusFromTxStatus(status: TxMeta.Status): String =
    status match {
      case TxMeta.Status.Succeeded => ApplicationStatus.Succeeded
      case TxMeta.Status.Failed    => ApplicationStatus.ScriptExecutionFailed
      case TxMeta.Status.Elided    => ApplicationStatus.Elided
    }

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
    implicit val config: Aux[Json.MacroOptions] = JsonConfiguration(optionHandlers = OptionHandlers.WritesNull)
    implicit val jsonWrites: OWrites[LeaseRef]  = Json.writes[LeaseRef]
  }
}
