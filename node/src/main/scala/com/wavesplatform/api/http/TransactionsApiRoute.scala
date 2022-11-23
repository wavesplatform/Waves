package com.wavesplatform.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import akka.util.ByteString
import cats.instances.either.*
import cats.instances.list.*
import cats.instances.try_.*
import cats.syntax.alternative.*
import cats.syntax.either.*
import cats.syntax.traverse.*
import com.wavesplatform.account.{Address, AddressOrAlias, Alias}
import com.wavesplatform.api.common.{CommonTransactionsApi, TransactionMeta}
import com.wavesplatform.api.http.ApiError.*
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.TransactionProof
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, *}
import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.database.protobuf.EthereumTransactionMeta.Payload
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.v1.serialization.SerdeV1
import com.wavesplatform.network.TransactionPublisher
import com.wavesplatform.protobuf.transaction.PBAmounts
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.{Blockchain, InvokeScriptResult, TxMeta}
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.lease.*
import com.wavesplatform.transaction.serialization.impl.InvokeScriptTxSerializer
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.utils.{EthEncoding, Time}
import com.wavesplatform.wallet.Wallet
import monix.eval.Task
import play.api.libs.json.*

import com.wavesplatform.database.protobuf.EthereumTransactionMeta.Payload
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
import com.wavesplatform.state.InvokeScriptResult.{Lease, LeaseCancel}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import monix.reactive.Observable

case class TransactionsApiRoute(
    settings: RestAPISettings,
    commonApi: CommonTransactionsApi,
    wallet: Wallet,
    blockchain: Blockchain,
    utxPoolSize: () => Int,
    transactionPublisher: TransactionPublisher,
    time: Time,
    routeTimeout: RouteTimeout
) extends ApiRoute
    with BroadcastRoute
    with AuthRoute {
  import TransactionsApiRoute.*

  private[this] val serializer                     = TransactionJsonSerializer(blockchain, commonApi)
  private[this] implicit val transactionMetaWrites = OWrites[TransactionMeta](serializer.transactionWithMetaJson)

  override lazy val route: Route =
    pathPrefix("transactions") {
      unconfirmed ~ addressWithLimit ~ info ~ status ~ sign ~ calculateFee ~ signedBroadcast ~ merkleProof
    }

  def addressWithLimit: Route = {
    (get & path("address" / AddrSegment / "limit" / IntNumber) & parameter("after".?)) { (address, limit, maybeAfter) =>
      routeTimeout.executeFromObservable {
        val after =
          maybeAfter.map(s => ByteStr.decodeBase58(s).getOrElse(throw ApiException(CustomValidationError(s"Unable to decode transaction id $s"))))
        if (limit > settings.transactionsByAddressLimit) throw ApiException(TooBigArrayAllocation)

        transactionsByAddress(address, limit, after) // Double list - [ [tx1, tx2, ...] ]
      }(jsonStreamMarshallerNew("[[", ",", "]]"))
    }
  }

  private[this] def readTransactionMeta(id: String): Either[ApiError, TransactionMeta] =
    for {
      id   <- ByteStr.decodeBase58(id).toEither.leftMap(err => CustomValidationError(err.toString))
      meta <- commonApi.transactionById(id).toRight(ApiError.TransactionDoesNotExist)
    } yield meta

  def info: Route = pathPrefix("info") {
    (get & path(TransactionId)) { id =>
      complete(commonApi.transactionById(id).toRight(ApiError.TransactionDoesNotExist))
    } ~ (pathEndOrSingleSlash & anyParam("id", limit = settings.transactionsByAddressLimit)) { ids =>
      val result = for {
        _    <- Either.cond(ids.nonEmpty, (), InvalidTransactionId("Transaction ID was not specified"))
        meta <- ids.map(readTransactionMeta).toList.sequence
      } yield meta

      complete(result)
    }
  }

  private[this] def loadTransactionStatus(id: ByteStr): JsObject = {
    import Status.*
    val statusJson = blockchain.transactionInfo(id) match {
      case Some((tm, tx)) =>
        Json.obj(
          "status"        -> Confirmed,
          "height"        -> JsNumber(tm.height),
          "confirmations" -> (blockchain.height - tm.height).max(0)
        ) ++ serializer.metaJson(tm)
      case None =>
        commonApi.unconfirmedTransactionById(id) match {
          case Some(_) => Json.obj("status" -> Unconfirmed)
          case None    => Json.obj("status" -> NotFound)
        }
    }
    statusJson ++ Json.obj("id" -> id.toString)
  }

  def status: Route = pathPrefix("status") {
    path(TransactionId) { id =>
      complete(loadTransactionStatus(id))
    } ~ pathEndOrSingleSlash {
      anyParam("id").filter(_.nonEmpty) { ids =>
        if (ids.toSeq.length > settings.transactionsByAddressLimit)
          complete(TooBigArrayAllocation)
        else {
          ids.map(id => ByteStr.decodeBase58(id).toEither.leftMap(_ => id)).toList.separate match {
            case (Nil, ids) =>
              val results = ids.toSet.map((id: ByteStr) => id -> loadTransactionStatus(id)).toMap
              complete(ids.map(id => results(id)))
            case (errors, _) => complete(InvalidIds(errors))
          }
        }
      } ~ pathEndOrSingleSlash {
        complete(CustomValidationError("Empty request"))
      }
    }
  }

  def unconfirmed: Route = (pathPrefix("unconfirmed") & get) {
    pathEndOrSingleSlash {
      complete(JsArray(commonApi.unconfirmedTransactions.map(serializer.unconfirmedTxExtendedJson)))
    } ~ utxSize ~ utxTransactionInfo
  }

  def utxSize: Route = (pathPrefix("size") & get) {
    complete(Json.obj("size" -> JsNumber(utxPoolSize())))
  }

  def utxTransactionInfo: Route = (pathPrefix("info") & get) {
    pathEndOrSingleSlash {
      complete(InvalidSignature)
    } ~
      path(TransactionId) { id =>
        commonApi.unconfirmedTransactionById(id) match {
          case Some(tx) =>
            complete(serializer.unconfirmedTxExtendedJson(tx))
          case None =>
            complete(ApiError.TransactionDoesNotExist)
        }
      }
  }

  def calculateFee: Route =
    path("calculateFee")(jsonPost[JsObject] { jsv =>
      val senderPk = (jsv \ "senderPublicKey").as[String]
      // Just for converting the request to the transaction
      val enrichedJsv = jsv ++ Json.obj(
        "fee"    -> 1234567,
        "sender" -> senderPk
      )

      createTransaction(senderPk, enrichedJsv) { tx =>
        commonApi
          .calculateFee(tx)
          .map { case (assetId, assetAmount, _) => Json.obj("feeAssetId" -> assetId, "feeAmount" -> assetAmount) }
      }
    })

  def sign: Route = (pathPrefix("sign") & withAuth) {
    pathEndOrSingleSlash(jsonPost[JsObject] { jsv =>
      TransactionFactory.parseRequestAndSign(wallet, (jsv \ "sender").as[String], time, jsv)
    }) ~ signWithSigner
  }

  def signWithSigner: Route = path(AddrSegment) { address =>
    jsonPost[JsObject](TransactionFactory.parseRequestAndSign(wallet, address.toString, time, _))
  }

  def signedBroadcast: Route = path("broadcast")(broadcast[JsValue](TransactionFactory.fromSignedRequest))

  def merkleProof: Route = path("merkleProof") {
    anyParam("id", limit = settings.transactionsByAddressLimit) { ids =>
      val result = Either
        .cond(ids.nonEmpty, (), InvalidTransactionId("Transaction ID was not specified"))
        .map(_ => merkleProof(ids.toList))
      complete(result)
    }
  }

  private def merkleProof(encodedIds: List[String]): ToResponseMarshallable =
    encodedIds.map(id => ByteStr.decodeBase58(id).toEither.leftMap(_ => id)).separate match {
      case (Nil, txIds) =>
        commonApi.transactionProofs(txIds) match {
          case Nil    => CustomValidationError(s"transactions do not exist or block version < ${Block.ProtoBlockVersion}")
          case proofs => proofs
        }
      case (errors, _) => InvalidIds(errors)
    }

  def transactionsByAddress(address: Address, limitParam: Int, maybeAfter: Option[ByteStr]): Observable[ByteString] = {
    val aliasesOfAddress: Task[Set[Alias]] =
      commonApi
        .aliasesOfAddress(address)
        .collect { case (_, cat) => cat.alias }
        .toListL
        .map(aliases => aliases.toSet)
        .memoize

    val blockV5Activation  = blockchain.activatedFeatures.get(BlockchainFeatures.BlockV5.id)
    val improvedSerializer = serializer.copy(blockchain = blockchain.compositeBlockchain)

    /** Produces compact representation for large transactions by stripping unnecessary data. Currently implemented for MassTransfer transaction only.
      */
    def compactJson(address: Address, meta: TransactionMeta): Task[ByteString] = {
      import com.wavesplatform.transaction.transfer.*
      meta.transaction match {
        case mtt: MassTransferTransaction if mtt.sender.toAddress != address =>
          (if (
             mtt.transfers.exists(pt =>
               pt.address match {
                 case address: Address => false
                 case a: Alias         => true
               }
             )
           ) aliasesOfAddress.map(aliases => mtt.compactJson(address, aliases))
           else Task.now(mtt.compactJson(address, Set.empty))).map(a => ByteString(Json.stringify(a ++ improvedSerializer.transactionMetaJson(meta))))

        case _ => Task.now(improvedSerializer.transactionWithMetaJsonNew(meta, h => blockV5Activation.exists(v5h => v5h <= h)))
      }
    }

    commonApi
      .transactionsByAddress(address, None, Set.empty, maybeAfter)
      .take(limitParam)
      .mapEval(compactJson(address, _))
  }
}

object TransactionsApiRoute {
  type LeaseStatus = LeaseStatus.Value

  // noinspection TypeAnnotation
  object LeaseStatus extends Enumeration {
    val active   = Value(1)
    val canceled = Value(0)

    def apply(bool: Boolean): LeaseStatus = if (bool) active else canceled
  }

  object Status {
    val Confirmed   = "confirmed"
    val Unconfirmed = "unconfirmed"
    val NotFound    = "not_found"
  }

  object ApplicationStatus {
    val Succeeded             = "succeeded"
    val ScriptExecutionFailed = "script_execution_failed"
  }

  implicit val transactionProofWrites: Writes[TransactionProof] = Writes { mi =>
    Json.obj(
      "id"               -> mi.id.toString,
      "transactionIndex" -> mi.transactionIndex,
      "merkleProof"      -> mi.digests.map(d => s"${Base58.encode(d)}")
    )
  }

  implicit val transactionProofReads: Reads[TransactionProof] = Reads { jsv =>
    for {
      encoded          <- (jsv \ "id").validate[String]
      id               <- ByteStr.decodeBase58(encoded).fold(_ => JsError(InvalidSignature.message), JsSuccess(_))
      transactionIndex <- (jsv \ "transactionIndex").validate[Int]
      merkleProof      <- (jsv \ "merkleProof").validate[List[String]].map(_.map(Base58.decode))
    } yield TransactionProof(id, transactionIndex, merkleProof)
  }

  object TransactionJsonSerializer {
    def applicationStatus(isBlockV5: Boolean, succeeded: Boolean): JsObject =
      if (isBlockV5)
        Json.obj("applicationStatus" -> (if (succeeded) ApplicationStatus.Succeeded else ApplicationStatus.ScriptExecutionFailed))
      else
        JsObject.empty

    def height(height: Int): JsObject =
      Json.obj("height" -> height)
  }

  final case class TransactionJsonSerializer(blockchain: Blockchain, commonApi: CommonTransactionsApi) {
    import com.github.plokhotnyuk.jsoniter_scala.macros.*
    import com.github.plokhotnyuk.jsoniter_scala.core.*

    case class EthereumInvokeMeta(
        id: String,
        fee: Long,
        feeAssetId: Option[String],
        timestamp: Long,
        version: Byte,
        chainId: Option[Byte],
        bytes: String,
        sender: String,
        senderPublicKey: String,
        height: Int,
        applicationStatus: Option[String],
        spentComplexity: Long,
        `type`: String,
        dApp: String,
        call: Option[FUNCTION_CALL],
        payment: Seq[Payment],
        stateChanges: Option[InvokeScriptResult]
    )

    case class InvokeMeta(
        `type`: Int,
        id: String,
        fee: Long,
        feeAssetId: Option[String],
        timestamp: Long,
        version: Byte,
        chainId: Option[Byte],
        sender: String,
        senderPublicKey: String,
        proofs: Seq[String],
        dApp: String,
        payment: Seq[Payment],
        call: FUNCTION_CALL,
        height: Int,
        applicationStatus: Option[String],
        spentComplexity: Long,
        stateChanges: Option[InvokeScriptResult]
    )

    val stringCodec: JsonValueCodec[String]               = JsonCodecMaker.make
    val optionStringCodec: JsonValueCodec[Option[String]] = JsonCodecMaker.make

    def optionCodec[A](implicit jsonValueCodec: JsonValueCodec[A]): JsonValueCodec[Option[A]] = JsonCodecMaker.make

    implicit val addressCodec: JsonValueCodec[Address] = new JsonValueCodec[Address] {
      override def decodeValue(in: JsonReader, default: Address): Address = ???
      override def encodeValue(x: Address, out: JsonWriter): Unit         = stringCodec.encodeValue(x.toString, out)
      override def nullValue: Address                                     = ???
    }

    implicit val addressOrAliasCodec: JsonValueCodec[AddressOrAlias] = new JsonValueCodec[AddressOrAlias] {
      override def decodeValue(in: JsonReader, default: AddressOrAlias): AddressOrAlias = ???
      override def encodeValue(x: AddressOrAlias, out: JsonWriter): Unit = x match {
        case addr: Address => stringCodec.encodeValue(addr.toString, out)
        case alias: Alias  => stringCodec.encodeValue(alias.toString, out)
      }
      override def nullValue: AddressOrAlias = ???
    }

    implicit val byteStrCodec: JsonValueCodec[ByteStr] = new JsonValueCodec[ByteStr] {
      override def decodeValue(in: JsonReader, default: ByteStr): ByteStr = ???
      override def encodeValue(x: ByteStr, out: JsonWriter): Unit         = stringCodec.encodeValue(x.toString, out)
      override def nullValue: ByteStr                                     = ???
    }

    implicit val assetCodec: JsonValueCodec[Asset] = new JsonValueCodec[Asset] {
      override def decodeValue(in: JsonReader, default: Asset): Asset = ???
      override def encodeValue(x: Asset, out: JsonWriter): Unit =
        optionStringCodec.encodeValue(
          x match {
            case Waves           => None
            case IssuedAsset(id) => Some(id.toString)
          },
          out
        )
      override def nullValue: Asset = ???
    }

    implicit val evaluatedCodec: JsonValueCodec[EVALUATED] = new JsonValueCodec[EVALUATED] {
      override def decodeValue(in: JsonReader, default: EVALUATED): EVALUATED = ???
      override def encodeValue(x: EVALUATED, out: JsonWriter): Unit = {
        out.writeObjectStart()
        x match {
          case CONST_LONG(num) =>
            out.writeKey("type")
            out.writeVal("Int")
            out.writeKey("value")
            out.writeVal(num)
          case CONST_BYTESTR(bs) =>
            out.writeKey("type")
            out.writeVal("ByteVector")
            out.writeKey("value")
            out.writeVal(bs.toString)
          case CONST_STRING(str) =>
            out.writeKey("type")
            out.writeVal("String")
            out.writeKey("value")
            out.writeVal(str)
          case CONST_BOOLEAN(b) =>
            out.writeKey("type")
            out.writeVal("Boolean")
            out.writeKey("value")
            out.writeVal(b)
          case CaseObj(caseType, fields) =>
            out.writeKey("type")
            out.writeVal(caseType.name)
            out.writeKey("value")
            out.writeObjectStart()
            fields.foreach { case (key, value) =>
              out.writeKey(key)
              encodeValue(value, out)
            }
            out.writeObjectEnd()
          case ARR(xs) =>
            out.writeKey("type")
            out.writeVal("Array")
            out.writeKey("value")
            out.writeArrayStart()
            xs.foreach(encodeValue(_, out))
            out.writeArrayEnd()
          case FAIL(reason) =>
            out.writeKey("error")
            out.writeVal(reason)
          case _ =>
        }
        out.writeObjectEnd()
      }
      override def nullValue: EVALUATED = ???
    }

    implicit val funcCallCodec: JsonValueCodec[FUNCTION_CALL] = new JsonValueCodec[FUNCTION_CALL] {
      def writeSingleArg(arg: EXPR, out: JsonWriter): Unit = {
        out.writeObjectStart()
        arg match {
          case CONST_LONG(num) =>
            out.writeKey("type")
            out.writeVal("integer")
            out.writeKey("value")
            out.writeVal(num)
          case CONST_BOOLEAN(bool) =>
            out.writeKey("type")
            out.writeVal("boolean")
            out.writeKey("value")
            out.writeVal(bool)
          case CONST_BYTESTR(bytes) =>
            out.writeKey("type")
            out.writeVal("binary")
            out.writeKey("value")
            out.writeVal(bytes.base64)
          case CONST_STRING(str) =>
            out.writeKey("type")
            out.writeVal("string")
            out.writeKey("value")
            out.writeVal(str)
          case ARR(_) =>
            out.writeKey("type")
            out.writeVal("list")
            out.writeKey("value")
            out.writeVal("unsupported")
          case arg => throw new NotImplementedError(s"Not supported: $arg")
        }
        out.writeObjectEnd()
      }

      override def decodeValue(in: JsonReader, default: FUNCTION_CALL): FUNCTION_CALL = ???
      override def encodeValue(x: FUNCTION_CALL, out: JsonWriter): Unit = {
        out.writeObjectStart()
        out.writeKey("function")
        out.writeVal(x.function.funcName)
        out.writeKey("args")
        out.writeArrayStart()
        x.args.foreach {
          case Terms.ARR(elements) =>
            out.writeObjectStart()
            out.writeKey("type")
            out.writeVal("list")
            out.writeKey("value")
            out.writeArrayStart()
            elements.foreach(writeSingleArg(_, out))
            out.writeArrayEnd()
            out.writeObjectEnd()
          case other => writeSingleArg(other, out)
        }
        out.writeArrayEnd()
        out.writeObjectEnd()
      }
      override def nullValue: FUNCTION_CALL = ???
    }

    implicit val leaseStatusCodec: JsonValueCodec[LeaseStatus] = new JsonValueCodec[LeaseStatus] {
      override def decodeValue(in: JsonReader, default: LeaseStatus): LeaseStatus = ???
      override def encodeValue(x: LeaseStatus, out: JsonWriter): Unit =
        if (x == LeaseStatus.active) out.writeVal("active") else out.writeVal("canceled")
      override def nullValue: LeaseStatus = ???
    }
    implicit val leaseRefCodec: JsonValueCodec[LeaseRef] = JsonCodecMaker.make(CodecMakerConfig.withTransientNone(false))

    implicit val leaseCodec: JsonValueCodec[Lease] = new JsonValueCodec[Lease] {
      override def decodeValue(in: JsonReader, default: Lease): Lease = ???
      override def encodeValue(x: Lease, out: JsonWriter): Unit =
        leaseRefCodec.encodeValue(leaseIdToLeaseRef(x.id), out)
      override def nullValue: Lease = ???
    }

    implicit val leaseCancelCodec: JsonValueCodec[LeaseCancel] = new JsonValueCodec[LeaseCancel] {
      override def decodeValue(in: JsonReader, default: LeaseCancel): LeaseCancel = ???
      override def encodeValue(x: LeaseCancel, out: JsonWriter): Unit =
        leaseRefCodec.encodeValue(leaseIdToLeaseRef(x.id), out)
      override def nullValue: LeaseCancel = ???
    }

    implicit val invokeMetaCodec: JsonValueCodec[InvokeMeta] =
      JsonCodecMaker.make(
        CodecMakerConfig.withDiscriminatorFieldName(None).withAllowRecursiveTypes(true).withTransientEmpty(false).withTransientDefault(false)
      )

    implicit val ethInvokeMetaCodec: JsonValueCodec[EthereumInvokeMeta] =
      JsonCodecMaker.make(
        CodecMakerConfig.withDiscriminatorFieldName(None).withAllowRecursiveTypes(true).withTransientEmpty(false).withTransientDefault(false)
      )

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

    def transactionWithMetaJsonNew(meta: TransactionMeta, isBlockV5: Int => Boolean): ByteString =
      meta match {
        case TransactionMeta.Invoke(height, transaction, succeeded, spentComplexity, invokeScriptResult) =>
          val invokeMeta = InvokeMeta(
            transaction.tpe.id.toByte,
            transaction.id().toString,
            transaction.assetFee._2,
            transaction.assetFee._1.maybeBase58Repr,
            transaction.timestamp,
            transaction.version,
            if (transaction.asInstanceOf[PBSince].isProtobufVersion) Some(transaction.chainId) else None,
            transaction.sender.toAddress(transaction.chainId).toString,
            transaction.sender.toString,
            transaction.proofs.proofs.map(_.toString),
            transaction.dApp.toString,
            transaction.payments,
            transaction.funcCall,
            height,
            if (isBlockV5(height))
              if (succeeded) Some(ApplicationStatus.Succeeded) else Some(ApplicationStatus.ScriptExecutionFailed)
            else
              None,
            spentComplexity,
            invokeScriptResult
          )
          ByteString.fromArrayUnsafe(writeToArray(invokeMeta)(invokeMetaCodec))
        case TransactionMeta.Ethereum(height, tx, succeeded, spentComplexity, Some(EthereumTransactionMeta(Payload.Invocation(i), _)), isr) =>
          val functionCallEi = SerdeV1.deserializeFunctionCall(i.functionCall.toByteArray).toOption
          val payments       = i.payments.map(p => InvokeScriptTransaction.Payment(p.amount, PBAmounts.toVanillaAssetId(p.assetId)))

          val ethereumInvokeMeta = EthereumInvokeMeta(
            tx.id().toString,
            tx.assetFee._2,
            tx.assetFee._1.maybeBase58Repr,
            tx.timestamp,
            tx.version,
            if (tx.isProtobufVersion) Some(tx.chainId) else None,
            EthEncoding.toHexString(tx.bytes()),
            tx.senderAddress().toString,
            tx.signerPublicKey().toString,
            height,
            if (isBlockV5(height))
              if (succeeded) Some(ApplicationStatus.Succeeded) else Some(ApplicationStatus.ScriptExecutionFailed)
            else
              None,
            spentComplexity,
            "invocation",
            Address(EthEncoding.toBytes(tx.underlying.getTo)).toString,
            functionCallEi,
            payments,
            isr
          )
          ByteString.fromArrayUnsafe(writeToArray(ethereumInvokeMeta)(ethInvokeMetaCodec))
        case other =>
          ByteString(Json.stringify(meta.transaction.json() ++ transactionMetaJson(meta)))
      }

    def unconfirmedTxExtendedJson(tx: Transaction): JsObject = tx match {
      case leaseCancel: LeaseCancelTransaction =>
        leaseCancel.json() ++ Json.obj("lease" -> leaseIdToLeaseRef(leaseCancel.leaseId))

      case t => t.json()
    }

    def metaJson(m: TxMeta): JsObject =
      TransactionJsonSerializer.applicationStatus(isBlockV5(m.height), m.succeeded) ++ Json.obj("spentComplexity" -> m.spentComplexity)

    private[this] def isBlockV5(height: Int): Boolean = blockchain.isFeatureActivated(BlockchainFeatures.BlockV5, height)

    // Extended lease format. Overrides default
    private[this] def leaseIdToLeaseRef(leaseId: ByteStr): LeaseRef = {
      val ld        = blockchain.leaseDetails(leaseId).get
      val tm        = blockchain.transactionMeta(ld.sourceId).get
      val recipient = blockchain.resolveAlias(ld.recipient).explicitGet()

      val (status, cancelHeight, cancelTxId) = ld.status match {
        case LeaseDetails.Status.Active                  => (true, None, None)
        case LeaseDetails.Status.Cancelled(height, txId) => (false, Some(height), txId)
        case LeaseDetails.Status.Expired(height)         => (false, Some(height), None)
      }

      LeaseRef(leaseId, ld.sourceId, ld.sender.toAddress, recipient, ld.amount, tm.height, LeaseStatus(status), cancelHeight, cancelTxId)
    }

    private[http] implicit val leaseWrites: OWrites[InvokeScriptResult.Lease] =
      LeaseRef.jsonWrites.contramap((l: InvokeScriptResult.Lease) => leaseIdToLeaseRef(l.id))

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

  final case class LeaseRef(
      id: ByteStr,
      originTransactionId: ByteStr,
      sender: Address,
      recipient: Address,
      amount: Long,
      height: Int,
      status: LeaseStatus,
      cancelHeight: Option[Int],
      cancelTransactionId: Option[ByteStr]
  )
  private[this] object LeaseRef {
    import com.wavesplatform.utils.byteStrFormat
    implicit val config                        = JsonConfiguration(optionHandlers = OptionHandlers.WritesNull)
    implicit val jsonWrites: OWrites[LeaseRef] = Json.writes[LeaseRef]
  }
}
