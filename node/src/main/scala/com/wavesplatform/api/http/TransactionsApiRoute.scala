package com.wavesplatform.api.http

import scala.concurrent.Future
import scala.util.Success

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import cats.instances.either._
import cats.instances.list._
import cats.instances.try_._
import cats.syntax.alternative._
import cats.syntax.either._
import cats.syntax.traverse._
import com.wavesplatform.account.{Address, AddressOrAlias}
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.common.CommonTransactionsApi.TransactionMeta
import com.wavesplatform.api.http.ApiError._
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.TransactionProof
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, _}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.network.TransactionPublisher
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.{Blockchain, InvokeScriptResult}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.lease._
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import monix.eval.Task
import monix.execution.Scheduler
import play.api.libs.json._

case class TransactionsApiRoute(
    settings: RestAPISettings,
    commonApi: CommonTransactionsApi,
    wallet: Wallet,
    blockchain: Blockchain,
    utxPoolSize: () => Int,
    transactionPublisher: TransactionPublisher,
    time: Time
) extends ApiRoute
    with BroadcastRoute
    with AuthRoute {
  import TransactionsApiRoute._

  private[this] val serializer                     = TransactionJsonSerializer(blockchain, commonApi)
  private[this] implicit val transactionMetaWrites = OWrites[TransactionMeta](serializer.transactionWithMetaJson)

  override lazy val route: Route =
    pathPrefix("transactions") {
      unconfirmed ~ addressWithLimit ~ info ~ status ~ sign ~ calculateFee ~ signedBroadcast ~ merkleProof
    }

  def addressWithLimit: Route = {
    (get & path("address" / AddrSegment / "limit" / IntNumber) & parameter("after".?)) { (address, limit, maybeAfter) =>
      val after =
        maybeAfter.map(s => ByteStr.decodeBase58(s).getOrElse(throw ApiException(CustomValidationError(s"Unable to decode transaction id $s"))))
      if (limit > settings.transactionsByAddressLimit) throw ApiException(TooBigArrayAllocation)
      extractScheduler { implicit sc =>
        complete(transactionsByAddress(address, limit, after).map(txs => List(txs))) // Double list - [ [tx1, tx2, ...] ]
      }
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
    } ~ (pathEndOrSingleSlash & anyParam("id")) { ids =>
      val result = for {
        _        <- Either.cond(ids.nonEmpty, (), InvalidTransactionId("Transaction ID was not specified"))
        statuses <- ids.map(readTransactionMeta).toList.sequence
      } yield statuses

      complete(result)
    }
  }

  private[this] def loadTransactionStatus(id: ByteStr): JsObject = {
    import Status._
    val statusJson = blockchain.transactionInfo(id) match {
      case Some((height, _, succeeded)) =>
        Json.obj(
          "status"        -> Confirmed,
          "height"        -> height,
          "confirmations" -> (blockchain.height - height).max(0)
        ) ++ serializer.applicationStatus(height, succeeded)
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
    jsonPost[JsObject](TransactionFactory.parseRequestAndSign(wallet, address.stringRepr, time, _))
  }

  def signedBroadcast: Route = path("broadcast")(broadcast[JsValue](TransactionFactory.fromSignedRequest))

  def merkleProof: Route = path("merkleProof") {
    (get & parameters("id".as[String].*))(ids => complete(merkleProof(ids.toList.reverse))) ~
      jsonPost[JsObject](
        jsv =>
          (jsv \ "ids").validate[List[String]] match {
            case JsSuccess(ids, _) => merkleProof(ids)
            case JsError(err)      => WrongJson(errors = err.toSeq)
          }
      )
  }

  private def merkleProof(encodedIds: List[String]): ToResponseMarshallable =
    encodedIds.traverse(ByteStr.decodeBase58) match {
      case Success(txIds) =>
        commonApi.transactionProofs(txIds) match {
          case Nil    => CustomValidationError(s"transactions do not exist or block version < ${Block.ProtoBlockVersion}")
          case proofs => proofs
        }
      case _ => InvalidSignature
    }

  def transactionsByAddress(address: Address, limitParam: Int, maybeAfter: Option[ByteStr])(implicit sc: Scheduler): Future[List[JsObject]] = {
    val aliasesOfAddress: Task[Set[AddressOrAlias]] =
      commonApi
        .aliasesOfAddress(address)
        .collect { case (_, cat) => cat.alias }
        .toListL
        .map(aliases => (address :: aliases).toSet)
        .memoize

    /**
      * Produces compact representation for large transactions by stripping unnecessary data.
      * Currently implemented for MassTransfer transaction only.
      */
    def compactJson(address: Address, meta: TransactionMeta): Task[JsObject] = {
      import com.wavesplatform.transaction.transfer._
      meta.transaction match {
        case mtt: MassTransferTransaction if mtt.sender.toAddress != address =>
          aliasesOfAddress.map(mtt.compactJson(_) ++ serializer.transactionMetaJson(meta))
        case _ => Task.now(serializer.transactionWithMetaJson(meta))
      }
    }

    commonApi
      .transactionsByAddress(address, None, Set.empty, maybeAfter)
      .take(limitParam)
      .mapEval(compactJson(address, _))
      .toListL
      .runToFuture
  }
}

object TransactionsApiRoute {
  type LeaseStatus = LeaseStatus.Value

  //noinspection TypeAnnotation
  object LeaseStatus extends Enumeration {
    val active = Value(1)
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

  private[http] object TransactionJsonSerializer {
    def applicationStatus(isBlockV5: Boolean, succeeded: Boolean): JsObject =
      if (isBlockV5)
        Json.obj("applicationStatus" -> (if (succeeded) ApplicationStatus.Succeeded else ApplicationStatus.ScriptExecutionFailed))
      else
        JsObject.empty

    def height(height: Int): JsObject =
      Json.obj("height" -> height)
  }

  private[http] final case class TransactionJsonSerializer(blockchain: Blockchain, commonApi: CommonTransactionsApi) {
    def transactionMetaJson(meta: TransactionMeta): JsObject = {
      val specificInfo = meta.transaction match {
        case lease: LeaseTransaction =>
          import com.wavesplatform.api.http.TransactionsApiRoute.LeaseStatus._
          Json.obj("status" -> (if (blockchain.leaseDetails(lease.id()).exists(_.isActive)) active else canceled))

        case leaseCancel: LeaseCancelTransaction =>
          Json.obj("lease" -> leaseIdToLeaseRef(leaseCancel.leaseId))

        case _ => JsObject.empty
      }

      val stateChanges = meta match {
        case i: TransactionMeta.Invoke => Json.obj("stateChanges" -> i.invokeScriptResult)
        case _                         => JsObject.empty
      }

      Seq(
        TransactionJsonSerializer.height(meta.height),
        applicationStatus(meta.height, meta.succeeded),
        stateChanges,
        specificInfo
      ).reduce(_ ++ _)
    }

    def transactionWithMetaJson(meta: TransactionMeta): JsObject = {
      meta.transaction.json() ++ transactionMetaJson(meta)
    }

    def unconfirmedTxExtendedJson(tx: Transaction): JsObject = tx match {
      case leaseCancel: LeaseCancelTransaction =>
        leaseCancel.json() ++ Json.obj("lease" -> leaseIdToLeaseRef(leaseCancel.leaseId))

      case t => t.json()
    }

    def applicationStatus(height: Int, succeeded: Boolean): JsObject =
      TransactionJsonSerializer.applicationStatus(isBlockV5(height), succeeded)

    private[this] def isBlockV5(height: Int): Boolean = blockchain.isFeatureActivated(BlockchainFeatures.BlockV5, height)

    // Extended lease format. Overrides default
    private[this] def leaseIdToLeaseRef(leaseId: ByteStr): LeaseRef = {
      val ld          = blockchain.leaseDetails(leaseId).get
      val (height, _) = blockchain.transactionMeta(ld.sourceId).get
      val recipient   = blockchain.resolveAlias(ld.recipient).explicitGet()
      LeaseRef(leaseId, ld.sourceId, ld.sender.toAddress, recipient, ld.amount, height, LeaseStatus(ld.isActive))
    }

    private[http] implicit val leaseWrites: OWrites[InvokeScriptResult.Lease] =
      LeaseRef.jsonWrites.contramap((l: InvokeScriptResult.Lease) => leaseIdToLeaseRef(l.id))

    private[http] implicit val leaseCancelWrites: OWrites[InvokeScriptResult.LeaseCancel] =
      LeaseRef.jsonWrites.contramap((l: InvokeScriptResult.LeaseCancel) => leaseIdToLeaseRef(l.id))

    // To override nested InvokeScriptResult writes
    private[http] implicit lazy val invocationWrites: OWrites[InvokeScriptResult.Invocation] = (i: InvokeScriptResult.Invocation) => Json.obj(
      "dApp" -> i.dApp,
      "call" -> i.call,
      "payments" -> i.payments,
      "stateChanges" -> invokeScriptResultWrites.writes(i.stateChanges)
    )

    private[http] implicit lazy val invokeScriptResultWrites: OWrites[InvokeScriptResult] = {
      import InvokeScriptResult.{issueFormat, reissueFormat, burnFormat, sponsorFeeFormat}
      Json.writes[InvokeScriptResult]
    }
  }

  private[this] final case class LeaseRef(id: ByteStr, originTransactionId: ByteStr, sender: Address, recipient: Address, amount: TxAmount, height: Int, status: LeaseStatus = LeaseStatus.active)
  private[this] object LeaseRef {
    implicit val jsonWrites: OWrites[LeaseRef] = {
      import com.wavesplatform.utils.byteStrFormat
      Json.writes[LeaseRef]
    }
  }
}
