package com.wavesplatform.it.api

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.DataEntry
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import io.grpc.{Metadata, Status => GrpcStatus}
import play.api.libs.json._

import scala.util.{Failure, Success}

// USCE no longer contains references to non-serializable Request/Response objects
// to work around https://github.com/scalatest/scalatest/issues/556
case class UnexpectedStatusCodeException(requestMethod: String, requestUrl: String, statusCode: Int, responseBody: String)
    extends Exception(s"Request: $requestMethod $requestUrl; Unexpected status code ($statusCode): $responseBody")

case class GrpcStatusRuntimeException(status: GrpcStatus, metaData: Metadata) extends Exception(s"$status $metaData")

case class Status(blockchainHeight: Int, stateHeight: Int, updatedTimestamp: Long, updatedDate: String)
object Status {
  implicit val statusFormat: Format[Status] = Json.format
}

case class Peer(address: String, declaredAddress: String, peerName: String)
object Peer {
  implicit val peerFormat: Format[Peer] = Json.format
}

case class Address(address: String)
object Address {
  implicit val addressFormat: Format[Address] = Json.format
}

case class Balance(address: String, confirmations: Int, balance: Long)
object Balance {
  implicit val balanceFormat: Format[Balance] = Json.format
}

case class BalanceDetails(address: String, regular: Long, generating: Long, available: Long, effective: Long)
object BalanceDetails {
  implicit val balanceDetailsFormat: Format[BalanceDetails] = Json.format
}

case class AssetBalance(address: String, assetId: String, balance: Long)
object AssetBalance {
  implicit val assetBalanceFormat: Format[AssetBalance] = Json.format
}

case class CompiledScript(script: String, complexity: Long, extraFee: Long)
object CompiledScript {
  implicit val compiledScriptFormat: Format[CompiledScript] = Json.format
}

case class EstimatedScript(script: String, scriptText: String, complexity: Long, extraFee: Long)
object EstimatedScript {
  implicit val estimatedScriptFormat: Format[EstimatedScript] = Json.format
}

case class DecompiledScript(script: String)
object DecompiledScript {
  implicit val decompiledScriptFormat: Format[DecompiledScript] = Json.format
}

case class FullAssetInfo(
    assetId: String,
    balance: Long,
    reissuable: Boolean,
    minSponsoredAssetFee: Option[Long],
    sponsorBalance: Option[Long],
    quantity: Long,
    issueTransaction: Option[Transaction]
)
object FullAssetInfo {
  implicit val fullAssetInfoFormat: Format[FullAssetInfo] = Json.format
}

case class NFTAssetInfo(assetId: String, reissuable: Boolean, quantity: Long, decimals: Byte)
object NFTAssetInfo {
  implicit val nftAssetInfo: Format[NFTAssetInfo] = Json.format
}

case class FullAssetsInfo(address: String, balances: List[FullAssetInfo])
object FullAssetsInfo {
  implicit val fullAssetsInfoFormat: Format[FullAssetsInfo] = Json.format
}

case class ScriptAssetInfo(scriptComplexity: Long, script: String, scriptText: String)
object ScriptAssetInfo {
  implicit val scriptAssetInfoFormat: Format[ScriptAssetInfo] = Json.format
}

case class AssetInfo(
    assetId: String,
    issueHeight: Int,
    issueTimestamp: Long,
    issuer: String,
    name: String,
    description: String,
    decimals: Int,
    reissuable: Boolean,
    quantity: Long,
    originTransactionId: String,
    minSponsoredAssetFee: Option[Long],
    scriptDetails: Option[ScriptAssetInfo]
) {
  def isNFT: Boolean = decimals == 0 && quantity == 1 && !reissuable
}
object AssetInfo {
  implicit val AssetInfoFormat: Format[AssetInfo] = Json.format
}

//noinspection ScalaStyle
class Transaction(
    val _type: Int,
    val id: String,
    val chainId: Option[Byte],
    val fee: Long,
    val timestamp: Long,
    val sender: Option[String],
    val version: Option[Byte],
    val name: Option[String],
    val amount: Option[Long],
    val description: Option[String],
    val attachment: Option[String],
    val price: Option[Long],
    val sellMatcherFee: Option[Long],
    val buyMatcherFee: Option[Long],
    val sellOrderMatcherFee: Option[Long],
    val buyOrderMatcherFee: Option[Long],
    val data: Option[Seq[DataEntry[_]]],
    val minSponsoredAssetFee: Option[Long],
    val transfers: Option[Seq[Transfer]],
    val totalAmount: Option[Long],
    val senderPublicKey: Option[String],
    val recipient: Option[String],
    val proofs: Option[Seq[String]],
    val applicationStatus: Option[String],
    val feeAssetId: Option[String],
    val expression: Option[String]
) {
  import Transaction._
  override def toString: String = Json.toJson(this).toString
  override def equals(x: Any): Boolean = {
    x match {
      case t: Transaction => id == t.id
      case _              => false
    }
  }
}
//noinspection ScalaStyle
object Transaction {
  def apply(
      _type: Int,
      id: String,
      chainId: Option[Byte],
      fee: Long,
      timestamp: Long,
      sender: Option[String],
      version: Option[Byte],
      name: Option[String],
      amount: Option[Long],
      description: Option[String],
      attachment: Option[String],
      price: Option[Long],
      sellMatcherFee: Option[Long],
      buyMatcherFee: Option[Long],
      sellOrderMatcherFee: Option[Long],
      buyOrderMatcherFee: Option[Long],
      data: Option[Seq[DataEntry[_]]],
      minSponsoredAssetFee: Option[Long],
      transfers: Option[Seq[Transfer]],
      totalAmount: Option[Long],
      senderPublicKey: Option[String],
      recipient: Option[String],
      proofs: Option[Seq[String]],
      applicationStatus: Option[String],
      feeAssetId: Option[String],
      expression: Option[String]
  ): Transaction = new Transaction(
    _type,
    id,
    chainId,
    fee,
    timestamp,
    sender,
    version,
    name,
    amount,
    description,
    attachment,
    price,
    sellMatcherFee,
    buyMatcherFee,
    sellOrderMatcherFee,
    buyOrderMatcherFee,
    data,
    minSponsoredAssetFee,
    transfers,
    totalAmount,
    senderPublicKey,
    recipient,
    proofs,
    applicationStatus,
    feeAssetId,
    expression
  )

  implicit val transactionFormat: Format[Transaction] = Format(
    Reads(jsv =>
      for {
        _type       <- (jsv \ "type").validate[Int]
        id          <- (jsv \ "id").validate[String]
        chainId     <- (jsv \ "chainId").validateOpt[Byte]
        fee         <- (jsv \ "fee").validate[Long]
        timestamp   <- (jsv \ "timestamp").validate[Long]
        sender      <- (jsv \ "sender").validateOpt[String]
        version     <- (jsv \ "version").validateOpt[Byte]
        name        <- (jsv \ "name").validateOpt[String]
        amount      <- (jsv \ "amount").validateOpt[Long]
        description <- (jsv \ "description").validateOpt[String]
        attachment <- version match {
          case Some(v) if _type == 4 || _type == 11 => (jsv \ "attachment").validateOpt[String]
          case _                                    => JsSuccess(None)
        }
        price                <- (jsv \ "price").validateOpt[Long]
        sellMatcherFee       <- (jsv \ "sellMatcherFee").validateOpt[Long]
        buyMatcherFee        <- (jsv \ "buyMatcherFee").validateOpt[Long]
        sellOrderMatcherFee  <- (jsv \ "order2" \ "matcherFee").validateOpt[Long]
        buyOrderMatcherFee   <- (jsv \ "order1" \ "matcherFee").validateOpt[Long]
        data                 <- (jsv \ "data").validateOpt[Seq[DataEntry[_]]]
        minSponsoredAssetFee <- (jsv \ "minSponsoredAssetFee").validateOpt[Long]
        transfers            <- (jsv \ "transfers").validateOpt[Seq[Transfer]]
        totalAmount          <- (jsv \ "totalAmount").validateOpt[Long]
        senderPublicKey      <- (jsv \ "senderPublicKey").validateOpt[String]
        recipient            <- (jsv \ "recipient").validateOpt[String]
        proofs               <- (jsv \ "proofs").validateOpt[Seq[String]]
        applicationStatus    <- (jsv \ "applicationStatus").validateOpt[String]
        feeAssetId           <- (jsv \ "feeAssetId").validateOpt[String]
        expression           <- (jsv \ "expression").validateOpt[String]
      } yield new Transaction(
        _type,
        id,
        chainId,
        fee,
        timestamp,
        sender,
        version,
        name,
        amount,
        description,
        attachment,
        price,
        sellMatcherFee,
        buyMatcherFee,
        sellOrderMatcherFee,
        buyOrderMatcherFee,
        data,
        minSponsoredAssetFee,
        transfers,
        totalAmount,
        senderPublicKey,
        recipient,
        proofs,
        applicationStatus,
        feeAssetId,
        expression
      )
    ),
    Writes { t =>
      Json.obj(
        "type"                 -> t._type,
        "id"                   -> t.id,
        "chainId"              -> t.chainId,
        "fee"                  -> t.fee,
        "timestamp"            -> t.timestamp,
        "sender"               -> t.sender,
        "version"              -> t.version,
        "name"                 -> t.name,
        "amount"               -> t.amount,
        "description"          -> t.description,
        "attachment"           -> t.attachment,
        "price"                -> t.price,
        "sellMatcherFee"       -> t.sellMatcherFee,
        "buyMatcherFee"        -> t.buyMatcherFee,
        "sellOrderFee"         -> t.sellOrderMatcherFee,
        "buyOrderFee"          -> t.buyOrderMatcherFee,
        "data"                 -> t.data,
        "minSponsoredAssetFee" -> t.minSponsoredAssetFee,
        "transfers"            -> t.transfers,
        "totalAmount"          -> t.totalAmount,
        "senderPublicKey"      -> t.senderPublicKey,
        "recipient"            -> t.recipient,
        "proofs"               -> t.proofs
      )
    }
  )
}

trait TxInfo {
  def _type: Int
  def id: String
  def fee: Long
  def timestamp: Long
  def sender: Option[String]
  def height: Int
  def minSponsoredAssetFee: Option[Long]
  def recipient: Option[String]
  def script: Option[String]
}

case class TransactionInfo(
    _type: Int,
    id: String,
    chainId: Option[Byte],
    fee: Long,
    timestamp: Long,
    sender: Option[String],
    name: Option[String],
    description: Option[String],
    amount: Option[Long],
    price: Option[Long],
    sellMatcherFee: Option[Long],
    buyMatcherFee: Option[Long],
    sellOrderMatcherFee: Option[Long],
    buyOrderMatcherFee: Option[Long],
    height: Int,
    minSponsoredAssetFee: Option[Long],
    recipient: Option[String],
    script: Option[String],
    version: Option[Byte],
    data: Option[Seq[DataEntry[_]]],
    transfers: Option[Seq[Transfer]],
    totalAmount: Option[Long],
    expression: Option[String],
    stateChanges: Option[StateChangesDetails]
) extends TxInfo
object TransactionInfo {
  implicit val transactionReads: Reads[TransactionInfo] =
    Reads(jsv =>
      for {
        _type                <- (jsv \ "type").validate[Int]
        id                   <- (jsv \ "id").validate[String]
        fee                  <- (jsv \ "fee").validate[Long]
        timestamp            <- (jsv \ "timestamp").validate[Long]
        sender               <- (jsv \ "sender").validateOpt[String]
        height               <- (jsv \ "height").validate[Int]
        minSponsoredAssetFee <- (jsv \ "minSponsoredAssetFee").validateOpt[Long]
        name                 <- (jsv \ "name").validateOpt[String]
        version              <- (jsv \ "version").validateOpt[Byte]
        amount               <- (jsv \ "amount").validateOpt[Long].orElse((jsv \ "quantity").validateOpt[Long])
        description          <- (jsv \ "description").validateOpt[String]
        recipient            <- (jsv \ "recipient").validateOpt[String]
        script               <- (jsv \ "script").validateOpt[String]
        chainId              <- (jsv \ "chainId").validateOpt[Byte]
        price                <- (jsv \ "price").validateOpt[Long]
        sellMatcherFee       <- (jsv \ "sellMatcherFee").validateOpt[Long]
        buyMatcherFee        <- (jsv \ "buyMatcherFee").validateOpt[Long]
        sellOrderMatcherFee  <- (jsv \ "order2" \ "matcherFee").validateOpt[Long]
        buyOrderMatcherFee   <- (jsv \ "order1" \ "matcherFee").validateOpt[Long]
        data                 <- (jsv \ "data").validateOpt[Seq[DataEntry[_]]]
        transfers            <- (jsv \ "transfers").validateOpt[Seq[Transfer]]
        totalAmount          <- (jsv \ "totalAmount").validateOpt[Long]
        expression           <- (jsv \ "expression").validateOpt[String]
        stateChanges         <- (jsv \ "stateChanges").validateOpt[StateChangesDetails]
      } yield TransactionInfo(
        _type,
        id,
        chainId,
        fee,
        timestamp,
        sender,
        name,
        description,
        amount,
        price,
        sellMatcherFee,
        buyMatcherFee,
        sellOrderMatcherFee,
        buyOrderMatcherFee,
        height,
        minSponsoredAssetFee,
        recipient,
        script,
        version,
        data,
        transfers,
        totalAmount,
        expression,
        stateChanges
      )
    )
}

case class TransactionStatus(
    id: String,
    status: String,
    confirmations: Option[Int],
    height: Option[Int],
    applicationStatus: Option[String]
)
object TransactionStatus {
  implicit val format: Format[TransactionStatus] = Json.format
}

case class OrderInfo(
    id: String,
    version: Option[Byte],
    sender: String,
    senderPublicKey: String,
    matcherPublicKey: String,
    assetPair: AssetPairResponse,
    orderType: String,
    amount: Long,
    price: Long,
    timestamp: Long,
    expiration: Long,
    matcherFee: Long,
    signature: String,
    proofs: Option[Seq[String]]
)
object OrderInfo {
  implicit val transactionFormat: Format[OrderInfo] = Json.format
}

case class AssetPairResponse(amountAsset: Option[String], priceAsset: Option[String])
object AssetPairResponse {
  implicit val pairResponseFormat: Format[AssetPairResponse] = Json.format
}

case class Call(function: String, args: Seq[JsObject])
object Call {
  implicit val callFormat: Reads[Call] = Json.reads[Call]
}

case class AttachedPayment(asset: Option[String], amount: Long)
object AttachedPayment {
  implicit val attachedPaymentFormat: Reads[AttachedPayment] = Json.reads[AttachedPayment]
}

case class Invocation(dApp: String, call: Call, payments: Seq[AttachedPayment], stateChanges: StateChangesDetails)
object Invocation {
  implicit val invokationFormat: Reads[Invocation] = new Reads[Invocation] {
    override def reads(json: JsValue): JsResult[Invocation] =
      JsSuccess(
        Invocation(
          (json \ "dApp").as[String],
          (json \ "call").as[Call],
          (json \ "payment").as[Seq[AttachedPayment]],
          (json \ "stateChanges").as[StateChangesDetails]
        )
      )
  }
}

case class StateChangesDetails(
    data: Seq[DataResponse],
    transfers: Seq[TransfersInfoResponse],
    issues: Seq[IssueInfoResponse],
    reissues: Seq[ReissueInfoResponse],
    burns: Seq[BurnInfoResponse],
    sponsorFees: Seq[SponsorFeeResponse],
    error: Option[ErrorMessageInfoResponse],
    invokes: Seq[Invocation] = Nil
)
object StateChangesDetails {
  implicit val stateChangeResponseFormat: Reads[StateChangesDetails] = Json.reads[StateChangesDetails]
}

case class StateChanges(
    _type: Int,
    id: String,
    fee: Long,
    timestamp: Long,
    sender: Option[String],
    height: Int,
    minSponsoredAssetFee: Option[Long],
    recipient: Option[String],
    script: Option[String],
    stateChanges: Option[StateChangesDetails],
    applicationStatus: Option[String]
) extends TxInfo
object StateChanges {
  implicit val stateChanges: Reads[StateChanges] =
    Reads(jsv =>
      for {
        _type                <- (jsv \ "type").validate[Int]
        id                   <- (jsv \ "id").validate[String]
        fee                  <- (jsv \ "fee").validate[Long]
        timestamp            <- (jsv \ "timestamp").validate[Long]
        sender               <- (jsv \ "sender").validateOpt[String]
        height               <- (jsv \ "height").validate[Int]
        minSponsoredAssetFee <- (jsv \ "minSponsoredAssetFee").validateOpt[Long]
        recipient            <- (jsv \ "recipient").validateOpt[String]
        script               <- (jsv \ "script").validateOpt[String]
        stateChanges         <- (jsv \ "stateChanges").validateOpt[StateChangesDetails]
        applicationStatus    <- (jsv \ "applicationStatus").validateOpt[String]
      } yield StateChanges(
        _type,
        id,
        fee,
        timestamp,
        sender,
        height,
        minSponsoredAssetFee,
        recipient,
        script,
        stateChanges,
        applicationStatus
      )
    )
}

case class IssueTransactionInfo(
    `type`: Int,
    id: String,
    chainId: Option[Byte],
    senderPublicKey: String,
    quantity: Long,
    fee: Long,
    description: String,
    version: Byte,
    sender: String,
    feeAssetId: Option[String],
    reissuable: Boolean,
    assetId: String,
    decimals: Byte,
    name: String,
    timestamp: Long
)
object IssueTransactionInfo {
  implicit val issueTransactionFormat: Format[IssueTransactionInfo] = Json.format
}

case class TransferTransactionInfo(
    _type: Int,
    id: String,
    chainId: Option[Byte],
    fee: Long,
    timestamp: Long,
    sender: Option[String],
    amount: Option[Long],
    height: Int,
    recipient: Option[String],
    version: Option[Byte],
    attachment: Option[String],
    proofs: Option[Seq[String]]
)
object TransferTransactionInfo {
  implicit val transactionFormat: Format[TransferTransactionInfo] = Format(
    Reads(jsv =>
      for {
        _type     <- (jsv \ "type").validate[Int]
        id        <- (jsv \ "id").validate[String]
        chainId   <- (jsv \ "chainId").validateOpt[Byte]
        fee       <- (jsv \ "fee").validate[Long]
        timestamp <- (jsv \ "timestamp").validate[Long]
        sender    <- (jsv \ "sender").validateOpt[String]
        height    <- (jsv \ "height").validate[Int]
        amount    <- (jsv \ "amount").validateOpt[Long]
        recipient <- (jsv \ "recipient").validateOpt[String]
        version   <- (jsv \ "version").validateOpt[Byte]
        chainId   <- (jsv \ "chainId").validateOpt[Byte]
        attachment <- version match {
          case Some(v) if _type == 4 || _type == 11 => (jsv \ "attachment").validateOpt[String]
          case _                                    => JsSuccess(None)
        }
        proofs <- (jsv \ "proofs").validateOpt[Seq[String]]
      } yield TransferTransactionInfo(
        _type,
        id,
        chainId,
        fee,
        timestamp,
        sender,
        amount,
        height,
        recipient,
        version,
        attachment,
        proofs
      )
    ),
    Json.writes[TransferTransactionInfo]
  )
}

case class MassTransferTransactionInfo(
    _type: Int,
    id: String,
    chainId: Option[Byte],
    fee: Long,
    timestamp: Long,
    sender: Option[String],
    amount: Option[Long],
    height: Int,
    recipient: Option[String],
    version: Option[Byte],
    attachment: Option[String],
    transfers: Option[Seq[Transfer]],
    totalAmount: Option[Long]
)
object MassTransferTransactionInfo {
  implicit val transactionFormat: Format[MassTransferTransactionInfo] = Format(
    Reads(jsv =>
      for {
        _type     <- (jsv \ "type").validate[Int]
        id        <- (jsv \ "id").validate[String]
        fee       <- (jsv \ "fee").validate[Long]
        timestamp <- (jsv \ "timestamp").validate[Long]
        sender    <- (jsv \ "sender").validateOpt[String]
        height    <- (jsv \ "height").validate[Int]
        amount    <- (jsv \ "amount").validateOpt[Long]
        recipient <- (jsv \ "recipient").validateOpt[String]
        version   <- (jsv \ "version").validateOpt[Byte]
        chainId   <- (jsv \ "chainId").validateOpt[Byte]
        attachment <- version match {
          case Some(v) if _type == 4 || _type == 11 => (jsv \ "attachment").validateOpt[String]
          case _                                    => JsSuccess(None)
        }
        transfers   <- (jsv \ "transfers").validateOpt[Seq[Transfer]]
        totalAmount <- (jsv \ "totalAmount").validateOpt[Long]
      } yield MassTransferTransactionInfo(
        _type,
        id,
        chainId,
        fee,
        timestamp,
        sender,
        amount,
        height,
        recipient,
        version,
        attachment,
        transfers,
        totalAmount
      )
    ),
    Json.writes[MassTransferTransactionInfo]
  )
}

case class BurnTransactionInfo(
    _type: Int,
    id: String,
    chainId: Option[Byte],
    fee: Long,
    timestamp: Long,
    sender: String,
    senderPublicKey: String,
    amount: Long,
    height: Int,
    assetId: String,
    feeAssetId: Option[String],
    version: Option[Byte]
)
object BurnTransactionInfo {
  implicit val transactionFormat: Format[BurnTransactionInfo] = Format(
    Reads(jsv =>
      for {
        _type           <- (jsv \ "type").validate[Int]
        id              <- (jsv \ "id").validate[String]
        fee             <- (jsv \ "fee").validate[Long]
        timestamp       <- (jsv \ "timestamp").validate[Long]
        sender          <- (jsv \ "sender").validate[String]
        senderPublicKey <- (jsv \ "senderPublicKey").validate[String]
        height          <- (jsv \ "height").validate[Int]
        version         <- (jsv \ "version").validateOpt[Byte]
        amount          <- (jsv \ "amount").validate[Long]
        assetId         <- (jsv \ "assetId").validate[String]
        feeAssetId      <- (jsv \ "feeAssetId").validateOpt[String]
        chainId         <- (jsv \ "chainId").validateOpt[Byte]
      } yield BurnTransactionInfo(
        _type,
        id,
        chainId,
        fee,
        timestamp,
        sender,
        senderPublicKey,
        amount,
        height,
        assetId,
        feeAssetId,
        version
      )
    ),
    Json.writes[BurnTransactionInfo]
  )
}
sealed trait DataResponse                                           extends Product with Serializable
case class PutDataResponse(`type`: String, value: Any, key: String) extends DataResponse
case class DeleteDataResponse(key: String)                          extends DataResponse
object DataResponse {
  def put(`type`: String, value: Any, key: String): PutDataResponse = PutDataResponse(`type`, value, key)
  def delete(key: String): DeleteDataResponse                       = DeleteDataResponse(key)
  implicit val dataResponseFormat: Reads[DataResponse] = Reads {
    case JsObject(fields) if fields.get("key").exists(_.isInstanceOf[JsString]) && fields.get("type").exists(_.isInstanceOf[JsString]) =>
      val key    = fields("key").asInstanceOf[JsString].value
      val `type` = fields("type").asInstanceOf[JsString].value
      val value = `type` match {
        case "binary"  => fields("value").asInstanceOf[JsString].value
        case "string"  => fields("value").asInstanceOf[JsString].value
        case "integer" => fields("value").asInstanceOf[JsNumber].value.toLongExact
        case "boolean" => fields("value").asInstanceOf[JsBoolean].value
        case _         => JsError()
      }
      JsSuccess(PutDataResponse(`type`, value, key))
    case JsObject(fields) if fields.get("key").exists(_.isInstanceOf[JsString]) && fields.get("type").forall(_ == JsNull) =>
      val key = fields("key").asInstanceOf[JsString].value
      JsSuccess(DeleteDataResponse(key))
    case _ => JsError()
  }
}

case class TransfersInfoResponse(address: String, asset: Option[String], amount: Long)
object TransfersInfoResponse {
  implicit val assetIdReads: Reads[Option[String]] = Reads {
    case JsString(str) => JsSuccess(Some(str))
    case JsNull        => JsSuccess(None)
    case _             => JsError("Unexpected value")
  }

  implicit val transfersInfoResponseFormat: Format[TransfersInfoResponse] = Json.format
}

case class IssueInfoResponse(
    assetId: String,
    name: String,
    description: String,
    quantity: Long,
    decimals: Int,
    isReissuable: Boolean,
    compiledScript: Option[String],
    nonce: Long
)
object IssueInfoResponse {
  implicit val IssueInfoFormat: Format[IssueInfoResponse] = Json.format
}

case class ReissueInfoResponse(assetId: String, isReissuable: Boolean, quantity: Long)
object ReissueInfoResponse {
  implicit val reissueInfoFormat: Format[ReissueInfoResponse] = Json.format
}

case class BurnInfoResponse(assetId: String, quantity: Long)
object BurnInfoResponse {
  implicit val burnInfoFormat: Format[BurnInfoResponse] = Json.format
}

case class SponsorFeeResponse(assetId: String, minSponsoredAssetFee: Option[Long])
object SponsorFeeResponse {
  implicit val sponsorFeeFormat: Format[SponsorFeeResponse] = Json.format
}

case class ErrorMessageInfoResponse(code: Int, text: String)
object ErrorMessageInfoResponse {
  implicit val errorMessageInfoFormat: Format[ErrorMessageInfoResponse] = Json.format
}

case class ExchangeTransaction(
    `type`: Int,
    version: Option[Byte],
    id: String,
    sender: String,
    senderPublicKey: String,
    fee: Long,
    timestamp: Long,
    proofs: Option[Seq[String]],
    order1: OrderInfo,
    order2: OrderInfo,
    amount: Long,
    price: Long,
    buyMatcherFee: Long,
    sellMatcherFee: Long,
    height: Option[Int]
)
object ExchangeTransaction {
  implicit val transactionFormat: Format[ExchangeTransaction] = Json.format
}

case class Block(
    id: String,
    signature: String,
    reference: String,
    height: Int,
    timestamp: Long,
    generator: String,
    generatorPublicKey: PublicKey,
    transactionCount: Int,
    generationSignature: Option[String],
    transactionsRoot: Option[String],
    baseTarget: Option[Int],
    blocksize: Int,
    transactions: Seq[Transaction],
    fee: Long,
    totalFee: Option[Long],
    features: Option[Set[Short]],
    reward: Option[Long],
    desiredReward: Option[Long],
    vrf: Option[String],
    version: Option[Byte] = None
)
object Block {
  import PublicKey._

  implicit val blockFormat: Format[Block] = Format(
    Reads(jsv =>
      for {
        id                  <- (jsv \ "id").validate[String]
        signature           <- (jsv \ "signature").validate[String]
        reference           <- (jsv \ "reference").validate[String]
        height              <- (jsv \ "height").validate[Int]
        timestamp           <- (jsv \ "timestamp").validate[Long]
        generator           <- (jsv \ "generator").validate[String]
        generatorPublicKey  <- (jsv \ "generatorPublicKey").validate[PublicKey]
        transactionCount    <- (jsv \ "transactionCount").validate[Int]
        blocksize           <- (jsv \ "blocksize").validate[Int]
        features            <- (jsv \ "features").validateOpt[Set[Short]]
        reward              <- (jsv \ "reward").validateOpt[Long]
        desiredReward       <- (jsv \ "desiredReward").validateOpt[Long]
        totalFee            <- (jsv \ "totalFee").validateOpt[Long]
        fee                 <- (jsv \ "fee").validate[Long]
        transactions        <- (jsv \ "transactions").validate[Seq[Transaction]]
        version             <- (jsv \ "version").validateOpt[Byte]
        generationSignature <- (jsv \ "nxt-consensus" \ "generation-signature").validateOpt[String]
        baseTarget          <- (jsv \ "nxt-consensus" \ "base-target").validateOpt[Int]
        transactionsRoot    <- (jsv \ "transactionsRoot").validateOpt[String]
        vrf                 <- (jsv \ "VRF").validateOpt[String]
      } yield Block(
        id,
        signature,
        reference,
        height,
        timestamp,
        generator,
        generatorPublicKey,
        transactionCount,
        generationSignature,
        transactionsRoot,
        baseTarget,
        blocksize,
        transactions,
        fee,
        totalFee,
        features,
        reward,
        desiredReward,
        vrf,
        version
      )
    ),
    Json.writes[Block]
  )
}

case class BlockHeader(
    id: String,
    signature: String,
    height: Int,
    timestamp: Long,
    generator: String,
    transactionCount: Int,
    generationSignature: Option[String],
    transactionsRoot: Option[String],
    baseTarget: Option[Int],
    blocksize: Int,
    features: Option[Set[Short]],
    reward: Option[Long],
    desiredReward: Option[Long],
    totalFee: Long,
    vrf: Option[String],
    version: Option[Byte] = None
)
object BlockHeader {
  implicit val blockHeadersFormat: Format[BlockHeader] = Format(
    Reads(jsv =>
      for {
        id                  <- (jsv \ "id").validate[String]
        signature           <- (jsv \ "signature").validate[String]
        height              <- (jsv \ "height").validate[Int]
        timestamp           <- (jsv \ "timestamp").validate[Long]
        generator           <- (jsv \ "generator").validate[String]
        transactionCount    <- (jsv \ "transactionCount").validate[Int]
        blocksize           <- (jsv \ "blocksize").validate[Int]
        features            <- (jsv \ "features").validateOpt[Set[Short]]
        reward              <- (jsv \ "reward").validateOpt[Long]
        desiredReward       <- (jsv \ "desiredReward").validateOpt[Long]
        totalFee            <- (jsv \ "totalFee").validate[Long]
        version             <- (jsv \ "version").validateOpt[Byte]
        generationSignature <- (jsv \ "nxt-consensus" \ "generation-signature").validateOpt[String]
        baseTarget          <- (jsv \ "nxt-consensus" \ "base-target").validateOpt[Int]
        transactionsRoot    <- (jsv \ "transactionsRoot").validateOpt[String]
        vrf                 <- (jsv \ "VRF").validateOpt[String]
      } yield BlockHeader(
        id,
        signature,
        height,
        timestamp,
        generator,
        transactionCount,
        generationSignature,
        transactionsRoot,
        baseTarget,
        blocksize,
        features,
        reward,
        desiredReward,
        totalFee,
        vrf,
        version
      )
    ),
    Json.writes[BlockHeader]
  )
}

case class GenerationSignatureResponse(generationSignature: String)
object GenerationSignatureResponse {
  implicit val generationSignatureResponseFormat: Format[GenerationSignatureResponse] = Json.format
}

case class MatcherMessage(id: String)
object MatcherMessage {
  implicit val matcherMessageFormat: Format[MatcherMessage] = Json.format
}

case class MatcherResponse(status: String, message: MatcherMessage)
object MatcherResponse {
  implicit val matcherResponseFormat: Format[MatcherResponse] = Json.format
}

case class RatesResponse(message: String)
object RatesResponse {
  implicit val format: Format[RatesResponse] = Json.format
}

case class MatcherErrorResponse(status: Option[String], message: Option[String])
object MatcherErrorResponse {
  implicit val matcherErrorResponseFormat: Format[MatcherErrorResponse] = Json.format
}

case class MarketDataInfo(matcherPublicKey: String, markets: Seq[MarketData])
object MarketDataInfo {
  implicit val marketDataInfoResponseFormat: Format[MarketDataInfo] = Json.format
}

case class AssetDecimalsInfo(decimals: Byte)
object AssetDecimalsInfo {
  implicit val assetDecimalsInfoResponseFormat: Format[AssetDecimalsInfo] = Json.format
}

case class MarketData(
    amountAsset: String,
    amountAssetName: String,
    priceAsset: String,
    priceAssetName: String,
    created: Long,
    amountAssetInfo: Option[AssetDecimalsInfo],
    priceAssetInfo: Option[AssetDecimalsInfo]
)
object MarketData {
  implicit val marketData: Format[MarketData] = Json.format
}

case class MatcherStatusResponse(status: String, filledAmount: Option[Long])
object MatcherStatusResponse {
  implicit val matcherStatusResponseFormat: Format[MatcherStatusResponse] = Json.format
}

case class MessageMatcherResponse(message: String)
object MessageMatcherResponse {
  implicit val messageMatcherResponseFormat: Format[MessageMatcherResponse] = Json.format
}

case class OrderbookHistory(
    id: String,
    `type`: String,
    amount: Long,
    price: Long,
    timestamp: Long,
    filled: Int,
    status: String,
    assetPair: AssetPair
) {
  def isActive: Boolean = status == "PartiallyFilled" || status == "Accepted"
}
object OrderbookHistory {
  implicit val byteStrFormat: Format[ByteStr] = Format(
    Reads {
      case JsString(str) =>
        ByteStr.decodeBase58(str) match {
          case Success(x) => JsSuccess(x)
          case Failure(e) => JsError(e.getMessage)
        }

      case _ => JsError("Can't read ByteStr")
    },
    Writes(x => JsString(x.toString))
  )

  implicit val assetPairFormat: Format[AssetPair] = Json.format[AssetPair]

  implicit val orderbookHistory: Format[OrderbookHistory] = Json.format
}

case class PairResponse(amountAsset: String, priceAsset: String)
object PairResponse {
  implicit val pairResponseFormat: Format[PairResponse] = Json.format
}

case class LevelResponse(amount: Long, price: Long)
object LevelResponse {
  implicit val levelResponseFormat: Format[LevelResponse] = Json.format
}

case class OrderBookResponse(timestamp: Long, pair: PairResponse, bids: List[LevelResponse], asks: List[LevelResponse])
object OrderBookResponse {
  implicit val orderBookResponseFormat: Format[OrderBookResponse] = Json.format
}

case class MerkleProofResponse(id: String, transactionIndex: Int, merkleProof: Seq[String])
object MerkleProofResponse {
  implicit val merkleProofResponseFormat: Format[MerkleProofResponse] = Json.format
}

case class MarketStatusResponse(
    lastPrice: Option[Long],
    lastSide: Option[String],
    bid: Option[Long],
    bidAmount: Option[Long],
    ask: Option[Long],
    askAmount: Option[Long]
)
object MarketStatusResponse {
  implicit val marketResponseFormat: Format[MarketStatusResponse] = Json.format
}

case class BlacklistedPeer(hostname: String, timestamp: Long, reason: String)
object BlacklistedPeer {
  implicit val blacklistedPeerFormat: Format[BlacklistedPeer] = Json.format
}

case class State(address: String, miningBalance: Long, timestamp: Long)
object State {
  implicit val StateFormat: Format[State] = Json.format
}

case class GeneratingBalance(address: String, balance: Long)
object GeneratingBalance {
  implicit val GeneratingBalanceFormat: Format[GeneratingBalance] = Json.format
}

case class BalanceHistory(height: Int, balance: Long)
object BalanceHistory {
  implicit val BalanceHistoryFormat: Format[BalanceHistory] = Json.format
}

case class TransactionSerialize(bytes: Array[Int])
object TransactionSerialize {
  implicit val TransactionSerializeFormat: Format[TransactionSerialize] = Json.format
}

case class FeeInfo(feeAssetId: Option[String], feeAmount: Long)
object FeeInfo {
  implicit val format: Format[FeeInfo] = Json.format
}

// Obsolete payment request
case class PaymentRequest(amount: Long, fee: Long, sender: String, recipient: String)
object PaymentRequest {
  implicit val paymentFormat: Format[PaymentRequest] = Json.format
}

case class LeaseInfo(
    id: String,
    originTransactionId: String,
    sender: String,
    recipient: String,
    amount: Long,
    height: Int
)
object LeaseInfo {
  implicit val format: Format[LeaseInfo] = Json.format
}
