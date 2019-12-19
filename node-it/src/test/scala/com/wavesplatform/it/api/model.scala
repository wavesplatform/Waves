package com.wavesplatform.it.api

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.AssetPair
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
    quantity: Long
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
    minSponsoredAssetFee: Option[Long],
    scriptDetails: Option[ScriptAssetInfo]
)
object AssetInfo {
  implicit val AssetInfoFormat: Format[AssetInfo] = Json.format
}

case class Transaction(`type`: Int, id: String, fee: Long, timestamp: Long, sender: Option[String])
object Transaction {
  implicit val transactionFormat: Format[Transaction] = Json.format
}

trait TxInfo {
  def `type`: Int
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
    `type`: Int,
    id: String,
    fee: Long,
    timestamp: Long,
    sender: Option[String],
    height: Int,
    minSponsoredAssetFee: Option[Long],
    recipient: Option[String],
    script: Option[String]
) extends TxInfo
object TransactionInfo {
  implicit val format: Format[TransactionInfo] = Json.format
}

case class TransactionStatus(
    id: String,
    status: String,
    confirmations: Option[Int],
    height: Option[Int]
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

case class StateChangesDetails(data: Seq[DataResponse], transfers: Seq[TransfersInfoResponse])
object StateChangesDetails {
  implicit val stateChangeResponseFormat: Format[StateChangesDetails] = Json.format[StateChangesDetails]
}

case class DebugStateChanges(
    `type`: Int,
    id: String,
    fee: Long,
    timestamp: Long,
    sender: Option[String],
    height: Int,
    minSponsoredAssetFee: Option[Long],
    recipient: Option[String],
    script: Option[String],
    stateChanges: Option[StateChangesDetails]
) extends TxInfo
object DebugStateChanges {
  implicit val debugStateChanges: Format[DebugStateChanges] = Json.format
}

case class DataResponse(`type`: String, value: Long, key: String)
object DataResponse {
  implicit val dataResponseFormat: Format[DataResponse] = Json.format
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
    signature: String,
    height: Int,
    timestamp: Long,
    generator: String,
    transactions: Seq[Transaction],
    fee: Long,
    features: Option[Seq[Short]],
    reward: Option[Long],
    desiredReward: Option[Long]
)
object Block {
  implicit val blockFormat: Format[Block] = Json.format
}

case class BlockHeaders(
    signature: String,
    height: Int,
    timestamp: Long,
    generator: String,
    transactionCount: Int,
    blocksize: Int,
    features: Option[Set[Short]],
    reward: Option[Long],
    desiredReward: Option[Long],
    totalFee: Long
)
object BlockHeaders {
  implicit val blockHeadersFormat: Format[BlockHeaders] = Json.format
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
    Writes(x => JsString(x.base58))
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
