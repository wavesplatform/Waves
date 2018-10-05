package com.wavesplatform.it.api

import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.AssetPair
import play.api.libs.json._

import scala.util.{Failure, Success}

// USCE no longer contains references to non-serializable Request/Response objects
// to work around https://github.com/scalatest/scalatest/issues/556
case class UnexpectedStatusCodeException(requestUrl: String, statusCode: Int, responseBody: String)
    extends Exception(s"Request: $requestUrl; Unexpected status code ($statusCode): $responseBody")

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

case class AssetBalance(address: String, assetId: String, balance: Long)
object AssetBalance {
  implicit val assetBalanceFormat: Format[AssetBalance] = Json.format
}

case class CompiledScript(script: String, complexity: Long, extraFee: Long)
object CompiledScript {
  implicit val compiledScriptFormat: Format[CompiledScript] = Json.format
}

case class FullAssetInfo(assetId: String,
                         balance: Long,
                         reissuable: Boolean,
                         minSponsoredAssetFee: Option[Long],
                         sponsorBalance: Option[Long],
                         quantity: Long)
object FullAssetInfo {
  implicit val fullAssetInfoFormat: Format[FullAssetInfo] = Json.format
}

case class FullAssetsInfo(address: String, balances: List[FullAssetInfo])
object FullAssetsInfo {
  implicit val fullAssetsInfoFormat: Format[FullAssetsInfo] = Json.format
}

case class AssetInfo(assetId: String,
                     issueHeight: Int,
                     issueTimestamp: Long,
                     issuer: String,
                     name: String,
                     description: String,
                     decimals: Int,
                     reissuable: Boolean,
                     quantity: Long,
                     minSponsoredAssetFee: Option[Long])
object AssetInfo {
  implicit val AssetInfoFormat: Format[AssetInfo] = Json.format
}

case class Transaction(`type`: Int, id: String, fee: Long, timestamp: Long, sender: Option[String])
object Transaction {
  implicit val transactionFormat: Format[Transaction] = Json.format
}

case class TransactionInfo(`type`: Int,
                           id: String,
                           fee: Long,
                           timestamp: Long,
                           sender: Option[String],
                           height: Int,
                           minSponsoredAssetFee: Option[Long],
                           recipient: Option[String])
object TransactionInfo {
  implicit val format: Format[TransactionInfo] = Json.format
}

case class OrderInfo(id: String,
                     sender: String,
                     senderPublicKey: String,
                     matcherPublicKey: String,
                     assetPair: AssetPairResponse,
                     orderType: String,
                     price: Long,
                     amount: Long,
                     timestamp: Long,
                     expiration: Long,
                     matcherFee: Long,
                     signature: String)
object OrderInfo {
  implicit val transactionFormat: Format[OrderInfo] = Json.format
}

case class AssetPairResponse(amountAsset: Option[String], priceAsset: Option[String])
object AssetPairResponse {
  implicit val pairResponseFormat: Format[AssetPairResponse] = Json.format
}

case class ExchangeTransaction(`type`: Int,
                               id: String,
                               sender: String,
                               senderPublicKey: String,
                               fee: Long,
                               timestamp: Long,
                               signature: String,
                               order1: OrderInfo,
                               order2: OrderInfo,
                               price: Long,
                               amount: Long,
                               buyMatcherFee: Long,
                               sellMatcherFee: Long,
                               height: Option[Int])
object ExchangeTransaction {
  implicit val transactionFormat: Format[ExchangeTransaction] = Json.format
}

case class Block(signature: String,
                 height: Int,
                 timestamp: Long,
                 generator: String,
                 transactions: Seq[Transaction],
                 fee: Long,
                 features: Option[Seq[Short]])
object Block {
  implicit val blockFormat: Format[Block] = Json.format
}

case class BlockHeaders(signature: String,
                        height: Int,
                        timestamp: Long,
                        generator: String,
                        transactionCount: Int,
                        blocksize: Int,
                        features: Option[Set[Short]])
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

case class MarketDataInfo(matcherPublicKey: String, markets: Seq[MarketData])
object MarketDataInfo {
  implicit val marketDataInfoResponseFormat: Format[MarketDataInfo] = Json.format
}

case class AssetDecimalsInfo(decimals: Byte)
object AssetDecimalsInfo {
  implicit val assetDecimalsInfoResponseFormat: Format[AssetDecimalsInfo] = Json.format
}

case class MarketData(amountAssetName: String,
                      priceAssetName: String,
                      created: Long,
                      amountAssetInfo: Option[AssetDecimalsInfo],
                      priceAssetInfo: Option[AssetDecimalsInfo])
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

case class OrderbookHistory(id: String,
                            `type`: String,
                            amount: Long,
                            price: Long,
                            timestamp: Long,
                            filled: Int,
                            status: String,
                            assetPair: AssetPair) {
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

case class LevelResponse(price: Long, amount: Long)
object LevelResponse {
  implicit val levelResponseFormat: Format[LevelResponse] = Json.format
}

case class OrderBookResponse(timestamp: Long, pair: PairResponse, bids: List[LevelResponse], asks: List[LevelResponse])
object OrderBookResponse {
  implicit val orderBookResponseFormat: Format[OrderBookResponse] = Json.format
}

case class MarketStatusResponse(lastPrice: Option[Long],
                                lastSide: Option[String],
                                bid: Option[Long],
                                bidAmount: Option[Long],
                                ask: Option[Long],
                                askAmount: Option[Long])
object MarketStatusResponse {
  implicit val marketResponseFormat: Format[MarketStatusResponse] = Json.format
}

case class DebugInfo(stateHeight: Long, stateHash: Long)
object DebugInfo {
  implicit val debugInfoFormat: Format[DebugInfo] = Json.format
}

case class BlacklistedPeer(hostname: String, timestamp: Long, reason: String)
object BlacklistedPeer {
  implicit val blacklistedPeerFormat: Format[BlacklistedPeer] = Json.format
}

case class State(address: String, miningBalance: Long, timestamp: Long)
object State {
  implicit val StateFormat: Format[State] = Json.format
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
