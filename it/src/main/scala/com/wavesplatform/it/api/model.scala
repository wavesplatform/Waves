package com.wavesplatform.it.api

import play.api.libs.json.{Format, Json}

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

case class CompiledScript(script: String)
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
                     script: Option[String],
                     scriptText: Option[String],
                     complexity: Int,
                     extraFee: Long,
                     minSponsoredAssetFee: Option[Long])
object AssetInfo {
  implicit val AssetInfoFormat: Format[AssetInfo] = Json.format
}

case class Transaction(`type`: Int, id: String, fee: Long, timestamp: Long, sender: Option[String])
object Transaction {
  implicit val transactionFormat: Format[Transaction] = Json.format
}

case class TransactionInfo(`type`: Int, id: String, fee: Long, timestamp: Long, sender: Option[String], height: Int)
object TransactionInfo {
  implicit val format: Format[TransactionInfo] = Json.format
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

case class MatcherStatusResponse(status: String, filledAmount: Option[Long])
object MatcherStatusResponse {
  implicit val matcherStatusResponseFormat: Format[MatcherStatusResponse] = Json.format
}

case class MessageMatcherResponse(message: String)
object MessageMatcherResponse {
  implicit val messageMatcherResponseFormat: Format[MessageMatcherResponse] = Json.format
}

case class OrderbookHistory(id: String, `type`: String, amount: Long, price: Long, timestamp: Long, filled: Int, status: String)
object OrderbookHistory {
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

// Obsolete payment request
case class PaymentRequest(amount: Long, fee: Long, sender: String, recipient: String)
object PaymentRequest {
  implicit val paymentFormat: Format[PaymentRequest] = Json.format
}
