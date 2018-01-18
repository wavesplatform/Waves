package com.wavesplatform.it.api

import com.typesafe.config.Config
import com.wavesplatform.it.NodeInfo
import com.wavesplatform.settings.WavesSettings
import org.asynchttpclient._
import org.slf4j.LoggerFactory
import play.api.libs.json._
import scorex.transaction.TransactionParser.TransactionType
import scorex.utils.{LoggerFacade, ScorexLogging}

import scala.concurrent.duration._

trait Node {

  val config: Config

  var nodeInfo: NodeInfo

  def settings: WavesSettings

  def privateKey: String

  def publicKey: String

  def address: String

  def accountSeed: String

  def name: String

  def restEndpoint: String
  def matcherRestEndpoint: String

  def networkPort: Int

  def blockDelay: FiniteDuration

  def client: AsyncHttpClient

  def fee(txValue: TransactionType.Value, asset: String = "WAVES"): Long

  lazy val log: LoggerFacade = LoggerFacade(LoggerFactory.getLogger(s"${getClass.getName} $name"))
}

object Node extends ScorexLogging {

  case class UnexpectedStatusCodeException(request: Request, response: Response) extends Exception(s"Request: ${request.getUrl}\n" +
    s"Unexpected status code (${response.getStatusCode}): ${response.getResponseBody}")

  case class Status(blockchainHeight: Int, stateHeight: Int, updatedTimestamp: Long, updatedDate: String)

  implicit val statusFormat: Format[Status] = Json.format

  case class Peer(address: String, declaredAddress: String, peerName: String)

  implicit val peerFormat: Format[Peer] = Json.format

  case class Address(address: String)

  implicit val addressFormat: Format[Address] = Json.format

  case class Balance(address: String, confirmations: Int, balance: Long)

  implicit val balanceFormat: Format[Balance] = Json.format

  case class AssetBalance(address: String, assetId: String, balance: Long)

  implicit val assetBalanceFormat: Format[AssetBalance] = Json.format

  case class FullAssetInfo(assetId: String, balance: Long, reissuable: Boolean, quantity: Long)

  implicit val fullAssetInfoFormat: Format[FullAssetInfo] = Json.format

  case class FullAssetsInfo(address: String, balances: List[FullAssetInfo])

  implicit val fullAssetsInfoFormat: Format[FullAssetsInfo] = Json.format

  case class Transaction(`type`: Int, id: String, fee: Long, timestamp: Long)

  implicit val transactionFormat: Format[Transaction] = Json.format

  case class Block(signature: String, height: Int, timestamp: Long, generator: String, transactions: Seq[Transaction],
                   fee: Long, features: Option[Seq[Short]])

  case class BlockHeaders(signature: String, height: Int, timestamp: Long, generator: String, transactionCount: Int, blocksize: Int)

  implicit val blockHeadersFormat: Format[BlockHeaders] = Json.format

  implicit val blockFormat: Format[Block] = Json.format

  case class MatcherMessage(id: String)

  implicit val matcherMessageFormat: Format[MatcherMessage] = Json.format

  case class MatcherResponse(status: String, message: MatcherMessage)

  implicit val matcherResponseFormat: Format[MatcherResponse] = Json.format

  case class MatcherStatusResponse(status: String)

  implicit val matcherStatusResponseFormat: Format[MatcherStatusResponse] = Json.format

  case class MessageMatcherResponse(message: String)

  implicit val messageMatcherResponseFormat: Format[MessageMatcherResponse] = Json.format

  case class OrderbookHistory(id: String, `type`: String, amount: Long, price: Long, timestamp: Long, filled: Int,
                              status: String)

  implicit val orderbookHistory: Format[OrderbookHistory] = Json.format

  case class PairResponse(amountAsset: String, priceAsset: String)

  implicit val pairResponseFormat: Format[PairResponse] = Json.format

  case class LevelResponse(price: Long, amount: Long)

  implicit val levelResponseFormat: Format[LevelResponse] = Json.format

  case class OrderBookResponse(timestamp: Long, pair: PairResponse, bids: List[LevelResponse], asks: List[LevelResponse])

  implicit val orderBookResponseFormat: Format[OrderBookResponse] = Json.format

  case class DebugInfo(stateHeight: Long, stateHash: Long)

  implicit val debugInfoFormat: Format[DebugInfo] = Json.format


  case class BlacklistedPeer(hostname: String, timestamp: Long, reason: String)

  implicit val blacklistedPeerFormat: Format[BlacklistedPeer] = Json.format

  // Obsolete payment request
  case class PaymentRequest(amount: Long, fee: Long, sender: String, recipient: String)

  object PaymentRequest {
    implicit val paymentFormat: Format[PaymentRequest] = Json.format
  }

}
