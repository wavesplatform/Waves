package com.wavesplatform.it.api

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.transfer.Attachment
import io.grpc.{Metadata, Status => GrpcStatus}
import org.scalatest.Assertions
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
)
object AssetInfo {
  implicit val AssetInfoFormat: Format[AssetInfo] = Json.format
}

case class Transaction (
                        _type: Int,
                        id: String,
                        chainId: Option[Byte],
                        fee: Long,
                        timestamp: Long,
                        sender: Option[String],
                        version: Byte,
                        name: Option[String],
                        amount: Option[Long],
                        description: Option[String],
                        typedAttachment: Option[Attachment],
                        attachment: Option[String]
                      )

object Transaction {
  implicit val transactionFormat: Format[Transaction] = Format( //Json.format
    Reads(jsv =>
      for {
        _type <- (jsv \ "type").validate[Int]
        id <- (jsv \ "id").validate[String]
        fee <- (jsv \ "fee").validate[Long]
        timestamp <- (jsv \ "timestamp").validate[Long]
        sender <- (jsv \ "sender").validateOpt[String]
        version <- (jsv \ "version").validate[Byte]
        chainId <- version match {
          case v if v > 2 => (jsv \ "chainId").validateOpt[Byte]
          case _ => JsSuccess(None)
        }
        name <- (jsv \ "name").validateOpt[String]
        amount <- (jsv \ "amount").validateOpt[Long]
        description <- (jsv \ "description").validateOpt[String]
        typedAttachment <- version match {
          case v if v > 2 && _type == 4 => (jsv \ "attachment").validateOpt[Attachment]
          case v if v > 1 && _type == 11 => (jsv \ "attachment").validateOpt[Attachment]
          case _ => JsSuccess(None)
        }
        attachment <- version match {
          case v if v < 3 && _type == 4 => (jsv \ "attachment").validateOpt[String]
          case v if v < 2 && _type == 11 => (jsv \ "attachment").validateOpt[String]
          case _ => JsSuccess(None)
        }
      }
        yield Transaction(
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
          typedAttachment,
          attachment
        )),
    Json.writes[Transaction]
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
    height: Int,
    minSponsoredAssetFee: Option[Long],
    name: Option[String],
    amount: Option[Long],
    description: Option[String],
    recipient: Option[String],
    script: Option[String],
    version: Byte,
    typedAttachment: Option[Attachment],
    attachment: Option[String]
) extends TxInfo
object TransactionInfo {
  implicit val format: Format[TransactionInfo] = Format(
    Reads(jsv =>
      for {
        _type <- (jsv \ "type").validate[Int]
        id <- (jsv \ "id").validate[String]
        fee <- (jsv \ "fee").validate[Long]
        timestamp <- (jsv \ "timestamp").validate[Long]
        sender <- (jsv \ "sender").validateOpt[String]
        height <- (jsv \ "height").validate[Int]
        minSponsoredAssetFee <- (jsv \ "minSponsoredAssetFee").validateOpt[Long]
        name <- (jsv \ "name").validateOpt[String]
        amount <- (jsv \ "amount").validateOpt[Long]
        description <- (jsv \ "description").validateOpt[String]
        recipient <- (jsv \ "recipient").validateOpt[String]
        script <- (jsv \ "script").validateOpt[String]
        version <- (jsv \ "version").validate[Byte]
        chainId <- version match {
          case v if v > 2 => (jsv \ "chainId").validateOpt[Byte]
          case _ => JsSuccess(None)
        }
        typedAttachment <- version match {
          case v if v > 2 && _type == 4 => (jsv \ "attachment").validateOpt[Attachment]
          case v if v > 1 && _type == 11 => (jsv \ "attachment").validateOpt[Attachment]
          case _ => JsSuccess(None)
        }
        attachment <- version match {
          case v if v < 3 && _type == 4 => (jsv \ "attachment").validateOpt[String]
          case v if v < 2 && _type == 11 => (jsv \ "attachment").validateOpt[String]
          case _ => JsSuccess(None)
        }
      }
        yield TransactionInfo(
          _type,
          id,
          chainId,
          fee,
          timestamp,
          sender,
          height,
          minSponsoredAssetFee,
          name,
          amount,
          description,
          recipient,
          script,
          version,
          typedAttachment,
          attachment
        )),
    Json.writes[TransactionInfo]
  )
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

case class StateChangesDetails(
    data: Seq[DataResponse],
    transfers: Seq[TransfersInfoResponse],
    issues: Seq[IssueInfoResponse],
    reissues: Seq[ReissueInfoResponse],
    burns: Seq[BurnInfoResponse]
)
object StateChangesDetails {
  implicit val stateChangeResponseFormat: Format[StateChangesDetails] = Json.format[StateChangesDetails]
}

case class DebugStateChanges(
    _type: Int,
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

case class IssueInfoResponse(
    assetId: String,
    name: String,
    description: String,
    quantity: Long,
    decimals: Int,
    isReissuable: Boolean,
    compiledScript: Option[String],
    nonce: Int
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
    version: Option[Byte] = None
)
object Block {
  implicit val blockFormat: Format[Block] = Format(
    Reads( jsv =>
      for {
        signature <- (jsv \ "signature").validate[String]
        height <- (jsv \ "height").validate[Int]
        timestamp <- (jsv \ "timestamp").validate[Long]
        generator <- (jsv \ "generator").validate[String]
        transactionCount <- (jsv \ "transactionCount").validate[Int]
        blocksize <- (jsv \ "blocksize").validate[Int]
        features <- (jsv \ "features").validateOpt[Set[Short]]
        reward <- (jsv \ "reward").validateOpt[Long]
        desiredReward <- (jsv \ "desiredReward").validateOpt[Long]
        totalFee <- (jsv \ "totalFee").validateOpt[Long]
        fee <- (jsv \ "fee").validate[Long]
        transactions <- (jsv \ "transactions").validate[Seq[Transaction]]
        version <- (jsv \ "version").validateOpt[Byte]
        generationSignature <- (jsv \ "nxt-consensus" \ "generation-signature").validateOpt[String]
        baseTarget <- (jsv \ "nxt-consensus" \ "base-target").validateOpt[Int]
        transactionsRoot <- (jsv \ "transactionsRoot").validateOpt[String]
      } yield Block(
        signature,
        height,
        timestamp,
        generator,
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
        version
      )
    ),
    Json.writes[Block]
  )
}

case class BlockHeaders(
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
    version: Option[Byte] = None
)
object BlockHeaders {
  implicit val blockHeadersFormat: Format[BlockHeaders] = Format(
    Reads( jsv =>
      for {
        signature <- (jsv \ "signature").validate[String]
        height <- (jsv \ "height").validate[Int]
        timestamp <- (jsv \ "timestamp").validate[Long]
        generator <- (jsv \ "generator").validate[String]
        transactionCount <- (jsv \ "transactionCount").validate[Int]
        blocksize <- (jsv \ "blocksize").validate[Int]
        features <- (jsv \ "features").validateOpt[Set[Short]]
        reward <- (jsv \ "reward").validateOpt[Long]
        desiredReward <- (jsv \ "desiredReward").validateOpt[Long]
        totalFee <- (jsv \ "totalFee").validate[Long]
        version <- (jsv \ "version").validateOpt[Byte]
        generationSignature <- (jsv \ "nxt-consensus" \ "generation-signature").validateOpt[String]
        baseTarget <- (jsv \ "nxt-consensus" \ "base-target").validateOpt[Int]
        transactionsRoot <- (jsv \ "transactionsRoot").validateOpt[String]
      } yield BlockHeaders(
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
        version
      )
    ),
    Json.writes[BlockHeaders]
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
object  MerkleProofResponse {
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
