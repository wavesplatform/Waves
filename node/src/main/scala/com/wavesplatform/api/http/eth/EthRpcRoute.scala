package com.wavesplatform.api.http.eth

import java.math.BigInteger

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters._

import akka.http.scaladsl.server._
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.http._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.{ERC20Address, EthereumTransaction}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.utils.EthEncoding
import org.web3j.abi._
import org.web3j.abi.datatypes.generated.{Uint256, Uint8}
import org.web3j.crypto._
import play.api.libs.json._
import play.api.libs.json.Json.JsValueWrapper

class EthRpcRoute(blockchain: Blockchain, transactionsApi: CommonTransactionsApi) extends ApiRoute {
  private def quantity(v: Long) = s"0x${java.lang.Long.toString(v, 16)}"

  private def resp(id: JsValue, resp: JsValueWrapper) = complete(Json.obj("id" -> id, "jsonrpc" -> "2.0", "result" -> resp))

  private def resp(id: JsValue, resp: Future[JsValueWrapper]) = complete(resp.map(r => Json.obj("id" -> id, "jsonrpc" -> "2.0", "result" -> r)))

  private def assetDescription(contractAddress: String) =
    assetId(contractAddress).flatMap(blockchain.assetDescription)

  private def assetId(contractAddress: String): Option[IssuedAsset] =
    blockchain.resolveERC20Address(ERC20Address(ByteStr(EthEncoding.toBytes(contractAddress))))

  private def encodeResponse(values: Type*): String = FunctionEncoder.encodeConstructor(values.map(Type.unwrap).asJava)

  private def extractTransaction(transactionHex: String) = TransactionDecoder.decode(transactionHex) match {
    case srt: SignedRawTransaction => EthereumTransaction(srt)
    case _: RawTransaction         => throw new UnsupportedOperationException("Cannot process unsigned transactions")
  }

  val route: Route = (post & path("eth") & entity(as[JsObject])) { jso =>
    val id     = (jso \ "id").get
    val params = (jso \ "params").asOpt[IndexedSeq[JsValue]]
    (jso \ "method").as[String] match {
      case "eth_chainId" =>
        resp(id, quantity(AddressScheme.current.chainId.toInt))
      case "eth_blockNumber" =>
        resp(id, quantity(blockchain.height))
      case "eth_getTransactionCount" =>
        resp(id, quantity(System.currentTimeMillis()))
      case "eth_getBlockByNumber" =>
        resp(
          id,
          Json.obj(
            "number" -> quantity(Integer.parseInt(params.get.head.as[String].drop(2), 16))
          )
        )
      case "eth_getBalance" =>
        resp(
          id,
          EthEncoding.toHexString(
            BigInt(blockchain.balance(Address.fromHexString(params.get.head.as[String]))) * EthereumTransaction.AmountMultiplier
          )
        )
      case "eth_sendRawTransaction" =>
        val str = params.get.head.as[String]
        log.info(str)

        val et = extractTransaction(str)

        resp(
          id,
          transactionsApi.broadcastTransaction(et).map[JsValueWrapper] { result =>
            log.info(s"Validation result from ${EthEncoding.toHexString(et.senderAddress().publicKeyHash)}: $result")
            EthEncoding.toHexString(et.id().arr)
          }
        )
      case "eth_getTransactionReceipt" =>
        val transactionHex = params.get.head.as[String]
        val txId           = ByteStr(EthEncoding.toBytes(transactionHex))
        log.info(s"Get receipt for $transactionHex/$txId")

        resp(
          id,
          blockchain.transactionInfo(txId).fold[JsValue](JsNull) {
            case (height, tx, _) =>
              Json.obj(
                "transactionHash"   -> EthEncoding.toHexString(tx.id().arr),
                "transactionIndex"  -> "0x01",
                "blockHash"         -> EthEncoding.toHexString(blockchain.lastBlockId.get.arr),
                "blockNumber"       -> EthEncoding.toHexString(BigInteger.valueOf(height)),
                "from"              -> EthEncoding.toHexString(new Array[Byte](20)),
                "to"                -> EthEncoding.toHexString(new Array[Byte](20)),
                "cumulativeGasUsed" -> EthEncoding.toHexString(BigInteger.ZERO),
                "gasUsed"           -> EthEncoding.toHexString(BigInteger.ZERO),
                "contractAddress"   -> JsNull,
                "logs"              -> Json.arr(),
                "logsBloom"         -> EthEncoding.toHexString(new Array[Byte](32))
              )
          }
        )
      case "eth_call" =>
        val call            = params.get.head.as[JsObject]
        val dataString      = (call \ "data").as[String]
        val contractAddress = (call \ "to").as[String]

        log.info(s"balance: contract address = $contractAddress, assetId = ${assetId(contractAddress)}")

        EthEncoding.cleanHexPrefix(dataString).take(8) match {
          case "95d89b41" =>
            resp(id, encodeResponse(assetDescription(contractAddress).get.name.toStringUtf8))
          case "313ce567" =>
            resp(id, encodeResponse(new Uint8(assetDescription(contractAddress).get.decimals)))
          case "70a08231" =>
            resp(
              id,
              encodeResponse(
                new Uint256(
                  blockchain.balance(Address.fromHexString(dataString.takeRight(40)), assetId(contractAddress).get)
                )
              )
            )
          case _ =>
            log.info(s"Unexpected call $dataString at $contractAddress")
            resp(id, "")
        }
      case "eth_estimateGas" =>
        val txParams = params.get.head.as[JsObject]
        val tx = RawTransaction.createEtherTransaction(
          BigInteger.valueOf(System.currentTimeMillis()),
          BigInteger.ZERO,
          BigInteger.ZERO,
          (txParams \ "to").as[String],
          new BigInteger(EthEncoding.cleanHexPrefix((txParams \ "value").as[String]), 16)
        )

        val (_, txFee, _) = transactionsApi.calculateFee(EthereumTransaction(tx)).explicitGet()

        resp(id, EthEncoding.toHexString(BigInteger.valueOf(txFee)))
      case "net_version" =>
        resp(id, "1")
      case "eth_gasPrice" =>
        resp(id, "0x1")
      case "eth_getCode" =>
        resp(id, if (blockchain.hasDApp(Address.fromHexString(params.get.head.as[String]))) "0xff" else "0x")
      case _ =>
        log.info(Json.stringify(jso))
        complete(Json.obj())
    }
  }
}
