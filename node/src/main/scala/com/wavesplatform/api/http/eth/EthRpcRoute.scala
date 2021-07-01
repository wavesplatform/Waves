package com.wavesplatform.api.http.eth

import java.math.BigInteger

import akka.http.scaladsl.server._
import com.wavesplatform.account.{AddressScheme, EthereumAddress}
import com.wavesplatform.api.http._
import com.wavesplatform.network.TransactionPublisher
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.{EthereumTransaction, Transaction}
import org.web3j.abi.datatypes.generated.Uint256
import org.web3j.abi.{TypeDecoder, TypeEncoder}
import org.web3j.crypto.{RawTransaction, SignedRawTransaction, TransactionDecoder}
import org.web3j.utils.Numeric._
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.reflect.ClassTag

class EthRpcRoute(blockchain: Blockchain, transactionPublisher: TransactionPublisher) extends ApiRoute {
  private def quantity(v: Long) = s"0x${java.lang.Long.toString(v, 16)}"

  private val decodeMethod = {
    val m = classOf[TypeDecoder].getDeclaredMethod("decode", classOf[String], classOf[Int], classOf[Class[_]])
    m.setAccessible(true)
    m
  }

  private def decode[A](source: String, offset: Int)(implicit ct: ClassTag[A]): A =
    decodeMethod.invoke(null, source, offset, ct.runtimeClass.asInstanceOf[Class[A]]).asInstanceOf[A]

  private def resp(id: JsValue, resp: JsValueWrapper) = complete(Json.obj("id" -> id, "jsonrpc" -> "2.0", "result" -> resp))

  private def resp(id: JsValue, resp: Future[JsValueWrapper]) = complete(resp.map(r => Json.obj("id" -> id, "jsonrpc" -> "2.0", "result" -> r)))

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
        val height    = Integer.parseInt(params.get.head.as[String].drop(2), 16)
        val blockMeta = blockchain.blockHeader(height).get
        resp(
          id,
          Json.obj(
            "number" -> quantity(height)
          )
        )
      case "eth_getBalance" =>
        resp(id, toHexStringWithPrefixSafe((BigInt(blockchain.balance(EthereumAddress(params.get.head.as[String]))) * EthereumTransaction.AmountMultiplier).bigInteger))
      case "eth_sendRawTransaction" =>
        val et: Transaction = TransactionDecoder.decode(params.get.head.as[String]) match {
          case srt: SignedRawTransaction => EthereumTransaction(srt)
          case _: RawTransaction         => throw new UnsupportedOperationException("Cannot process unsigned transactions")
        }

        resp(id, transactionPublisher.validateAndBroadcast(et, None).map[JsValueWrapper] { result =>
          log.info(s"Published transaction $et")
          toHexString(et.id().arr)
        })
      case "eth_getTransactionReceipt" =>
        resp(
          id,
          Json.obj(
            "transactionHash"   -> toHexString(new Array[Byte](32)),
            "transactionIndex"  -> "0x01",
            "blockHash"         -> toHexString(blockchain.lastBlockId.get.arr),
            "blockNumber"       -> toHexStringWithPrefixSafe(BigInteger.valueOf(blockchain.height)),
            "from"              -> toHexString(new Array[Byte](20)),
            "to"                -> toHexString(new Array[Byte](20)),
            "cumulativeGasUsed" -> toHexStringWithPrefixSafe(BigInteger.valueOf(blockchain.height)),
            "gasUsed"           -> toHexStringWithPrefixSafe(BigInteger.valueOf(blockchain.height)),
            "contractAddress"   -> JsNull,
            "logs"              -> Json.arr(),
            "logsBloom"         -> toHexString(new Array[Byte](32))
          )
        )
      case "eth_call" =>
        val call              = params.get.head.as[JsObject]
        val dataString        = (call \ "data").as[String]
        val dataBytes         = hexStringToByteArray(dataString)
        val functionSignature = dataBytes.take(4)

        val address     = decode[org.web3j.abi.datatypes.Address](dataString, 10)
        val returnValue = TypeEncoder.encode(new Uint256(BigInt(10_000_000_00000000L).bigInteger))
        log.info(s"REQ ${toHexString(functionSignature)}: $address = $returnValue")
        resp(id, returnValue)
      case "eth_estimateGas" =>
        resp(id, toHexStringWithPrefixSafe(BigInteger.valueOf(21000)))
      case "net_version" =>
        resp(id, "1")
      case "eth_gasPrice" =>
        resp(id, "0x1")
      case _ =>
        log.info(Json.stringify(jso))
        complete(Json.obj())
    }
  }
}
