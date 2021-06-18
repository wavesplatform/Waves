package com.wavesplatform.api.http.eth

import java.util.Collections

import akka.http.scaladsl.server._
import com.google.common.io.BaseEncoding
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.api.http._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.protobuf.transaction.PBRecipients
import com.wavesplatform.state.Blockchain
import org.web3j.abi.{FunctionReturnDecoder, TypeDecoder, TypeReference}
import org.web3j.crypto.{RawTransaction, SignedRawTransaction, TransactionDecoder, TransactionEncoder}
import org.web3j.rlp.{RlpDecoder, RlpList, RlpString, RlpType}
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json.{JsObject, JsValue, Json}
import org.web3j.utils.Numeric.{hexStringToByteArray, toHexString}

import scala.jdk.CollectionConverters._

class EthRpcRoute(blockchain: Blockchain) extends ApiRoute {
  private def quantity(v: Long)   = s"0x${java.lang.Long.toString(v, 16)}"

  private def resp(id: JsValue, resp: JsValueWrapper) = complete(Json.obj("id" -> id, "jsonrpc" -> "2.0", "result" -> resp))
  val route: Route = (post & path("eth") & entity(as[JsObject])) { jso =>
    val id     = (jso \ "id").get
    val params = (jso \ "params").as[IndexedSeq[JsValue]]
    (jso \ "method").as[String] match {
      case "eth_chainId" =>
        resp(id, quantity(AddressScheme.current.chainId.toInt))
      case "eth_blockNumber" =>
        resp(id, quantity(blockchain.height))
      case "eth_getTransactionCount" =>
        resp(id, quantity(2))
      case "eth_getBlockByNumber" =>
        val height    = Integer.parseInt(params.head.as[String].drop(2), 16)
        val blockMeta = blockchain.blockHeader(height).get
        resp(
          id,
          Json.obj(
            "number" -> quantity(height)
          )
        )
      case "eth_getBalance" =>
        val address = PBRecipients.toAddress(hexStringToByteArray(params.head.as[String]), AddressScheme.current.chainId).explicitGet()
        val balance = (BigInt(System.currentTimeMillis()) * 1_000_000_000_000L).bigInteger.toString(16)
        log.info(s"\n\t$balance\n")

        resp(id, s"0x$balance")
      case "eth_sendRawTransaction" =>
        val rawTransaction = TransactionDecoder.decode(params.head.as[String]) match {
          case srt: SignedRawTransaction =>
            log.info(s"""Signed raw transaction:
                 |from = ${srt.getFrom}
                 |to = ${srt.getTo}
                 |data = ${srt.getData}""".stripMargin)
            srt

          case rt: RawTransaction =>
            rt
        }
        resp(
          id,
          s"0x${BaseEncoding.base16().lowerCase().encode(crypto.secureHash(TransactionEncoder.encode(rawTransaction, AddressScheme.current.chainId.toLong)))}"
        )
      case "eth_getTransactionReceipt" =>
        resp(
          id,
          Json.obj(
            )
        )
      case "eth_call" =>
        val call = params.head.as[JsObject]
        val dataString = (call \ "data").as[String]
        val dataBytes = hexStringToByteArray(dataString)
        val functionSignature = dataBytes.take(4)
        val address = FunctionReturnDecoder.decode(dataString.drop(10), Collections.singletonList(TypeReference.makeTypeReference("uint256")))
        log.info(s"REQ ${toHexString(functionSignature)}: $address")
        resp(id, "0x0")
      case _ =>
        log.info(Json.stringify(jso))
        complete(Json.obj())
    }
  }
}
