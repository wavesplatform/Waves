package com.wavesplatform.api.http.eth

import java.math.BigInteger

import akka.http.scaladsl.server._
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.http._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{ABIConverter, ERC20Address, EthereumTransaction}
import com.wavesplatform.utils.EthEncoding._
import org.web3j.abi._
import org.web3j.abi.datatypes.generated.{Uint256, Uint8}
import org.web3j.crypto._
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters._

class EthRpcRoute(blockchain: Blockchain, transactionsApi: CommonTransactionsApi) extends ApiRoute {
  private def quantity(v: Long) = s"0x${java.lang.Long.toString(v, 16)}"

  private def resp(id: JsValue, resp: JsValueWrapper) = complete(Json.obj("id" -> id, "jsonrpc" -> "2.0", "result" -> resp))

  private def resp(id: JsValue, resp: Future[JsValueWrapper]) = complete(resp.map(r => Json.obj("id" -> id, "jsonrpc" -> "2.0", "result" -> r)))

  private def assetDescription(contractAddress: String) =
    assetId(contractAddress).flatMap(blockchain.assetDescription)

  private def assetId(contractAddress: String): Option[IssuedAsset] =
    blockchain.resolveERC20Address(ERC20Address(ByteStr(toBytes(contractAddress))))

  private def encodeResponse(values: Type*): String = FunctionEncoder.encodeConstructor(values.map(Type.unwrap).asJava)

  private def extractTransaction(transactionHex: String) = TransactionDecoder.decode(transactionHex) match {
    case srt: SignedRawTransaction => EthereumTransaction(srt)
    case _: RawTransaction         => throw new UnsupportedOperationException("Cannot process unsigned transactions")
  }

  val route: Route = pathPrefix("eth") {
    (get & path("abi" / AddrSegment)) { addr =>
      complete(blockchain.accountScript(addr).map(as => ABIConverter(as.script).jsonABI))
    } ~ (pathEndOrSingleSlash & post & entity(as[JsObject])) { jso =>
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
            toHexString(
              BigInt(blockchain.balance(Address.fromHexString(params.get.head.as[String]))) * EthereumTransaction.AmountMultiplier
            )
          )
        case "eth_sendRawTransaction" =>
          val str = params.get.head.as[String]
          log.info(str)

          extractTransaction(str) match {
            case Left(value) => resp(id, ApiError.fromValidationError(value).json)
            case Right(et) =>
              resp(
                id,
                transactionsApi.broadcastTransaction(et).map[JsValueWrapper] { result =>
                  log.info(s"Validation result from ${toHexString(et.senderAddress().publicKeyHash)}: $result")
                  toHexString(et.id().arr)
                }
              )
          }

        case "eth_getTransactionReceipt" =>
          val transactionHex = params.get.head.as[String]
          val txId           = ByteStr(toBytes(transactionHex))
          log.info(s"Get receipt for $transactionHex/$txId")

          resp(
            id,
            transactionsApi.transactionById(txId).fold[JsValue](JsNull) { tm =>
                tm.transaction match {
                  case tx: EthereumTransaction =>
                    Json.obj(
                      "transactionHash"   -> toHexString(tm.transaction.id().arr),
                      "transactionIndex"  -> "0x01",
                      "blockHash"         -> toHexString(blockchain.lastBlockId.get.arr),
                      "blockNumber"       -> toHexString(BigInteger.valueOf(tm.height)),
                      "from"              -> toHexString(tx.senderAddress().publicKeyHash),
                      "to"                -> tx.underlying.getTo,
                      "cumulativeGasUsed" -> toHexString(tx.fee),
                      "gasUsed"           -> toHexString(tx.fee),
                      "contractAddress"   -> JsNull,
                      "logs"              -> Json.arr(),
                      "logsBloom"         -> toHexString(new Array[Byte](32)),
                      "status" -> (if (tm.succeeded) "0x1" else "0x0")
                    )
                  case _ => JsNull
                }

            }
          )
        case "eth_call" =>
          val call            = params.get.head.as[JsObject]
          val dataString      = (call \ "data").as[String]
          val contractAddress = (call \ "to").as[String]

          log.info(s"balance: contract address = $contractAddress, assetId = ${assetId(contractAddress)}")

          cleanHexPrefix(dataString).take(8) match {
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
          val tx = RawTransaction.createTransaction(
            BigInteger.valueOf(System.currentTimeMillis()),
            EthereumTransaction.GasPrice,
            BigInteger.ONE,
            (txParams \ "to").as[String],
            (txParams \ "value").asOpt[String].fold(BigInteger.ZERO)(s => new BigInteger(cleanHexPrefix(s), 16)),
            (txParams \ "data").asOpt[String].getOrElse("0x")
          )

          val errorOrLong = for {
            et <- EthereumTransaction(tx)
            (_, txFee, _) <- transactionsApi.calculateFee(et)
          } yield txFee

          log.info(s"Fee: $errorOrLong")

          resp(
            id,
            errorOrLong
              .fold[JsValueWrapper](e => ApiError.fromValidationError(e).json, fee => toHexString(BigInteger.valueOf(fee)))
          )

        case "net_version" =>
          resp(id, "1")
        case "eth_gasPrice" =>
          resp(id, toHexString(EthereumTransaction.GasPrice))
        case "eth_getCode" =>
          resp(id, if (blockchain.hasDApp(Address.fromHexString(params.get.head.as[String]))) "0xff" else "0x")
        case _ =>
          log.info(Json.stringify(jso))
          complete(Json.obj())
      }
    }
  }
}
