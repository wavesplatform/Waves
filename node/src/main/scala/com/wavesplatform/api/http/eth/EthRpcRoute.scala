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
  val route: Route = pathPrefix("eth") {
    (get & path("abi" / AddrSegment)) { addr =>
      complete(blockchain.accountScript(addr).map(as => ABIConverter(as.script).jsonABI))
    } ~ (pathEndOrSingleSlash & post & entity(as[JsObject])) { jso =>
      val id     = (jso \ "id").get
      val params = (jso \ "params").asOpt[IndexedSeq[JsValue]].getOrElse(Nil)
      lazy val param1 = params.head
      lazy val param1Str = param1.as[String]
      
      (jso \ "method").as[String] match {
        case "eth_chainId" =>
          resp(id, quantity(AddressScheme.current.chainId.toInt))
        case "eth_blockNumber" =>
          resp(id, quantity(blockchain.height))
        case "eth_getTransactionCount" =>  // FIXME: Not implementd
          resp(id, quantity(System.currentTimeMillis()))
        case "eth_getBlockByNumber" =>  // FIXME: Not implementd
          resp(
            id,
            Json.obj(
              "number" -> quantity(Integer.parseInt(param1Str.drop(2), 16))
            )
          )

        case "eth_getBlockByHash" =>
          val blockId = ByteStr(toBytes(params.get.head.as[String]))

          resp(
            id,
            blockchain.heightOf(blockId).flatMap(blockchain.blockHeader).fold[JsValue](JsNull) { header =>
              Json.obj(
                "baseFeePerGas" -> "0x0"
              )
            }
          )
        case "eth_getBalance" =>
          val address = Address.fromHexString(param1Str)
          resp(
            id,
            toHexString(
              BigInt(blockchain.balance(address)) * EthereumTransaction.AmountMultiplier
            )
          )
        case "eth_sendRawTransaction" =>
          extractTransaction(param1Str) match {
            case Left(value) => resp(id, ApiError.fromValidationError(value).json)
            case Right(et) =>
              resp(
                id,
                transactionsApi.broadcastTransaction(et).map[JsValueWrapper] { result =>
                  result.resultE match {
                    case Left(error) => ApiError.fromValidationError(error).json
                    case Right(_)    => toHexString(et.id().arr)
                  }
                }
              )
          }

        case "eth_getTransactionReceipt" =>
          val transactionHex = param1Str
          val txId           = ByteStr(toBytes(transactionHex))
          log.info(s"Get receipt for $transactionHex/$txId")  // TODO remove logging

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
                    "status"            -> (if (tm.succeeded) "0x1" else "0x0")
                  )
                case _ => JsNull
              }

            }
          )
        case "eth_call" =>
          val call            = param1.as[JsObject]
          val dataString      = (call \ "data").as[String]
          val contractAddress = (call \ "to").as[String]

          log.info(s"balance: contract address = $contractAddress, assetId = ${assetId(contractAddress)}")  // TODO remove logging

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
          val txParams = param1.as[JsObject]
          val tx = RawTransaction.createTransaction(
            BigInteger.valueOf(System.currentTimeMillis()),
            EthereumTransaction.GasPrice,
            BigInteger.ONE,
            (txParams \ "to").as[String],
            (txParams \ "value").asOpt[String].fold(BigInteger.ZERO)(s => new BigInteger(cleanHexPrefix(s), 16)),
            (txParams \ "data").asOpt[String].getOrElse("0x")
          )

          val errorOrLong = for {
            et            <- EthereumTransaction(tx)
            (_, txFee, _) <- transactionsApi.calculateFee(et)
          } yield txFee

          log.info(s"Fee: $errorOrLong") // TODO remove logging

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
          val address = Address.fromHexString(param1Str)
          resp(id, if (blockchain.hasDApp(address)) "0xff" else "0x")
        case _ =>
          log.info(Json.stringify(jso))   // TODO remove logging
          complete(Json.obj())
      }
    }
  }

  @inline
  private[this] def quantity(v: Long) = "0x" + java.lang.Long.toString(v, 16)

  private[this] def resp(id: JsValue, resp: JsValueWrapper) = complete(Json.obj("id" -> id, "jsonrpc" -> "2.0", "result" -> resp))

  private[this] def resp(id: JsValue, resp: Future[JsValueWrapper]) = complete(resp.map(r => Json.obj("id" -> id, "jsonrpc" -> "2.0", "result" -> r)))

  private[this] def assetDescription(contractAddress: String) =
    assetId(contractAddress).flatMap(blockchain.assetDescription)

  private[this] def assetId(contractAddress: String): Option[IssuedAsset] =
    blockchain.resolveERC20Address(ERC20Address(ByteStr(toBytes(contractAddress))))

  private[this] def encodeResponse(values: Type*): String = FunctionEncoder.encodeConstructor(values.map(Type.unwrap).asJava)

  private[this] def extractTransaction(transactionHex: String) = TransactionDecoder.decode(transactionHex) match {
    case srt: SignedRawTransaction => EthereumTransaction(srt)
    case _: RawTransaction         => throw new UnsupportedOperationException("Cannot process unsigned transactions")
  }
}
