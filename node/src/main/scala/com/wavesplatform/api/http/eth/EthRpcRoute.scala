package com.wavesplatform.api.http.eth

import akka.http.scaladsl.server.*
import cats.data.Validated
import cats.instances.vector.*
import cats.syntax.either.*
import cats.syntax.traverse.*
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.http.*
import com.wavesplatform.api.http.ApiError.{CustomValidationError, InvalidIds}
import com.wavesplatform.api.http.assets.AssetsApiRoute
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{Blockchain, TxMeta}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.{EthABIConverter, ERC20Address, EthereumTransaction}
import com.wavesplatform.utils.EthEncoding.*
import com.wavesplatform.utils.{EthEncoding, Time}
import org.web3j.abi.*
import org.web3j.abi.datatypes.Bool
import org.web3j.abi.datatypes.generated.{Uint256, Uint8}
import org.web3j.crypto.*
import play.api.libs.json.*
import play.api.libs.json.Json.JsValueWrapper

import java.math.BigInteger
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import scala.util.Try

class EthRpcRoute(blockchain: Blockchain, transactionsApi: CommonTransactionsApi, time: Time) extends ApiRoute {
  val route: Route = pathPrefix("eth") {
    (path("assets") & anyParam("id", nonEmpty = true, limit = 100).massValidateEthereumIds) { erc20Ids =>
      val results = erc20Ids
        .map(id =>
          id -> (for {
            wavesId   <- blockchain.resolveERC20Address(ERC20Address(id))
            assetDesc <- blockchain.assetDescription(wavesId)
          } yield (wavesId, assetDesc))
        )
        .map { case (id, assetOpt) => Validated.fromOption(assetOpt, EthEncoding.toHexString(id.arr)).toValidatedNel }
        .sequence

      results match {
        case Validated.Invalid(ids) =>
          complete(InvalidIds(ids.toList)) // TODO: Something more obvious like "assets does not exist" ?

        case Validated.Valid(assets) =>
          val jsons = for {
            (assetId, desc) <- assets
          } yield AssetsApiRoute.jsonDetails(blockchain)(assetId, desc, full = false)
          complete(jsons.sequence.leftMap(CustomValidationError(_)).map(JsArray(_))) // TODO: Only first error is displayed
      }
    } ~ (get & path("abi" / AddrSegment)) { addr =>
      complete(blockchain.accountScript(addr).map(as => EthABIConverter(as.script).jsonABI))
    } ~ (pathEndOrSingleSlash & post & entity(as[JsObject])) { jso =>
      val id = (jso \ "id").getOrElse(JsNull)
      (jso \ "method").asOpt[String] match {
        case Some("eth_chainId" | "net_version") =>
          resp(id, quantity(blockchain.settings.addressSchemeCharacter.toInt))
        case Some("eth_blockNumber") =>
          resp(id, quantity(blockchain.height))
        case Some("eth_getTransactionCount") =>
          resp(id, quantity(time.getTimestamp()))
        case Some("eth_getBlockByNumber") =>
          extractParam1[String](jso) { str =>
            val blockNumberOpt = str match {
              case "earliest" => Some(1).asRight
              case "latest"   => Some(blockchain.height).asRight
              case "pending"  => None.asRight
              case _ =>
                Try(Some(Integer.parseInt(str.drop(2), 16))).toEither
                  .leftMap(_ => GenericError("Request parameter is not number nor supported tag"))
            }
            blockNumberOpt.fold(
              error => complete(error),
              { numberOpt =>
                val result = numberOpt.map(n => JsString(quantity(n)))
                resp(id, Json.obj("number" -> result))
              }
            )
          }
        case Some("eth_getBlockByHash") =>
          extractParam1[String](jso) { str =>
            val blockId = ByteStr(toBytes(str))
            resp(
              id,
              blockchain.heightOf(blockId).flatMap(blockchain.blockHeader).fold[JsValue](JsNull) { _ =>
                Json.obj(
                  "baseFeePerGas" -> "0x0"
                )
              }
            )
          }
        case Some("eth_getBalance") =>
          extractParam1[String](jso) { str =>
            val address = Address.fromHexString(str)
            resp(
              id,
              toHexString(
                BigInt(blockchain.balance(address)) * EthereumTransaction.AmountMultiplier
              )
            )
          }
        case Some("eth_sendRawTransaction") =>
          extractParam1[String](jso) { str =>
            extractTransaction(str) match {
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
          }
        case Some("eth_getTransactionReceipt") =>
          extractParam1[String](jso) { transactionHex =>
            val txId = ByteStr(toBytes(transactionHex))
            resp(
              id,
              transactionsApi.transactionById(txId).fold[JsValue](JsNull) { tm =>
                tm.transaction match {
                  case tx: EthereumTransaction =>
                    Json.obj(
                      "transactionHash"   -> toHexString(tm.transaction.id().arr),
                      "transactionIndex"  -> "0x1",
                      "blockHash"         -> toHexString(blockchain.lastBlockId.get.arr),
                      "blockNumber"       -> toHexString(BigInteger.valueOf(tm.height)),
                      "from"              -> toHexString(tx.senderAddress().publicKeyHash),
                      "to"                -> tx.underlying.getTo,
                      "cumulativeGasUsed" -> toHexString(tx.fee),
                      "gasUsed"           -> toHexString(tx.fee),
                      "contractAddress"   -> JsNull,
                      "logs"              -> Json.arr(),
                      "logsBloom"         -> toHexString(new Array[Byte](32)),
                      "status"            -> (if (tm.status == TxMeta.Status.Succeeded) "0x1" else "0x0")
                    )
                  case _ => JsNull
                }
              }
            )
          }
        case Some("eth_getTransactionByHash") =>
          extractParam1[String](jso) { transactionHex =>
            val txId = ByteStr(toBytes(transactionHex))
            resp(
              id,
              transactionsApi.transactionById(txId).fold[JsValue](JsNull) { tm =>
                tm.transaction match {
                  case tx: EthereumTransaction =>
                    Json.obj(
                      "hash"             -> toHexString(tm.transaction.id().arr),
                      "nonce"            -> "0x1",
                      "blockHash"        -> toHexString(blockchain.lastBlockId.get.arr),
                      "blockNumber"      -> toHexString(BigInteger.valueOf(tm.height)),
                      "transactionIndex" -> "0x1",
                      "from"             -> toHexString(tx.senderAddress().publicKeyHash),
                      "to"               -> tx.underlying.getTo,
                      "value"            -> "0x10",
                      "gasPrice"         -> toHexString(tx.fee),
                      "gas"              -> toHexString(tx.fee),
                      "input"            -> "0x20",
                      "v"                -> "0x30",
                      "standardV"        -> "0x40",
                      "r"                -> "0x50",
                      "raw"              -> "0x60",
                      "publickey"        -> toHexString(tx.signerPublicKey().arr)
                    )
                  case _ => JsNull
                }
              }
            )
          }
        case Some("eth_call") =>
          extractParam1[JsObject](jso) { call =>
            val dataString      = (call \ "data").as[String]
            val contractAddress = (call \ "to").as[String]

            cleanHexPrefix(dataString).take(8) match {
              case "95d89b41" => // symbol()
                resp(id, encodeResponse(assetDescription(contractAddress).fold("")(_.name.toStringUtf8)))
              case "313ce567" => // decimals()
                resp(id, encodeResponse(new Uint8(assetDescription(contractAddress).fold(0)(_.decimals))))
              case "70a08231" => // balanceOf(address)
                resp(
                  id,
                  encodeResponse(
                    new Uint256(
                      assetId(contractAddress).fold(0L)(ia => blockchain.balance(Address.fromHexString(dataString.takeRight(40)), ia))
                    )
                  )
                )
              case "01ffc9a7" => // supportsInterface() https://eips.ethereum.org/EIPS/eip-165
                resp(id, encodeResponse(new Bool(false)))
              case _ =>
                resp(id, "0x")
            }
          }
        case Some("eth_estimateGas") =>
          extractParam1[JsObject](jso) { txParams =>
            val tx = RawTransaction.createTransaction(
              BigInteger.valueOf(System.currentTimeMillis()),
              EthereumTransaction.GasPrice,
              BigInteger.ONE,
              // "to" may be missing when estimating base currency transfer
              (txParams \ "to").asOpt[String].getOrElse("0x0000000000000000000000000000000000000000"),
              (txParams \ "value").asOpt[String].fold(BigInteger.ZERO)(s => new BigInteger(cleanHexPrefix(s), 16)),
              (txParams \ "data").asOpt[String].getOrElse("0x")
            )

            val errorOrLong = for {
              et            <- EthereumTransaction(tx)
              (_, txFee, _) <- transactionsApi.calculateFee(et)
            } yield txFee

            resp(
              id,
              errorOrLong
                .fold[JsValueWrapper](e => ApiError.fromValidationError(e).json, fee => toHexString(BigInteger.valueOf(fee)))
            )
          }

        case Some("eth_gasPrice") =>
          resp(id, toHexString(EthereumTransaction.GasPrice))
        case Some("eth_getCode") =>
          extractParam1[String](jso) { str =>
            val address = Address.fromHexString(str)
            resp(id, if (blockchain.hasDApp(address) || assetDescription(str).isDefined) "0xff" else "0x")
          }
        case _ =>
          log.trace(s"Unexpected call: ${Json.stringify(jso)}")
          complete(Json.obj())
      }
    }
  }

  private def extractParam1[A: Reads](jso: JsObject)(f: A => StandardRoute) =
    (jso \ "params" \ 0).asOpt[A] match {
      case Some(v) => f(v)
      case None    => complete(GenericError("Error extracting required parameter"))
    }

  @inline
  private[this] def quantity(v: Long) = "0x" + java.lang.Long.toString(v, 16)

  private[this] def resp(id: JsValue, resp: JsValueWrapper) = complete(Json.obj("id" -> id, "jsonrpc" -> "2.0", "result" -> resp))

  private[this] def resp(id: JsValue, resp: Future[JsValueWrapper]) = complete(resp.map(r => Json.obj("id" -> id, "jsonrpc" -> "2.0", "result" -> r)))

  private[this] def assetDescription(contractAddress: String) =
    assetId(contractAddress).flatMap(blockchain.assetDescription)

  private[this] def assetId(contractAddress: String): Option[IssuedAsset] =
    blockchain.resolveERC20Address(ERC20Address(ByteStr(toBytes(contractAddress))))

  private[this] def encodeResponse(values: Type*): String = "0x" + FunctionEncoder.encodeConstructor(values.map(Type.unwrap).asJava)

  private[this] def extractTransaction(transactionHex: String) = TransactionDecoder.decode(transactionHex) match {
    case srt: SignedRawTransaction => EthereumTransaction(srt)
    case _: RawTransaction         => throw new UnsupportedOperationException("Cannot process unsigned transactions")
  }
}
