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
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.{ABIConverter, ERC20Address, EthereumTransaction}
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
      complete(blockchain.accountScript(addr).map(as => ABIConverter(as.script).jsonABI))
    } ~ (pathEndOrSingleSlash & post & entity(as[JsObject])) { jso =>
      val id     = (jso \ "id").get
      val params = (jso \ "params").asOpt[IndexedSeq[JsValue]].getOrElse(Nil)
      lazy val param1E =
        params.headOption
          .fold(complete(GenericError("Expected parameter not found")).asLeft[JsValue])(_.asRight[StandardRoute])
      lazy val param1StrE =
        param1E.flatMap(p =>
          p.asOpt[String].fold(complete(GenericError(s"Expected string parameter, but $p found")).asLeft[String])(_.asRight[StandardRoute])
        )

      (jso \ "method").as[String] match {
        case "eth_chainId" | "net_version" =>
          resp(id, quantity(blockchain.settings.addressSchemeCharacter.toInt))
        case "eth_blockNumber" =>
          resp(id, quantity(blockchain.height))
        case "eth_getTransactionCount" =>
          resp(id, quantity(time.getTimestamp()))
        case "eth_getBlockByNumber" =>
          param1StrE
            .map { str =>
              val blockNumber = str match {
                case "earliest" => Some(1)
                case "latest"   => Some(blockchain.height)
                case "pending"  => None
                case _          => Some(Integer.parseInt(str.drop(2), 16))
              }
              val result = blockNumber.map(n => JsString(quantity(n))).getOrElse(JsNull)
              resp(id, Json.obj("number" -> result))
            }
            .fold(identity, identity)

        case "eth_getBlockByHash" =>
          param1StrE
            .map { str =>
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
            .fold(identity, identity)
        case "eth_getBalance" =>
          param1StrE
            .map { str =>
              val address = Address.fromHexString(str)
              resp(
                id,
                toHexString(
                  BigInt(blockchain.balance(address)) * EthereumTransaction.AmountMultiplier
                )
              )

            }
            .fold(identity, identity)
        case "eth_sendRawTransaction" =>
          param1StrE
            .map { str =>
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
            .fold(identity, identity)

        case "eth_getTransactionReceipt" =>
          param1StrE
            .map { transactionHex =>
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
                        "status"            -> (if (tm.succeeded) "0x1" else "0x0")
                      )
                    case _ => JsNull
                  }

                }
              )
            }
            .fold(identity, identity)
        case "eth_call" =>
          param1E
            .map { param =>
              val call            = param.as[JsObject]
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
            .fold(identity, identity)
        case "eth_estimateGas" =>
          param1E
            .map { param =>
              val txParams = param.as[JsObject]
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
            .fold(identity, identity)

        case "eth_gasPrice" =>
          resp(id, toHexString(EthereumTransaction.GasPrice))
        case "eth_getCode" =>
          param1StrE
            .map { str =>
              val address = Address.fromHexString(str)
              resp(id, if (blockchain.hasDApp(address) || assetDescription(str).isDefined) "0xff" else "0x")
            }
            .fold(identity, identity)
        case _ =>
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

  private[this] def encodeResponse(values: Type*): String = "0x" + FunctionEncoder.encodeConstructor(values.map(Type.unwrap).asJava)

  private[this] def extractTransaction(transactionHex: String) = TransactionDecoder.decode(transactionHex) match {
    case srt: SignedRawTransaction => EthereumTransaction(srt)
    case _: RawTransaction         => throw new UnsupportedOperationException("Cannot process unsigned transactions")
  }
}
