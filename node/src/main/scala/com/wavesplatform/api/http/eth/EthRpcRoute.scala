package com.wavesplatform.api.http.eth

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.model.*
import org.apache.pekko.http.scaladsl.model.headers.*
import org.apache.pekko.http.scaladsl.model.ws.{Message, TextMessage}
import org.apache.pekko.http.scaladsl.server.*
import org.apache.pekko.stream.Materializer
import org.apache.pekko.stream.scaladsl.Flow
import cats.data.Validated
import cats.instances.vector.*
import cats.syntax.either.*
import cats.syntax.traverse.*
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.http.*
import com.wavesplatform.api.http.ApiError.{CustomValidationError, InvalidIds}
import com.wavesplatform.api.http.assets.AssetsApiRoute
import com.wavesplatform.block.BlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{BinaryDataEntry, Blockchain, BooleanDataEntry, DataEntry, IntegerDataEntry, StringDataEntry, TxMeta}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.{ERC20Address, EthABIConverter, EthereumTransaction}
import com.wavesplatform.utils.EthEncoding.*
import com.wavesplatform.utils.{EthEncoding, Time}
import org.web3j.abi.FunctionEncoder
import org.web3j.abi.datatypes.generated.{Uint256, Uint8}
import org.web3j.abi.datatypes.{Bool, Utf8String}
import org.web3j.crypto.*
import org.web3j.utils.Numeric
import play.api.libs.json.*
import play.api.libs.json.Json.JsValueWrapper

import java.math.BigInteger
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import scala.util.Try

class EthRpcRoute(
    blockchain: Blockchain,
    transactionsApi: CommonTransactionsApi,
    time: Time
)(implicit val actorSystem: ActorSystem = null)
    extends ApiRoute {

  private val corsHeaders = List(
    `Access-Control-Allow-Origin`.*,
    `Access-Control-Allow-Methods`(HttpMethods.GET, HttpMethods.POST, HttpMethods.OPTIONS),
    `Access-Control-Allow-Headers`("Content-Type", "Authorization", "X-Requested-With", "User-Agent", "DNT", "Cache-Control", "Range")
  )

  private def withCors(r: Route): Route =
    respondWithHeaders(corsHeaders) {
      options {
        complete(StatusCodes.OK)
      } ~ r
    }

  val route: Route = withCors {
    pathPrefix("eth") {
      assetsRoute ~ abiRoute ~ wsRoute ~ rpcHttpRoute
    } ~ pathPrefix("ws") {
      wsRoute
    } ~ wsRoute ~ rpcHttpRoute
  }

  private def assetsRoute: Route =
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
          complete(InvalidIds(ids.toList))

        case Validated.Valid(assets) =>
          val jsons = for {
            (assetId, desc) <- assets
          } yield AssetsApiRoute.jsonDetails(blockchain)(assetId, desc, full = false)
          complete(jsons.sequence.leftMap(CustomValidationError(_)).map(JsArray(_)))
      }
    }

  private def abiRoute: Route =
    (get & path("abi" / AddrSegment)) { addr =>
      complete(blockchain.accountScript(addr).map(as => EthABIConverter(as.script).jsonABI))
    }

  private def wsRoute: Route =
    if (actorSystem != null) {
      implicit val mat: Materializer = Materializer(actorSystem)
      extractWebSocketUpgrade { upgrade =>
        complete(upgrade.handleMessages(createWsFlow()))
      }
    } else {
      reject
    }

  private def createWsFlow()(implicit mat: Materializer): Flow[Message, Message, Any] = {
    Flow[Message].collect {
      case TextMessage.Strict(text) => Future.successful(text)
      case tm: TextMessage =>
        tm.textStream.runFold("")(_ + _)
    }.mapAsync(1)(identity).mapAsync(1) { text =>
      processWsJson(text).map(res => TextMessage.Strict(res))
    }
  }

  private def processWsJson(text: String): Future[String] = {
    Try(Json.parse(text)).toOption match {
      case Some(jso: JsObject) =>
        dispatchSingle(jso).map(Json.stringify)
      case Some(jsa: JsArray) =>
        val futures = jsa.value.map {
          case obj: JsObject => dispatchSingle(obj)
          case _ => Future.successful(Json.obj("jsonrpc" -> "2.0", "id" -> JsNull, "error" -> Json.obj("code" -> -32600, "message" -> "Invalid Request")))
        }
        Future.sequence(futures).map(res => Json.stringify(JsArray(res)))
      case _ =>
        Future.successful(Json.stringify(Json.obj("jsonrpc" -> "2.0", "id" -> JsNull, "error" -> Json.obj("code" -> -32700, "message" -> "Parse error"))))
    }
  }

  private def rpcHttpRoute: Route =
    (pathEndOrSingleSlash & post & entity(as[JsValue])) {
      case jso: JsObject =>
        (jso \ "method").asOpt[String] match {
          case Some(_) =>
            complete(dispatchSingle(jso))
          case None =>
            complete(Json.obj()) // Matches existing test "absence of method"
        }
      case jsa: JsArray =>
        val futures = jsa.value.map {
          case obj: JsObject => dispatchSingle(obj)
          case _ => Future.successful(Json.obj("jsonrpc" -> "2.0", "id" -> JsNull, "error" -> Json.obj("code" -> -32600, "message" -> "Invalid Request")))
        }
        complete(Future.sequence(futures).map(JsArray(_)))
      case _ =>
        complete(Json.obj())
    }

  private def dispatchSingle(jso: JsObject): Future[JsObject] = {
    val id = (jso \ "id").getOrElse(JsNull)
    val methodOpt = (jso \ "method").asOpt[String]

    methodOpt match {
      case Some("eth_chainId") =>
        Future.successful(respObj(id, quantity(blockchain.settings.addressSchemeCharacter.toInt)))

      case Some("net_version") =>
        Future.successful(respObj(id, JsString(blockchain.settings.addressSchemeCharacter.toInt.toString)))

      case Some("eth_blockNumber") =>
        Future.successful(respObj(id, quantity(blockchain.height)))

      case Some("eth_getTransactionCount") =>
        Future.successful(respObj(id, quantity(time.correctedTime())))

      case Some("eth_getBlockByNumber") =>
        val tagOrHex = (jso \ "params" \ 0).asOpt[String]

        tagOrHex match {
          case None =>
            Future.successful(apiErrorToRpcError(id, GenericError("Error extracting required parameter")))
          case Some("pending") =>
            Future.successful(respObj(id, Json.obj("number" -> JsNull)))
          case Some(tag) =>
            val targetHeightOpt: Either[GenericError, Int] = tag match {
              case "earliest" => Right(1)
              case "latest"   => Right(blockchain.height)
              case other =>
                Try(Integer.parseInt(cleanHexPrefix(other), 16)).toEither
                  .leftMap(_ => GenericError("Request parameter is not number nor supported tag"))
            }

            targetHeightOpt match {
              case Left(err) =>
                Future.successful(apiErrorToRpcError(id, err))
              case Right(h) =>
                val blockHeaderOpt = blockchain.blockHeader(h)
                val blockJson = blockHeaderOpt match {
                  case Some(sbh) => formatBlock(sbh.header, sbh.id(), h)
                  case None      => formatEmptyBlock(h)
                }
                Future.successful(respObj(id, blockJson))
            }
        }

      case Some("eth_getBlockByHash") =>
        (jso \ "params" \ 0).asOpt[String] match {
          case Some(hashStr) =>
            val blockId = ByteStr(toBytes(hashStr))
            val resOpt = for {
              h   <- blockchain.heightOf(blockId)
              sbh <- blockchain.blockHeader(h)
            } yield formatBlock(sbh.header, blockId, h)

            val finalJson = resOpt.getOrElse(
              Json.obj("baseFeePerGas" -> "0x0")
            )
            Future.successful(respObj(id, finalJson))
          case None =>
            Future.successful(apiErrorToRpcError(id, GenericError("Error extracting required parameter")))
        }

      case Some("eth_getBalance") =>
        extractParam1[String](jso) { str =>
          val addressOpt = resolveAccountAddress(str)
          val bal = addressOpt.fold(BigInt(0))(addr => BigInt(blockchain.balance(addr)) * EthereumTransaction.AmountMultiplier)
          Future.successful(respObj(id, toHexString(bal)))
        }

      case Some("eth_sendRawTransaction") =>
        extractParam1[String](jso) { str =>
          extractTransaction(str) match {
            case Left(value) =>
              Future.successful(apiErrorToRpcError(id, value))
            case Right(et) =>
              transactionsApi.broadcastTransaction(et).map { result =>
                result.resultE match {
                  case Left(error) =>
                    apiErrorToRpcError(id, error)
                  case Right(_) =>
                    respObj(id, toHexString(et.id().arr))
                }
              }
          }
        }

      case Some("eth_getTransactionReceipt") =>
        extractParam1[String](jso) { transactionHex =>
          val txId = ByteStr(toBytes(transactionHex))
          val resultJson = transactionsApi.transactionById(txId).fold[JsValue](JsNull) { tm =>
            tm.transaction match {
              case tx: EthereumTransaction =>
                val logs = tx.payload match {
                  case EthereumTransaction.Transfer(Some(tokenAddress), amount, recipient) =>
                    val tokenEthAddr = toHexString(tokenAddress.arr)
                    val fromAddrPadded = "0x" + zeroes(24) + toHexString(tx.senderAddress().publicKeyHash).drop(2)
                    val toAddrPadded = "0x" + zeroes(24) + toHexString(recipient.publicKeyHash).drop(2)
                    val isNft = assetDescription(tokenEthAddr).exists(d => d.nft && d.script.isEmpty)
                    val (topics, dataHex) = if (isNft) {
                      val tokenIdPadded = "0x" + zeroes(63) + "1"
                      (Json.arr("0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef", fromAddrPadded, toAddrPadded, tokenIdPadded), "0x")
                    } else {
                      val amountHexPadded = "0x" + String.format("%064x", BigInteger.valueOf(amount))
                      (Json.arr("0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef", fromAddrPadded, toAddrPadded), amountHexPadded)
                    }
                    Json.arr(
                      Json.obj(
                        "address"          -> tokenEthAddr,
                        "topics"           -> topics,
                        "data"             -> dataHex,
                        "blockNumber"      -> toHexString(BigInteger.valueOf(tm.height.toInt)),
                        "transactionHash"  -> toHexString(tm.transaction.id().arr),
                        "transactionIndex" -> "0x1",
                        "blockHash"        -> toHexString(blockchain.lastBlockId.get.arr),
                        "logIndex"         -> "0x0",
                        "removed"          -> false
                      )
                    )
                  case _ =>
                    Json.arr()
                }

                Json.obj(
                  "transactionHash"   -> toHexString(tm.transaction.id().arr),
                  "transactionIndex"  -> "0x1",
                  "blockHash"         -> toHexString(blockchain.lastBlockId.get.arr),
                  "blockNumber"       -> toHexString(BigInteger.valueOf(tm.height.toInt)),
                  "from"              -> toHexString(tx.senderAddress().publicKeyHash),
                  "to"                -> tx.underlying.getTo,
                  "cumulativeGasUsed" -> toHexString(tx.fee),
                  "gasUsed"           -> toHexString(tx.fee),
                  "contractAddress"   -> JsNull,
                  "logs"              -> logs,
                  "logsBloom"         -> ("0x" + zeroes(64)),
                  "status"            -> (if (tm.status == TxMeta.Status.Succeeded) "0x1" else "0x0")
                )
              case _ => JsNull
            }
          }
          Future.successful(respObj(id, resultJson))
        }

      case Some("eth_getTransactionByHash") =>
        extractParam1[String](jso) { transactionHex =>
          val txId = ByteStr(toBytes(transactionHex))
          val resultJson = transactionsApi.transactionById(txId).fold[JsValue](JsNull) { tm =>
            tm.transaction match {
              case tx: EthereumTransaction =>
                Json.obj(
                  "hash"             -> toHexString(tm.transaction.id().arr),
                  "nonce"            -> "0x1",
                  "blockHash"        -> toHexString(blockchain.lastBlockId.get.arr),
                  "blockNumber"      -> toHexString(BigInteger.valueOf(tm.height.toInt)),
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
          Future.successful(respObj(id, resultJson))
        }

      case Some("eth_call") =>
        (jso \ "params" \ 0).asOpt[JsObject] match {
          case Some(call) =>
            val dataString = (call \ "data").asOpt[String].getOrElse("")
            val contractAddress = (call \ "to").asOpt[String].getOrElse("")
            val cleanData = cleanHexPrefix(dataString).toLowerCase

            cleanData.take(8) match {
              case "06fdde03" => // name()
                val name = assetDescription(contractAddress).fold("")(_.name.toStringUtf8)
                Future.successful(respObj(id, encodeResponse(new Utf8String(name))))

              case "95d89b41" => // symbol()
                val sym = assetDescription(contractAddress).fold("")(_.name.toStringUtf8)
                Future.successful(respObj(id, encodeResponse(new Utf8String(sym))))

              case "313ce567" => // decimals()
                val dec = assetDescription(contractAddress).fold(0)(_.decimals)
                Future.successful(respObj(id, encodeResponse(new Uint8(dec))))

              case "18160ddd" => // totalSupply()
                val supply = assetDescription(contractAddress).fold(BigInteger.ZERO)(d => BigInteger.valueOf(d.totalVolume.toLong))
                Future.successful(respObj(id, encodeResponse(new Uint256(supply))))

              case "70a08231" => // balanceOf(address)
                val ownerHex = if (cleanData.length >= 48) cleanData.takeRight(40) else zeroes(40)
                val ownerAddr = resolveAccountAddress(ownerHex)
                val balance = (for {
                  addr <- ownerAddr
                  ia   <- assetId(contractAddress)
                } yield blockchain.balance(addr, ia)).getOrElse(0L)
                Future.successful(respObj(id, encodeResponse(new Uint256(BigInteger.valueOf(balance)))))

              case "dd62ed3e" => // allowance(owner, spender)
                Future.successful(respObj(id, encodeResponse(new Uint256(BigInteger.valueOf(Long.MaxValue)))))

              case "01ffc9a7" => // supportsInterface(bytes4)
                val ifaceId = if (cleanData.length >= 16) cleanData.substring(8, 16) else ""
                val isNft = assetDescription(contractAddress).exists(d => d.nft && d.script.isEmpty)
                val supported = if (isNft) {
                  ifaceId == "80ac58cd" || // ERC-721
                  ifaceId == "5b5e139f" || // ERC-721 Metadata
                  ifaceId == "780e9d63" || // ERC-721 Enumerable
                  ifaceId == "01ffc9a7"    // ERC-165
                } else {
                  ifaceId == "01ffc9a7"
                }
                Future.successful(respObj(id, encodeResponse(new Bool(supported))))

              case "c87b56dd" => // tokenURI(uint256)
                val uri = assetDescription(contractAddress).fold("")(_.description.toStringUtf8)
                Future.successful(respObj(id, encodeResponse(new Utf8String(uri))))

              case "6352211e" => // ownerOf(uint256)
                val ownerEthAddr = assetDescription(contractAddress).fold("0x" + zeroes(40))(d => toHexString(d.issuer.toAddress.publicKeyHash))
                Future.successful(respObj(id, encodeResponse(new org.web3j.abi.datatypes.Address(ownerEthAddr))))

              case "4f6ccce7" | "2f745159" => // tokenByIndex(uint256) / tokenOfOwnerByIndex(address,uint256)
                Future.successful(respObj(id, encodeResponse(new Uint256(BigInteger.ONE))))

              case _ =>
                Future.successful(respObj(id, "0x"))
            }
          case None =>
            Future.successful(respObj(id, "0x"))
        }

      case Some("eth_estimateGas") =>
        (jso \ "params" \ 0).asOpt[JsObject] match {
          case Some(txParams) =>
            val toAddr = (txParams \ "to").asOpt[String].getOrElse("0x0000000000000000000000000000000000000000")
            val data = (txParams \ "data").asOpt[String].getOrElse("0x")
            val valBigInt = (txParams \ "value").asOpt[String].fold(BigInteger.ZERO)(s => Try(new BigInteger(cleanHexPrefix(s), 16)).getOrElse(BigInteger.ZERO))

            val tx = RawTransaction.createTransaction(
              BigInteger.valueOf(System.currentTimeMillis()),
              EthereumTransaction.GasPrice,
              BigInteger.ONE,
              toAddr,
              valBigInt,
              data
            )

            val errorOrLong = for {
              et            <- EthereumTransaction(tx)
              (_, txFee, _) <- transactionsApi.calculateFee(et)
            } yield txFee

            val resVal = errorOrLong.fold[JsValueWrapper](
              e => ApiError.fromValidationError(e).json,
              fee => toHexString(BigInteger.valueOf(fee))
            )
            Future.successful(respObj(id, resVal))
          case None =>
            Future.successful(apiErrorToRpcError(id, GenericError("Error extracting required parameter")))
        }

      case Some("eth_feeHistory") =>
        val blockCount = (jso \ "params" \ 0).asOpt[Long].orElse(
          (jso \ "params" \ 0).asOpt[String].flatMap(s => Try(java.lang.Long.parseLong(cleanHexPrefix(s), 16)).toOption)
        ).getOrElse(4L).toInt
        val safeCount = Math.max(1, Math.min(1024, blockCount))
        val curHeight = blockchain.height
        val endBlock = Math.max(1, curHeight)
        val oldest = Math.max(1, endBlock - safeCount + 1)
        val baseFees = (oldest to endBlock + 1).map(_ => "0x0")
        val gasUsedRatios = (oldest to endBlock).map(_ => 0.0)
        val rewards = (oldest to endBlock).map(_ => Json.arr("0x3b9aca00"))
        Future.successful(
          respObj(
            id,
            Json.obj(
              "oldestBlock"   -> quantity(oldest),
              "baseFeePerGas" -> baseFees,
              "gasUsedRatio"  -> gasUsedRatios,
              "reward"        -> rewards
            )
          )
        )

      case Some("eth_maxPriorityFeePerGas") =>
        Future.successful(respObj(id, "0x3b9aca00")) // 1 Gwei

      case Some("eth_gasPrice") =>
        Future.successful(respObj(id, toHexString(EthereumTransaction.GasPrice)))

      case Some("eth_syncing") =>
        Future.successful(respObj(id, JsBoolean(false)))

      case Some("eth_accounts") =>
        Future.successful(respObj(id, Json.arr()))

      case Some("web3_clientVersion") =>
        Future.successful(respObj(id, s"Waves/v${com.wavesplatform.Version.VersionString}"))

      case Some("net_listening") =>
        Future.successful(respObj(id, JsBoolean(true)))

      case Some("net_peerCount") =>
        Future.successful(respObj(id, "0x1"))

      case Some("eth_protocolVersion") =>
        Future.successful(respObj(id, "0x41"))

      case Some("eth_getBlockTransactionCountByNumber") =>
        val tagOrHex = (jso \ "params" \ 0).asOpt[String]
        val targetHeightOpt: Option[Int] = tagOrHex match {
          case Some("earliest") => Some(1)
          case Some("latest") | Some("pending") => Some(blockchain.height)
          case Some(other) => Try(Integer.parseInt(cleanHexPrefix(other), 16)).toOption
          case None => None
        }
        val countJson: JsValue = targetHeightOpt.flatMap(h => blockchain.blockHeader(h)).fold[JsValue](JsNull)(_ => JsString("0x0"))
        Future.successful(respObj(id, countJson))

      case Some("eth_getBlockTransactionCountByHash") =>
        val countJson: JsValue = (jso \ "params" \ 0).asOpt[String].flatMap { hashStr =>
          val blockId = ByteStr(toBytes(hashStr))
          blockchain.heightOf(blockId).flatMap(h => blockchain.blockHeader(h))
        }.fold[JsValue](JsNull)(_ => JsString("0x0"))
        Future.successful(respObj(id, countJson))

      case Some("eth_getTransactionByBlockHashAndIndex") | Some("eth_getTransactionByBlockNumberAndIndex") =>
        Future.successful(respObj(id, JsNull))

      case Some("eth_getUncleCountByBlockHash") | Some("eth_getUncleCountByBlockNumber") =>
        Future.successful(respObj(id, "0x0"))

      case Some("eth_getUncleByBlockHashAndIndex") | Some("eth_getUncleByBlockNumberAndIndex") =>
        Future.successful(respObj(id, JsNull))

      case Some("eth_getStorageAt") =>
        val addressStrOpt = (jso \ "params" \ 0).asOpt[String]
        val posOpt        = (jso \ "params" \ 1).asOpt[String]
        val storageVal = (for {
          addrStr <- addressStrOpt
          addr    <- resolveAccountAddress(addrStr)
          pos     <- posOpt
        } yield {
          val cleanPos = cleanHexPrefix(pos).toLowerCase
          val directKeyOpt = Try(new String(toBytes(cleanPos), java.nio.charset.StandardCharsets.UTF_8)).toOption
            .filter(k => k.nonEmpty && k.forall(c => !Character.isISOControl(c)))
          val entryByKey = directKeyOpt.flatMap(k => blockchain.accountData(addr, k))

          entryByKey match {
            case Some(entry) => formatDataEntryToStorage(entry)
            case None =>
              blockchain.accountData(addr, pos) match {
                case Some(entry) => formatDataEntryToStorage(entry)
                case None =>
                  val slotIndexOpt = Try(new BigInteger(cleanPos, 16)).toOption
                  slotIndexOpt.flatMap(idx => blockchain.accountData(addr, idx.toString)) match {
                    case Some(entry) => formatDataEntryToStorage(entry)
                    case None => "0x" + zeroes(64)
                  }
              }
          }
        }).getOrElse("0x" + zeroes(64))
        Future.successful(respObj(id, storageVal))

      case Some("personal_listAccounts") =>
        Future.successful(respObj(id, Json.arr()))

      case Some("personal_ecRecover") =>
        val msgOpt = (jso \ "params" \ 0).asOpt[String]
        val sigOpt = (jso \ "params" \ 1).asOpt[String]
        val recoveredOpt = for {
          msg <- msgOpt
          sig <- sigOpt
          cleanSig = cleanHexPrefix(sig)
          if cleanSig.length == 130
          r = new BigInteger(cleanSig.substring(0, 64), 16)
          s = new BigInteger(cleanSig.substring(64, 128), 16)
          v = Integer.parseInt(cleanSig.substring(128, 130), 16).toByte
          msgBytes = if (msg.startsWith("0x")) toBytes(msg) else msg.getBytes(java.nio.charset.StandardCharsets.UTF_8)
          sigData = new Sign.SignatureData(v, Numeric.toBytesPadded(r, 32), Numeric.toBytesPadded(s, 32))
          key <- Try(Sign.signedPrefixedMessageToKey(msgBytes, sigData)).orElse(
            Try(Sign.signedMessageHashToKey(msgBytes, sigData))
          ).toOption
        } yield Keys.toChecksumAddress(Keys.getAddress(key))
        Future.successful(respObj(id, recoveredOpt.fold[JsValueWrapper](JsNull)(JsString.apply)))

      case Some("web3_sha3") =>
        extractParam1[String](jso) { hexStr =>
          val hash = Hash.sha3(hexStr)
          Future.successful(respObj(id, JsString(hash)))
        }

      case Some("eth_getLogs") =>
        Future.successful(respObj(id, Json.arr()))

      case Some("eth_sendTransaction") | Some("eth_sign") | Some("personal_sign") =>
        Future.successful(apiErrorToRpcError(id, GenericError("Node does not hold private keys; sign locally with web3.eth.accounts or MetaMask and use eth_sendRawTransaction")))

      case Some("eth_coinbase") =>
        Future.successful(respObj(id, "0x0000000000000000000000000000000000000000"))

      case Some("eth_mining") =>
        Future.successful(respObj(id, JsBoolean(false)))

      case Some("eth_hashrate") =>
        Future.successful(respObj(id, "0x0"))

      case Some("eth_subscribe") =>
        val subId = "0x" + java.lang.Long.toHexString(System.nanoTime())
        Future.successful(respObj(id, subId))

      case Some("eth_unsubscribe") =>
        Future.successful(respObj(id, JsBoolean(true)))

      case Some("eth_getCode") =>
        extractParam1[String](jso) { str =>
          val addressOpt = resolveAccountAddress(str)
          val hasCode = addressOpt.exists(blockchain.hasDApp) || assetDescription(str).isDefined
          Future.successful(
            respObj(id, if (hasCode) "0xff" else "0x")
          )
        }

      case _ =>
        Future.successful(Json.obj())
    }
  }

  private def formatBlock(header: BlockHeader, blockId: ByteStr, height: Int): JsObject = {
    val blockHash = toHexString(blockId.arr)
    val parentHash = toHexString(header.reference.arr)
    val minerEthAddress = toHexString(header.generator.toAddress.publicKeyHash)
    val timestampSeconds = quantity(header.timestamp / 1000)

    Json.obj(
      "number"           -> quantity(height),
      "hash"             -> blockHash,
      "parentHash"       -> parentHash,
      "nonce"            -> "0x0000000000000000",
      "sha3Uncles"       -> "0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347",
      "logsBloom"        -> ("0x" + zeroes(512)),
      "transactionsRoot" -> toHexString(header.transactionsRoot.arr),
      "stateRoot"        -> header.stateHash.fold("0x" + zeroes(64))(s => toHexString(s.arr)),
      "receiptsRoot"     -> toHexString(header.transactionsRoot.arr),
      "miner"            -> minerEthAddress,
      "difficulty"       -> "0x1",
      "totalDifficulty"  -> "0x1",
      "extraData"        -> "0x",
      "size"             -> "0x400",
      "gasLimit"         -> "0x1c9c380", // 30,000,000
      "gasUsed"          -> "0x0",
      "timestamp"        -> timestampSeconds,
      "transactions"     -> Json.arr(),
      "uncles"           -> Json.arr(),
      "baseFeePerGas"    -> "0x0"
    )
  }

  private def formatEmptyBlock(height: Int): JsObject = {
    Json.obj(
      "number"           -> quantity(height),
      "hash"             -> ("0x" + zeroes(64)),
      "parentHash"       -> ("0x" + zeroes(64)),
      "nonce"            -> "0x0000000000000000",
      "sha3Uncles"       -> "0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347",
      "logsBloom"        -> ("0x" + zeroes(512)),
      "transactionsRoot" -> ("0x" + zeroes(64)),
      "stateRoot"        -> ("0x" + zeroes(64)),
      "receiptsRoot"     -> ("0x" + zeroes(64)),
      "miner"            -> "0x0000000000000000000000000000000000000000",
      "difficulty"       -> "0x1",
      "totalDifficulty"  -> "0x1",
      "extraData"        -> "0x",
      "size"             -> "0x400",
      "gasLimit"         -> "0x1c9c380",
      "gasUsed"          -> "0x0",
      "timestamp"        -> quantity(time.correctedTime() / 1000),
      "transactions"     -> Json.arr(),
      "uncles"           -> Json.arr(),
      "baseFeePerGas"    -> "0x0"
    )
  }

  private def extractParam1[A: Reads](jso: JsObject)(f: A => Future[JsObject]): Future[JsObject] = {
    val id = (jso \ "id").getOrElse(JsNull)
    (jso \ "params" \ 0).asOpt[A] match {
      case Some(v) => f(v)
      case None    => Future.successful(apiErrorToRpcError(id, GenericError("Error extracting required parameter")))
    }
  }

  private def apiErrorToRpcError(id: JsValue, err: com.wavesplatform.lang.ValidationError): JsObject = {
    val apiErr = ApiError.fromValidationError(err)
    Json.obj(
      "id"      -> id,
      "jsonrpc" -> "2.0",
      "error"   -> apiErr.id,
      "message" -> apiErr.message
    )
  }

  @inline
  private def zeroes(n: Int): String = "0".repeat(n)

  @inline
  private def quantity(v: Long) = "0x" + java.lang.Long.toString(v, 16)

  private def respObj(id: JsValue, result: JsValueWrapper): JsObject =
    Json.obj("id" -> id, "jsonrpc" -> "2.0", "result" -> result)

  private def assetDescription(contractAddress: String) =
    assetId(contractAddress).flatMap(blockchain.assetDescription)

  private def assetId(contractAddress: String): Option[IssuedAsset] =
    Try(ERC20Address(ByteStr(toBytes(contractAddress)))).toOption.flatMap(blockchain.resolveERC20Address)

  private def encodeResponse(values: org.web3j.abi.datatypes.Type[?]*): String =
    "0x" + FunctionEncoder.encodeConstructor(values.asJava)

  private def resolveAccountAddress(addressStr: String): Option[Address] = {
    val cleanStr = cleanHexPrefix(addressStr)
    if (addressStr.startsWith("0x") || cleanStr.length == 40) {
      Try(Address.fromHexString(cleanStr)).toOption
    } else {
      Address.fromString(addressStr).toOption
    }
  }

  private def formatDataEntryToStorage(entry: DataEntry[?]): String = {
    entry match {
      case IntegerDataEntry(_, value) =>
        "0x" + String.format("%064x", BigInteger.valueOf(value))
      case BooleanDataEntry(_, value) =>
        "0x" + zeroes(63) + (if (value) "1" else "0")
      case BinaryDataEntry(_, value) =>
        val hex = toHexString(value.arr).drop(2)
        if (hex.length >= 64) "0x" + hex.take(64)
        else "0x" + hex + zeroes(64 - hex.length)
      case StringDataEntry(_, value) =>
        val hex = toHexString(value.getBytes(java.nio.charset.StandardCharsets.UTF_8)).drop(2)
        if (hex.length >= 64) "0x" + hex.take(64)
        else "0x" + hex + zeroes(64 - hex.length)
      case _ =>
        "0x" + zeroes(64)
    }
  }

  private def extractTransaction(transactionHex: String) = TransactionDecoder.decode(transactionHex) match {
    case srt: SignedRawTransaction => EthereumTransaction(srt)
    case _: RawTransaction         => throw new UnsupportedOperationException("Cannot process unsigned transactions")
  }
}
