package com.wavesplatform.api.http

import akka.http.scaladsl.server.{Route, StandardRoute}
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.api.http.ApiError.{BlockDoesNotExist, CustomValidationError, InvalidSignature, TooBigArrayAllocation}
import com.wavesplatform.block.Block.BlockInfo
import com.wavesplatform.block.serialization.BlockHeaderSerializer
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json._

case class BlocksApiRoute(settings: RestAPISettings, blockchain: Blockchain) extends ApiRoute with CommonApiFunctions {
  private[this] val MaxBlocksPerRequest = 100 // todo: make this configurable and fix integration tests
  private[this] val commonApi           = new CommonBlocksApi(blockchain)

  override lazy val route: Route =
    pathPrefix("blocks") {
      signature ~ first ~ last ~ lastHeaderOnly ~ at ~ atHeaderOnly ~ seq ~ seqHeaderOnly ~ height ~ heightEncoded ~ child ~ address ~ delay
    }

  def address: Route =
    extractScheduler(
      implicit sc =>
        (path("address" / Segment / IntNumber / IntNumber) & get) {
          case (address, start, end) =>
            if (end >= 0 && start >= 0 && end - start >= 0 && end - start < MaxBlocksPerRequest) {
              val result = for {
                address <- Address.fromString(address)
                jsonBlocks = commonApi
                  .blockHeadersRange(start, end)
                  .filter(_._1.header.generator.toAddress == address)
                  .map {
                    case (_, h) =>
                      blockchain.blockAt(h).get.json().addBlockFields(h)
                  }
                result = jsonBlocks.toListL.map(JsArray(_))
              } yield result.runToFuture

              complete(result)
            } else {
              complete(TooBigArrayAllocation)
            }
        }
    )

  def child: Route = (path("child" / Segment) & get) { encodedSignature =>
    withBlock(blockchain, encodedSignature) { block =>
      val childJson =
        for ((child, height) <- commonApi.childBlock(block.uniqueId))
          yield child.json().addBlockFields(height)

      complete(childJson.getOrElse[JsObject](Json.obj("status" -> "error", "details" -> "No child blocks")))
    }
  }

  def delay: Route = (path("delay" / Segment / IntNumber) & get) { (encodedSignature, count) =>
    withBlock(blockchain, encodedSignature) { block =>
      val result = if (count <= 0) {
        Left(CustomValidationError("Block count should be positive"))
      } else {
        commonApi
          .calcBlocksDelay(block.uniqueId, count)
          .map(delay => Json.obj("delay" -> delay))
      }

      complete(result)
    }
  }

  def heightEncoded: Route = (path("height" / Segment) & get) { encodedSignature =>
    if (encodedSignature.length > requests.SignatureStringLength)
      complete(InvalidSignature)
    else {
      val result: Either[ApiError, JsObject] = for {
        signature <- ByteStr
          .decodeBase58(encodedSignature)
          .toOption
          .toRight(InvalidSignature)

        height <- commonApi.blockHeight(signature).toRight(BlockDoesNotExist)
      } yield Json.obj("height" -> height)

      complete(result)
    }
  }

  def height: Route = (path("height") & get) {
    complete(Json.obj("height" -> commonApi.currentHeight()))
  }

  def at: Route = (path("at" / IntNumber) & get)(at(_, includeTransactions = true))

  def atHeaderOnly: Route = (path("headers" / "at" / IntNumber) & get)(at(_, includeTransactions = false))

  private def at(height: Int, includeTransactions: Boolean): StandardRoute = {

    (if (includeTransactions) {
       commonApi.blockAtHeight(height).map(_.json())
     } else {
       commonApi.blockHeaderAtHeight(height).map { case BlockInfo(bh, s, tc, sig) => BlockHeaderSerializer.toJson(bh, s, tc, sig) }
     }) match {
      case Some(json) => complete(json.addBlockFields(height))
      case None       => complete(Json.obj("status" -> "error", "details" -> "No block for this height"))
    }
  }

  def seq: Route = (path("seq" / IntNumber / IntNumber) & get) { (start, end) =>
    seq(start, end, includeTransactions = true)
  }

  def seqHeaderOnly: Route = (path("headers" / "seq" / IntNumber / IntNumber) & get) { (start, end) =>
    seq(start, end, includeTransactions = false)
  }

  private def seq(start: Int, end: Int, includeTransactions: Boolean): Route = {
    if (end >= 0 && start >= 0 && end - start >= 0 && end - start < MaxBlocksPerRequest) {
      val blocks = if (includeTransactions) {
        commonApi
          .blocksRange(start, end)
          .map(bh => bh._1.json().addBlockFields(bh._2))
      } else {
        commonApi
          .blockHeadersRange(start, end)
          .map {
            case (BlockInfo(bh, size, transactionCount, signature), height) =>
              BlockHeaderSerializer.toJson(bh, size, transactionCount, signature).addBlockFields(height)
          }
      }

      extractScheduler(implicit sc => complete(blocks.toListL.map(JsArray(_)).runToFuture))
    } else {
      complete(TooBigArrayAllocation)
    }
  }

  def last: Route = (path("last") & get)(last(includeTransactions = true))

  def lastHeaderOnly: Route = (path("headers" / "last") & get)(last(includeTransactions = false))

  def last(includeTransactions: Boolean): StandardRoute = {
    complete {
      val height = blockchain.height
      (if (includeTransactions) {
         commonApi.lastBlock().map(_.json())
       } else {
         commonApi
           .lastBlock()
           .map(block => BlockHeaderSerializer.toJson(block.header, block.bytes().length, block.transactionData.size, block.signature))
       }).map(_.addBlockFields(height))
    }
  }

  def first: Route = (path("first") & get) {
    complete(commonApi.firstBlock().json().addBlockFields(1))
  }

  def signature: Route = (path("signature" / Segment) & get) { encodedSignature =>
    if (encodedSignature.length > requests.SignatureStringLength) {
      complete(InvalidSignature)
    } else {
      val result = for {
        blockId <- ByteStr
          .decodeBase58(encodedSignature)
          .toOption
          .toRight(InvalidSignature)

        block <- commonApi.blockBySignature(blockId).toRight(BlockDoesNotExist)
      } yield block.json().addBlockFields(block.uniqueId)

      complete(result)
    }
  }

  private[this] implicit class JsonObjectOps(json: JsObject) {
    import com.wavesplatform.features.FeatureProvider._

    def addBlockFields(blockId: ByteStr): JsObject =
      json ++ blockchain
        .heightOf(blockId)
        .map(height => createFields(height))
        .getOrElse(JsObject.empty)

    def addBlockFields(height: Int): JsObject =
      json ++ createFields(height)

    private[this] def createFields(height: Int) = {
      def optional(feature: BlockchainFeature)(fields: => Seq[(String, JsValueWrapper)]): JsObject =
        (if (blockchain.isFeatureActivated(feature, height)) Json.obj(fields:_*) else Json.obj())

      Seq(
        Json.obj(
          "height"   -> height,
          "totalFee" -> blockchain.totalFee(height).fold(JsNull: JsValue)(JsNumber(_))
        ),
        optional(BlockchainFeatures.BlockReward)(Seq("reward" -> blockchain.blockReward(height).fold(JsNull: JsValue)(JsNumber(_)))),
        optional(BlockchainFeatures.BlockV5)(Seq("VRF" -> blockchain.hitSourceAtHeight(height).fold(JsNull: JsValue)(bs => JsString(bs.toString))))
      ).reduce(_ ++ _)
    }
  }
}
