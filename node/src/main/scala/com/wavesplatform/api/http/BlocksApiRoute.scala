package com.wavesplatform.api.http

import akka.http.scaladsl.server.{Route, StandardRoute}
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.api.common.{CommonBlocksApi, CommonTransactionsApi}
import com.wavesplatform.api.http.ApiError.{BlockDoesNotExist, TooBigArrayAllocation}
import com.wavesplatform.api.http.TransactionsApiRoute.TransactionJsonSerializer
import com.wavesplatform.block.Block
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Transaction
import play.api.libs.json._

case class BlocksApiRoute(settings: RestAPISettings, commonApi: CommonBlocksApi, transactionsApi: CommonTransactionsApi, blockchain: Blockchain) extends ApiRoute {
  private[this] val MaxBlocksPerRequest = 100 // todo: make this configurable and fix integration tests

  private[this] val serializer = TransactionJsonSerializer(blockchain, transactionsApi)

  override lazy val route: Route = (pathPrefix("blocks") & get) {
    path("at" / IntNumber) { height =>
      at(height, includeTransactions = true)
    } ~ path("first") {
      at(1, includeTransactions = true)
    } ~ path("seq" / IntNumber / IntNumber) { (start, end) =>
      seq(start, end, includeTransactions = true)
    } ~ path("last") {
      at(commonApi.currentHeight, includeTransactions = true)
    } ~ path("height") {
      complete(Json.obj("height" -> commonApi.currentHeight))
    } ~ path("delay" / BlockId / IntNumber) { (blockId, count) =>
      complete(
        commonApi
          .blockDelay(blockId, count)
          .map(delay => Json.obj("delay" -> delay))
          .toRight(BlockDoesNotExist)
      )
    } ~ path("height" / BlockId) { signature =>
      complete(for {
        meta <- commonApi.meta(signature).toRight(BlockDoesNotExist)
      } yield Json.obj("height" -> meta.height))
    } ~ path("signature" / BlockId) { signature => // TODO: Delete
      complete(commonApi.block(signature).map(toJson).toRight(BlockDoesNotExist))
    } ~ path("address" / AddrSegment / IntNumber / IntNumber) { (address, start, end) =>
      if (end >= 0 && start >= 0 && end - start >= 0 && end - start < MaxBlocksPerRequest) extractScheduler { implicit ec =>
        complete(
          commonApi
            .blocksRange(start, end, address)
            .map(toJson)
            .toListL
            .runToFuture
        )
      } else {
        complete(TooBigArrayAllocation)
      }
    } ~ pathPrefix("headers") {
      path("at" / IntNumber) { height =>
        at(height, includeTransactions = false)
      } ~ path("seq" / IntNumber / IntNumber) { (start, end) =>
        seq(start, end, includeTransactions = false)
      } ~ path("last") {
        at(commonApi.currentHeight, includeTransactions = false)
      } ~ path(BlockId) { id =>
        complete(commonApi.meta(id).map(_.json()).toRight(BlockDoesNotExist))
      }
    } ~ path(BlockId) { id =>
      complete(commonApi.block(id).map(toJson).toRight(BlockDoesNotExist))
    }
  }

  private def at(height: Int, includeTransactions: Boolean): StandardRoute = complete {
    if (includeTransactions) commonApi.blockAtHeight(height).map(toJson) else commonApi.metaAtHeight(height).map(_.json())
  }

  private def seq(start: Int, end: Int, includeTransactions: Boolean): Route = {
    if (end >= 0 && start >= 0 && end - start >= 0 && end - start < MaxBlocksPerRequest) {
      val blocks = if (includeTransactions) {
        commonApi
          .blocksRange(start, end)
          .map(toJson)
      } else {
        commonApi
          .metaRange(start, end)
          .map(_.json())
      }

      extractScheduler(implicit sc => complete(blocks.toListL.map(JsArray(_)).runToFuture))
    } else {
      complete(TooBigArrayAllocation)
    }
  }

  private def toJson(v: (BlockMeta, Seq[(Transaction, Boolean)])): JsObject = v match {
    case (meta, transactions) =>
      meta.json() ++ transactionField(meta.header.version, transactions)
  }

  private def transactionField(blockVersion: Byte, transactions: Seq[(Transaction, Boolean)]): JsObject = Json.obj(
    "fee" -> transactions.map(_._1.assetFee).collect { case (Waves, feeAmt) => feeAmt }.sum,
    "transactions" -> JsArray(transactions.map {
      case (transaction, succeeded) =>
        transaction.json() ++
          serializer.resolvedAliasTxFields(transaction) ++
          TransactionJsonSerializer.applicationStatus(blockVersion >= Block.ProtoBlockVersion, succeeded)
    })
  )
}
