package com.wavesplatform.api.http

import scala.annotation.tailrec
import scala.concurrent.duration.{FiniteDuration, _}

import akka.http.scaladsl.server.{Route, StandardRoute}
import cats.syntax.either._
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.api.http.ApiError.{BlockDoesNotExist, TooBigArrayAllocation}
import com.wavesplatform.api.http.TransactionsApiRoute.TransactionJsonSerializer
import com.wavesplatform.block.Block
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxValidationError.GenericError
import monix.eval.Task
import play.api.libs.json._

case class BlocksApiRoute(settings: RestAPISettings, commonApi: CommonBlocksApi, avgBlockTime: FiniteDuration = 1 minute) extends ApiRoute {
  import BlocksApiRoute._
  private[this] val MaxBlocksPerRequest = 100 // todo: make this configurable and fix integration tests

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
    } ~ path("heightByTimestamp" / LongNumber) { timestamp =>
      val task = for {
        _ <- Task(Either.cond(timestamp > System.currentTimeMillis(), (), "Indicated timestamp belongs to the future"))
        genesisTimestamp = commonApi.metaAtHeight(1).fold(0L)(_.header.timestamp)
        _      <- Task(Either.cond(timestamp <= genesisTimestamp, (), "Indicated timestamp belongs to the future"))
        result <- heightByTimestamp(timestamp)
      } yield result.leftMap(GenericError(_))

      extractScheduler(implicit sc => complete(task.runToFuture))
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

  private[this] def heightByTimestamp(target: Long): Task[Either[String, Int]] = {
    sealed trait State
    case object Starting                 extends State
    case class Scanning(prevHeight: Int) extends State
    sealed trait Result                  extends State
    case class HeightFound(height: Int)  extends Result
    case class Error(message: String)    extends Result

    @tailrec
    def heuristicStartHeight(seekHeight: Int): Int = {
      assert(seekHeight >= 1)

      val timestamp = commonApi.metaAtHeight(seekHeight).fold(0L)(_.header.timestamp)
      if (timestamp <= target || seekHeight == 1) seekHeight
      else {
        val predicted = seekHeight - ((timestamp - target) / avgBlockTime.toMillis).toInt.max(1)
        heuristicStartHeight(predicted max 1)
      }
    }

    val startHeight = heuristicStartHeight(commonApi.currentHeight)
    commonApi
      .metaRange(startHeight, commonApi.currentHeight)
      .map(b => (b.header.timestamp, b.height))
      .:+((Long.MaxValue, 1))
      .scan(Starting: State) {
        case (Starting, (_, firstHeight)) =>
          Scanning(firstHeight)

        case (Scanning(prevHeight), (blockTimestamp, blockHeight)) =>
          if (blockTimestamp > target) HeightFound(prevHeight)
          else Scanning(blockHeight)
      }
      .collect {
        case HeightFound(height) => Right(height)
        case Error(err)          => Left(err)
      }
      .firstL
  }
}

object BlocksApiRoute {
  private def toJson(v: (BlockMeta, Seq[(Transaction, Boolean)])): JsObject = v match {
    case (meta, transactions) =>
      meta.json() ++ transactionField(meta.header.version, transactions)
  }

  private def transactionField(blockVersion: Byte, transactions: Seq[(Transaction, Boolean)]): JsObject = Json.obj(
    "fee" -> transactions.map(_._1.assetFee).collect { case (Waves, feeAmt) => feeAmt }.sum,
    "transactions" -> JsArray(transactions.map {
      case (transaction, succeeded) =>
        transaction.json() ++ TransactionJsonSerializer.applicationStatus(blockVersion >= Block.ProtoBlockVersion, succeeded)
    })
  )
}
