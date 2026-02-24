package com.wavesplatform.api.http

import cats.syntax.either.*
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.api.http.ApiError.{BlockDoesNotExist, NotFinalized, TooBigArrayAllocation}
import com.wavesplatform.block.Block
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.{Height, TxMeta}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils.Time
import org.apache.pekko.http.scaladsl.server.{Route, StandardRoute}
import play.api.libs.json.*

import scala.annotation.tailrec
import scala.util.Try

case class BlocksApiRoute(settings: RestAPISettings, commonApi: CommonBlocksApi, time: Time, routeTimeout: RouteTimeout) extends ApiRoute {
  import BlocksApiRoute.*

  override lazy val route: Route = (pathPrefix("blocks") & get) {
    path("at" / IntNumber) { height =>
      at(Height(height), includeTransactions = true)
    } ~ path("seq" / IntNumber / IntNumber) { (start, end) =>
      seq(Height(start), Height(end), includeTransactions = true)
    } ~ path("last") {
      at(commonApi.currentHeight, includeTransactions = true)
    } ~ path("height") {
      complete(Json.obj("height" -> commonApi.currentHeight.toInt))
    } ~ path("height" / "finalized") {
      complete(Json.obj("height" -> commonApi.currentFinalizedHeight))
    } ~ path("delay" / BlockId / IntNumber) { (blockId, count) =>
      if (count > MaxBlocksForDelay) {
        complete(TooBigArrayAllocation(MaxBlocksForDelay))
      } else {
        complete(
          commonApi
            .blockDelay(blockId(), count)
            .map(delay => Json.obj("delay" -> delay))
            .toRight(BlockDoesNotExist)
        )
      }
    } ~ path("height" / BlockId) { signature =>
      complete(for {
        meta <- commonApi.meta(signature()).toRight(BlockDoesNotExist)
      } yield Json.obj("height" -> meta.height))
    } ~ path("address" / AddrSegment / IntNumber / IntNumber) { (address, start, end) =>
      if (end >= 0 && start >= 0 && end - start >= 0 && end - start < settings.blocksRequestLimit) {
        routeTimeout.executeToFuture {
          commonApi
            .blocksRange(Height(start), Height(end), address)
            .map(toJson)
            .toListL
        }
      } else {
        complete(TooBigArrayAllocation)
      }
    } ~ pathPrefix("headers") {
      path("at" / IntNumber) { height =>
        at(Height(height), includeTransactions = false)
      } ~ path("seq" / IntNumber / IntNumber) { (start, end) =>
        seq(Height(start), Height(end), includeTransactions = false)
      } ~ path("last") {
        at(commonApi.currentHeight, includeTransactions = false)
      } ~ path("finalized") {
        at(commonApi.currentFinalizedHeight, includeTransactions = false)
      } ~ path(BlockId) { id =>
        complete(commonApi.meta(id()).map(_.json()).toRight(BlockDoesNotExist))
      }
    } ~ path("finalized" / "at" / IntNumber) { height =>
      complete {
        commonApi
          .finalizedHeightAt(Height(height))
          .map(h => Json.obj("height" -> h))
          .toRight(NotFinalized)
      }
    } ~ path("heightByTimestamp" / LongNumber) { timestamp =>
      val heightE = for {
        _ <- Either.cond(timestamp <= time.correctedTime(), (), "Indicated timestamp belongs to the future")
        genesisTimestamp = commonApi.metaAtHeight(Height(1)).fold(0L)(_.header.timestamp)
        _      <- Either.cond(timestamp >= genesisTimestamp, (), "Indicated timestamp is before the start of the blockchain")
        result <- Try(heightByTimestamp(timestamp)).toEither.leftMap(_.getMessage)
      } yield result

      complete(heightE.bimap(GenericError(_), h => Json.obj("height" -> h.toInt)))
    } ~ path(BlockId) { id =>
      complete(commonApi.block(id()).map(toJson).toRight(BlockDoesNotExist))
    }
  }

  private def at(height: Height, includeTransactions: Boolean): StandardRoute = {
    val result =
      if (includeTransactions)
        commonApi.blockAtHeight(height).map(toJson)
      else
        commonApi.metaAtHeight(height).map(_.json())

    complete(result.toRight(BlockDoesNotExist))
  }

  private def seq(start: Height, end: Height, includeTransactions: Boolean): Route = {
    if (end >= Height(0) && start >= Height(0) && end - start >= 0 && end - start < settings.blocksRequestLimit) {
      routeTimeout.executeToFuture {
        val blocks = if (includeTransactions) {
          commonApi
            .blocksRange(start, end)
            .map(toJson)
        } else {
          commonApi
            .metaRange(start, end)
            .map(_.json())
        }
        blocks.toListL.map(JsArray(_))
      }
    } else {
      complete(TooBigArrayAllocation)
    }
  }

  @throws[IllegalStateException]("if the state is altered while executing")
  private def heightByTimestamp(target: Long): Height = {
    def timestampOf(height: Height, default: => Long = throw new IllegalStateException("State was altered")): Long =
      commonApi.metaAtHeight(height).fold(default)(_.header.timestamp)

    @tailrec
    def findHeightRec(lowerBound: Height = Height(1), upperBound: Height = commonApi.currentHeight): Height = {
      val lowerTimestamp = timestampOf(lowerBound)
      val upperTimestamp = timestampOf(upperBound)

      require(lowerTimestamp <= target)

      val averageBlockTime = (upperTimestamp - lowerTimestamp) / (upperBound - lowerBound).max(1)
      val offset = {
        val blocksBetween = ((target - lowerTimestamp) / averageBlockTime).toInt
        blocksBetween
      }

      val predictedHeight = Height((lowerBound + offset).max(lowerBound).toInt.min(upperBound.toInt))

      val timestamp      = timestampOf(predictedHeight)
      val rightTimestamp = timestampOf(predictedHeight + 1, Long.MaxValue)
      val leftHit        = timestamp <= target
      val rightHit       = rightTimestamp <= target

      val (newLower, newUpper) = {
        if (!leftHit) (lowerBound, (predictedHeight - 1).max(lowerBound))
        else if (rightHit) (Height((predictedHeight + 1).toInt.min(upperBound.toInt)), upperBound)
        else (predictedHeight, predictedHeight)
      }

      if (newLower == newUpper) predictedHeight else findHeightRec(newLower, newUpper)
    }

    findHeightRec()
  }
}

object BlocksApiRoute {
  val MaxBlocksForDelay = 10000

  private def toJson(v: (BlockMeta, Seq[(TxMeta, Transaction)])): JsObject = v match {
    case (meta, transactions) =>
      meta.json() ++ transactionField(meta.header.version, transactions)
  }

  private def transactionField(blockVersion: Byte, transactions: Seq[(TxMeta, Transaction)]): JsObject = Json.obj(
    "fee" -> transactions.map(_._2.assetFee).collect { case (Waves, feeAmt) => feeAmt }.sum,
    "transactions" -> JsArray(transactions.map { case (tm, transaction) =>
      transaction.json() ++ TransactionJsonSerializer.applicationStatus(blockVersion >= Block.ProtoBlockVersion, tm.status)
    })
  )
}
