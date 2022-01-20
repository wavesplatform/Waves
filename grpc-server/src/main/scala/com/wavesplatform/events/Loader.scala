package com.wavesplatform.events

import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.api.grpc._
import com.wavesplatform.database.DBResource
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.{BlockchainUpdated => PBBlockchainUpdated}
import com.wavesplatform.protobuf._
import com.wavesplatform.protobuf.block.PBBlock
import com.wavesplatform.utils.ScorexLogging
import monix.reactive.Observable

import scala.util.{Failure, Success, Try}

class Loader(loadBatch: Int => Observable[Try[Seq[PBBlockchainUpdated]]]) extends ScorexLogging {
  private def streamFrom(fromHeight: Int): Observable[PBBlockchainUpdated] =
    loadBatch(fromHeight).flatMap {
      case Success(nextBatch) =>
        if (nextBatch.isEmpty) Observable.empty[PBBlockchainUpdated]
        else Observable.fromIterable(nextBatch) ++ streamFrom(fromHeight + nextBatch.size)
      case Failure(exception) =>
        Observable.raiseError(exception)
    }

  def loadUpdates(fromHeight: Int): Observable[PBBlockchainUpdated] =
    streamFrom(fromHeight)
}

object Loader {
  def parseUpdate(bs: Array[Byte], blocksApi: CommonBlocksApi, height: Int): PBBlockchainUpdated =
    PBBlockchainUpdated
      .parseFrom(bs)
      .update(
        _.append.update(
          _.body.modify {
            case Body.Block(value) =>
              Body.Block(value.copy(block = blocksApi.blockAtHeight(height).map {
                case (meta, txs) => PBBlock(Some(meta.header.toPBHeader), meta.signature.toByteString, txs.map(_._2.toPB))
              }))
            case other => other
          }
        )
      )

  def loadUpdate(res: DBResource, blocksApi: CommonBlocksApi, height: Int): PBBlockchainUpdated =
    parseUpdate(res.get(Repo.keyForHeight(height)), blocksApi, height)

}
