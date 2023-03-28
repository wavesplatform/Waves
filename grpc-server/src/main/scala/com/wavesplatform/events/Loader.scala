package com.wavesplatform.events

import com.google.common.primitives.Ints
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.api.grpc.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.database.{DBExt, DBResource}
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated as PBBlockchainUpdated
import com.wavesplatform.protobuf.*
import com.wavesplatform.protobuf.block.PBBlock
import com.wavesplatform.utils.ScorexLogging
import monix.reactive.Observable
import org.rocksdb.RocksDB

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

class Loader(db: RocksDB, blocksApi: CommonBlocksApi, target: Option[(Int, ByteStr)], streamId: String) extends ScorexLogging {
  private def loadBatch(res: DBResource, fromHeight: Int): Try[Seq[PBBlockchainUpdated]] = Try {
    res.fullIterator.seek(Ints.toByteArray(fromHeight))
    val buffer = ArrayBuffer[PBBlockchainUpdated]()

    while (res.fullIterator.isValid && buffer.size < 100 && target.forall { case (h, _) => fromHeight + buffer.size <= h }) {
      buffer.append(Loader.parseUpdate(res.fullIterator.value(), blocksApi, fromHeight + buffer.size))
      res.fullIterator.next()
    }

    for ((h, id) <- target if h == fromHeight + buffer.size - 1; u <- buffer.lastOption) {
      require(
        u.id.toByteArray.sameElements(id.arr),
        s"Stored update ${Base58.encode(u.id.toByteArray)} at ${u.height} does not match target $id at $h"
      )
    }

    buffer.toSeq
  }

  private def streamFrom(fromHeight: Int): Observable[PBBlockchainUpdated] = db.resourceObservable.flatMap { res =>
    loadBatch(res, fromHeight) match {
      case Success(nextBatch) =>
        if (nextBatch.isEmpty) Observable.empty[PBBlockchainUpdated]
        else Observable.fromIterable(nextBatch) ++ streamFrom(fromHeight + nextBatch.size)
      case Failure(exception) => Observable.raiseError(exception)
    }
  }

  def loadUpdates(fromHeight: Int): Observable[PBBlockchainUpdated] = {
    log.trace(s"[$streamId] Loading stored updates from $fromHeight up to ${target.fold("the most recent one") { case (h, id) => s"$id at $h" }}")
    streamFrom(fromHeight)
  }
}

object Loader {
  def parseUpdate(bs: Array[Byte], blocksApi: CommonBlocksApi, height: Int): PBBlockchainUpdated =
    PBBlockchainUpdated
      .parseFrom(bs)
      .update(
        _.append.update(
          _.body.modify {
            case Body.Block(value) =>
              Body.Block(value.copy(block = blocksApi.blockAtHeight(height).map { case (meta, txs) =>
                PBBlock(Some(meta.header.toPBHeader), meta.signature.toByteString, txs.map(_._2.toPB))
              }))
            case other => other
          }
        )
      )

  def loadUpdate(res: DBResource, blocksApi: CommonBlocksApi, height: Int): PBBlockchainUpdated =
    parseUpdate(res.get(Repo.keyForHeight(height)), blocksApi, height)

}
