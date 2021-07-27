package com.wavesplatform.events

import com.google.common.primitives.Ints
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{DBExt, DBResource}
import com.wavesplatform.events.protobuf.{BlockchainUpdated => PBBlockchainUpdated}
import com.wavesplatform.utils.ScorexLogging
import monix.reactive.Observable
import org.iq80.leveldb.DB

import scala.util.{Failure, Success, Try}

class Loader(db: DB, toHeight: Int, toBlockId: ByteStr) extends ScorexLogging {
  private def loadBatch(res: DBResource, fromHeight: Int): Try[Seq[PBBlockchainUpdated]] =
    if (fromHeight > toHeight) Success(Seq.empty)
    else
      Try((fromHeight to (fromHeight + 100).min(toHeight)).map { h =>
        log.debug(s"Loading update at $h")
        val u = PBBlockchainUpdated.parseFrom(res.get(Ints.toByteArray(h)))

        if (u.height == toHeight) require(u.id.toByteArray.sameElements(toBlockId.arr))

        u
      })

  def streamFrom(fromHeight: Int): Observable[PBBlockchainUpdated] = db.resourceObservable.flatMap { res =>
    loadBatch(res, fromHeight) match {
      case Success(nextBatch) =>
        if (nextBatch.isEmpty) Observable.empty[PBBlockchainUpdated]
        else Observable.fromIterable(nextBatch) ++ streamFrom(fromHeight + nextBatch.size)
      case Failure(exception) => Observable.raiseError(exception)
    }

  }
}
