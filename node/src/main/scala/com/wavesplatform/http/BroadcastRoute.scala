package com.wavesplatform.http

import com.wavesplatform.api.http.ApiError
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network._
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup

trait BroadcastRoute {
  def utx: UtxPool
  def allChannels: ChannelGroup

  protected def doBroadcast(v: Either[ValidationError, Transaction]): TracedResult[ApiError, Transaction] = {
    val r = for {
      tx <- TracedResult(v)
      r  <- utx.putIfNewTraced(tx)
    } yield {
      val (added, _) = r
      if (added) allChannels.broadcastTx(tx, None)
      tx
    }

    r.leftMap(ApiError.fromValidationError)
  }
}
