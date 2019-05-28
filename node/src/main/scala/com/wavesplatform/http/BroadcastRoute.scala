package com.wavesplatform.http

import com.wavesplatform.api.http.{ApiError, WithSettings}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network._
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup

trait BroadcastRoute {
  self: WithSettings =>
  def utx: UtxPool
  def allChannels: ChannelGroup

  protected def doBroadcast(v: Either[ValidationError, Transaction]): TracedResult[ApiError, Transaction] = {
    val result = for {
      transaction <- TracedResult(v)
      isNew <- utx.putIfNew(transaction)
    } yield {
      if (isNew || settings.allowTxRebroadcasting) allChannels.broadcastTx(transaction, None)
      transaction
    }

    result.leftMap(ApiError.fromValidationError)
  }
}
