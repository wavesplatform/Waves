package com.wavesplatform.generator

import com.wavesplatform.network.RawBytes
import com.wavesplatform.transaction.Transaction
import io.netty.channel.Channel
import io.netty.util.concurrent.{Future => NFuture}
import monix.eval.Task

import scala.concurrent.Promise

class NewWorker(transactionSource: Iterable[Transaction], channel: Channel, canContinue: => Boolean) {
  def utxSpace: Task[Int] = Task(0)
  def writeTransactions(txs: Iterable[Transaction]): Task[Unit] = Task.fromFuture {
    val p = Promise[Unit]()
    // txCount will only be decremented from one thread, so there's no need
    @volatile var txCount = 0
    txs.foreach { tx =>
      txCount += 1
      channel.write(RawBytes.from(tx)).addListener { _: NFuture[_] =>
        txCount -= 1
        if (txCount == 0) p.success(())
      }
    }
    p.future
  }

  val tt: Task[Unit] = if (!canContinue) Task(()) else for {
    txCount <- utxSpace
    _ <- writeTransactions(transactionSource.take(txCount))
    _ <- tt
  } yield ()
}
