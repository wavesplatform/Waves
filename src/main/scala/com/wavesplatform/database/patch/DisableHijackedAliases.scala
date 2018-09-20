package com.wavesplatform.database.patch

import java.nio.ByteBuffer
import java.util

import com.google.common.primitives.Shorts
import com.wavesplatform.database.{Keys, RW}
import com.wavesplatform.account.Alias
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.block.BlockHeader
import com.wavesplatform.transaction.{CreateAliasTransaction, TransactionParsers}

import scala.collection.JavaConverters._

object DisableHijackedAliases extends ScorexLogging {
  private val txBuffer = new Array[Byte](150 * 1024)
  def apply(rw: RW): Unit = {
    log.info("Collecting hijacked aliases")
    val aliases = new util.HashMap[Alias, Seq[CreateAliasTransaction]]()
    val height  = rw.get(Keys.height)

    for (h <- 1 to height) {
      val (header, txBytes) = BlockHeader.parseBytes(rw.get(Keys.blockBytes(h)).get).get
      if (header.transactionCount > 0) {
        val buffer = ByteBuffer.wrap(txBytes)
        header.version match {
          case 1 | 2 => buffer.get()
          case 3     => buffer.getInt
        }
        while (buffer.hasRemaining) {
          val length = buffer.getInt
          buffer.mark()
          val txType = buffer.get()
          if (txType == CreateAliasTransaction.typeId || (txType == 0 && buffer.get() == CreateAliasTransaction.typeId)) {
            buffer.reset()
            buffer.get(txBuffer, 0, length)
            TransactionParsers.parseBytes(txBuffer).get match {
              case cat: CreateAliasTransaction => aliases.compute(cat.alias, (_, prevTx) => Option(prevTx).fold(Seq(cat))(_ :+ cat))
            }
          } else {
            buffer.reset()
            val newPosition = buffer.position() + length
            buffer.position(newPosition)
          }
        }
      }
    }

    val hijackedAliases = for {
      (alias, txs) <- aliases.asScala
      if txs.size > 1
    } yield alias

    log.info(s"Collected ${aliases.size()} aliases, of which ${hijackedAliases.size} were hijacked")

    for (alias <- hijackedAliases) rw.put(Keys.aliasIsDisabled(alias), true)
  }

  def revert(rw: RW): Unit = {
    log.info("Discarding the list of hijacked aliases")
    val prefixBytes = Shorts.toByteArray(Keys.AliasIsDisabledPrefix)
    val iterator    = rw.iterator

    try {
      iterator.seek(prefixBytes)
      while (iterator.hasNext && iterator.peekNext().getKey.startsWith(prefixBytes)) {
        rw.delete(iterator.next().getKey)
      }
    } finally {
      iterator.close()
    }
  }
}
