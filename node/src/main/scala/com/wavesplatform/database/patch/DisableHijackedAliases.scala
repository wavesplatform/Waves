package com.wavesplatform.database.patch

import java.util

import com.google.common.primitives.Shorts
import com.wavesplatform.account.Alias
import com.wavesplatform.database.{BlocksWriter, Keys, RW}
import com.wavesplatform.state._
import com.wavesplatform.transaction.CreateAliasTransaction
import com.wavesplatform.utils.ScorexLogging

import scala.collection.JavaConverters._

object DisableHijackedAliases extends ScorexLogging {
  def apply(rw: RW, bw: BlocksWriter): Unit = {
    log.info("Collecting hijacked aliases")
    val aliases = new util.HashMap[Alias, Seq[CreateAliasTransaction]]()
    val height  = Height(rw.get(Keys.height))

    for (h <- 1 until height) {
      val block = bw.getBlock(Height @@ h, withTxs = true)
      for (tx <- block.transactionData) tx match {
        case cat: CreateAliasTransaction =>
          aliases.compute(cat.alias, (_, prevTx) => Option(prevTx).fold(Seq(cat))(_ :+ cat))

        case _ =>
          // Ignore
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
        rw.delete(iterator.next().getKey, "hijacked-aliases")
      }
    } finally {
      iterator.close()
    }
  }
}
