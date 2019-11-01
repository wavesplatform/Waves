package com.wavesplatform.database.patch

import java.util

import com.wavesplatform.account.Alias
import com.wavesplatform.database.{Keys, RW}
import com.wavesplatform.state._
import com.wavesplatform.transaction.{CreateAliasTransaction, TransactionParsers}
import com.wavesplatform.utils.ScorexLogging

import scala.collection.JavaConverters._

object DisableHijackedAliases extends ScorexLogging {
  def apply(rw: RW): Set[Alias] = {
    log.info("Collecting hijacked aliases")
    val aliases = new util.HashMap[Alias, Seq[CreateAliasTransaction]]()
    val height  = Height(rw.get(Keys.height))

    for (h <- 1 until height) {
      val (header, _, transactionCount, _) = rw
        .get(Keys.blockHeaderAndSizeAt(Height(h)))
        .get

      for (n <- 0 until transactionCount) {
        val txNum = TxNum(n.toShort)

        val transactionBytes = rw
          .get(Keys.transactionBytesAt(Height(h), txNum))
          .get

        val isCreateAlias = transactionBytes(0) == CreateAliasTransaction.typeId ||
          transactionBytes(0) == 0 &&
            transactionBytes(1) == CreateAliasTransaction.typeId

        if (isCreateAlias) {
          TransactionParsers
            .parseBytes(transactionBytes)
            .foreach {
              case cat: CreateAliasTransaction => aliases.compute(cat.alias, (_, prevTx) => Option(prevTx).fold(Seq(cat))(_ :+ cat))
              case _                           =>
            }
        }
      }
    }

    val hijackedAliases: Set[Alias] = (for {
      (alias, txs) <- aliases.asScala
      if txs.size > 1
    } yield alias).toSet

    log.info(s"Collected ${aliases.size()} aliases, of which ${hijackedAliases.size} were hijacked")

    rw.put(Keys.disabledAliases, hijackedAliases)

    hijackedAliases
  }

  def revert(rw: RW): Unit = {
    log.info("Discarding the list of hijacked aliases")
    rw.put(Keys.disabledAliases, Set.empty: Set[Alias])
  }
}
