package com.wavesplatform.state.appender

import com.wavesplatform.common.utils._
import com.wavesplatform.db.WithDomain
import com.wavesplatform.it.util._
import com.wavesplatform.network.{InvalidBlockStorage, PeerDatabase}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.SystemTime
import com.wavesplatform.utx.UtxPoolImpl
import monix.execution.Scheduler.Implicits.global
import monix.reactive.subjects.ConcurrentSubject
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{FlatSpec, Matchers}

class ExtensionAppenderSpec extends FlatSpec with Matchers with WithDomain with PathMockFactory {
  "Extension appender" should "drop duplicate transactions from UTX" in withDomain() { d =>
    val utx = new UtxPoolImpl(SystemTime, d.blockchain, ConcurrentSubject.publish, defaultDomainSettings.utxSettings)
    val extensionAppender = ExtensionAppender(d.blockchain, utx, d.posSelector, SystemTime, stub[InvalidBlockStorage], stub[PeerDatabase], global)(null, _)

    d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress, 100_000_000.waves))
    val tx = TxHelpers.transfer()
    val block1 = d.appendBlock(tx)
    d.rollbackTo(1)
    d.appendBlock(tx)

    extensionAppender(Seq(block1)).runSyncUnsafe().explicitGet()
    d.blockchain.height shouldBe 2
  }

}
