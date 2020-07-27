package com.wavesplatform.it.sync.transactions

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.IntegrationSuiteWithThreeAddresses
import com.wavesplatform.it.sync.calcDataFee
import com.wavesplatform.state.BinaryDataEntry

import scala.concurrent.duration._

trait OverflowBlock { self: IntegrationSuiteWithThreeAddresses =>

  // Hack to bypass instant micro mining
  def overflowBlock(): Unit = {
    import com.wavesplatform.it.api.SyncHttpApi._
    val entries = List.tabulate(4)(n => BinaryDataEntry(n.toString, ByteStr(Array.fill(32724)(n.toByte))))
    val addr    = sender.createKeyPair()
    val fee     = calcDataFee(entries, 1)
    sender.transfer(sender.keyPair, addr.toAddress.toString, fee * 8 + 36, waitForTx = true)
    sender.waitFor("empty utx")(n => n.utxSize, (utxSize: Int) => utxSize == 0, 100.millis)
    nodes.waitForHeightArise()
    for (i <- 1 to 8) sender.putData(addr, entries, fee + i)
    sender.waitFor("empty utx")(n => n.utxSize, (utxSize: Int) => utxSize == 0, 100.millis)
  }
}
