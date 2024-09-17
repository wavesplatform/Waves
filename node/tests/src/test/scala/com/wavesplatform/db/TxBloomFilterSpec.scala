package com.wavesplatform.db

import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers

class TxBloomFilterSpec extends PropSpec with SharedDomain {
  private val richAccount = TxHelpers.signer(1200)

  override def settings: WavesSettings = DomainPresets.TransactionStateSnapshot

  override def genesisBalances: Seq[AddrWithBalance] = Seq(AddrWithBalance(richAccount.toAddress, 10000.waves))

  property("Filter rotation works") {
    val transfer = TxHelpers.transfer(richAccount, TxHelpers.address(1201), 10.waves)
    1 to 8 foreach { _ => domain.appendBlock() }
    domain.blockchain.height shouldEqual 9
    domain.appendBlock(transfer) // transfer at height 10
    domain.appendBlock()         // height = 11
    domain.appendBlock()         // solid state height = 11, filters are rotated
    domain.appendBlockE(transfer) should produce("AlreadyInTheState")

    domain.appendBlock()
    val tf2 = TxHelpers.transfer(richAccount, TxHelpers.address(1202), 20.waves)
    domain.appendBlock(tf2)
    1 to 20 foreach { _ =>
      withClue(s"height = ${domain.blockchain.height}") {
        domain.appendBlockE(tf2) should produce("AlreadyInTheState")
      }
      domain.appendBlock()
    }
  }
}
