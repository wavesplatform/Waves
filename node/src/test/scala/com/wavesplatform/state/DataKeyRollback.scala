package com.wavesplatform.state

import com.wavesplatform.db.WithState
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V7
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers

class DataKeyRollback extends PropSpec with SharedDomain {
  private val richAccount = TxHelpers.signer(1500)

  override def genesisBalances: Seq[WithState.AddrWithBalance] = Seq(AddrWithBalance(richAccount.toAddress, 10_000_000.waves))
  override def settings: WavesSettings                         = DomainPresets.TransactionStateSnapshot

  property("check new entries") {
    val oracleAccount = TxHelpers.signer(1501)
    val dappAccount   = TxHelpers.signer(1502)

    val dataSenderCount = 5
    val dataEntryCount  = 5

    val dataSenders = IndexedSeq.tabulate(dataSenderCount)(i => TxHelpers.signer(1550 + i))
    domain.appendBlock(
      TxHelpers
        .massTransfer(
          richAccount,
          dataSenders.map(kp => kp.toAddress -> 100.waves) ++
            Seq(oracleAccount.toAddress -> 100.waves, dappAccount.toAddress -> 10.waves),
          fee = 0.05.waves
        ),
      TxHelpers.setScript(
        dappAccount,
        TestCompiler(V7).compileContract(s"""
          let oracleAddress = Address(base58'${oracleAccount.toAddress}')
          @Callable(i)
          func default() = [
            IntegerEntry("loadedHeight_" + height.toString() + i.transactionId.toBase58String(), oracleAddress.getIntegerValue("lastUpdatedBlock"))
          ]
        """)
      ),
      TxHelpers.data(oracleAccount, Seq(IntegerDataEntry("lastUpdatedBlock", 2)))
    )
    domain.appendBlock(dataSenders.map(kp => TxHelpers.data(kp, Seq.tabulate(dataEntryCount)(i => IntegerDataEntry("kv_" + i, 501)), 0.01.waves))*)
    domain.appendBlock(dataSenders.map(kp => TxHelpers.data(kp, Seq.tabulate(dataEntryCount)(i => IntegerDataEntry("kv_" + i, 503)), 0.01.waves))*)
    domain.appendBlock(
      (dataSenders.map(kp => TxHelpers.data(kp, Seq.tabulate(dataEntryCount)(i => IntegerDataEntry("kv_" + i, 504)), 0.01.waves)) ++
        Seq(
          TxHelpers.invoke(dappAccount.toAddress, invoker = richAccount),
          TxHelpers.data(oracleAccount, Seq(IntegerDataEntry("lastUpdatedBlock", 5)))
        ))*
    )
    domain.appendBlock()
    val discardedBlocks = domain.rollbackTo(domain.blockchain.blockId(domain.blockchain.height - 2).get)
    discardedBlocks.foreach { case (block, _, _) =>
      domain.appendBlock(block)
    }
  }
}
