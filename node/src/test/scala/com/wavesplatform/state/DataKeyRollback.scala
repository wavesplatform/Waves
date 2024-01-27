package com.wavesplatform.state

import com.wavesplatform.db.WithState
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V7
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.*
import com.wavesplatform.transaction.{Transaction, TxHelpers}

class DataKeyRollback extends PropSpec with SharedDomain {
  private val richAccount = TxHelpers.signer(1500)

  override def genesisBalances: Seq[WithState.AddrWithBalance] = Seq(AddrWithBalance(richAccount.toAddress, 10_000_000.waves))
  override def settings: WavesSettings                         = DomainPresets.TransactionStateSnapshot

  private val oracleAccount = TxHelpers.signer(1501)
  private val dappAccounts  = Vector.tabulate(10)(i => TxHelpers.signer(1510 + i))

  private val dappScript = TestCompiler(V7).compileContract(s"""
    let oracleAddress = Address(base58'${oracleAccount.toAddress}')
    @Callable(i)
    func default() = [
      IntegerEntry("loadedHeight_" + height.toString() + i.transactionId.toBase58String(), oracleAddress.getIntegerValue("lastUpdatedBlock"))
    ]
  """)

  private def mkData() = TxHelpers.data(oracleAccount,
    IntegerDataEntry("lastUpdatedBlock", System.nanoTime()) +: Seq.tabulate(10)(i => StringDataEntry("k_" + System.nanoTime(), "bbar")),
    fee = 0.01.waves)

  private def mkInvocations(): Seq[Transaction] = {
    (1 to 10).flatMap { _ =>
      for {
        _    <- 1 to 10
        dapp <- dappAccounts
      } yield TxHelpers.invoke(dapp.toAddress, invoker = richAccount)
    }
  }

  property("data tx rollback") {
    domain.appendBlock(
      TxHelpers.massTransfer(richAccount, (oracleAccount +: dappAccounts).map(kp => kp.toAddress -> 100.waves), fee = 0.007.waves),
      TxHelpers.data(oracleAccount, Seq(IntegerDataEntry("lastUpdatedBlock", System.nanoTime())), fee = 0.001.waves)
    )
    domain.appendBlock(dappAccounts.map(kp => TxHelpers.setScript(kp, dappScript))*)
    domain.appendBlock()
    domain.appendBlock()

    1 to 100 foreach { _ =>
      domain.appendBlock(mkInvocations()*)
      domain.appendBlock()
      domain.appendBlock(mkData())
      domain.appendBlock()
      domain.rollbackTo(domain.blockchain.blockId(domain.blockchain.height - 4).get).foreach {
        case (block, _, _) => domain.appendBlock(block)
      }

    }
  }

  property("check new entries") {
    domain.appendBlock(TxHelpers.data(richAccount, Seq(IntegerDataEntry("k", 1))))
    domain.appendBlock(TxHelpers.data(richAccount, Seq(IntegerDataEntry("k", 2))))
    domain.appendBlock(TxHelpers.data(richAccount, Seq(IntegerDataEntry("k", 3))))
    domain.appendBlock(TxHelpers.data(richAccount, Seq(IntegerDataEntry("k", 4))))
    domain.appendBlock(TxHelpers.data(richAccount, Seq(IntegerDataEntry("k", 5))))
    domain.appendBlock(TxHelpers.data(richAccount, Seq(IntegerDataEntry("k", 6))))
    domain.appendBlock(TxHelpers.data(richAccount, Seq(IntegerDataEntry("k", 7))))
    domain.appendBlock()
    domain.appendBlock(TxHelpers.data(richAccount, Seq(IntegerDataEntry("k", 8))))
    domain.appendBlock()
    domain.rollbackTo(9)
    domain.appendBlock(TxHelpers.data(richAccount, Seq(IntegerDataEntry("k", 8))))
  }
}
