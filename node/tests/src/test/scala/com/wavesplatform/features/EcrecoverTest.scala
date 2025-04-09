package com.wavesplatform.features

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.*
import com.wavesplatform.test.BlockchainExt.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.OptionValues

class EcrecoverTest extends PropSpec, WithDomain, OptionValues {
  private val dappScriptText = """
    @Callable(i)
    func default() = {
      let msg = base16'da74793f1299abeb213430596f281261355e29af0fdf5d359fe23cd9aca824c8'
      let sig = base16'a57deea68952929239bd764d1f6966ea982af65fa6305f3bb71819a0376bd0ff42887b4496780434bd954af05f2b24ab54f10d63ba11e3ce0a2c73c6e25a77cd1c'
      let rec = ecrecover(msg, sig) # ERROR APPEARS AT THIS FUNC
      [IntegerEntry("TEST_MSG_SIZE", msg.size()), IntegerEntry("TEST_SIG_SIZE", sig.size()), StringEntry("REC", rec.toBase16String())]
    }
  """

  private val dapp    = TxHelpers.signer(1003)
  private val invoker = TxHelpers.signer(1004)

  for {
    stdlibVersion <- Seq(V4, V5, V6, V7, V8)
  } yield property(s"Version $stdlibVersion") {
    val script = TestCompiler(stdlibVersion).compileContract(dappScriptText)
    withDomain(
      DomainPresets.TransactionStateSnapshot.setFeaturesHeight(BlockchainFeatures.EcrecoverFix -> 4),
      Seq(
        AddrWithBalance(dapp.toAddress, 10.waves),
        AddrWithBalance(invoker.toAddress, 10.waves)
      )
    ) { d =>
      d.appendBlock(TxHelpers.setScript(dapp, script))
      d.blockchain.height shouldBe 2
      d.appendAndAssertFailed(TxHelpers.invoke(dapp.toAddress, invoker = invoker), "Invalid input length 127")
      d.blockchain.height shouldBe 3
      d.appendAndAssertSucceed(TxHelpers.invoke(dapp.toAddress, invoker = invoker))
      d.blockchain.integerData(dapp.toAddress, "TEST_MSG_SIZE").value shouldBe 32
      d.blockchain.integerData(dapp.toAddress, "TEST_SIG_SIZE").value shouldBe 65
      d.blockchain.stringData(dapp.toAddress, "REC").value shouldBe "0c9af283046995d88527c7acc82dc7f7e5a29a3119d68b8903789541348e008f4d0b8d7d8047c23818ec2063f6299ba469f79245d07d78f2b55f500f5d953e4f"
    }
  }
}
