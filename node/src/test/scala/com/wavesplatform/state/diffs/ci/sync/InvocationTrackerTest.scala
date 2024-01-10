package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers

class InvocationTrackerTest extends FreeSpec with WithDomain {
  private val caller, dapp1, dapp2, dapp3 = TxHelpers.signer()
  "nested invocation tree" in withDomain(DomainPresets.RideV6, balances =
    Seq(
      AddrWithBalance(caller.toAddress, 10.waves),
      AddrWithBalance(dapp1.toAddress, 10.waves),
      AddrWithBalance(dapp2.toAddress, 10.waves),
      AddrWithBalance(dapp3.toAddress, 10.waves)
    )
  ) { d =>
    d.appendBlock(
      TxHelpers.setScript(
        dapp1,
        TestCompiler(V6).compileContract(s"""@Callable(i)
                                            |func default() = {
                                            |  strict a1 = invoke(Address(base58'${dapp2.toAddress}'),"default",[],[])
                                            |  strict a2 = invoke(Address(base58'${dapp2.toAddress}'),"default",[],[])
                                            |  strict a3 = invoke(Address(base58'${dapp2.toAddress}'),"default",[],[])
                                            |  []
                                            |}
                                            |""".stripMargin)
      ),
      TxHelpers.setScript(
        dapp2,
        TestCompiler(V6).compileContract(s"""@Callable(i)
                                            |func default() = {
                                            |  strict a1 = invoke(Address(base58'${dapp3.toAddress}'),"default",[],[])
                                            |  strict a2 = invoke(Address(base58'${dapp3.toAddress}'),"default",[],[])
                                            |  []
                                            |}
                                            |""".stripMargin)
      ),
      TxHelpers.setScript(
        dapp3,
        TestCompiler(V6).compileContract("""@Callable(i)
                                           |func default() = {
                                           |  []
                                           |}
                                           |""".stripMargin)
      )
    )
    val invocation = TxHelpers.invoke(dapp1.toAddress, invoker = caller)
    d.appendBlock(invocation)

    println(d.transactionsApi.transactionById(invocation.id()))
  }
}
