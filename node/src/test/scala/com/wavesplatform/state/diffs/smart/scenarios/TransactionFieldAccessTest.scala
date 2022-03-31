package com.wavesplatform.state.diffs.smart.scenarios

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state.diffs.{ENOUGH_AMT, produce}
import com.wavesplatform.state.diffs.smart._
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.{GenesisTransaction, TxHelpers}
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer._

class TransactionFieldAccessTest extends PropSpec with WithState {

  private def preconditionsTransferAndLease(code: String): (GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransaction) = {
    val master = TxHelpers.signer(1)
    val recipient = TxHelpers.signer(2)

    val genesis = TxHelpers.genesis(master.toAddress)
    val untyped = Parser.parseExpr(code).get.value
    val typed   = ExpressionCompiler(compilerContext(V1, Expression, isAssetScript = false), untyped).explicitGet()._1
    val setScript = TxHelpers.setScript(master, ExprScript(typed).explicitGet())
    val transfer = TxHelpers.transfer(master, recipient.toAddress, ENOUGH_AMT / 2)
    val lease = TxHelpers.lease(master, recipient.toAddress, ENOUGH_AMT / 2)

    (genesis, setScript, lease, transfer)
  }

  private val script =
    """
      |
      | match tx {
      | case ttx: TransferTransaction =>
      |       isDefined(ttx.assetId)==false
      | case _ =>
      |       false
      | }
      """.stripMargin

  property("accessing field of transaction without checking its type first results on exception") {
    val (genesis, setScript, lease, transfer) = preconditionsTransferAndLease(script)
    assertDiffAndState(Seq(TestBlock.create(Seq(genesis, setScript))), TestBlock.create(Seq(transfer)), smartEnabledFS) { case _ => () }
    assertDiffEi(Seq(TestBlock.create(Seq(genesis, setScript))), TestBlock.create(Seq(lease)), smartEnabledFS)(
      totalDiffEi => totalDiffEi should produce("TransactionNotAllowedByScript")
    )
  }
}
