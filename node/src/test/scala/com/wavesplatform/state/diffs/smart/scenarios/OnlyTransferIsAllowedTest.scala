package com.wavesplatform.state.diffs.smart.scenarios

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.utils.*
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.smart.*
import com.wavesplatform.test.*
import com.wavesplatform.transaction.{GenesisTransaction, TxHelpers}
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction

class OnlyTransferIsAllowedTest extends PropSpec with WithState {

  property("transfer is allowed but lease is not due to predicate") {

    val scriptText =
      s"""
         |
         | match tx {
         |  case ttx: TransferTransaction | MassTransferTransaction =>
         |     sigVerify(ttx.bodyBytes,ttx.proofs[0],ttx.senderPublicKey)
         |  case _ =>
         |     false
         | }
      """.stripMargin
    val untyped         = Parser.parseExpr(scriptText).get.value
    val transferAllowed = ExpressionCompiler(compilerContext(V1, Expression, isAssetScript = false), V1, untyped).explicitGet()._1

    val (genesis, script, lease, transfer) = preconditions(transferAllowed)
    assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), smartEnabledFS) { case _ => () }
    assertDiffEi(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), smartEnabledFS)(
      totalDiffEi => totalDiffEi should produce("TransactionNotAllowedByScript")
    )
  }

  private def preconditions(typed: EXPR): (GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransaction) = {
    val master    = TxHelpers.signer(1)
    val recipient = TxHelpers.signer(2)

    val genesis   = TxHelpers.genesis(master.toAddress)
    val setScript = TxHelpers.setScript(master, ExprScript(typed).explicitGet())
    val transfer  = TxHelpers.transfer(master, recipient.toAddress, ENOUGH_AMT / 2)
    val lease     = TxHelpers.lease(master, recipient.toAddress, ENOUGH_AMT / 2)

    (genesis, setScript, lease, transfer)
  }
}
