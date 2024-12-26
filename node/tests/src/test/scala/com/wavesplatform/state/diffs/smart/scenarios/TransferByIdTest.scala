package com.wavesplatform.state.diffs.smart.scenarios

import com.wavesplatform.common.utils._
import com.wavesplatform.db.WithState
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.{Expression, V3}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.utils.compilerContext
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state.BinaryDataEntry
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.smart.smartEnabledFS
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{DataTransaction, GenesisTransaction, TxHelpers, TxVersion}

class TransferByIdTest extends PropSpec with WithState {

  val scriptSrc: String =
    s"""
       |match tx {
       |  case dtx: DataTransaction =>
       |    let txId    = extract(getBinary(dtx.data, "transfer_id"))
       |    let maybeTx = transferTransactionById(txId)
       |
       |    isDefined(maybeTx)
       |
       |  case _ => false
       |}
     """.stripMargin

  val expr: EXPR = {
    val parsed = Parser.parseExpr(scriptSrc).get.value
    ExpressionCompiler(compilerContext(V3, Expression, isAssetScript = false), V3, parsed).explicitGet()._1
  }

  property("Transfer by id works fine") {
    preconditions.foreach {
      case (genesis, transfer, setScript, data) =>
        assertDiffEi(
          Seq(TestBlock.create(Seq(genesis, transfer))),
          TestBlock.create(Seq(setScript, data)),
          smartEnabledFS
        )(_ shouldBe an[Right[_, _]])
    }
  }

  private def preconditions: Seq[(GenesisTransaction, TransferTransaction, SetScriptTransaction, DataTransaction)] = {
    val master    = TxHelpers.signer(1)
    val recipient = TxHelpers.signer(2)

    val genesis   = TxHelpers.genesis(master.toAddress)
    val setScript = TxHelpers.setScript(master, ExprScript(V3, expr).explicitGet())

    Seq(
      TxHelpers.transfer(master, recipient.toAddress, ENOUGH_AMT / 2),
      TxHelpers.transfer(master, recipient.toAddress, ENOUGH_AMT / 2, version = TxVersion.V1)
    ).map { transfer =>
      val data = TxHelpers.data(master, Seq(BinaryDataEntry("transfer_id", transfer.id())))

      (genesis, transfer, setScript, data)
    }
  }
}
