package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.{Expression, V1}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.utils.*
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{GenesisTransaction, PaymentTransaction, TxHelpers}

class ObsoleteTransactionBindingsTest extends PropSpec with WithState {

  def script(g: GenesisTransaction, p: PaymentTransaction): String =
    s"""let genTx = extract(transactionById(base58'${g.id().toString}'))
       |let payTx = extract(transactionById(base58'${p.id().toString}'))
       |
       |let genTotal = match genTx {
       |  case gen: GenesisTransaction =>
       |    let genId = gen.id == base58'${g.id().toString}'
       |    let genFee = gen.fee == ${g.fee}
       |    let genTimestamp = gen.timestamp== ${g.timestamp}
       |    let genVersion = gen.version == 1
       |    let genAmount = gen.amount == ${g.amount}
       |    let genRecipient = gen.recipient == Address(base58'${g.recipient}')
       |    genId && genFee && genTimestamp && genVersion && genAmount && genRecipient
       |   case _ => false
       | }
       |
       |let payTotal = match payTx {
       |  case pay: PaymentTransaction =>
       |    let payId = pay.id == base58'${p.id().toString}'
       |    let payFee = pay.fee == ${p.fee}
       |    let payTimestamp = pay.timestamp== ${p.timestamp}
       |    let payVersion = pay.version == 1
       |    let payAmount = pay.amount == ${p.amount}
       |    let payRecipient = pay.recipient == Address(base58'${p.recipient}')
       |
       |    let bodyBytes = pay.bodyBytes == base64'${ByteStr(p.bodyBytes.apply()).base64}'
       |    let sender = pay.sender == addressFromPublicKey(base58'${p.sender}')
       |    let senderPublicKey = pay.senderPublicKey == base58'${p.sender}'
       |    let signature = pay.proofs[0]== base58'${p.signature.toString}'
       |    let empty1 = pay.proofs[1]== base58''
       |    let empty2 = pay.proofs[2]== base58''
       |    let empty3 = pay.proofs[3]== base58''
       |    let empty4 = pay.proofs[4]== base58''
       |    let empty5 = pay.proofs[5]== base58''
       |    let empty6 = pay.proofs[6]== base58''
       |    let empty7 = pay.proofs[7]== base58''
       |
       |    let payBindings = payId && payFee && payTimestamp && payVersion && payAmount && payRecipient
       |    let payBindings1 = bodyBytes && sender && senderPublicKey && signature
       |    let payBindings2 = empty1 && empty2 && empty3 && empty4 && empty5 && empty6 && empty7
       |
       |    payBindings && payBindings1 && payBindings2
       |  case _ => false
       |}
       |
       |genTotal && payTotal
       |""".stripMargin

  val preconditionsAndPayments: Seq[(GenesisTransaction, PaymentTransaction, SetScriptTransaction, TransferTransaction)] = {
    val master     = TxHelpers.signer(1)
    val recipients = Seq(master, TxHelpers.signer(2))

    val genesis = TxHelpers.genesis(master.toAddress, ENOUGH_AMT * 3)
    recipients.map { recipient =>
      val payment       = TxHelpers.payment(master, recipient.toAddress, ENOUGH_AMT * 2)
      val untypedScript = Parser.parseExpr(script(genesis, payment)).get.value
      val typedScript = ExprScript(ExpressionCompiler(compilerContext(V1, Expression, isAssetScript = false), V1, untypedScript).explicitGet()._1)
        .explicitGet()
      val setScriptTransaction = TxHelpers.setScript(recipient, typedScript)
      val nextTransfer         = TxHelpers.transfer(recipient, master.toAddress)
      (genesis, payment, setScriptTransaction, nextTransfer)
    }
  }

  val settings: FunctionalitySettings = TestFunctionalitySettings.Enabled.copy(blockVersion3AfterHeight = 100)
  property("Obsolete transaction bindings") {
    preconditionsAndPayments.foreach { case (genesis, payment, setScriptTransaction, nextTransfer) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, payment, setScriptTransaction))), TestBlock.create(Seq(nextTransfer)), settings) {
        (_, _) =>
          ()
      }
    }
  }
}
