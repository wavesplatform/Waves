package com.wavesplatform.state.diffs.smart.scenarios

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.state.diffs.smart.smartEnabledFS
import com.wavesplatform.state.diffs.{ENOUGH_AMT, produce}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{GenesisTransaction, Proofs}

class OneProofForNonScriptedAccountTest extends PropSpec with WithState {

  property("exactly 1 proof required for non-scripted accounts") {
    val s = for {
      master    <- accountGen
      recepient <- accountGen
      amt       <- positiveLongGen
      fee       <- smallFeeGen
      ts        <- positiveIntGen
      genesis = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      setScript <- selfSignedSetScriptTransactionGenP(master, ExprScript(TRUE).explicitGet())
      transfer = TransferTransaction.selfSigned(2.toByte, master, recepient.toAddress, Waves, amt, Waves, fee, ByteStr.empty, ts).explicitGet()
    } yield (genesis, setScript, transfer)

    forAll(s) {
      case (genesis, script, transfer) =>
        val transferWithExtraProof = transfer.copy(proofs = Proofs(Seq(ByteStr.empty, ByteStr(Array(1: Byte)))))
        assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(transferWithExtraProof)), smartEnabledFS)(
          totalDiffEi => totalDiffEi should produce("must have exactly 1 proof")
        )
    }
  }

}
