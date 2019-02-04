package com.wavesplatform.state.diffs.smart.scenarios

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.state.diffs.smart.smartEnabledFS
import com.wavesplatform.state.diffs.{ENOUGH_AMT, assertDiffEi, produce}
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{GenesisTransaction, Proofs}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class OneProofForNonScriptedAccountTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  property("exactly 1 proof required for non-scripted accounts") {
    val s = for {
      version   <- Gen.oneOf(TransferTransactionV2.supportedVersions.toSeq)
      master    <- accountGen
      recepient <- accountGen
      amt       <- positiveLongGen
      fee       <- smallFeeGen
      ts        <- positiveIntGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      setScript <- selfSignedSetScriptTransactionGenP(master, ExprScript(TRUE).explicitGet())
      transfer = TransferTransactionV2.selfSigned(version, None, master, recepient, amt, ts, None, fee, Array.emptyByteArray).explicitGet()
    } yield (genesis, setScript, transfer)

    forAll(s) {
      case ((genesis, script, transfer)) =>
        val transferWithExtraProof = transfer.copy(proofs = Proofs(Seq(ByteStr.empty, ByteStr(Array(1: Byte)))))
        assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(transferWithExtraProof)), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("must have exactly 1 proof"))
    }
  }

}
