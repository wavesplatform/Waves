package com.wavesplatform.state.diffs.smart.scenarios

import com.wavesplatform.lang.v1.Terms.Typed
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.smart.smartEnabledFS
import com.wavesplatform.state.diffs.{ENOUGH_AMT, assertDiffEi, produce}
import com.wavesplatform.{NoShrink, OldTransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.assets.VersionedTransferTransaction
import scorex.transaction.smart.script.v1.ScriptV1
import scorex.transaction.{GenesisTransaction, Proofs}

class OneProofForNonScriptedAccountTest extends PropSpec with PropertyChecks with Matchers with OldTransactionGen with NoShrink {

  property("exactly 1 proof required for non-scripted accounts") {
    val s = for {
      version   <- Gen.oneOf(VersionedTransferTransaction.supportedVersions.toSeq)
      master    <- accountGen
      recepient <- accountGen
      amt       <- positiveLongGen
      fee       <- smallFeeGen
      ts        <- positiveIntGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      setScript <- selfSignedSetScriptTransactionGenP(master, ScriptV1(Typed.TRUE).explicitGet())
      transfer = VersionedTransferTransaction.selfSigned(version, None, master, recepient, amt, ts, fee, Array.emptyByteArray).explicitGet()
    } yield (genesis, setScript, transfer)

    forAll(s) {
      case ((genesis, script, transfer)) =>
        val transferWithExtraProof = transfer.copy(proofs = Proofs(Seq(ByteStr.empty, ByteStr(Array(1: Byte)))))
        assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(transferWithExtraProof)), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("must have exactly 1 proof"))
    }
  }

}
