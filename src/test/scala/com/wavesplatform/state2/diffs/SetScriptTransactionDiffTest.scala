package com.wavesplatform.state2.diffs

import com.wavesplatform.state2._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.GenesisTransaction
import scorex.transaction.smart.SetScriptTransaction

class SetScriptTransactionDiffTest extends PropSpec
  with PropertyChecks with Matchers with TransactionGen with NoShrink {
  val preconditionsAndSetScript: Gen[(GenesisTransaction, SetScriptTransaction)] = for {
    master <- accountGen
    ts <- timestampGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    fee <- smallFeeGen
    script <- scriptGen
  } yield (genesis, SetScriptTransaction.selfSigned(master, script, fee, ts).explicitGet())

  property("setting script results in account state") {
    forAll(preconditionsAndSetScript) { case (genesis, setScript) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(setScript))) { case (blockDiff, newState) =>
        newState.accountScript(setScript.sender) shouldBe setScript.script
      }
    }
  }
}
