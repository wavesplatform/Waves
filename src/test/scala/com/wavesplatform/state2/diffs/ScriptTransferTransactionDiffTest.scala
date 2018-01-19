package com.wavesplatform.state2.diffs

import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.GenesisTransaction
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.smart.SetScriptTransaction

class ScriptTransferTransactionDiffTest extends PropSpec
  with PropertyChecks with Matchers with TransactionGen with NoShrink {

  val preconditionsAndTransfer: Gen[(GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransaction)] = for {
    master <- accountGen
    recepient <- accountGen
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    setSctipt <- selfSignedSetScriptTransactionGenP(master)
    transfer <- transferGeneratorP(master, recepient.toAddress, None, None)
    lease <- leaseAndCancelGeneratorP(master, recepient.toAddress, master)
  } yield (genesis, setSctipt, lease._1, transfer)

  property("transfer is allowed but lease is not due to predicate") {
    forAll(preconditionsAndTransfer) { case ((genesis, script, lease, transfer)) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer))) { case (totalDiff, newState) => () }
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)))(totalDiffEi =>
        totalDiffEi should produce("TransactionNotAllowedByScript"))
    }
  }
}