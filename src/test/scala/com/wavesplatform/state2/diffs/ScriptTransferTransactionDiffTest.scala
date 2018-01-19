package com.wavesplatform.state2.diffs

import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.GenesisTransaction
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.smart.{Script, SetScriptTransaction}
import scorex.transaction.smart.lang.Terms._

class ScriptTransferTransactionDiffTest extends PropSpec
  with PropertyChecks with Matchers with TransactionGen with NoShrink {

  val onlySend: BOOL = AND(
    OR(
      EQINT(Accessor(TX, Field.Type), CONST(4)),
      EQINT(Accessor(TX, Field.Type), CONST(11))),
    SIGVERIFY(Accessor(TX, Field.BodyBytes), Accessor(TX, Field.Proof_0), Accessor(TX, Field.SenderPk))
  )

  val preconditionsAndTransfer: Gen[(GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransaction)] = for {
    master <- accountGen
    recepient <- accountGen
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    setSctipt <- selfSignedSetScriptTransactionGenP(master, Script(onlySend))
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