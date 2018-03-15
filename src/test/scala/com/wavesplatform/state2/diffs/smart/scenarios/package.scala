package com.wavesplatform.state2.diffs.smart

import com.wavesplatform.TransactionGen
import com.wavesplatform.lang.Terms.Typed
import com.wavesplatform.lang.{Parser, TypeChecker}
import com.wavesplatform.state2._
import com.wavesplatform.utils._
import com.wavesplatform.state2.diffs.ENOUGH_AMT
import org.scalacheck.Gen
import scorex.transaction.GenesisTransaction
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.smart.{Script, SetScriptTransaction}

package object scenarios extends TransactionGen {
  def preconditionsTransferAndLease(code: String): Gen[(GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransaction)] = {
    val untyped = Parser(code).get.value
    val typed   = TypeChecker(dummyTypeCheckerContext, untyped).explicitGet()
    preconditionsTransferAndLease(typed)
  }

  def preconditionsTransferAndLease(typed: Typed.EXPR): Gen[(GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransaction)] =
    for {
      master    <- accountGen
      recipient <- accountGen
      ts        <- positiveIntGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      setScript <- selfSignedSetScriptTransactionGenP(master, Script(typed))
      transfer  <- transferGeneratorP(master, recipient.toAddress, None, None)
      lease     <- leaseAndCancelGeneratorP(master, recipient.toAddress, master)
    } yield (genesis, setScript, lease._1, transfer)

}
