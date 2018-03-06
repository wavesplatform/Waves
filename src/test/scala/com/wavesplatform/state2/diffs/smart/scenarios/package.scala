package com.wavesplatform.state2.diffs.smart

import com.wavesplatform.TransactionGen
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.Terms.Typed
import com.wavesplatform.lang.{Parser, TypeChecker}
import com.wavesplatform.lang.ctx.Context
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.diffs.ENOUGH_AMT
import com.wavesplatform.state2._
import monix.eval.Coeval
import org.scalacheck.Gen
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.GenesisTransaction
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.smart.{ConsensusContext, Script, SetScriptTransaction}

package object scenarios extends TransactionGen {
  val fs: FunctionalitySettings                               = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0))
  val dummyContext: Context                                   = new ConsensusContext(Coeval(???), Coeval(???), null).build()
  val dummyTypeCheckerContext: TypeChecker.TypeCheckerContext = TypeChecker.TypeCheckerContext.fromContext(dummyContext)

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
