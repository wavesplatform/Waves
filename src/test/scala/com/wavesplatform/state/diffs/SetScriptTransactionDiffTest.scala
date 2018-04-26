package com.wavesplatform.state.diffs

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state._
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.account.AddressScheme
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.GenesisTransaction
import scorex.transaction.base.SetScriptTxBase
import scorex.transaction.modern.smart.{SetScriptPayload, SetScriptTx}
import scorex.transaction.smart.SetScriptTransaction

class SetScriptTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  private val fs = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0))

  val preconditionsAndSetScript: Gen[(GenesisTransaction, SetScriptTxBase)] = for {
    version <- Gen.oneOf(SetScriptTransaction.supportedVersions.toSeq)
    master  <- accountGen
    ts      <- timestampGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    fee    <- smallFeeGen
    script <- Gen.option(scriptGen)
    setScriptTx <- for {
      header <- txHeaderGen(master, SetScriptTx)
      payload = SetScriptPayload(AddressScheme.current.chainId, script)
      tx <- Gen.oneOf(
        SetScriptTransaction.selfSigned(version, master, script, fee, ts).explicitGet(),
        SetScriptTx.selfSigned(header, payload).get
      )
    } yield tx
  } yield (genesis, setScriptTx)

  property("setting script results in account state") {
    forAll(preconditionsAndSetScript) {
      case (genesis, setScript) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(setScript)), fs) {
          case (blockDiff, newState) =>
            newState.accountScript(setScript.sender) shouldBe setScript.script
        }
    }
  }
}
