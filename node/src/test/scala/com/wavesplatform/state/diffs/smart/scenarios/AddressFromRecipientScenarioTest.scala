package com.wavesplatform.state.diffs.smart.scenarios

import com.wavesplatform.account.{AddressOrAlias, KeyPair}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, CaseObj}
import com.wavesplatform.state.diffs._
import com.wavesplatform.state.diffs.smart.predef._
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{CreateAliasTransaction, GenesisTransaction}
import org.scalacheck.Gen

class AddressFromRecipientScenarioTest extends PropSpec with WithState {

  val preconditionsAndAliasCreations: Gen[(Seq[GenesisTransaction], CreateAliasTransaction, TransferTransaction, TransferTransaction)] = for {
    master                   <- accountGen
    ts                       <- timestampGen
    other: KeyPair <- accountGen
    genesis1: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
    genesis2: GenesisTransaction = GenesisTransaction.create(other.toAddress, ENOUGH_AMT, ts).explicitGet()
    alias              <- aliasGen
    fee                <- smallFeeGen
    aliasTx            <- createAliasGen(other, alias, fee, ts)
    transferViaAddress <- transferGeneratorP(master, other.toAddress, Waves, Waves)
    transferViaAlias   <- transferGeneratorP(master, AddressOrAlias.fromBytes(alias.bytes).explicitGet(), Waves, Waves)
  } yield (Seq(genesis1, genesis2), aliasTx, transferViaAddress, transferViaAlias)

  val script = """
    | match tx {
    |  case t : TransferTransaction =>  addressFromRecipient(t.recipient)
    |  case _ => throw()
    |  }
    |  """.stripMargin

  property("Script can resolve AddressOrAlias") {
    forAll(preconditionsAndAliasCreations) {
      case (gen, aliasTx, transferViaAddress, transferViaAlias) =>
        assertDiffAndState(Seq(TestBlock.create(gen)), TestBlock.create(Seq(aliasTx))) {
          case (_, state) =>
            val addressBytes = runScript[CaseObj](script, transferViaAddress, state).explicitGet().fields("bytes").asInstanceOf[CONST_BYTESTR]
            addressBytes.bs.arr.sameElements(transferViaAddress.recipient.bytes) shouldBe true
            val resolvedAddressBytes =
              runScript[CaseObj](script, transferViaAlias, state).explicitGet().fields("bytes").asInstanceOf[CONST_BYTESTR]

            resolvedAddressBytes.bs.arr.sameElements(transferViaAddress.recipient.bytes) shouldBe true
        }
    }
  }

  property("Script can't resolve alias that doesn't exist") {
    forAll(preconditionsAndAliasCreations) {
      case (gen, _, _, transferViaAlias) =>
        assertDiffAndState(Seq(TestBlock.create(gen)), TestBlock.create(Seq())) {
          case (_, state) =>
            runScript(script, transferViaAlias, state) should produce(" does not exists")
        }
    }
  }
}
