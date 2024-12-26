package com.wavesplatform.state.diffs.smart.scenarios

import com.wavesplatform.account.{AddressOrAlias, Alias}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, CaseObj}
import com.wavesplatform.state.diffs.smart.predef.*
import com.wavesplatform.test.*
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.transaction.{CreateAliasTransaction, GenesisTransaction, TxHelpers}

class AddressFromRecipientScenarioTest extends PropSpec with WithState {

  val preconditionsAndAliasCreations: (Seq[GenesisTransaction], CreateAliasTransaction, TransferTransaction, TransferTransaction) = {
    val master = TxHelpers.signer(1)
    val other  = TxHelpers.signer(2)

    val genesis            = Seq(master, other).map(acc => TxHelpers.genesis(acc.toAddress))
    val alias              = Alias.create("alias").explicitGet()
    val createAlias        = TxHelpers.createAlias(alias.name, other)
    val transferViaAddress = TxHelpers.transfer(master, other.toAddress)
    val transferViaAlias   = TxHelpers.transfer(master, AddressOrAlias.fromBytes(alias.bytes).explicitGet())

    (genesis, createAlias, transferViaAddress, transferViaAlias)
  }

  val script: String = """
    | match tx {
    |  case t : TransferTransaction =>  addressFromRecipient(t.recipient)
    |  case _ => throw()
    |  }
    |  """.stripMargin

  property("Script can resolve AddressOrAlias") {
    val (gen, aliasTx, transferViaAddress, transferViaAlias) = preconditionsAndAliasCreations
    assertDiffAndState(Seq(TestBlock.create(gen)), TestBlock.create(Seq(aliasTx))) {
      case (_, state) =>
        val addressBytes = runScript[CaseObj](script, transferViaAddress, state).explicitGet().fields("bytes").asInstanceOf[CONST_BYTESTR]
        addressBytes.bs.arr.sameElements(transferViaAddress.recipient.bytes) shouldBe true
        val resolvedAddressBytes =
          runScript[CaseObj](script, transferViaAlias, state).explicitGet().fields("bytes").asInstanceOf[CONST_BYTESTR]

        resolvedAddressBytes.bs.arr.sameElements(transferViaAddress.recipient.bytes) shouldBe true
    }
  }

  property("Script can't resolve alias that doesn't exist") {
    val (gen, _, _, transferViaAlias) = preconditionsAndAliasCreations
    assertDiffAndState(Seq(TestBlock.create(gen)), TestBlock.create(Seq())) {
      case (_, state) =>
        runScript(script, transferViaAlias, state) should produce(" does not exist")
    }
  }
}
