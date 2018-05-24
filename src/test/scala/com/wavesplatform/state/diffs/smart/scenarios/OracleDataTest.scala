package com.wavesplatform.state.diffs.smart.scenarios

import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state._
import com.wavesplatform.state.diffs._
import com.wavesplatform.state.diffs.smart.smartEnabledFS
import com.wavesplatform.utils.dummyTypeCheckerContext
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.smart.script.v1.ScriptV1
import scorex.transaction.transfer._
import scorex.transaction.{DataTransaction, GenesisTransaction, Proofs}

class OracleDataTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {
  val preconditions: Gen[(GenesisTransaction, GenesisTransaction, SetScriptTransaction, DataTransaction, TransferTransactionV2)] =
    for {
      master <- accountGen
      oracle <- accountGen
      alice  <- accountGen
      ts     <- positiveIntGen
      genesis  = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      genesis2 = GenesisTransaction.create(oracle, ENOUGH_AMT, ts).explicitGet()
      long            <- longEntryGen(dataAsciiKeyGen)
      bool            <- booleanEntryGen(dataAsciiKeyGen).filter(_.key != long.key)
      bin             <- binaryEntryGen(500, dataAsciiKeyGen).filter(e => e.key != long.key && e.key != bool.key)
      str             <- stringEntryGen(500, dataAsciiKeyGen).filter(e => e.key != long.key && e.key != bool.key && e.key != bin.key)
      dataTransaction <- dataTransactionGenP(oracle, List(long, bool, bin, str))
      allFieldsRequiredScript = s"""
                    |
                    | let oracle = extract(addressFromString("${oracle.address}"))
                    | let long = extract(getLong(oracle,"${long.key}")) == ${long.value}
                    | let bool = extract(getBoolean(oracle,"${bool.key}")) == ${bool.value}
                    | let bin = extract(getByteArray(oracle,"${bin.key}")) == base58'${bin.value.base58}'
                    | let str = extract(getString(oracle,"${str.key}")) == "${str.value}"
                    | long && bool && bin && str
                    |
                    |
                    |
        """.stripMargin
      setScript <- {
        val untypedAllFieldsRequiredScript = Parser(allFieldsRequiredScript).get.value
        assert(untypedAllFieldsRequiredScript.size == 1)
        val typedAllFieldsRequiredScript = CompilerV1(dummyTypeCheckerContext, untypedAllFieldsRequiredScript.head).explicitGet()
        selfSignedSetScriptTransactionGenP(master, ScriptV1(typedAllFieldsRequiredScript).explicitGet())
      }
      transferFromScripted <- versionedTransferGenP(master, alice, Proofs.empty)

    } yield (genesis, genesis2, setScript, dataTransaction, transferFromScripted)

  property("simple oracle value required to transfer") {
    forAll(preconditions) {
      case (genesis, genesis2, setScript, dataTransaction, transferFromScripted) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, genesis2, setScript, dataTransaction))),
                           TestBlock.create(Seq(transferFromScripted)),
                           smartEnabledFS) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis2, setScript))), TestBlock.create(Seq(transferFromScripted)), smartEnabledFS)(
          totalDiffEi => totalDiffEi should produce("Script execution error"))
    }
  }
}
