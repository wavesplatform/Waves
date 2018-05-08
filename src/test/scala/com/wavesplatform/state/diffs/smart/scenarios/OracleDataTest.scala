package com.wavesplatform.state.diffs.smart.scenarios

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.v1.{Parser, TypeChecker}
import com.wavesplatform.state._
import com.wavesplatform.state.diffs._
import com.wavesplatform.state.diffs.smart.smartEnabledFS
import com.wavesplatform.utils.dummyTypeCheckerContext
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.data.DataTransaction
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.smart.script.v1.ScriptV1
import scorex.transaction.transfer._
import scorex.transaction.{GenesisTransaction, Proofs}

class OracleDataTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {
  val functionalitySettings = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id     -> 0,
      BlockchainFeatures.DataTransaction.id   -> 0,
      BlockchainFeatures.DataTransactionV2.id -> 0
    )
  )

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
      bin             <- binaryEntryGen(dataAsciiKeyGen).filter(e => e.key != long.key && e.key != bool.key)
      dataTransaction <- dataTransactionGenP(oracle, List(long, bool, bin))
      allFieldsRequiredScript        = s"""
                    |
                    | let oracle = extract(addressFromString("${oracle.address}"))
                    | let long = extract(getLong(oracle,"${long.key}")) == ${long.value}
                    | let bool = extract(getBoolean(oracle,"${bool.key}")) == ${bool.value}
                    | let bin = extract(getByteArray(oracle,"${bin.key}")) == base58'${bin.value.base58}'
                    | long && bool && bin
                    |
                    |
                    |
        """.stripMargin
      untypedAllFieldsRequiredScript = Parser(allFieldsRequiredScript).get.value
      typedAllFieldsRequiredScript   = TypeChecker(dummyTypeCheckerContext, untypedAllFieldsRequiredScript).explicitGet()
      setScript            <- selfSignedSetScriptTransactionGenP(master, ScriptV1(typedAllFieldsRequiredScript).explicitGet())
      transferFromScripted <- versionedTransferGenP(master, alice, Proofs.empty)

    } yield (genesis, genesis2, setScript, dataTransaction, transferFromScripted)

  property("simple oracle value required to transfer") {
    forAll(preconditions) {
      case ((genesis, genesis2, setScript, dataTransaction, transferFromScripted)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, genesis2, setScript, dataTransaction))),
                           TestBlock.create(Seq(transferFromScripted)),
                           functionalitySettings) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis2, setScript))), TestBlock.create(Seq(transferFromScripted)), smartEnabledFS)(
          totalDiffEi => totalDiffEi should produce("Script execution error"))
    }
  }
}
