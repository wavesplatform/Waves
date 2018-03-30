package com.wavesplatform.state2.diffs.smart.scenarios

import com.wavesplatform.lang.{Parser, TypeChecker}
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs._
import com.wavesplatform.state2.diffs.smart.smartEnabledFS
import com.wavesplatform.utils.dummyTypeCheckerContext
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.assets.VersionedTransferTransaction
import scorex.transaction.smart.{Script, SetScriptTransaction}
import scorex.transaction.{DataTransaction, GenesisTransaction, Proofs}

class OracleDataTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {
  val preconditions: Gen[(GenesisTransaction, GenesisTransaction, SetScriptTransaction, DataTransaction, VersionedTransferTransaction)] =
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
      setScript            <- selfSignedSetScriptTransactionGenP(master, Script(typedAllFieldsRequiredScript))
      transferFromScripted <- versionedTransferGenP(master, alice, Proofs.empty)

    } yield (genesis, genesis2, setScript, dataTransaction, transferFromScripted)

  property("simple oracle value required to transfer") {
    forAll(preconditions) {
      case ((genesis, genesis2, setScript, dataTransaction, transferFromScripted)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, genesis2, setScript, dataTransaction))),
                           TestBlock.create(Seq(transferFromScripted)),
                           smartEnabledFS) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis2, setScript))), TestBlock.create(Seq(transferFromScripted)), smartEnabledFS)(
          totalDiffEi => totalDiffEi should produce("Script execution error"))
    }
  }
}
