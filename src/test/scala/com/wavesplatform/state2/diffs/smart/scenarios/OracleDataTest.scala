package com.wavesplatform.state2.diffs.smart.scenarios

import com.wavesplatform.lang.{Parser, TypeChecker}
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs._
import com.wavesplatform.state2.diffs.smart.smartEnabledFS
import com.wavesplatform.utils.{dummyNetworkByte, dummyTypeCheckerContext}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.account.Address
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.assets.{TransferTransaction, VersionedTransferTransaction}
import scorex.transaction.smart.{Script, SetScriptTransaction}
import scorex.transaction.{DataTransaction, GenesisTransaction, Proofs}

class OracleDataTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {
  val preconditions: Gen[(GenesisTransaction, TransferTransaction, SetScriptTransaction, DataTransaction, VersionedTransferTransaction)] =
    for {
      master <- accountGen
      oracle <- accountGen
      alice  <- accountGen
      ts     <- positiveIntGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      fundOracle            <- transferGeneratorP(master, oracle.toAddress, None, None)
      long                <- longEntryGen
      bool                <- booleanEntryGen
      bin                 <- binaryEntryGen
      dataTransactionGenP <- dataTransactionGenP(oracle, List(long, bool, bin))
      allFieldsRequiredScript        = s"""
                    |
                    | let oracle = addressFromPublicKey(base58'${Address.fromPublicKey(oracle.publicKey, dummyNetworkByte)}')
                    | let longCorrect = extract(getLong(oracle,"${long.key}")) == ${long.value}
                    | longCorrect
                    |
                    |
        """.stripMargin
      untypedAllFieldsRequiredScript = Parser(allFieldsRequiredScript).get.value
      typedAllFieldsRequiredScript   = TypeChecker(dummyTypeCheckerContext, untypedAllFieldsRequiredScript).explicitGet()
      setScript <- selfSignedSetScriptTransactionGenP(master, Script(typedAllFieldsRequiredScript))
      transferFromScripted  <- versionedTransferGenP(master, alice, Proofs.empty)

    } yield (genesis, fundOracle, setScript, dataTransactionGenP, transferFromScripted)


  property("simple oracle value required to transfer") {
    forAll(preconditions) { case((genesis, fundOracle, setScript, dataTransactionGenP, transferFromScripted)) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, fundOracle, setScript, dataTransactionGenP))), TestBlock.create(Seq(transferFromScripted)), smartEnabledFS) { case _ => () }
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, fundOracle,setScript))), TestBlock.create(Seq(transferFromScripted)), smartEnabledFS)(totalDiffEi =>
        totalDiffEi should produce("TransactionNotAllowedByScript"))
    }
  }
}
