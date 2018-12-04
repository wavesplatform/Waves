package com.wavesplatform.state.diffs

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.Version
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract.{CallableAnnotation, ContractFunction}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTEVECTOR, CONST_STRING, FUNCTION_CALL, REF}
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.smart.{ContractInvocationTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.smart.script.v1.ScriptV2
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector

class ContractInvocationTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  private val fs = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0, BlockchainFeatures.Ride4DApps.id -> 0))

  def contract(senderBinding: String, argName: String, funcName: String) = Contract(
    List.empty,
    List(
      ContractFunction(
        CallableAnnotation(senderBinding),
        None,
        Terms.FUNC(
          funcName,
          List(argName),
          FUNCTION_CALL(
            User("WriteSet"),
            List(FUNCTION_CALL(
              Native(1102),
              List(
                FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("argument"), REF(argName))),
                FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender"), REF(senderBinding)))
              )
            ))
          )
        )
      )),
    None
  )
  val preconditionsAndSetContract: Gen[(List[GenesisTransaction], SetScriptTransaction, ContractInvocationTransaction)] = for {
    setScriptVersion <- Gen.oneOf(SetScriptTransaction.supportedVersions.toSeq)
    ciVersion        <- Gen.oneOf(ContractInvocationTransaction.supportedVersions.toSeq)
    master           <- accountGen
    invoker          <- accountGen
    ts               <- timestampGen
    genesis: GenesisTransaction  = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    genesis2: GenesisTransaction = GenesisTransaction.create(invoker, ENOUGH_AMT, ts).explicitGet()
    fee           <- smallFeeGen
    senderBinging <- validAliasStringGen
    argBinding    <- validAliasStringGen
    funcBinding   <- validAliasStringGen
    arg           <- genBoundedString(1, 32)
    script = ScriptV2(
      Version.V3,
      contract(senderBinging, argBinding, funcBinding)
    )
    setContract = SetScriptTransaction.selfSigned(setScriptVersion, master, Some(script), fee, ts).explicitGet()
    fc          = Terms.FUNCTION_CALL(FunctionHeader.User(funcBinding), List(CONST_BYTEVECTOR(ByteVector(arg))))
    ci          = ContractInvocationTransaction.selfSigned(ciVersion, invoker, master, fc, fee, ts).explicitGet()
  } yield (List(genesis, genesis2), setContract, ci)

  property("invoking contract results contract's state") {
    forAll(preconditionsAndSetContract) {
      case (genesis, setScript, ci) =>
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          case (blockDiff, newState) =>
            newState.accountData(genesis(0).recipient) shouldBe AccountDataInfo(
              Map(
                "sender"   -> BinaryDataEntry("sender", ci.sender.toAddress.bytes),
                "argument" -> BinaryDataEntry("argument", ByteStr(ci.fc.args(0).asInstanceOf[CONST_BYTEVECTOR].bs.toArray))
              ))
        }
    }
  }
}
