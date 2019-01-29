package com.wavesplatform.state.diffs

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.Version
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract.{CallableAnnotation, ContractFunction}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.FieldNames
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.smart.script.v1.ContractScript
import com.wavesplatform.transaction.smart.{ContractInvocationTransaction, SetScriptTransaction}
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ContractInvocationTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  private val fs = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0, BlockchainFeatures.Ride4DApps.id -> 0))

  def dataContract(senderBinding: String, argName: String, funcName: String) = Contract(
    List.empty,
    List(
      ContractFunction(
        CallableAnnotation(senderBinding),
        Terms.FUNC(
          funcName,
          List(argName),
          FUNCTION_CALL(
            User(FieldNames.WriteSet),
            List(FUNCTION_CALL(
              Native(1102),
              List(
                FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("argument"), REF(argName))),
                FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender"), GETTER(GETTER(REF(senderBinding), "caller"), "bytes")))
              )
            ))
          )
        )
      )),
    None
  )

  def paymentContract(senderBinding: String, argName: String, funcName: String, recipientAddress: Address, recipientAmount: Long) = Contract(
    List.empty,
    List(
      ContractFunction(
        CallableAnnotation(senderBinding),
        Terms.FUNC(
          funcName,
          List(argName),
          FUNCTION_CALL(
            User(FieldNames.TransferSet),
            List(FUNCTION_CALL(
              Native(1102),
              List(
                FUNCTION_CALL(
                  User(FieldNames.ContractTransfer),
                  List(FUNCTION_CALL(User("Address"), List(CONST_BYTESTR(recipientAddress.bytes))), CONST_LONG(recipientAmount), REF("unit"))
                )
              )
            ))
          )
        )
      )),
    None
  )

  def dataContractGen(func: String) =
    for {
      senderBinging <- validAliasStringGen
      argBinding    <- validAliasStringGen
    } yield dataContract(senderBinging, argBinding, func)

  def paymentContractGen(address: Address, amount: Long)(func: String) =
    for {
      senderBinging <- validAliasStringGen
      argBinding    <- validAliasStringGen
    } yield paymentContract(senderBinging, argBinding, func, address, amount)

  def preconditionsAndSetContract(
      senderBindingToContract: String => Gen[Contract]): Gen[(List[GenesisTransaction], SetScriptTransaction, ContractInvocationTransaction)] =
    for {
      setScriptVersion <- Gen.oneOf(SetScriptTransaction.supportedVersions.toSeq)
      ciVersion        <- Gen.oneOf(ContractInvocationTransaction.supportedVersions.toSeq)
      master           <- accountGen
      invoker          <- accountGen
      ts               <- timestampGen
      genesis: GenesisTransaction  = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      genesis2: GenesisTransaction = GenesisTransaction.create(invoker, ENOUGH_AMT, ts).explicitGet()
      fee         <- smallFeeGen
      arg         <- genBoundedString(1, 32)
      funcBinding <- validAliasStringGen
      contract    <- senderBindingToContract(funcBinding)
      script      = ContractScript(Version.ContractV, contract)
      setContract = SetScriptTransaction.selfSigned(setScriptVersion, master, Some(script), fee, ts).explicitGet()
      fc          = Terms.FUNCTION_CALL(FunctionHeader.User(funcBinding), List(CONST_BYTESTR(ByteStr(arg))))
      ci          = ContractInvocationTransaction.selfSigned(ciVersion, invoker, master, fc, None, fee, ts).explicitGet()
    } yield (List(genesis, genesis2), setContract, ci)

  property("invoking contract results contract's state") {
    forAll(for {
      r <- preconditionsAndSetContract(dataContractGen)
    } yield (r._1, r._2, r._3)) {
      case (genesis, setScript, ci) =>
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          case (blockDiff, newState) =>
            newState.accountData(genesis(0).recipient) shouldBe AccountDataInfo(
              Map(
                "sender"   -> BinaryDataEntry("sender", ci.sender.toAddress.bytes),
                "argument" -> BinaryDataEntry("argument", ci.fc.args(0).asInstanceOf[CONST_BYTESTR].bs)
              ))
        }
    }
  }

  property("invoking payment contract results in accounts state") {
    forAll(for {
      a  <- accountGen
      am <- smallFeeGen
      contractGen = (paymentContractGen(a, am) _)
      r <- preconditionsAndSetContract(contractGen)
    } yield (a, am, r._1, r._2, r._3)) {
      case (acc, amount, genesis, setScript, ci) =>
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          case (blockDiff, newState) =>
            newState.balance(acc, None) shouldBe amount
        }
    }
  }

}
