package com.wavesplatform.state.diffs

import com.wavesplatform.account.{Address, AddressScheme, PrivateKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.StdLibVersion
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.FieldNames
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.{AssetId, GenesisTransaction, ValidationError}
import com.wavesplatform.transaction.smart.script.ContractScript
import com.wavesplatform.transaction.smart.{ContractInvocationTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.smart.ContractInvocationTransaction.Payment
import com.wavesplatform.transaction.assets.IssueTransactionV2
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import com.wavesplatform.transaction.transfer.TransferTransactionV2
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ContractInvocationTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  def ciFee(sc: Int = 0): Gen[Long] =
    Gen.choose(
      CommonValidation.FeeUnit * CommonValidation.FeeConstants(ContractInvocationTransaction.typeId) + sc * CommonValidation.ScriptExtraFee,
      CommonValidation.FeeUnit * CommonValidation.FeeConstants(ContractInvocationTransaction.typeId) + (sc + 1) * CommonValidation.ScriptExtraFee - 1
    )

  private val fs = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures =
      Map(BlockchainFeatures.SmartAccounts.id -> 0, BlockchainFeatures.SmartAssets.id -> 0, BlockchainFeatures.Ride4DApps.id -> 0))

  val assetAllowed = ExprScript(TRUE).explicitGet()
  val assetBanned  = ExprScript(FALSE).explicitGet()

  def dataContract(senderBinding: String, argName: String, funcName: String, bigData: Boolean) = {
    val datas =
      if (!bigData)
        List(
          FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("argument"), REF(argName))),
          FUNCTION_CALL(
            Native(1100),
            List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender"), GETTER(GETTER(REF(senderBinding), "caller"), "bytes"))), REF("nil")))
        )
      else
        List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("argument"), CONST_STRING("abcde" * 1024))), REF("nil"))
    Contract(
      List.empty,
      List(
        CallableFunction(
          CallableAnnotation(senderBinding),
          Terms.FUNC(
            funcName,
            List(argName),
            FUNCTION_CALL(
              User(FieldNames.WriteSet),
              List(
                FUNCTION_CALL(
                  Native(1100),
                  datas
                ))
            )
          )
        )),
      None
    )
  }

  def paymentContract(senderBinding: String,
                      argName: String,
                      funcName: String,
                      recipientAddress: Address,
                      recipientAmount: Long,
                      assetId: Option[AssetId] = None) = Contract(
    List.empty,
    List(
      CallableFunction(
        CallableAnnotation(senderBinding),
        Terms.FUNC(
          funcName,
          List(argName),
          FUNCTION_CALL(
            User(FieldNames.TransferSet),
            List(FUNCTION_CALL(
              Native(1100),
              List(
                FUNCTION_CALL(
                  User(FieldNames.ContractTransfer),
                  List(
                    FUNCTION_CALL(User("Address"), List(CONST_BYTESTR(recipientAddress.bytes))),
                    CONST_LONG(recipientAmount),
                    assetId.fold(REF("unit"): EXPR)(id => CONST_BYTESTR(id))
                  )
                ),
                REF("nil")
              )
            ))
          )
        )
      )),
    None
  )

  def dataContractGen(func: String, bigData: Boolean) =
    for {
      senderBinging <- validAliasStringGen
      argBinding    <- validAliasStringGen
    } yield dataContract(senderBinging, argBinding, func, bigData)

  def paymentContractGen(address: Address, amount: Long, assetId: Option[AssetId] = None)(func: String) =
    for {
      senderBinging <- validAliasStringGen
      argBinding    <- validAliasStringGen
    } yield paymentContract(senderBinging, argBinding, func, address, amount, assetId)

  def preconditionsAndSetContract(
      senderBindingToContract: String => Gen[Contract],
      invokerGen: Gen[PrivateKeyAccount] = accountGen,
      masterGen: Gen[PrivateKeyAccount] = accountGen,
      payment: Option[Payment] = None,
      feeGen: Gen[Long] = ciFee(0)): Gen[(List[GenesisTransaction], SetScriptTransaction, ContractInvocationTransaction)] =
    for {
      master  <- masterGen
      invoker <- invokerGen
      ts      <- timestampGen
      genesis: GenesisTransaction  = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      genesis2: GenesisTransaction = GenesisTransaction.create(invoker, ENOUGH_AMT, ts).explicitGet()
      fee         <- feeGen
      arg         <- genBoundedString(1, 32)
      funcBinding <- validAliasStringGen
      contract    <- senderBindingToContract(funcBinding)
      script      = ContractScript(StdLibVersion.V3, contract)
      setContract = SetScriptTransaction.selfSigned(master, script.toOption, fee, ts).explicitGet()
      fc          = Terms.FUNCTION_CALL(FunctionHeader.User(funcBinding), List(CONST_BYTESTR(ByteStr(arg))))
      ci          = ContractInvocationTransaction.selfSigned(invoker, master, fc, payment, fee, ts).explicitGet()
    } yield (List(genesis, genesis2), setContract, ci)

  property("invoking contract results contract's state") {
    forAll(for {
      r <- preconditionsAndSetContract(s => dataContractGen(s, false))
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

  property("can't more than 5kb of data") {
    forAll(for {
      r <- preconditionsAndSetContract(s => dataContractGen(s, true))
    } yield (r._1, r._2, r._3)) {
      case (genesis, setScript, ci) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          _ should produce("WriteSet size can't exceed")
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

  val chainId   = AddressScheme.current.chainId
  val enoughFee = CommonValidation.ScriptExtraFee + CommonValidation.FeeConstants(IssueTransactionV2.typeId) * CommonValidation.FeeUnit

  property("invoking contract receive payment") {
    forAll(for {
      a  <- accountGen
      am <- smallFeeGen
      contractGen = (paymentContractGen(a, am) _)
      invoker <- accountGen
      ts      <- timestampGen
      asset = IssueTransactionV2
        .selfSigned(chainId, invoker, "Asset#1".getBytes, "".getBytes, 1000000, 8, false, Some(assetAllowed), enoughFee, ts)
        .explicitGet()
      r <- preconditionsAndSetContract(contractGen,
                                       invokerGen = Gen.oneOf(Seq(invoker)),
                                       payment = Some(Payment(1, Some(asset.id()))),
                                       feeGen = ciFee(1))
    } yield (a, am, r._1, r._2, r._3, asset, invoker)) {
      case (acc, amount, genesis, setScript, ci, asset, invoker) =>
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci)), fs) {
          case (blockDiff, newState) =>
            newState.balance(acc, None) shouldBe amount
            newState.balance(invoker, Some(asset.id())) shouldBe (asset.quantity - 1)
            newState.balance(ci.contractAddress, Some(asset.id())) shouldBe 1
        }
    }
  }

  property("asset script ban invoking contract with payment") {
    forAll(for {
      a  <- accountGen
      am <- smallFeeGen
      contractGen = (paymentContractGen(a, am) _)
      invoker <- accountGen
      ts      <- timestampGen
      asset = IssueTransactionV2
        .selfSigned(chainId, invoker, "Asset#1".getBytes, "".getBytes, 1000000, 8, false, Some(assetBanned), enoughFee, ts)
        .explicitGet()
      r <- preconditionsAndSetContract(contractGen,
                                       invokerGen = Gen.oneOf(Seq(invoker)),
                                       payment = Some(Payment(1, Some(asset.id()))),
                                       feeGen = ciFee(1))
    } yield (a, am, r._1, r._2, r._3, asset, invoker)) {
      case (acc, amount, genesis, setScript, ci, asset, invoker) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci)), fs) { blockDiffEi =>
          blockDiffEi should produce("TransactionNotAllowedByScript")
        }
    }
  }

  property("invoking contract make payment by asset") {
    forAll(for {
      a <- accountGen
      quantity = 1000000
      am     <- Gen.choose[Long](1L, quantity)
      master <- accountGen
      ts     <- timestampGen
      asset = IssueTransactionV2
        .selfSigned(chainId, master, "Asset#1".getBytes, "".getBytes, quantity, 8, false, Some(assetAllowed), enoughFee, ts)
        .explicitGet()
      contractGen = (paymentContractGen(a, am, Some(asset.id())) _)
      r <- preconditionsAndSetContract(contractGen, masterGen = Gen.oneOf(Seq(master)), feeGen = ciFee(1))
    } yield (a, am, r._1, r._2, r._3, asset, master)) {
      case (acc, amount, genesis, setScript, ci, asset, master) =>
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci)), fs) {
          case (blockDiff, newState) =>
            newState.balance(master, Some(asset.id())) shouldBe (asset.quantity - amount)
            newState.balance(acc, Some(asset.id())) shouldBe amount
        }
    }
  }

  property("invoking contract disable by payment smart asset") {
    forAll(for {
      a <- accountGen
      quantity = 1000000
      am     <- Gen.choose[Long](1L, quantity)
      master <- accountGen
      ts     <- timestampGen
      asset = IssueTransactionV2
        .selfSigned(chainId, master, "Asset#1".getBytes, "".getBytes, quantity, 8, false, Some(assetBanned), enoughFee, ts)
        .explicitGet()
      contractGen = (paymentContractGen(a, am, Some(asset.id())) _)
      r <- preconditionsAndSetContract(contractGen, masterGen = Gen.oneOf(Seq(master)), feeGen = ciFee(1))
    } yield (a, am, r._1, r._2, r._3, asset, master)) {
      case (acc, amount, genesis, setScript, ci, asset, master) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci)), fs) { blockDiffEi =>
          blockDiffEi should produce("TransactionNotAllowedByScript")
        }
    }
  }

  property("Contract payment should be positive") {
    forAll(for {
      a <- accountGen
      quantity = 1000000
      am     <- Gen.choose[Long](1L, quantity)
      master <- accountGen
      ts     <- timestampGen
      asset = IssueTransactionV2
        .selfSigned(chainId, master, "Asset#1".getBytes, "".getBytes, quantity, 8, false, Some(assetAllowed), enoughFee, ts)
        .explicitGet()
      contractGen = (paymentContractGen(a, -1, Some(asset.id())) _)
      r <- preconditionsAndSetContract(contractGen, masterGen = Gen.oneOf(Seq(master)), feeGen = ciFee(1))
    } yield (a, am, r._1, r._2, r._3, asset, master, ts)) {
      case (acc, amount, genesis, setScript, ci, asset, master, ts) =>
        val t = TransferTransactionV2.selfSigned(Some(asset.id()), master, acc, asset.quantity / 10, ts, None, enoughFee, Array[Byte]()).explicitGet()
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(asset, t, setScript))), TestBlock.create(Seq(ci)), fs) { blockDiffEi =>
          blockDiffEi should produce("NegativeAmount")
        }
    }
  }

  property("payment should be positive") {
    forAll(for {
      invoker     <- accountGen
      master      <- accountGen
      ts          <- timestampGen
      arg         <- genBoundedString(1, 32)
      funcBinding <- validAliasStringGen
      fee         <- ciFee(1)
      fc = Terms.FUNCTION_CALL(FunctionHeader.User(funcBinding), List(CONST_BYTESTR(ByteStr(arg))))
      ci = ContractInvocationTransaction.selfSigned(invoker, master, fc, Some(Payment(-1, None)), fee, ts)
    } yield (ci)) { _ should produce("NegativeAmount") }
  }

  property("smart asset paynment require extra fee") {
    forAll(for {
      a <- accountGen
      quantity = 1000000
      am     <- Gen.choose[Long](1L, quantity)
      master <- accountGen
      ts     <- timestampGen
      asset = IssueTransactionV2
        .selfSigned(chainId, master, "Asset#1".getBytes, "".getBytes, quantity, 8, false, Some(assetBanned), enoughFee, ts)
        .explicitGet()
      contractGen = (paymentContractGen(a, am, Some(asset.id())) _)
      r <- preconditionsAndSetContract(contractGen, masterGen = Gen.oneOf(Seq(master)), feeGen = ciFee(0))
    } yield (a, am, r._1, r._2, r._3, asset, master)) {
      case (acc, amount, genesis, setScript, ci, asset, master) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci)), fs) { blockDiffEi =>
          blockDiffEi should produce("does not exceed minimal value")
        }
    }
  }

  property("contract with payment of smart asset require extra fee") {
    forAll(for {
      a  <- accountGen
      am <- smallFeeGen
      contractGen = (paymentContractGen(a, am) _)
      invoker <- accountGen
      ts      <- timestampGen
      asset = IssueTransactionV2
        .selfSigned(chainId, invoker, "Asset#1".getBytes, "".getBytes, 1000000, 8, false, Some(assetAllowed), enoughFee, ts)
        .explicitGet()
      r <- preconditionsAndSetContract(contractGen,
                                       invokerGen = Gen.oneOf(Seq(invoker)),
                                       payment = Some(Payment(1, Some(asset.id()))),
                                       feeGen = ciFee(0))
    } yield (a, am, r._1, r._2, r._3, asset, invoker)) {
      case (acc, amount, genesis, setScript, ci, asset, invoker) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci)), fs) { blockDiffEi =>
          blockDiffEi should produce("does not exceed minimal value")
        }
    }
  }

}
