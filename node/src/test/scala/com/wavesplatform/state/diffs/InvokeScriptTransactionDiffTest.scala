package com.wavesplatform.state.diffs

import cats.kernel.Monoid
import com.wavesplatform.account.{Address, AddressScheme, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction, DefaultFuncAnnotation, DefaultFunction}
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.FunctionIds.{CREATE_LIST, THROW}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{FieldNames, WavesContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.{FunctionIds, ScriptResult}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.{FunctionHeader, compiler}
import com.wavesplatform.lang.{Global, utils}
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.TransactionNotAllowedByScript
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.trace.{AssetVerifierTrace, InvokeScriptTrace}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction, WavesEnvironment}
import com.wavesplatform.transaction.transfer.TransferTransactionV2
import com.wavesplatform.transaction.{Asset, GenesisTransaction, Transaction}
import com.wavesplatform.utils.EmptyBlockchain
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import monix.eval.Coeval
import org.scalacheck.Gen
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.collection.immutable

class InvokeScriptTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB with Inside {

  def ciFee(sc: Int = 0): Gen[Long] =
    Gen.choose(
      CommonValidation.FeeUnit * CommonValidation.FeeConstants(InvokeScriptTransaction.typeId) + sc * CommonValidation.ScriptExtraFee,
      CommonValidation.FeeUnit * CommonValidation.FeeConstants(InvokeScriptTransaction.typeId) + (sc + 1) * CommonValidation.ScriptExtraFee - 1
    )

  private val fs = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id  -> 0,
      BlockchainFeatures.SmartAssets.id    -> 0,
      BlockchainFeatures.Ride4DApps.id     -> 0,
      BlockchainFeatures.FeeSponsorship.id -> 0
    ))

  val assetAllowed = ExprScript(
    FUNCTION_CALL(FunctionHeader.Native(FunctionIds.GT_LONG), List(GETTER(REF("tx"), "fee"), CONST_LONG(-1)))
  ).explicitGet()

  val assetBanned = ExprScript(FALSE).explicitGet()

  val throwingAsset = ExprScript(FUNCTION_CALL(Native(THROW), Nil)).explicitGet()

  def dataContract(senderBinding: String, argName: String, funcName: String, bigData: Boolean) = {
    val datas =
      if (bigData) List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("argument"), CONST_STRING("abcde" * 1024))), REF("nil"))
      else
        List(
          FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("argument"), REF(argName))),
          FUNCTION_CALL(
            Native(1100),
            List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender"), GETTER(GETTER(REF(senderBinding), "caller"), "bytes"))), REF("nil")))
        )

    DApp(
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
      None,
      None
    )
  }

  def paymentContract(senderBinding: String,
                      argName: String,
                      funcName: String,
                      recipientAddress: Address,
                      recipientAmount: Long,
                      assets: List[Asset] = List(Waves)): DApp = {

    val transfers: immutable.Seq[FUNCTION_CALL] = assets.map(
      a =>
        FUNCTION_CALL(
          User(FieldNames.ScriptTransfer),
          List(
            FUNCTION_CALL(User("Address"), List(CONST_BYTESTR(recipientAddress.bytes))),
            CONST_LONG(recipientAmount),
            a.fold(REF("unit"): EXPR)(asset => CONST_BYTESTR(asset.id))
          )
      ))

    val payments: EXPR = transfers.foldRight(REF("nil"): EXPR) {
      case (elem, tail) => FUNCTION_CALL(Native(CREATE_LIST), List(elem, tail))
    }

    DApp(
      List.empty,
      List(
        CallableFunction(
          CallableAnnotation(senderBinding),
          Terms.FUNC(
            funcName,
            List(argName),
            FUNCTION_CALL(
              User(FieldNames.TransferSet),
              List(payments)
            )
          )
        )),
      None,
      None
    )
  }

  def defaultPaymentContract(senderBinding: String,
                             argName: String,
                             funcName: String,
                             recipientAddress: Address,
                             recipientAmount: Long,
                             assets: List[Asset] = List(Waves)): DApp = {

    val transfers: immutable.Seq[FUNCTION_CALL] = assets.map(
      a =>
        FUNCTION_CALL(
          User(FieldNames.ScriptTransfer),
          List(
            FUNCTION_CALL(User("Address"), List(CONST_BYTESTR(recipientAddress.bytes))),
            CONST_LONG(recipientAmount),
            a.fold(REF("unit"): EXPR)(asset => CONST_BYTESTR(asset.id))
          )
      ))

    val payments: EXPR = transfers.foldRight(REF("nil"): EXPR) {
      case (elem, tail) => FUNCTION_CALL(Native(CREATE_LIST), List(elem, tail))
    }

    DApp(
      List.empty,
      List.empty,
      Some(
        DefaultFunction(
          DefaultFuncAnnotation(senderBinding),
          Terms.FUNC(
            funcName,
            List(argName),
            FUNCTION_CALL(
              User(FieldNames.TransferSet),
              List(payments)
            )
          )
        )
      ),
      None
    )
  }

  def simpleContract(funcName: String): Either[String, DApp] = {
    val expr = {
      val script =
        s"""
          |
          |{-# STDLIB_VERSION 3 #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |@Callable(xx)
          |func $funcName(amount: Int) = {
          |    if (amount + 1 != 0) then throw() else throw()
          |}
          |
          |@Verifier(txx)
          |func verify() = {
          |    false
          |}
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }

    val ctx = {
      utils.functionCosts(V3)
      Monoid
        .combineAll(
          Seq(
            PureContext.build(V3),
            CryptoContext.build(Global),
            WavesContext.build(
              DirectiveSet(V3, Account, Expression).explicitGet(),
              new WavesEnvironment('T'.toByte, Coeval(???), Coeval(???), EmptyBlockchain, Coeval(???))
            )
          ))
    }

    compiler.ContractCompiler(ctx.compilerContext, expr)
  }

  def simplePreconditionsAndSetContract(invokerGen: Gen[KeyPair] = accountGen,
                                        masterGen: Gen[KeyPair] = accountGen,
                                        payment: Option[Payment] = None,
                                        feeGen: Gen[Long] = ciFee(0),
                                        sponsored: Boolean = false)
    : Gen[(List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, KeyPair, IssueTransaction, SponsorFeeTransaction)] = {
    for {
      master  <- masterGen
      invoker <- invokerGen
      ts      <- timestampGen
      genesis: GenesisTransaction  = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      genesis2: GenesisTransaction = GenesisTransaction.create(invoker, ENOUGH_AMT, ts).explicitGet()
      fee         <- feeGen
      arg         <- genBoundedString(1, 32)
      funcBinding <- Gen.const("funcForTesting")
      contract    = simpleContract(funcBinding).explicitGet()
      script      = ContractScript(V3, contract)
      setContract = SetScriptTransaction.selfSigned(master, script.toOption, fee, ts).explicitGet()
      (issueTx, sponsorTx, sponsor1Tx, cancelTx) <- sponsorFeeCancelSponsorFeeGen(master)
      fc = Terms.FUNCTION_CALL(FunctionHeader.User(funcBinding), List(CONST_STRING("Not-a-Number")))
      ci = InvokeScriptTransaction
        .selfSigned(
          invoker,
          master,
          Some(fc),
          payment.toSeq,
          if (sponsored) { sponsorTx.minSponsoredAssetFee.get * 5 } else { fee },
          if (sponsored) {
            IssuedAsset(issueTx.id())
          } else { Waves },
          ts
        )
        .explicitGet()
    } yield (List(genesis, genesis2), setContract, ci, master, issueTx, sponsorTx)
  }

  def dataContractGen(func: String, bigData: Boolean) =
    for {
      senderBinging <- validAliasStringGen
      argBinding    <- validAliasStringGen
    } yield dataContract(senderBinging, argBinding, func, bigData)

  def paymentContractGen(address: Address, amount: Long, assets: List[Asset] = List(Waves))(func: String) =
    for {
      senderBinging <- validAliasStringGen
      argBinding    <- validAliasStringGen
    } yield paymentContract(senderBinging, argBinding, func, address, amount, assets)

  def defaultPaymentContractGen(address: Address, amount: Long, assets: List[Asset] = List(Waves))(func: String) =
    for {
      senderBinging <- validAliasStringGen
      argBinding    <- validAliasStringGen
    } yield defaultPaymentContract(senderBinging, argBinding, func, address, amount, assets)

  def preconditionsAndSetContract(senderBindingToContract: String => Gen[DApp],
                                  invokerGen: Gen[KeyPair] = accountGen,
                                  masterGen: Gen[KeyPair] = accountGen,
                                  payment: Option[Payment] = None,
                                  feeGen: Gen[Long] = ciFee(0),
                                  sponsored: Boolean = false,
                                  isCIDefaultFunc: Boolean = false)
    : Gen[(List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, KeyPair, IssueTransaction, SponsorFeeTransaction)] =
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
      script      = ContractScript(V3, contract)
      setContract = SetScriptTransaction.selfSigned(master, script.toOption, fee, ts).explicitGet()
      (issueTx, sponsorTx, sponsor1Tx, cancelTx) <- sponsorFeeCancelSponsorFeeGen(master)
      fc = if (!isCIDefaultFunc)
        Some(Terms.FUNCTION_CALL(FunctionHeader.User(funcBinding), List(CONST_BYTESTR(ByteStr(arg)))))
      else
        None
      ci = InvokeScriptTransaction
        .selfSigned(
          invoker,
          master,
          fc,
          payment.toSeq,
          if (sponsored) { sponsorTx.minSponsoredAssetFee.get * 5 } else { fee },
          if (sponsored) {
            IssuedAsset(issueTx.id())
          } else { Waves },
          ts
        )
        .explicitGet()
    } yield (List(genesis, genesis2), setContract, ci, master, issueTx, sponsorTx)

  property("invoking contract results contract's state") {
    forAll(for {
      r <- preconditionsAndSetContract(s => dataContractGen(s, false))
    } yield (r._1, r._2, r._3)) {
      case (genesis, setScript, ci) =>
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          case (blockDiff, newState) => {
            newState.accountData(genesis(0).recipient) shouldBe AccountDataInfo(
              Map(
                "sender"   -> BinaryDataEntry("sender", ci.sender.toAddress.bytes),
                "argument" -> BinaryDataEntry("argument", ci.funcCallOpt.get.args(0).asInstanceOf[CONST_BYTESTR].bs)
              ))

            blockDiff.transactions(ci.id())._3.contains(setScript.sender) shouldBe true
          }
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
            newState.balance(acc, Waves) shouldBe amount
        }
    }
  }

  property("invoking default func payment contract results in accounts state") {
    forAll(for {
      a  <- accountGen
      am <- smallFeeGen
      contractGen = (defaultPaymentContractGen(a, am) _)
      r <- preconditionsAndSetContract(contractGen, accountGen, accountGen, None, ciFee(0), false, true)
    } yield (a, am, r._1, r._2, r._3)) {
      case (acc, amount, genesis, setScript, ci) =>
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          case (blockDiff, newState) =>
            newState.balance(acc, Waves) shouldBe amount
        }
    }
  }

  property("can't make more than 10 payments") {
    forAll(for {
      a  <- accountGen
      am <- smallFeeGen
      contractGen = (paymentContractGen(a, am, List.fill(11)(Waves)) _)
      r <- preconditionsAndSetContract(contractGen)
    } yield (a, am, r._1, r._2, r._3)) {
      case (acc, amount, genesis, setScript, ci) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          _ should produce("many ScriptTransfers")
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
                                       payment = Some(Payment(1, IssuedAsset(asset.id()))),
                                       feeGen = ciFee(1))
    } yield (a, am, r._1, r._2, r._3, asset, invoker)) {
      case (acc, amount, genesis, setScript, ci, asset, invoker) =>
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci)), fs) {
          case (blockDiff, newState) =>
            newState.balance(acc, Waves) shouldBe amount
            newState.balance(invoker, IssuedAsset(asset.id())) shouldBe (asset.quantity - 1)
            newState.balance(ci.dappAddress, IssuedAsset(asset.id())) shouldBe 1
        }
    }
  }

  property("successfully invoked contract trace should contain both attached and transferring asset script info") {
    forAll(for {
      invoker <- accountGen
      quantity = 1000000000
      am     <- smallFeeGen
      master <- accountGen
      ts     <- timestampGen

      transferringAsset = IssueTransactionV2
        .selfSigned(chainId, invoker, "Asset#1".getBytes, "".getBytes, quantity, 8, false, Some(assetAllowed), enoughFee, ts)
        .explicitGet()

      attachedAsset = IssueTransactionV2
        .selfSigned(chainId, invoker, "Asset#2".getBytes, "".getBytes, quantity, 8, false, Some(assetAllowed), enoughFee, ts)
        .explicitGet()

      contractGen = paymentContractGen(master, am, List(IssuedAsset(transferringAsset.id()))) _

      r <- preconditionsAndSetContract(
        contractGen,
        masterGen = Gen.oneOf(Seq(master)),
        invokerGen = Gen.oneOf(Seq(invoker)),
        payment = Some(Payment(1, IssuedAsset(attachedAsset.id()))),
        feeGen = ciFee(2)
      )
    } yield (invoker, am, r._1, r._2, r._3, transferringAsset, attachedAsset, master)) {
      case (acc, amount, genesis, setScript, ci, transferringAsset, attachedAsset, master) =>
        assertDiffEiTraced(
          Seq(TestBlock.create(genesis ++ Seq(transferringAsset, attachedAsset, setScript))),
          TestBlock.create(Seq(ci)),
          fs
        ) { blockDiffEi =>
          blockDiffEi.resultE shouldBe 'right
          inside(blockDiffEi.trace) {
            case List(
                AssetVerifierTrace(attachedAssetId, None),
                InvokeScriptTrace(_, _, Right(ScriptResult(_, transactions))),
                AssetVerifierTrace(transferringAssetId, None)
                ) =>
              attachedAssetId shouldBe attachedAsset.id.value
              transferringAssetId shouldBe transferringAsset.id.value
              transactions.head._3.get shouldBe transferringAsset.id.value
          }
        }
    }
  }

  property("asset script ban invoking contract with payment and produce trace") {
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
                                       payment = Some(Payment(1, IssuedAsset(asset.id()))),
                                       feeGen = ciFee(1))
    } yield (a, am, r._1, r._2, r._3, asset, invoker)) {
      case (acc, amount, genesis, setScript, ci, asset, invoker) =>
        assertDiffEiTraced(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci)), fs) { blockDiffEi =>
          blockDiffEi.resultE should produce("TransactionNotAllowedByScript")
          inside(blockDiffEi.trace) {
            case List(AssetVerifierTrace(assetId, Some(TransactionNotAllowedByScript(_, isAssetScript)))) =>
              assetId shouldBe asset.id.value
              isAssetScript shouldBe true
          }
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
      contractGen = paymentContractGen(a, am, List(IssuedAsset(asset.id()))) _
      r <- preconditionsAndSetContract(contractGen, masterGen = Gen.oneOf(Seq(master)), feeGen = ciFee(1))
    } yield (a, am, r._1, r._2, r._3, asset, master)) {
      case (acc, amount, genesis, setScript, ci, asset, master) =>
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci)), fs) {
          case (blockDiff, newState) =>
            newState.balance(master, IssuedAsset(asset.id())) shouldBe (asset.quantity - amount)
            newState.balance(acc, IssuedAsset(asset.id())) shouldBe amount
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
      contractGen = paymentContractGen(a, am, List(IssuedAsset(asset.id()))) _
      r <- preconditionsAndSetContract(contractGen, masterGen = Gen.oneOf(Seq(master)), feeGen = ciFee(1))
    } yield (a, am, r._1, r._2, r._3, asset, master)) {
      case (acc, amount, genesis, setScript, ci, asset, master) =>
        assertDiffEiTraced(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci)), fs) { blockDiffEi =>
          blockDiffEi.resultE should produce("TransactionNotAllowedByScript")
        }
    }
  }

  property("invoking contract disable by one of payment smart asset with trace") {
    forAll(for {
      a <- accountGen
      quantity = 1000000
      am     <- Gen.choose[Long](1L, quantity)
      master <- accountGen
      ts     <- timestampGen
      asset1 = IssueTransactionV2
        .selfSigned(chainId, master, "Asset#1".getBytes, "".getBytes, quantity, 8, false, Some(assetAllowed), enoughFee, ts)
        .explicitGet()
      asset2 = IssueTransactionV2
        .selfSigned(chainId, master, "Asset#2".getBytes, "".getBytes, quantity, 8, false, Some(assetBanned), enoughFee, ts)
        .explicitGet()
      contractGen = paymentContractGen(a, am, List(IssuedAsset(asset1.id()), IssuedAsset(asset2.id()))) _
      r <- preconditionsAndSetContract(contractGen, masterGen = Gen.oneOf(Seq(master)), feeGen = ciFee(2))
    } yield (a, am, r._1, r._2, r._3, asset1, asset2, master)) {
      case (acc, amount, genesis, setScript, ci, asset1, asset2, master) =>
        assertDiffEiTraced(Seq(TestBlock.create(genesis ++ Seq(asset1, asset2, setScript))), TestBlock.create(Seq(ci)), fs) { blockDiffEi =>
          blockDiffEi.resultE should produce("TransactionNotAllowedByScript")
          inside(blockDiffEi.trace) {
            case List(
                InvokeScriptTrace(dAppAddress, functionOpt, Right(ScriptResult(_, transactions))),
                AssetVerifierTrace(allowedAssetId, None),
                AssetVerifierTrace(bannedAssetId, Some(TransactionNotAllowedByScript(_, _)))
                ) =>
              dAppAddress shouldBe ci.dappAddress
              functionOpt shouldBe ci.funcCallOpt

              allowedAssetId shouldBe asset1.id.value
              bannedAssetId shouldBe asset2.id.value

              transactions.flatMap(_._3.toList) shouldBe List(allowedAssetId, bannedAssetId)
          }
        }
    }
  }

  property("trace contains attached asset script invocation result when transferring asset script produce error") {
    forAll(for {
      a       <- accountGen
      am      <- smallFeeGen
      invoker <- accountGen
      ts      <- timestampGen

      attachedAsset = IssueTransactionV2
        .selfSigned(chainId, invoker, "Asset#1".getBytes, "".getBytes, 1000000, 8, false, Some(assetAllowed), enoughFee, ts)
        .explicitGet()

      transferringAsset = IssueTransactionV2
        .selfSigned(chainId, invoker, "Asset#2".getBytes, "".getBytes, 1000000, 8, false, Some(throwingAsset), enoughFee, ts)
        .explicitGet()

      r <- preconditionsAndSetContract(
        paymentContractGen(a, am, List(IssuedAsset(transferringAsset.id()))),
        invokerGen = Gen.oneOf(Seq(invoker)),
        payment = Some(Payment(1, IssuedAsset(attachedAsset.id()))),
        feeGen = ciFee(1)
      )
    } yield (a, am, r._1, r._2, r._3, transferringAsset, attachedAsset, invoker)) {
      case (acc, amount, genesis, setScript, ci, transferringAsset, attachedAsset, invoker) =>
        assertDiffEiTraced(
          Seq(TestBlock.create(genesis ++ Seq(transferringAsset, attachedAsset, setScript))),
          TestBlock.create(Seq(ci)),
          fs
        ) { blockDiffEi =>
          blockDiffEi.resultE should produce("TransactionValidationError")
          inside(blockDiffEi.trace) {
            case List(
                AssetVerifierTrace(attachedAssetId, None),
                InvokeScriptTrace(_, _, Right(ScriptResult(_, transactions)))
                ) =>
              attachedAssetId shouldBe attachedAsset.id.value
              transactions.head._3.get shouldBe transferringAsset.id.value
          }
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
      contractGen = (paymentContractGen(a, -1, List(IssuedAsset(asset.id()))) _)
      r <- preconditionsAndSetContract(contractGen, masterGen = Gen.oneOf(Seq(master)), feeGen = ciFee(1))
    } yield (a, am, r._1, r._2, r._3, asset, master, ts)) {
      case (acc, amount, genesis, setScript, ci, asset, master, ts) =>
        val t =
          TransferTransactionV2
            .selfSigned(IssuedAsset(asset.id()), master, acc, asset.quantity / 10, ts, Waves, enoughFee, Array[Byte]())
            .explicitGet()
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
      ci = InvokeScriptTransaction.selfSigned(invoker, master, Some(fc), Seq(Payment(-1, Waves)), fee, Waves, ts)
    } yield ci) { _ should produce("NonPositiveAmount") }
  }

  property("smart asset payment require extra fee") {
    forAll(for {
      a <- accountGen
      quantity = 1000000
      am     <- Gen.choose[Long](1L, quantity)
      master <- accountGen
      ts     <- timestampGen
      asset = IssueTransactionV2
        .selfSigned(chainId, master, "Asset#1".getBytes, "".getBytes, quantity, 8, false, Some(assetBanned), enoughFee, ts)
        .explicitGet()
      contractGen = (paymentContractGen(a, am, List(IssuedAsset(asset.id()))) _)
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
                                       payment = Some(Payment(1, IssuedAsset(asset.id()))),
                                       feeGen = ciFee(0))
    } yield (a, am, r._1, r._2, r._3, asset, invoker)) {
      case (acc, amount, genesis, setScript, ci, asset, invoker) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci)), fs) { blockDiffEi =>
          blockDiffEi should produce("does not exceed minimal value")
        }
    }
  }

  property("can't overflow payment + fee") {
    forAll(for {
      a  <- accountGen
      am <- smallFeeGen
      contractGen = (paymentContractGen(a, am) _)
      invoker <- accountGen
      ts      <- timestampGen
      r <- preconditionsAndSetContract(contractGen,
                                       invokerGen = Gen.oneOf(Seq(invoker)),
                                       payment = Some(Payment(Long.MaxValue, Waves)),
                                       feeGen = ciFee(1))
    } yield (r._1, r._2, r._3)) {
      case (genesis, setScript, ci) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          _ should produce("Attempt to transfer unavailable funds")
        }
    }
  }

  property("can't overflow sum of payment in contract") {
    forAll(for {
      a  <- accountGen
      am <- smallFeeGen
      contractGen = (paymentContractGen(a, Long.MaxValue / 2 + 2, List.fill(4)(Waves)) _)
      invoker <- accountGen
      ts      <- timestampGen
      r       <- preconditionsAndSetContract(contractGen, invokerGen = Gen.oneOf(Seq(invoker)), payment = Some(Payment(1, Waves)), feeGen = ciFee(1))
    } yield (r._1, r._2, r._3)) {
      case (genesis, setScript, ci) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          _ should produce("Attempt to transfer unavailable funds")
        }
    }
  }

  property("invoking contract with sponsored fee") {
    forAll(for {
      a  <- accountGen
      am <- smallFeeGen
      contractGen = (paymentContractGen(a, am) _)
      r  <- preconditionsAndSetContract(contractGen, sponsored = true)
      ts <- timestampGen
    } yield (ts, a, am, r._1, r._2, r._3, r._4, r._5, r._6)) {
      case (ts, acc, amount, genesis, setScript, ci, master, sponsoredAsset, setSponsorship) =>
        val t =
          TransferTransactionV2
            .selfSigned(IssuedAsset(sponsoredAsset.id()), master, ci.sender, sponsoredAsset.quantity / 10, ts, Waves, enoughFee, Array[Byte]())
            .explicitGet()
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq[Transaction](sponsoredAsset, t, setSponsorship, setScript))),
                           TestBlock.create(Seq(ci)),
                           fs) {
          case (blockDiff, newState) =>
            newState.balance(acc, Waves) shouldBe amount
            newState.balance(ci.sender, IssuedAsset(sponsoredAsset.id())) shouldBe (sponsoredAsset.quantity / 10 - ci.fee)
            newState.balance(master, IssuedAsset(sponsoredAsset.id())) shouldBe (sponsoredAsset.quantity - sponsoredAsset.quantity / 10 + ci.fee)
        }
    }
  }

  property("argument passed to callable function has wrong type") {
    forAll(for {
      r <- simplePreconditionsAndSetContract()
    } yield (r._1, r._2, r._3)) {
      case (genesis, setScript, ci) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          _ should produce("Passed argument with wrong type")
        }
    }
  }
}
