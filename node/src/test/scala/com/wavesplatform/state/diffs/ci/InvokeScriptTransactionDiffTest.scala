package com.wavesplatform.state.diffs.ci

import cats.kernel.Monoid
import com.google.protobuf.ByteString
import com.wavesplatform.account._
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithState}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{DApp => DAppType, _}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.FunctionIds.{CREATE_LIST, THROW}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{FieldNames, WavesContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.{FunctionIds, ScriptResultV3}
import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader, compiler}
import com.wavesplatform.lang.{Global, utils}
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.settings.{TestFunctionalitySettings, TestSettings}
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.FeeValidation.FeeConstants
import com.wavesplatform.state.diffs.{ENOUGH_AMT, FeeValidation, InvokeScriptTransactionDiff, produce}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.trace.{AssetVerifierTrace, InvokeScriptTrace}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, _}
import com.wavesplatform.utils._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.collection.immutable

class InvokeScriptTransactionDiffTest
    extends PropSpec
    with PropertyChecks
    with Matchers
    with TransactionGen
    with NoShrink
    with Inside
    with WithState
    with DBCacheSettings
    with MockFactory {

  private val fs = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id  -> 0,
      BlockchainFeatures.SmartAssets.id    -> 0,
      BlockchainFeatures.Ride4DApps.id     -> 0,
      BlockchainFeatures.FeeSponsorship.id -> 0
    )
  )

  private val fsWithV5 = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id  -> 0,
      BlockchainFeatures.SmartAssets.id    -> 0,
      BlockchainFeatures.Ride4DApps.id     -> 0,
      BlockchainFeatures.FeeSponsorship.id -> 0,
      BlockchainFeatures.BlockV5.id -> 0
    )
  )

  val assetAllowed: Script = ExprScript(
    FUNCTION_CALL(FunctionHeader.Native(FunctionIds.GT_LONG), List(GETTER(REF("tx"), "fee"), CONST_LONG(-1)))
  ).explicitGet()

  val assetUsingThis: Script = ExprScript(
    V3,
    FUNCTION_CALL(
      FunctionHeader.Native(FunctionIds.EQ),
      List(REF("this"), REF("this"))
    ),
    checkSize = false
  ).explicitGet()

  val assetBanned: Script = ExprScript(FALSE).explicitGet()

  val throwingAsset: Script = ExprScript(FUNCTION_CALL(Native(THROW), Nil)).explicitGet()

  private def dataContract(senderBinding: String, argName: String, funcName: String, bigData: Boolean, emptyData: Boolean): DApp = {
    val datas =
      if (bigData)
        List(
          FUNCTION_CALL(
            User("DataEntry"),
            List(CONST_STRING("argument").explicitGet(), CONST_STRING("abcde" * 1024).explicitGet())
          ),
          REF("nil")
        )
      else if (emptyData)
        List(
          FUNCTION_CALL(
            User("DataEntry"),
            List(CONST_STRING("").explicitGet(), CONST_STRING("abcde").explicitGet())
          ),
          REF("nil")
        )
      else
        List(
          FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("argument").explicitGet(), REF(argName))),
          FUNCTION_CALL(
            Native(1100),
            List(
              FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender").explicitGet(), GETTER(GETTER(REF(senderBinding), "caller"), "bytes"))),
              REF("nil")
            )
          )
        )

    DApp(
      DAppMeta(),
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
                )
              )
            )
          )
        )
      ),
      None
    )
  }

  def paymentContract(
      senderBinding: String,
      argName: String,
      funcName: String,
      recipientAddress: Address,
      recipientAmount: Long,
      assets: List[Asset] = List(Waves),
      version: StdLibVersion = V3
  ): DApp = {

    val transfers: immutable.Seq[FUNCTION_CALL] = assets.map(
      a =>
        FUNCTION_CALL(
          User(FieldNames.ScriptTransfer),
          List(
            FUNCTION_CALL(User("Address"), List(CONST_BYTESTR(ByteStr(recipientAddress.bytes)).explicitGet())),
            CONST_LONG(recipientAmount),
            a.fold(REF("unit"): EXPR)(asset => CONST_BYTESTR(asset.id).explicitGet())
          )
        )
    )

    val payments: EXPR = transfers.foldRight(REF("nil"): EXPR) {
      case (elem, tail) => FUNCTION_CALL(Native(CREATE_LIST), List(elem, tail))
    }

    DApp(
      DAppMeta(),
      List.empty,
      List(
        CallableFunction(
          CallableAnnotation(senderBinding),
          Terms.FUNC(
            funcName,
            List(argName),
            if (version >= V4) payments
            else FUNCTION_CALL(User(FieldNames.TransferSet), List(payments))
          )
        )
      ),
      None
    )
  }

  def defaultPaymentContract(
      senderBinding: String,
      argName: String,
      recipientAddress: AddressOrAlias,
      recipientAmount: Long,
      assets: List[Asset] = List(Waves)
  ): DApp = {

    val transfers: immutable.Seq[FUNCTION_CALL] = assets.map(
      a =>
        FUNCTION_CALL(
          User(FieldNames.ScriptTransfer),
          List(
            recipientAddress match {
              case recipientAddress: Address => FUNCTION_CALL(User("Address"), List(CONST_BYTESTR(ByteStr(recipientAddress.bytes)).explicitGet()))
              case recipientAddress: Alias   => FUNCTION_CALL(User("Alias"), List(CONST_STRING(recipientAddress.name).explicitGet()))
            },
            CONST_LONG(recipientAmount),
            a.fold(REF("unit"): EXPR)(asset => CONST_BYTESTR(asset.id).explicitGet())
          )
        )
    )

    val payments: EXPR = transfers.foldRight(REF("nil"): EXPR) {
      case (elem, tail) => FUNCTION_CALL(Native(CREATE_LIST), List(elem, tail))
    }

    DApp(
      DAppMeta(),
      List.empty,
      List(
        CallableFunction(
          CallableAnnotation(senderBinding),
          Terms.FUNC(
            "default",
            Nil,
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
          |func $funcName(str: String, num: Int) = {
          |    if (parseInt(str) == num) then throw() else throw()
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
            PureContext.build(Global, V3).withEnvironment[Environment],
            CryptoContext.build(Global, V3).withEnvironment[Environment],
            WavesContext.build(
              DirectiveSet(V3, Account, Expression).explicitGet()
            )
          )
        )
    }

    compiler.ContractCompiler(ctx.compilerContext, expr, V3)
  }

  def writeSet(funcName: String, count: Int): DApp = {
    val DataEntries = Array.tabulate(count)(i => s"""DataEntry("$i", $i)""").mkString(",")

    val expr = {
      val script =
        s"""
           |
           | {-#STDLIB_VERSION 3 #-}
           | {-#CONTENT_TYPE DAPP#-}
           | {-#SCRIPT_TYPE ACCOUNT#-}
           |
           | @Callable(i)
           | func $funcName(b: ByteVector) = {
           |    WriteSet([
           |      $DataEntries
           |        ])
           |}
           |
        """.stripMargin
      Parser.parseContract(script).get.value
    }

    compileContractFromExpr(expr)
  }

  def writeSetWithKeyLength(funcName: String, length: Int = 1): DApp = {
    val keyName = Array.fill(length)("a").mkString

    val expr = {
      val script =
        s"""
           |
           | {-#STDLIB_VERSION 3 #-}
           | {-#CONTENT_TYPE DAPP#-}
           | {-#SCRIPT_TYPE ACCOUNT#-}
           |
           | @Callable(i)
           | func $funcName(b: ByteVector) = {
           |    WriteSet([
           |      DataEntry("$keyName", 0)
           |        ])
           |}
           |
        """.stripMargin
      Parser.parseContract(script).get.value
    }

    compileContractFromExpr(expr)
  }

  def compileContractFromExpr(expr: Expressions.DAPP, stdLibVersion: StdLibVersion = V3): DApp = {
    val ctx = {
      utils.functionCosts(stdLibVersion)
      Monoid
        .combineAll(
          Seq(
            PureContext.build(Global, stdLibVersion).withEnvironment[Environment],
            CryptoContext.build(Global, stdLibVersion).withEnvironment[Environment],
            WavesContext.build(
              DirectiveSet(stdLibVersion, Account, DAppType).explicitGet()
            )
          )
        )
    }

    compiler.ContractCompiler(ctx.compilerContext, expr, stdLibVersion).right.get
  }

  def simplePreconditionsAndSetContract(
      invokerGen: Gen[KeyPair] = accountGen,
      masterGen: Gen[KeyPair] = accountGen,
      payment: Option[Payment] = None,
      feeGen: Gen[Long] = ciFee(0),
      sponsored: Boolean = false,
      invocationParamsCount: Int = 1
  ): Gen[(List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, KeyPair, IssueTransaction, SponsorFeeTransaction)] = {
    for {
      master  <- masterGen
      invoker <- invokerGen
      ts      <- timestampGen
      genesis: GenesisTransaction  = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      genesis2: GenesisTransaction = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      fee         <- feeGen
      arg         <- genBoundedString(1, 32)
      funcBinding <- Gen.const("funcForTesting")
      contract    = simpleContract(funcBinding).explicitGet()
      script      = ContractScript(V3, contract)
      setContract = SetScriptTransaction.selfSigned(1.toByte, master, script.toOption, fee, ts).explicitGet()
      (issueTx, sponsorTx, sponsor1Tx, cancelTx) <- sponsorFeeCancelSponsorFeeGen(master)
      fc = Terms.FUNCTION_CALL(
        FunctionHeader.User(funcBinding),
        List.fill(invocationParamsCount)(FALSE)
      )
      ci = InvokeScriptTransaction
        .selfSigned(
          1.toByte,
          invoker,
          master.toAddress,
          Some(fc),
          payment.toSeq,
          if (sponsored) {
            sponsorTx.minSponsoredAssetFee.get * 5
          } else {
            fee
          },
          if (sponsored) {
            IssuedAsset(issueTx.id())
          } else {
            Waves
          },
          ts
        )
        .explicitGet()
    } yield (List(genesis, genesis2), setContract, ci, master, issueTx, sponsorTx)
  }

  def dataContractGen(func: String, bigData: Boolean = false, emptyData: Boolean = false): Gen[DApp] =
    for {
      senderBinging <- validAliasStringGen
      argBinding    <- validAliasStringGen
    } yield dataContract(senderBinging, argBinding, func, bigData, emptyData)

  def paymentContractGen(address: Address, amount: Long, assets: List[Asset] = List(Waves), version: StdLibVersion = V3)(func: String): Gen[DApp] =
    for {
      senderBinging <- validAliasStringGen
      argBinding    <- validAliasStringGen
    } yield paymentContract(senderBinging, argBinding, func, address, amount, assets, version)

  def defaultPaymentContractGen(address: AddressOrAlias, amount: Long, assets: List[Asset] = List(Waves))(someName: String): Gen[DApp] =
    for {
      senderBinging <- validAliasStringGen
      argBinding    <- validAliasStringGen
    } yield defaultPaymentContract(senderBinging, argBinding, address, amount, assets)

  def preconditionsAndSetContract(
      senderBindingToContract: String => Gen[DApp],
      invokerGen: Gen[KeyPair] = accountGen,
      masterGen: Gen[KeyPair] = accountGen,
      payment: Option[Payment] = None,
      feeGen: Gen[Long] = ciFee(0),
      sponsored: Boolean = false,
      isCIDefaultFunc: Boolean = false,
      version: StdLibVersion = V3,
      txVersion: TxVersion = TxVersion.V1,
      selfSend: Boolean = false
  ): Gen[(List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, KeyPair, IssueTransaction, SponsorFeeTransaction)] =
    for {
      master  <- masterGen
      invoker <- if (selfSend) Gen.const(master) else invokerGen
      ts      <- timestampGen
      genesis: GenesisTransaction  = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      genesis2: GenesisTransaction = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      fee         <- feeGen
      arg         <- genBoundedStringBytes(1, 32)
      funcBinding <- funcNameGen
      contract    <- senderBindingToContract(funcBinding)
      script      = ContractScript(version, contract)
      setContract = SetScriptTransaction.selfSigned(1.toByte, master, script.toOption, fee, ts + 2).explicitGet()
      (issueTx, sponsorTx, _, _) <- sponsorFeeCancelSponsorFeeGen(master)
      sponsoredFee = Sponsorship.fromWaves(900000L, sponsorTx.minSponsoredAssetFee.get)
      fc = if (!isCIDefaultFunc)
        Some(Terms.FUNCTION_CALL(FunctionHeader.User(funcBinding), List(CONST_BYTESTR(ByteStr(arg)).explicitGet())))
      else
        None
      ci = InvokeScriptTransaction
        .selfSigned(
          txVersion,
          invoker,
          master.toAddress,
          fc,
          payment.toSeq,
          if (sponsored) sponsoredFee else fee,
          if (sponsored) sponsorTx.asset else Waves,
          ts + 3
        )
        .explicitGet()
    } yield (if (selfSend) List(genesis) else List(genesis, genesis2), setContract, ci, master, issueTx, sponsorTx)

  def preconditionsAndSetContractWithVerifier(
      verifier: DApp,
      senderBindingToContract: String => Gen[DApp],
      invokerGen: Gen[KeyPair] = accountGen,
      masterGen: Gen[KeyPair] = accountGen,
      payment: Option[Payment] = None,
      feeGen: Gen[Long] = ciFee(1),
      sponsored: Boolean = false,
      isCIDefaultFunc: Boolean = false
  ): Gen[
    (List[GenesisTransaction], SetScriptTransaction, SetScriptTransaction, InvokeScriptTransaction, KeyPair, IssueTransaction, SponsorFeeTransaction)
  ] =
    for {
      master  <- masterGen
      invoker <- invokerGen
      ts      <- timestampGen
      genesis: GenesisTransaction  = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      genesis2: GenesisTransaction = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      fee         <- feeGen
      arg         <- genBoundedStringBytes(1, 32)
      funcBinding <- funcNameGen
      contract    <- senderBindingToContract(funcBinding)
      script      = ContractScript(V3, contract)
      setVerifier = SetScriptTransaction.selfSigned(1.toByte, invoker, ContractScript(V3, verifier).toOption, fee, ts + 2).explicitGet()
      setContract = SetScriptTransaction.selfSigned(1.toByte, master, script.toOption, fee, ts + 2).explicitGet()
      (issueTx, sponsorTx, sponsor1Tx, cancelTx) <- sponsorFeeCancelSponsorFeeGen(master)
      fc = if (!isCIDefaultFunc)
        Some(Terms.FUNCTION_CALL(FunctionHeader.User(funcBinding), List(CONST_BYTESTR(ByteStr(arg)).explicitGet())))
      else
        None
      ci = InvokeScriptTransaction
        .selfSigned(
          1.toByte,
          invoker,
          master.toAddress,
          fc,
          payment.toSeq,
          if (sponsored) {
            sponsorTx.minSponsoredAssetFee.get * 5
          } else {
            fee
          },
          if (sponsored) {
            IssuedAsset(issueTx.id())
          } else {
            Waves
          },
          ts + 3
        )
        .explicitGet()
    } yield (List(genesis, genesis2), setVerifier, setContract, ci, master, issueTx, sponsorTx)

  def preconditionsAndSetContractWithAlias(
      senderBindingToContract: String => Gen[DApp],
      invokerGen: Gen[KeyPair] = accountGen,
      masterGen: Gen[KeyPair] = accountGen,
      payment: Option[Payment] = None,
      feeGen: Gen[Long] = ciFee(0),
      sponsored: Boolean = false,
      isCIDefaultFunc: Boolean = false
  ): Gen[(List[GenesisTransaction], KeyPair, SetScriptTransaction, InvokeScriptTransaction, InvokeScriptTransaction, CreateAliasTransaction)] =
    for {
      master  <- masterGen
      invoker <- invokerGen
      ts      <- timestampGen
      genesis: GenesisTransaction  = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      genesis2: GenesisTransaction = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      fee         <- feeGen
      arg         <- genBoundedStringBytes(1, 32)
      funcBinding <- validAliasStringGen
      contract    <- senderBindingToContract(funcBinding)
      masterAlias = Alias.create("alias").explicitGet()
      fakeAlias   = Alias.create("fakealias").explicitGet()
      aliasTx <- createAliasGen(master, masterAlias, fee, ts + 1)
      script      = ContractScript(V3, contract)
      setContract = SetScriptTransaction.selfSigned(1.toByte, master, script.toOption, fee, ts + 2).explicitGet()
      (issueTx, sponsorTx, sponsor1Tx, cancelTx) <- sponsorFeeCancelSponsorFeeGen(master)
      fc = if (!isCIDefaultFunc)
        Some(Terms.FUNCTION_CALL(FunctionHeader.User(funcBinding), List(CONST_BYTESTR(ByteStr(arg)).explicitGet())))
      else
        None
      ciWithAlias = InvokeScriptTransaction
        .selfSigned(
          1.toByte,
          invoker,
          masterAlias,
          fc,
          payment.toSeq,
          if (sponsored) {
            sponsorTx.minSponsoredAssetFee.get * 5
          } else {
            fee
          },
          if (sponsored) {
            IssuedAsset(issueTx.id())
          } else {
            Waves
          },
          ts + 3
        )
        .explicitGet()
      ciWithFakeAlias = InvokeScriptTransaction
        .selfSigned(
          1.toByte,
          invoker,
          fakeAlias,
          fc,
          payment.toSeq,
          if (sponsored) {
            sponsorTx.minSponsoredAssetFee.get * 5
          } else {
            fee
          },
          if (sponsored) {
            IssuedAsset(issueTx.id())
          } else {
            Waves
          },
          ts + 3
        )
        .explicitGet()
    } yield (List(genesis, genesis2), master, setContract, ciWithAlias, ciWithFakeAlias, aliasTx)

  property("invoking contract results contract's state") {
    forAll(for {
      r <- preconditionsAndSetContract(s => dataContractGen(s, bigData = false))
    } yield (r._1, r._2, r._3)) {
      case (genesis, setScript, ci) =>
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci), Block.ProtoBlockVersion), fs) {
          case (blockDiff, newState) =>
            blockDiff.scriptsRun shouldBe 1
            newState.accountData(genesis(0).recipient, "sender") shouldBe Some(BinaryDataEntry("sender", ByteStr(ci.sender.toAddress.bytes)))
            newState.accountData(genesis(0).recipient, "argument") shouldBe Some(
              BinaryDataEntry("argument", ci.funcCallOpt.get.args.head.asInstanceOf[CONST_BYTESTR].bs)
            )

            blockDiff.transactions(ci.id())._2.contains(setScript.sender.toAddress) shouldBe true
        }

    }
  }

  property("can't more than 5kb of data") {
    forAll(for {
      r <- preconditionsAndSetContract(s => dataContractGen(s, bigData = true))
    } yield (r._1, r._2, r._3)) {
      case (genesis, setScript, ci) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fsWithV5) {
          _ should produce("WriteSet size can't exceed")
        }
    }
  }

  property("can't use empty keys in v2") {
    forAll(for {
      r <- preconditionsAndSetContract(s => dataContractGen(s, emptyData = true), txVersion = TxVersion.V1)
    } yield (r._1, r._2, r._3)) {
      case (genesis, setScript, ci) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fsWithV5) {
          _ shouldBe 'right
        }
    }

    forAll(for {
      r <- preconditionsAndSetContract(s => dataContractGen(s, emptyData = true), txVersion = TxVersion.V2)
    } yield (r._1, r._2, r._3)) {
      case (genesis, setScript, ci) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fsWithV5) {
          _ should produce("Empty keys aren't allowed")
        }
    }
  }

  property("invoking payment contract results in accounts state") {
    forAll(for {
      a  <- accountGen
      am <- smallFeeGen
      contractGen = paymentContractGen(a.toAddress, am) _
      r <- preconditionsAndSetContract(contractGen)
    } yield (a, am, r._1, r._2, r._3)) {
      case (acc, amount, genesis, setScript, ci) =>
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci), Block.ProtoBlockVersion), fs) {
          case (blockDiff, newState) =>
            blockDiff.scriptsRun shouldBe 1
            newState.balance(acc.toAddress, Waves) shouldBe amount
        }
    }
  }

  property("invoking script transaction can be found for recipient") {
    forAll(for {
      a  <- accountGen
      am <- smallFeeGen
      contractGen = paymentContractGen(a.toAddress, am) _
      r <- preconditionsAndSetContract(contractGen)
    } yield (a, am, r._1, r._2, r._3)) {
      case (_, _, genesis, setScript, ci) =>
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci), Block.ProtoBlockVersion), fs) {
          case (blockDiff, _) =>
            blockDiff.transactions should contain key (ci.id())
        }
    }
  }

  property("invoking default func payment contract results in accounts state") {
    forAll(for {
      a  <- accountGen
      am <- smallFeeGen
      contractGen = defaultPaymentContractGen(a.toAddress, am) _
      r <- preconditionsAndSetContract(contractGen, accountGen, accountGen, None, ciFee(0), sponsored = false, isCIDefaultFunc = true)
    } yield (a, am, r._1, r._2, r._3)) {
      case (acc, amount, genesis, setScript, ci) =>
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci), Block.ProtoBlockVersion), fs) {
          case (blockDiff, newState) =>
            newState.balance(acc.toAddress, Waves) shouldBe amount
        }
    }
  }

  property("invoking default func payment to alias contract results in accounts state") {
    forAll(for {
      ts  <- timestampGen
      fee <- ciFee(0)
      a   <- accountGen
      genesis2: GenesisTransaction = GenesisTransaction.create(a.toAddress, fee, ts).explicitGet()
      alias                        = Alias.create("alias").explicitGet()
      aliasTx <- createAliasGen(a, alias, fee, ts)
      am      <- smallFeeGen
      contractGen = defaultPaymentContractGen(alias, am) _
      r <- preconditionsAndSetContract(contractGen, accountGen, accountGen, None, ciFee(0), sponsored = false, isCIDefaultFunc = true)
    } yield (a, aliasTx, am, genesis2, r._1, r._2, r._3)) {
      case (acc, aliasTx, amount, genesis2, genesis, setScript, ci) =>
        val features = fs.copy(preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0))
        assertDiffAndState(
          Seq(TestBlock.create(genesis ++ Seq(genesis2, setScript, aliasTx))),
          TestBlock.create(Seq(ci), Block.ProtoBlockVersion),
          features
        ) {
          case (blockDiff, newState) =>
            newState.balance(acc.toAddress, Waves) shouldBe amount
        }
    }
  }

  property("payment to alias before feature activation") {
    forAll(for {
      ts  <- timestampGen
      fee <- ciFee(0)
      a   <- accountGen
      genesis2: GenesisTransaction = GenesisTransaction.create(a.toAddress, fee, ts).explicitGet()
      alias                        = Alias.create("alias").explicitGet()
      aliasTx <- createAliasGen(a, alias, fee, ts)
      am      <- smallFeeGen
      contractGen = defaultPaymentContractGen(alias, am) _
      r <- preconditionsAndSetContract(contractGen, accountGen, accountGen, None, ciFee(0), sponsored = false, isCIDefaultFunc = true)
    } yield (a, aliasTx, am, genesis2, r._1, r._2, r._3)) {
      case (acc, aliasTx, amount, genesis2, genesis, setScript, ci) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(genesis2, setScript, aliasTx))), TestBlock.create(Seq(ci), Block.ProtoBlockVersion), fs) {
          err =>
            err shouldBe 'Left
        }
    }
  }

  property("suitable verifier error message on incorrect proofs number") {
    forAll(for {
      proofCount <- Gen.oneOf(0, 2)
      a          <- accountGen
      am         <- smallFeeGen
      contractGen = defaultPaymentContractGen(a.toAddress, am) _
      r <- preconditionsAndSetContract(contractGen, accountGen, accountGen, None, ciFee(0), sponsored = false, isCIDefaultFunc = true)
    } yield (a, am, r._1, r._2, r._3, proofCount)) {
      case (acc, amount, genesis, setScript, ci, proofCount) =>
        val proofs = Proofs(
          List.fill(proofCount)(ByteStr.fromBytes(1, 1))
        )

        assertDiffEi(
          Seq(TestBlock.create(genesis ++ Seq(setScript))),
          TestBlock.create(Seq(ci.copy(1.toByte, proofs = proofs))),
          fs
        ) {
          _ should produce("Transactions from non-scripted accounts must have exactly 1 proof")
        }
    }
  }

  property("suitable verifier error message on incorrect proof") {
    forAll(for {
      a  <- accountGen
      am <- smallFeeGen
      contractGen = defaultPaymentContractGen(a.toAddress, am) _
      r <- preconditionsAndSetContract(contractGen, accountGen, accountGen, None, ciFee(0), sponsored = false, isCIDefaultFunc = true)
    } yield (a, am, r._1, r._2, r._3)) {
      case (acc, amount, genesis, setScript, ci) =>
        val proofs = Proofs(List(ByteStr.fromBytes(1, 1)))

        assertDiffEi(
          Seq(TestBlock.create(genesis ++ Seq(setScript))),
          TestBlock.create(Seq(ci.copy(1.toByte, proofs = proofs))),
          fs
        ) {
          _ should produce("Proof doesn't validate as signature")
        }
    }
  }

  property("invoke script by alias") {
    forAll(for {
      a  <- accountGen
      am <- smallFeeGen
      contractGen = paymentContractGen(a.toAddress, am) _
      r <- preconditionsAndSetContractWithAlias(contractGen)
    } yield (a, am, r._1, r._3, r._6, r._4)) {
      case (acc, amount, genesis, setScript, aliasTx, ciWithAlias) =>
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(aliasTx, setScript))), TestBlock.create(Seq(ciWithAlias), Block.ProtoBlockVersion), fs) {
          case (blockDiff, newState) =>
            blockDiff.scriptsRun shouldBe 1
            newState.balance(acc.toAddress, Waves) shouldBe amount
        }
    }
  }

  property("invoke script by non existed alias") {
    forAll(for {
      a  <- accountGen
      am <- smallFeeGen
      contractGen = paymentContractGen(a.toAddress, am) _
      r <- preconditionsAndSetContractWithAlias(contractGen)
    } yield (a, am, r._1, r._3, r._6, r._5)) {
      case (acc, amount, genesis, setScript, aliasTx, ciWithFakeAlias) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(aliasTx, setScript))), TestBlock.create(Seq(ciWithFakeAlias)), fs) {
          _ should produce("does not exist")
        }
    }
  }

  property("can't make more than 10 payments") {
    forAll(for {
      a  <- accountGen
      am <- smallFeeGen
      contractGen = paymentContractGen(a.toAddress, am, List.fill(11)(Waves)) _
      r <- preconditionsAndSetContract(contractGen)
    } yield (a, am, r._1, r._2, r._3)) {
      case (acc, amount, genesis, setScript, ci) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          _ should produce("Too many script actions")
        }
    }
  }

  val chainId: TxVersion     = AddressScheme.current.chainId
  val enoughFee: TxTimestamp = FeeValidation.ScriptExtraFee + FeeValidation.FeeConstants(IssueTransaction.typeId) * FeeValidation.FeeUnit

  property("invoking contract receive payment") {
    forAll(for {
      a  <- accountGen
      am <- smallFeeGen
      contractGen = paymentContractGen(a.toAddress, am) _
      invoker <- accountGen
      ts      <- timestampGen
      asset = IssueTransaction(
        TxVersion.V2,
        invoker.publicKey,
        "Asset#1".utf8Bytes,
        Array.emptyByteArray,
        1000000,
        8,
        reissuable = false,
        Some(assetAllowed),
        enoughFee,
        ts
      ).signWith(invoker.privateKey)
      r <- preconditionsAndSetContract(
        contractGen,
        invokerGen = Gen.oneOf(Seq(invoker)),
        payment = Some(Payment(1, IssuedAsset(asset.id()))),
        feeGen = ciFee(1)
      )
    } yield (a, am, r._1, r._2, r._3, r._4, asset, invoker)) {
      case (acc, amount, genesis, setScript, ci, dAppAddress, asset, invoker) =>
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci), Block.ProtoBlockVersion), fs) {
          case (blockDiff, newState) =>
            blockDiff.scriptsRun shouldBe 2
            newState.balance(acc.toAddress, Waves) shouldBe amount
            newState.balance(invoker.toAddress, IssuedAsset(asset.id())) shouldBe (asset.quantity - 1)
            newState.balance(dAppAddress.toAddress, IssuedAsset(asset.id())) shouldBe 1
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

      transferringAsset = IssueTransaction(
        TxVersion.V2,
        invoker.publicKey,
        "Asset#1".utf8Bytes,
        Array.emptyByteArray,
        quantity,
        8,
        reissuable = false,
        Some(assetAllowed),
        enoughFee,
        ts
      ).signWith(invoker.privateKey)

      attachedAsset = IssueTransaction(
        TxVersion.V2,
        invoker.publicKey,
        "Asset#2".utf8Bytes,
        Array.emptyByteArray,
        quantity,
        8,
        reissuable = false,
        Some(assetAllowed),
        enoughFee,
        ts
      ).signWith(invoker.privateKey)

      contractGen = paymentContractGen(master.toAddress, am, List(IssuedAsset(transferringAsset.id()))) _

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
          blockDiffEi.resultE.right.get.scriptsRun shouldBe 3
          inside(blockDiffEi.trace) {
            case List(
                InvokeScriptTrace(_, _, Right(ScriptResultV3(_, transfers)), _),
                AssetVerifierTrace(transferringAssetId, None),
                AssetVerifierTrace(attachedAssetId, None)
                ) =>
              attachedAssetId shouldBe attachedAsset.id.value
              transferringAssetId shouldBe transferringAsset.id.value
              transfers.head.assetId.get shouldBe transferringAsset.id.value
          }
        }
    }
  }

  property("asset script ban invoking contract with payment and produce trace") {
    forAll(for {
      a  <- accountGen
      am <- smallFeeGen
      contractGen = paymentContractGen(a.toAddress, am) _
      invoker <- accountGen
      ts      <- timestampGen
      asset = IssueTransaction(
        TxVersion.V2,
        invoker.publicKey,
        "Asset#1".utf8Bytes,
        Array.emptyByteArray,
        1000000,
        8,
        reissuable = false,
        Some(assetBanned),
        enoughFee,
        ts
      ).signWith(invoker.privateKey)
      r <- preconditionsAndSetContract(
        contractGen,
        invokerGen = Gen.oneOf(Seq(invoker)),
        payment = Some(Payment(1, IssuedAsset(asset.id()))),
        feeGen = ciFee(1)
      )
    } yield (a, am, r._1, r._2, r._3, asset, invoker)) {
      case (acc, amount, genesis, setScript, ci, asset, invoker) =>
        assertDiffEiTraced(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci)), fs) { blockDiffEi =>
          blockDiffEi.resultE should produce("TransactionNotAllowedByScript")
          inside(blockDiffEi.trace) {
            case List(_, AssetVerifierTrace(assetId, Some(TransactionNotAllowedByScript(_, isAssetScript)))) =>
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
      asset = IssueTransaction(
        TxVersion.V2,
        master.publicKey,
        "Asset#1".utf8Bytes,
        Array.emptyByteArray,
        quantity,
        8,
        reissuable = false,
        Some(assetAllowed),
        enoughFee,
        ts
      ).signWith(master.privateKey)
      contractGen = paymentContractGen(a.toAddress, am, List(IssuedAsset(asset.id()))) _
      r <- preconditionsAndSetContract(contractGen, masterGen = Gen.oneOf(Seq(master)), feeGen = ciFee(1))
    } yield (a, am, r._1, r._2, r._3, asset, master)) {
      case (acc, amount, genesis, setScript, ci, asset, master) =>
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(asset, ci), Block.ProtoBlockVersion), fs) {
          case (blockDiff, newState) =>
            blockDiff.scriptsRun shouldBe 3
            newState.balance(master.toAddress, IssuedAsset(asset.id())) shouldBe (asset.quantity - amount)
            newState.balance(acc.toAddress, IssuedAsset(asset.id())) shouldBe amount
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
      asset = IssueTransaction(
        TxVersion.V2,
        master.publicKey,
        "Asset#1".utf8Bytes,
        Array.emptyByteArray,
        quantity,
        8,
        reissuable = false,
        Some(assetBanned),
        enoughFee,
        ts
      ).signWith(master.privateKey)
      contractGen = paymentContractGen(a.toAddress, am, List(IssuedAsset(asset.id()))) _
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
      asset1 = IssueTransaction(
        TxVersion.V2,
        master.publicKey,
        "Asset#1".utf8Bytes,
        Array.emptyByteArray,
        quantity,
        8,
        reissuable = false,
        Some(assetAllowed),
        enoughFee,
        ts
      ).signWith(master.privateKey)
      asset2 = IssueTransaction(
        TxVersion.V2,
        master.publicKey,
        "Asset#2".utf8Bytes,
        Array.emptyByteArray,
        quantity,
        8,
        reissuable = false,
        Some(assetBanned),
        enoughFee,
        ts
      ).signWith(master.privateKey)
      contractGen = paymentContractGen(a.toAddress, am, List(IssuedAsset(asset1.id()), IssuedAsset(asset2.id()))) _
      r <- preconditionsAndSetContract(contractGen, masterGen = Gen.oneOf(Seq(master)), feeGen = ciFee(2))
    } yield (a, am, r._1, r._2, r._3, asset1, asset2, master)) {
      case (acc, amount, genesis, setScript, ci, asset1, asset2, master) =>
        assertDiffEiTraced(Seq(TestBlock.create(genesis ++ Seq(asset1, asset2, setScript))), TestBlock.create(Seq(ci)), fs) { blockDiffEi =>
          blockDiffEi.resultE should produce("TransactionNotAllowedByScript")
          inside(blockDiffEi.trace) {
            case List(
                InvokeScriptTrace(dAppAddress, functionCall, Right(ScriptResultV3(_, transfers)), _),
                AssetVerifierTrace(allowedAssetId, None),
                AssetVerifierTrace(bannedAssetId, Some(TransactionNotAllowedByScript(_, _)))
                ) =>
              dAppAddress shouldBe ci.dAppAddressOrAlias
              functionCall shouldBe ci.funcCall

              allowedAssetId shouldBe asset1.id.value
              bannedAssetId shouldBe asset2.id.value

              transfers.flatMap(_.assetId.toList) shouldBe List(allowedAssetId, bannedAssetId)
          }
        }
    }
  }

  property("trace not contains attached asset script invocation result when transferring asset script produce error") {
    forAll(for {
      a       <- accountGen
      am      <- smallFeeGen
      invoker <- accountGen
      ts      <- timestampGen

      attachedAsset = IssueTransaction(
        TxVersion.V2,
        invoker.publicKey,
        "Asset#1".utf8Bytes,
        Array.emptyByteArray,
        1000000,
        8,
        reissuable = false,
        Some(assetAllowed),
        enoughFee,
        ts
      ).signWith(invoker.privateKey)

      transferringAsset = IssueTransaction(
        TxVersion.V2,
        invoker.publicKey,
        "Asset#2".utf8Bytes,
        Array.emptyByteArray,
        1000000,
        8,
        reissuable = false,
        Some(throwingAsset),
        enoughFee,
        ts
      ).signWith(invoker.privateKey)

      r <- preconditionsAndSetContract(
        paymentContractGen(a.toAddress, am, List(IssuedAsset(transferringAsset.id()))),
        invokerGen = Gen.oneOf(Seq(invoker)),
        payment = Some(Payment(1, IssuedAsset(attachedAsset.id()))),
        feeGen = ciFee(2)
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
                InvokeScriptTrace(_, _, Right(ScriptResultV3(_, transfers)), _),
                AssetVerifierTrace(transferringAssetId, Some(_))
                ) =>
              transferringAssetId shouldBe transferringAsset.id.value
              transfers.head.assetId.get shouldBe transferringAsset.id.value
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
      asset = IssueTransaction(
        TxVersion.V2,
        master.publicKey,
        "Asset#1" utf8Bytes,
        Array.emptyByteArray,
        quantity,
        8,
        reissuable = false,
        Some(assetAllowed),
        enoughFee,
        ts
      ).signWith(master.privateKey)
      contractGen = paymentContractGen(a.toAddress, -1, List(IssuedAsset(asset.id()))) _
      r <- preconditionsAndSetContract(contractGen, masterGen = Gen.oneOf(Seq(master)), feeGen = ciFee(1))
    } yield (a, am, r._1, r._2, r._3, asset, master, ts)) {
      case (acc, amount, genesis, setScript, ci, asset, master, ts) =>
        val t =
          TransferTransaction
            .selfSigned(2.toByte, master, acc.toAddress, IssuedAsset(asset.id()), asset.quantity / 10, Waves, enoughFee, None, ts)
            .explicitGet()
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(asset, t, setScript))), TestBlock.create(Seq(ci)), fs) { blockDiffEi =>
          blockDiffEi should produce("Negative amount")
        }
    }
  }

  property("payment should be positive") {
    forAll(for {
      invoker     <- accountGen
      master      <- accountGen
      ts          <- timestampGen
      arg         <- genBoundedStringBytes(1, 32)
      funcBinding <- validAliasStringGen
      fee         <- ciFee(1)
      fc = Terms.FUNCTION_CALL(FunctionHeader.User(funcBinding), List(CONST_BYTESTR(ByteStr(arg)).explicitGet()))
      ci = InvokeScriptTransaction.selfSigned(1.toByte, invoker, master.toAddress, Some(fc), Seq(Payment(-1, Waves)), fee, Waves, ts)
    } yield ci) { _ should produce("NonPositiveAmount") }
  }

  property("smart asset payment require extra fee") {
    forAll(for {
      a <- accountGen
      quantity = 1000000
      am     <- Gen.choose[Long](1L, quantity)
      master <- accountGen
      ts     <- timestampGen
      asset = IssueTransaction(
        TxVersion.V2,
        master.publicKey,
        "Asset#1".utf8Bytes,
        Array.emptyByteArray,
        quantity,
        8,
        reissuable = false,
        Some(assetBanned),
        enoughFee,
        ts
      ).signWith(master.privateKey)
      contractGen = paymentContractGen(a.toAddress, am, List(IssuedAsset(asset.id()))) _
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
      contractGen = paymentContractGen(a.toAddress, am) _
      invoker <- accountGen
      ts      <- timestampGen
      asset = IssueTransaction(
        TxVersion.V2,
        invoker.publicKey,
        "Asset#1".utf8Bytes,
        Array.emptyByteArray,
        1000000,
        8,
        reissuable = false,
        Some(assetAllowed),
        enoughFee,
        ts
      ).signWith(invoker.privateKey)
      r <- preconditionsAndSetContract(
        contractGen,
        invokerGen = Gen.oneOf(Seq(invoker)),
        payment = Some(Payment(1, IssuedAsset(asset.id()))),
        feeGen = ciFee(0)
      )
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
      contractGen = paymentContractGen(a.toAddress, am) _
      invoker <- accountGen
      ts      <- timestampGen
      r <- preconditionsAndSetContract(
        contractGen,
        invokerGen = Gen.oneOf(Seq(invoker)),
        payment = Some(Payment(Long.MaxValue, Waves)),
        feeGen = ciFee(1)
      )
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
      contractGen = paymentContractGen(a.toAddress, Long.MaxValue / 2 + 2, List.fill(4)(Waves)) _
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
      contractGen = paymentContractGen(a.toAddress, am) _
      r  <- preconditionsAndSetContract(contractGen, sponsored = true)
      ts <- timestampGen
    } yield (ts, a, am, r._1, r._2, r._3, r._4, r._5, r._6)) {
      case (ts, acc, amount, genesis, setScript, ci, master, sponsoredAsset, setSponsorship) =>
        val t =
          TransferTransaction
            .selfSigned(
              2.toByte,
              master,
              ci.sender.toAddress,
              IssuedAsset(sponsoredAsset.id()),
              sponsoredAsset.quantity / 10,
              Waves,
              enoughFee,
              None,
              ts
            )
            .explicitGet()
        assertDiffAndState(
          Seq(TestBlock.create(genesis ++ Seq[Transaction](sponsoredAsset, t, setSponsorship, setScript))),
          TestBlock.create(Seq(ci), Block.ProtoBlockVersion),
          fs
        ) {
          case (blockDiff, newState) =>
            blockDiff.scriptsRun shouldBe 1
            newState.balance(acc.toAddress, Waves) shouldBe amount
            newState.balance(ci.sender.toAddress, IssuedAsset(sponsoredAsset.id())) shouldBe (sponsoredAsset.quantity / 10 - ci.fee)
            newState.balance(master.toAddress, IssuedAsset(sponsoredAsset.id())) shouldBe (sponsoredAsset.quantity - sponsoredAsset.quantity / 10 + ci.fee)
        }
    }
  }

  property("argument passed to callable function has wrong type") {
    forAll(for {
      r <- simplePreconditionsAndSetContract(invocationParamsCount = 2)
    } yield (r._1, r._2, r._3)) {
      case (genesis, setScript, ci) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          _ should produce("Can't apply (CONST_BOOLEAN) to 'parseInt(str: String)'")
        }
    }
  }

  property("can't write more than 100 entries") {
    forAll(for {
      r <- preconditionsAndSetContract(s => writeSet(s, ContractLimits.MaxWriteSetSize + 1))
    } yield (r._1, r._2, r._3)) {
      case (genesis, setScript, ci) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          _ should produce("can't contain more than")
        }
    }
  }

  property("can write 100 entries") {
    forAll(for {
      r <- preconditionsAndSetContract(s => writeSet(s, ContractLimits.MaxWriteSetSize))
    } yield (r._1, r._2, r._3)) {
      case (genesis, setScript, ci) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          _ shouldBe 'right
        }
    }
  }

  property("can't write entry with key size > 100") {
    forAll(for {
      r <- preconditionsAndSetContract(s => writeSetWithKeyLength(s, ContractLimits.MaxKeySizeInBytes + 1))
    } yield (r._1, r._2, r._3)) {
      case (genesis, setScript, ci) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          _ should produce("Key size must be less than")
        }
    }
  }

  property("can write entry with key <= 100") {
    forAll(for {
      r <- preconditionsAndSetContract(s => writeSetWithKeyLength(s, ContractLimits.MaxKeySizeInBytes))
    } yield (r._1, r._2, r._3)) {
      case (genesis, setScript, ci) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          _ shouldBe 'right
        }
    }
  }

  property("Function call args count should be equal @Callable func one") {
    forAll(for {
      invocationArgsCount <- Gen.oneOf(0, 3)
      r                   <- simplePreconditionsAndSetContract(invocationParamsCount = invocationArgsCount)
    } yield (r._1, r._2, r._3, invocationArgsCount)) {
      case (genesis, setScript, ci, count) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          _ should produce(s"takes 2 args but $count were(was) given")
        }
    }
  }

  property("dApp multisig verify") {
    def multiSigCheckDApp(proofs: Int): DApp = {
      val expr = {
        val script =
          s"""
             |
             | {-# STDLIB_VERSION 3       #-}
             | {-# CONTENT_TYPE   DAPP    #-}
             | {-# SCRIPT_TYPE    ACCOUNT #-}
             |
             | @Verifier(tx)
             | func verify() = {
             |   ${0 until proofs map (i => s"sigVerify(tx.bodyBytes, tx.proofs[$i], tx.senderPublicKey)") mkString "&&"}
             | }
             |
        """.stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr)
    }

    forAll(for {
      proofsCount <- Gen.choose(2, 9)
      r           <- preconditionsAndSetContractWithVerifier(multiSigCheckDApp(proofsCount), writeSetWithKeyLength(_))
    } yield (r._1, r._2, r._3, r._4, proofsCount)) {
      case (genesis, setVerifier, setContract, ci, proofsCount) =>
        val proof         = ci.proofs
        val multiSigProof = ci.proofs.copy(proofs = List.fill(proofsCount)(proof.proofs.head))
        val multiSigCi    = ci.copy(1.toByte, proofs = multiSigProof)

        assertDiffEi(
          Seq(TestBlock.create(genesis ++ Seq(setVerifier, setContract))),
          TestBlock.create(Seq(multiSigCi)),
          fs
        ) {
          _ shouldBe 'right
        }
    }
  }

  property("Default function invocation should produce error if contract default function has arguments") {
    forAll(for {
      a             <- accountGen
      am            <- smallFeeGen
      senderBinging <- validAliasStringGen
      argBinding    <- validAliasStringGen
      contractGen   <- Gen.const((someStr: String) => Gen.const(paymentContract(senderBinging, argBinding, "default", a.toAddress, am, List(Waves))))
      r             <- preconditionsAndSetContract(contractGen, accountGen, accountGen, None, ciFee(0), sponsored = false, isCIDefaultFunc = true)
    } yield (a, am, r._1, r._2, r._3)) {
      case (acc, amount, genesis, setScript, ci) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          _ should produce(s"takes 1 args but 0 were(was) given")
        }
    }
  }

  property("Default function invocation should produce error if contract does't have default function") {
    forAll(for {
      a             <- accountGen
      am            <- smallFeeGen
      senderBinging <- validAliasStringGen
      argBinding    <- validAliasStringGen
      contractGen   <- Gen.const((someStr: String) => Gen.const(paymentContract(senderBinging, argBinding, "undefault", a.toAddress, am, List(Waves))))
      r             <- preconditionsAndSetContract(contractGen, accountGen, accountGen, None, ciFee(0), sponsored = false, isCIDefaultFunc = true)
    } yield (a, am, r._1, r._2, r._3)) {
      case (acc, amount, genesis, setScript, ci) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          _ should produce("Cannot find callable function `default` complexity")
        }
    }
  }

  property("self-payment and self-transfer V3") {
    forAll(for {
      acc <- accountGen
      am  <- smallFeeGen
      contractGen = paymentContractGen(acc.toAddress, am, assets = List(Waves)) _
      r <- preconditionsAndSetContract(
        contractGen,
        invokerGen = Gen.const(acc),
        masterGen = Gen.const(acc),
        payment = Some(Payment(1, Waves)),
        feeGen = ciFee(1)
      )
    } yield (r._1, r._2, r._3)) {
      case (genesis, setScript, ci) =>
        val features = fs.copy(preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0))
        assertDiffEi(Seq(TestBlock.create(Seq(genesis.head, setScript))), TestBlock.create(Seq(ci)), features) {
          _ shouldBe 'right
        }
    }
  }

  property("self-payment V4") {
    forAll(for {
      acc <- accountGen
      am  <- smallFeeGen
      contractGen = paymentContractGen(acc.toAddress, am, assets = Nil, version = V4) _
      r <- preconditionsAndSetContract(
        contractGen,
        invokerGen = Gen.const(acc),
        masterGen = Gen.const(acc),
        payment = Some(Payment(1, Waves)),
        feeGen = ciFee(1),
        version = V4
      )
    } yield (r._1, r._2, r._3)) {
      case (genesis, setScript, ci) =>
        val features = fs.copy(preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0))
        assertDiffEi(Seq(TestBlock.create(Seq(genesis.head, setScript))), TestBlock.create(Seq(ci)), features) {
          _ should produce("DApp self-payment is forbidden since V4")
        }
    }
  }

  property("self-transfer V4") {
    forAll(for {
      acc <- accountGen
      am  <- smallFeeGen
      contractGen = paymentContractGen(acc.toAddress, am, assets = List(Waves), version = V4) _
      r <- preconditionsAndSetContract(
        contractGen,
        invokerGen = Gen.const(acc),
        masterGen = Gen.const(acc),
        payment = None,
        feeGen = ciFee(1),
        version = V4
      )
    } yield (r._1, r._2, r._3)) {
      case (genesis, setScript, ci) =>
        val features = fs.copy(preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0))
        assertDiffEi(Seq(TestBlock.create(Seq(genesis.head, setScript))), TestBlock.create(Seq(ci)), features) {
          _ should produce("DApp self-transfer is forbidden since V4")
        }
    }
  }

  property("transferring asset this value") {
    forAll(for {
      a <- accountGen
      quantity = 1000000
      am     <- Gen.choose[Long](1L, quantity)
      master <- accountGen
      ts     <- timestampGen
      asset = IssueTransaction(
        TxVersion.V2,
        master.publicKey,
        "Asset#1".utf8Bytes,
        Array.emptyByteArray,
        quantity,
        8,
        reissuable = false,
        Some(assetUsingThis),
        enoughFee,
        ts
      ).signWith(master.privateKey)
      contractGen = paymentContractGen(a.toAddress, am, List(IssuedAsset(asset.id()))) _
      r <- preconditionsAndSetContract(contractGen, masterGen = Gen.oneOf(Seq(master)), feeGen = ciFee(1))
    } yield (a, am, r._1, r._2, r._3, asset, master)) {
      case (acc, amount, genesis, setScript, ci, asset, master) =>
        val features = fs.copy(
          preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0)
        )
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(asset, ci), Block.ProtoBlockVersion), features) {
          case (blockDiff, newState) =>
            blockDiff.scriptsRun shouldBe 3
            newState.balance(master.toAddress, IssuedAsset(asset.id())) shouldBe (asset.quantity - amount)
            newState.balance(acc.toAddress, IssuedAsset(asset.id())) shouldBe amount
        }
    }
  }

  private def issueContract(funcName: String): DApp = {
    val expr = {
      val script =
        s"""
           |{-# STDLIB_VERSION 4 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-#SCRIPT_TYPE ACCOUNT#-}
           |
           |@Callable(i)
           |func $funcName() = [Issue("InvokeAsset", "InvokeDesc", 100, 0, true, unit, 0)]
           |""".stripMargin
      Parser.parseContract(script).get.value
    }

    compileContractFromExpr(expr, V4)
  }

  private def throwContract(funcName: String): DApp = {
    val expr = {
      val script =
        s"""
           |{-# STDLIB_VERSION 4 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-#SCRIPT_TYPE ACCOUNT#-}
           |
           |@Callable(i)
           |func $funcName() = throw("bad news")
           |""".stripMargin
      Parser.parseContract(script).get.value
    }
    compileContractFromExpr(expr, V4)
  }

  property("issuing asset with existed id should produce error") {
    val uniqueAssetIdScenario =
      for {
        master  <- accountGen
        invoker <- accountGen
        ts      <- timestampGen

        assetTx     <- issueGen
        fee         <- ciFee(nonNftIssue = 1)
        funcBinding <- funcNameGen
        contract = issueContract(funcBinding)
        script   = ContractScript(V4, contract)

        fc = Terms.FUNCTION_CALL(FunctionHeader.User(funcBinding), List.empty)
        invokeTx = InvokeScriptTransaction
          .selfSigned(TxVersion.V2, invoker, master.toAddress, Some(fc), Seq(), fee, Waves, ts + 3)
          .explicitGet()
      } yield (assetTx, invokeTx, master, script, funcBinding)

    val blockchain: Blockchain = mock[Blockchain]
    forAll(uniqueAssetIdScenario) {
      case (asset, invoke, master, script, funcBinding) =>
        (blockchain.settings _).expects().returning(TestSettings.Default.blockchainSettings)
        (blockchain.assetScript _).expects(*).returning(None)
        (blockchain.accountScript _)
          .expects(master.toAddress)
          .returning(Some(AccountScriptInfo(master.publicKey, script.explicitGet(), 10L, Map(1 -> Map(funcBinding -> 10L)))))
          .anyNumberOfTimes()
        (blockchain.accountScript _).expects(invoke.sender.toAddress).returning(None).anyNumberOfTimes()
        (blockchain.hasAccountScript _).expects(invoke.sender.toAddress).returning(false).anyNumberOfTimes()
        (blockchain.activatedFeatures _)
          .expects()
          .returning(Map(BlockchainFeatures.Ride4DApps.id -> 0))
          .anyNumberOfTimes()
        (blockchain.height _).expects().returning(1).anyNumberOfTimes()
        (blockchain.assetDescription _)
          .expects(*)
          .returning(Some(AssetDescription(asset.id(), master.publicKey, ByteString.EMPTY, ByteString.EMPTY, 1, false, BigInt(1), Height(1), None, 0L, false)))
        InvokeScriptTransactionDiff
          .apply(blockchain, invoke.timestamp)(invoke)
          .resultE should produce("is already issued")
    }
  }

  def reissueContract(funcName: String, asset: ByteStr): DApp = {
    val expr = {
      val script =
        s"""
           |{-# STDLIB_VERSION 4 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-#SCRIPT_TYPE ACCOUNT#-}
           |
           |@Callable(i)
           |func $funcName() = [Reissue(base58'$asset', false, 1), Reissue(base58'$asset', true, 4)]
           |""".stripMargin
      Parser.parseContract(script).get.value
    }

    compileContractFromExpr(expr, V4)
  }
  val reissueAssetIdScenario =
    for {
      master  <- accountGen
      invoker <- accountGen
      ts      <- timestampGen
      genesis1Tx = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      genesis2Tx = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      assetTx     <- issueV2TransactionGen(master, None, reissuableParam = Some(true))
      fee         <- ciFee()
      funcBinding <- funcNameGen
      contract    = reissueContract(funcBinding, assetTx.id.value)
      script      = ContractScript(V4, contract)
      setScriptTx = SetScriptTransaction.selfSigned(1.toByte, master, script.toOption, fee, ts + 2).explicitGet()
      fc          = Terms.FUNCTION_CALL(FunctionHeader.User(funcBinding), List.empty)
      invokeTx = InvokeScriptTransaction
        .selfSigned(TxVersion.V2, invoker, master.toAddress, Some(fc), Seq(), fee, Waves, ts + 3)
        .explicitGet()
    } yield (invokeTx, Seq(genesis1Tx, genesis2Tx, assetTx, setScriptTx))

  property("Reissuing unreissued asset should produce error") {
    forAll(reissueAssetIdScenario) {
      case (invoke, genesisTxs) =>
        tempDb { db =>
          val features = fs.copy(
            preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0)
          )

          assertDiffEi(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), features) { ei =>
            ei should produce("Asset is not reissuable")
          }
        }
    }
  }

  private def transferIssueContract(funcName: String): DApp = {
    val expr = {
      val script =
        s"""
           |{-# STDLIB_VERSION 4 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-#SCRIPT_TYPE ACCOUNT#-}
           |
           |@Callable(i)
           |func $funcName() = {
           | let v = Issue("InvokeAsset", "InvokeDesc", 100, 0, true, unit, 0)
           | [v, ScriptTransfer(i.caller, 1, v.calculateAssetId())]
           |}
           |""".stripMargin
      Parser.parseContract(script).get.value
    }

    compileContractFromExpr(expr, V4)
  }

  private val transferAssetIdScenario =
    for {
      master  <- accountGen
      invoker <- accountGen
      ts      <- timestampGen
      genesis1Tx = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      genesis2Tx = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      fee         <- ciFee(nonNftIssue = 1)
      funcBinding <- funcNameGen
      contract    = transferIssueContract(funcBinding)
      script      = ContractScript(V4, contract)
      setScriptTx = SetScriptTransaction.selfSigned(1.toByte, master, script.toOption, fee, ts + 2).explicitGet()
      fc          = Terms.FUNCTION_CALL(FunctionHeader.User(funcBinding), List.empty)
      invokeTx = InvokeScriptTransaction
        .selfSigned(TxVersion.V2, invoker, master.toAddress, Some(fc), Seq(), fee, Waves, ts + 3)
        .explicitGet()
    } yield (invokeTx, Seq(genesis1Tx, genesis2Tx, setScriptTx))

  property("issued asset can be transfered") {
    forAll(transferAssetIdScenario) {
      case (invoke, genesisTxs) =>
        tempDb { db =>
          val features = fs.copy(
            preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0)
          )

          assertDiffEi(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), features) { ei =>
            ei shouldBe 'Right
          }
        }
    }
  }

  private def transferNonIssueContract(funcName: String): DApp = {
    val expr = {
      val script =
        s"""
           |{-# STDLIB_VERSION 4 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-#SCRIPT_TYPE ACCOUNT#-}
           |
           |@Callable(i)
           |func $funcName() = {
           | let v = Issue("InvokeAsset", "InvokeDesc", 100, 0, true, unit, 0)
           | [ScriptTransfer(i.caller, 1, v.calculateAssetId())]
           |}
           |""".stripMargin
      Parser.parseContract(script).get.value
    }

    compileContractFromExpr(expr, V4)
  }

  private val transferNonAssetIdScenario =
    for {
      master  <- accountGen
      invoker <- accountGen
      ts      <- timestampGen
      genesis1Tx = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      genesis2Tx = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      fee         <- ciFee()
      funcBinding <- funcNameGen
      contract    = transferNonIssueContract(funcBinding)
      script      = ContractScript(V4, contract)
      setScriptTx = SetScriptTransaction.selfSigned(1.toByte, master, script.toOption, fee, ts + 2).explicitGet()
      fc          = Terms.FUNCTION_CALL(FunctionHeader.User(funcBinding), List.empty)
      invokeTx = InvokeScriptTransaction
        .selfSigned(TxVersion.V2, invoker, master.toAddress, Some(fc), Seq(), fee, Waves, ts + 3)
        .explicitGet()
    } yield (invokeTx, Seq(genesis1Tx, genesis2Tx, setScriptTx))

  property("nonissued asset cann't be transfered") {
    forAll(transferNonAssetIdScenario) {
      case (invoke, genesisTxs) =>
        tempDb { db =>
          val features = fs.copy(
            preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0)
          )

          assertDiffEi(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), features) { ei =>
            ei should produce("negative asset balance")
          }
        }
    }
  }

  private def doubleIssueContract(funcName: String): DApp = {
    val expr = {
      val script =
        s"""
           |{-# STDLIB_VERSION 4 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-#SCRIPT_TYPE ACCOUNT#-}
           |
           |@Callable(i)
           |func $funcName() = {
           | let v = Issue("InvokeAsset", "InvokeDesc", 100, 0, true, unit, 0)
           | [v, v]
           |}
           |""".stripMargin
      Parser.parseContract(script).get.value
    }

    compileContractFromExpr(expr, V4)
  }

  private val doubleAssetIdScenario =
    for {
      master  <- accountGen
      invoker <- accountGen
      ts      <- timestampGen
      genesis1Tx = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      genesis2Tx = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      fee         <- ciFee(nonNftIssue = 2)
      funcBinding <- funcNameGen
      contract    = doubleIssueContract(funcBinding)
      script      = ContractScript(V4, contract)
      setScriptTx = SetScriptTransaction.selfSigned(1.toByte, master, script.toOption, fee, ts + 2).explicitGet()
      fc          = Terms.FUNCTION_CALL(FunctionHeader.User(funcBinding), List.empty)
      invokeTx = InvokeScriptTransaction
        .selfSigned(TxVersion.V2, invoker, master.toAddress, Some(fc), Seq(), fee, Waves, ts + 3)
        .explicitGet()
    } yield (invokeTx, Seq(genesis1Tx, genesis2Tx, setScriptTx))

  property("duplicate issuing asset should produce diff error") {
    forAll(doubleAssetIdScenario) {
      case (invoke, genesisTxs) =>
        tempDb { db =>
          val features = fs.copy(
            preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0)
          )
          assertDiffEi(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), features) { ei =>
            inside(ei) {
              case Right(diff) => diff.scriptResults(invoke.id()).errorMessage.get.text should include("is already issued")
            }
          }
        }
    }
  }

  property("correctly counts sponsored fee") {
    forAll(for {
      (genesis, setScript, invoke, _, issue, sponsorFee) <- preconditionsAndSetContract(s => writeSet(s, 1), sponsored = true, selfSend = true)
    } yield (genesis, setScript, issue, sponsorFee, invoke)) {
      case (genesis, setScript, issue, sponsorFee, invoke) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(issue, sponsorFee, setScript))), TestBlock.create(Seq(invoke)), fsWithV5) { diff =>
          invoke.feeAssetId shouldBe sponsorFee.asset
          invoke.dAppAddressOrAlias shouldBe invoke.sender.toAddress

          val dv           = diff.explicitGet()
          val senderChange = dv.portfolios(invoke.sender.toAddress).balanceOf(sponsorFee.asset)
          senderChange shouldBe 0L
        }
    }
  }

  property(s"accepts failed transactions after ${BlockchainFeatures.BlockV5} activation") {
    def failInvariant(funcBinding: String, sponsorTx: SponsorFeeTransaction, issueTx: IssueTransaction): Gen[(TxAmount, Asset, DApp, List[EXPR])] = {
      val feeInWaves = FeeConstants(InvokeScriptTransaction.typeId) * FeeValidation.FeeUnit
      val feeInAsset = Sponsorship.fromWaves(FeeConstants(InvokeScriptTransaction.typeId) * FeeValidation.FeeUnit, sponsorTx.minSponsoredAssetFee.get)
      Gen.oneOf(
        Gen.const((feeInWaves, Waves, issueContract(funcBinding), List.empty[EXPR])),           // insufficient fee
        Gen.const((feeInAsset, sponsorTx.asset, issueContract(funcBinding), List.empty[EXPR])), // insufficient fee
        Gen.const((feeInWaves, Waves, throwContract(funcBinding), List.empty[EXPR])),           // DApp script execution
        Gen.const((feeInAsset, sponsorTx.asset, throwContract(funcBinding), List.empty[EXPR])), // DApp script execution
        for {                                                                                   // smart asset script execution
          fee             <- ciFee(1)
          acc             <- accountGen
          amt             <- Gen.choose(1, issueTx.quantity)
          arg             <- genBoundedStringBytes(1, 32)
          paymentContract <- paymentContractGen(acc.toAddress, amt, List(IssuedAsset(issueTx.assetId)), V4)(funcBinding)
        } yield (fee, Waves, paymentContract, List(CONST_BYTESTR(ByteStr(arg)).explicitGet()))
      )
    }

    val failedTxScenario =
      for {
        master  <- accountGen
        other   <- accountGen
        invoker <- accountGen
        ts      <- timestampGen
        g1Tx = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
        g2Tx = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
        g3Tx = GenesisTransaction.create(other.toAddress, ENOUGH_AMT, ts).explicitGet()
        (i1Tx, sTx, _, _) <- sponsorFeeCancelSponsorFeeGen(other)
        i2Tx = IssueTransaction
          .selfSigned(TxVersion.V2, invoker, "Asset", "", 1000000, 8, false, Some(throwingAsset), enoughFee, ts)
          .explicitGet()
        tTx = TransferTransaction
          .selfSigned(TxVersion.V3, other, invoker.toAddress, sTx.asset, i1Tx.quantity, Waves, enoughFee, None, ts)
          .explicitGet()
        funcBinding                     <- funcNameGen
        (fee, feeAsset, contract, args) <- failInvariant(funcBinding, sTx, i2Tx)
        script = ContractScript(V4, contract)
        ssTx   = SetScriptTransaction.selfSigned(1.toByte, master, script.toOption, fee, ts + 2).explicitGet()
        fc     = Terms.FUNCTION_CALL(FunctionHeader.User(funcBinding), args)
        invokeTx = InvokeScriptTransaction
          .selfSigned(TxVersion.V2, invoker, master.toAddress, Some(fc), Seq(), fee, feeAsset, ts + 3)
          .explicitGet()
      } yield (invokeTx, (ENOUGH_AMT - enoughFee, i1Tx.quantity), Seq(g1Tx, g2Tx, g3Tx, i1Tx, i2Tx, sTx, tTx, ssTx))

    forAll(failedTxScenario) {
      case (invoke, (wavesBalance, sponsoredBalance), genesisTxs) =>
        assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) {
          case (diff, state) =>
            diff.scriptsRun shouldBe 0
            diff.portfolios(invoke.sender.toAddress).balanceOf(invoke.feeAssetId)
            state.balance(invoke.sender.toAddress, invoke.feeAssetId) shouldBe invoke.feeAssetId.fold(wavesBalance)(_ => sponsoredBalance) - invoke.fee
            state.transactionInfo(invoke.id()).map(r => r._2 -> r._3) shouldBe Some((invoke, false))
        }
    }
  }

  property(
    s"rejects withdrawal of fee from the funds received as a result of the script call execution after ${BlockchainFeatures.BlockV5} activation"
  ) {
    val scenario =
      for {
        master  <- accountGen
        other   <- accountGen
        invoker <- accountGen
        ts      <- timestampGen
        g1Tx = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
        g2Tx = GenesisTransaction.create(other.toAddress, ENOUGH_AMT, ts).explicitGet()
        (iTx, sTx, _, _) <- sponsorFeeCancelSponsorFeeGen(other)
        sponsoredAsset = IssuedAsset(iTx.assetId)
        tTx            = TransferTransaction.selfSigned(TxVersion.V3, other, master.toAddress, sponsoredAsset, iTx.quantity / 2, Waves, enoughFee, None, ts).explicitGet()
        wavesFee <- ciFee(1)
        sponsoredFee = Sponsorship.fromWaves(wavesFee, sTx.minSponsoredAssetFee.get)
        (feeAsset, fee) <- Gen.oneOf((Waves, wavesFee), (sponsoredAsset, sponsoredFee))
        funcBinding     <- funcNameGen
        arg             <- genBoundedStringBytes(1, 32)
        contract        <- paymentContractGen(invoker.toAddress, fee, List(feeAsset), V4)(funcBinding)
        script = ContractScript(V4, contract)
        ssTx   = SetScriptTransaction.selfSigned(1.toByte, master, script.toOption, fee, ts + 2).explicitGet()
        fc     = Terms.FUNCTION_CALL(FunctionHeader.User(funcBinding), List(CONST_BYTESTR(ByteStr(arg)).explicitGet()))
        invokeTx = InvokeScriptTransaction
          .selfSigned(TxVersion.V2, invoker, master.toAddress, Some(fc), Seq(), fee, feeAsset, ts + 3)
          .explicitGet()
      } yield (invokeTx, Seq(g1Tx, g2Tx, iTx, sTx, tTx, ssTx))

    forAll(scenario) {
      case (invoke, genesisTxs) =>
        assertDiffEi(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) { ei =>
          ei should produce("AccountBalanceError")
        }
    }
  }
}
