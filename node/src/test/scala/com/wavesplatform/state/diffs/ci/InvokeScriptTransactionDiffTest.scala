package com.wavesplatform.state.diffs.ci

import cats.kernel.Monoid
import com.google.protobuf.ByteString
import com.wavesplatform.TestValues
import com.wavesplatform.account._
import com.wavesplatform.block.{Block, BlockHeader, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithState}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.values.{DApp => DAppType, _}
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
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
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionDiff
import com.wavesplatform.state.diffs.{ENOUGH_AMT, FeeValidation, produce}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.script.trace.{AssetVerifierTrace, InvokeScriptTrace}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{Asset, _}
import org.scalamock.scalatest.MockFactory
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{EitherValues, Inside}

import scala.collection.immutable
import scala.util.{Random, Try}

class InvokeScriptTransactionDiffTest extends PropSpec with WithState with DBCacheSettings with EitherValues with Inside with MockFactory {

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
      BlockchainFeatures.SmartAccounts.id    -> 0,
      BlockchainFeatures.SmartAssets.id      -> 0,
      BlockchainFeatures.Ride4DApps.id       -> 0,
      BlockchainFeatures.FeeSponsorship.id   -> 0,
      BlockchainFeatures.DataTransaction.id  -> 0,
      BlockchainFeatures.BlockV5.id          -> 0,
      BlockchainFeatures.SynchronousCalls.id -> 0
    ),
    syncDAppCheckTransfersHeight = 999
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

  val enoughFee: TxAmount = FeeValidation.ScriptExtraFee + FeeValidation.FeeConstants(IssueTransaction.typeId) * FeeValidation.FeeUnit

  private def dataContract(
      senderBinding: String,
      argName: String = "arg",
      funcName: String = "func",
      bigData: Boolean = false,
      emptyData: Boolean = false
  ): DApp = {
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

  def transferContract(
      senderBinding: String,
      recipientAddress: Address = TxHelpers.signer(5).toAddress,
      recipientAmount: Long = 123,
      argName: String = "a",
      funcName: String = "func",
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

  def defaultTransferContract(
      senderBinding: String,
      recipientAddress: AddressOrAlias,
      recipientAmount: Long,
      assets: List[Asset] = List(Waves)
  ): DApp = {
    val transfers: immutable.Seq[FUNCTION_CALL] = assets.map(
      a =>
        FUNCTION_CALL(
          User(FieldNames.ScriptTransfer),
          List(
            (recipientAddress: @unchecked) match {
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

  def writeSetWithKeyLength(funcName: String, length: Int = 1, version: StdLibVersion = V3): DApp = {
    val keyName = Array.fill(length)("a").mkString

    val expr = {
      val body =
        if (version == V3)
          s""" WriteSet([DataEntry("$keyName", 0)]) """
        else
          s""" [IntegerEntry("$keyName", 0)] """

      val script =
        s"""
           |
           | {-#STDLIB_VERSION $version #-}
           | {-#CONTENT_TYPE DAPP#-}
           | {-#SCRIPT_TYPE ACCOUNT#-}
           |
           | @Callable(i)
           | func $funcName(b: ByteVector) =
           |    $body
           |
        """.stripMargin
      Parser.parseContract(script).get.value
    }

    compileContractFromExpr(expr, version)
  }

  def compileContractFromExpr(expr: Expressions.DAPP, stdLibVersion: StdLibVersion = V3): DApp = {
    val ctx = {
      utils.functionCosts(stdLibVersion)
      Monoid
        .combineAll(
          Seq(
            PureContext.build(stdLibVersion, fixUnicodeFunctions = true, useNewPowPrecision = true).withEnvironment[Environment],
            CryptoContext.build(Global, stdLibVersion).withEnvironment[Environment],
            WavesContext.build(
              Global,
              DirectiveSet(stdLibVersion, Account, DAppType).explicitGet()
            )
          )
        )
    }

    compiler.ContractCompiler(ctx.compilerContext, expr, stdLibVersion).explicitGet()
  }

  def simplePreconditionsAndSetContract(
      invoker: KeyPair = TxHelpers.signer(0),
      dApp: KeyPair = TxHelpers.signer(1),
      payment: Option[Payment] = None,
      sponsored: Boolean = false,
      invocationParamsCount: Int = 1
  ): (List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, KeyPair, IssueTransaction, SponsorFeeTransaction) = {
    val genesis     = TxHelpers.genesis(dApp.toAddress)
    val genesis2    = TxHelpers.genesis(invoker.toAddress)
    val contract    = simpleContract("funcForTesting").explicitGet()
    val script      = ContractScript(V3, contract)
    val setContract = TxHelpers.setScript(dApp, script.explicitGet())
    val issue       = TxHelpers.issue(invoker)
    val asset       = IssuedAsset(issue.id())
    val sponsor     = TxHelpers.sponsor(asset)
    val ci = TxHelpers.invoke(
      dApp.toAddress,
      Some("funcForTesting"),
      List.fill(invocationParamsCount)(FALSE),
      payment.toSeq,
      feeAssetId = if (sponsored) asset else Waves
    )
    (List(genesis, genesis2), setContract, ci, dApp, issue, sponsor)
  }

  def preconditionsAndSetContract(
      senderBindingToContract: String => DApp,
      invoker: KeyPair = TxHelpers.signer(0),
      dApp: KeyPair = TxHelpers.signer(1),
      payment: Option[Payment] = None,
      sponsored: Boolean = false,
      isCIDefaultFunc: Boolean = false,
      version: StdLibVersion = V3,
      txVersion: TxVersion = TxVersion.V1,
      selfSend: Boolean = false
  ): (List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, KeyPair, IssueTransaction, SponsorFeeTransaction) = {
    val genesis      = TxHelpers.genesis(dApp.toAddress)
    val genesis2     = TxHelpers.genesis(invoker.toAddress)
    val contract     = senderBindingToContract("func")
    val script       = ContractScript(version, contract).explicitGet()
    val setContract  = TxHelpers.setScript(dApp, script)
    val issue        = TxHelpers.issue(dApp)
    val asset        = IssuedAsset(issue.id())
    val sponsor      = TxHelpers.sponsor(asset, sender = dApp)
    val sponsoredFee = Sponsorship.fromWaves(TestValues.invokeFee, sponsor.minSponsoredAssetFee.get)
    val ci = TxHelpers.invoke(
      dApp.toAddress,
      if (isCIDefaultFunc) None else Some("func"),
      List(CONST_BYTESTR(ByteStr.fromBytes(1, 2, 3)).explicitGet()),
      payment.toSeq,
      if (selfSend) dApp else invoker,
      if (sponsored) sponsoredFee else TestValues.invokeFee,
      if (sponsored) asset else Waves,
      txVersion
    )
    (if (selfSend) List(genesis) else List(genesis, genesis2), setContract, ci, dApp, issue, sponsor)
  }

  def preconditionsAndSetContractWithVerifier(
      verifier: DApp,
      senderBindingToContract: String => DApp,
      invoker: KeyPair = TxHelpers.signer(0),
      dApp: KeyPair = TxHelpers.signer(1),
      payment: Option[Payment] = None,
      sponsored: Boolean = false,
      isCIDefaultFunc: Boolean = false
  ): (
      List[GenesisTransaction],
      SetScriptTransaction,
      SetScriptTransaction,
      InvokeScriptTransaction,
      KeyPair,
      IssueTransaction,
      SponsorFeeTransaction
  ) = {
    val genesis     = TxHelpers.genesis(dApp.toAddress, ENOUGH_AMT)
    val genesis2    = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)
    val contract    = senderBindingToContract("func")
    val setVerifier = TxHelpers.setScript(invoker, ContractScript(V3, verifier).explicitGet())
    val setContract = TxHelpers.setScript(dApp, ContractScript(V3, contract).explicitGet())
    val issue       = TxHelpers.issue(invoker)
    val asset       = IssuedAsset(issue.id())
    val sponsor     = TxHelpers.sponsor(asset)
    val ci = TxHelpers
      .invoke(
        dApp.toAddress,
        if (isCIDefaultFunc) None else Some("func"),
        List(CONST_BYTESTR(ByteStr.fromBytes(1, 2, 3)).explicitGet()),
        payment.toSeq,
        feeAssetId = if (sponsored) asset else Waves
      )
    (List(genesis, genesis2), setVerifier, setContract, ci, dApp, issue, sponsor)
  }

  def preconditionsAndSetContractWithAlias(
      senderBindingToContract: String => DApp,
      invoker: KeyPair = TxHelpers.signer(0),
      dApp: KeyPair = TxHelpers.signer(1),
      payment: Option[Payment] = None,
      sponsored: Boolean = false,
      isCIDefaultFunc: Boolean = false
  ): (List[GenesisTransaction], KeyPair, SetScriptTransaction, InvokeScriptTransaction, InvokeScriptTransaction, CreateAliasTransaction) = {
    val genesis     = TxHelpers.genesis(dApp.toAddress)
    val genesis2    = TxHelpers.genesis(invoker.toAddress)
    val contract    = senderBindingToContract("func")
    val dAppAlias   = Alias.create("alias").explicitGet()
    val fakeAlias   = Alias.create("fakealias").explicitGet()
    val aliasTx     = TxHelpers.createAlias("alias")
    val script      = ContractScript(V3, contract).explicitGet()
    val setContract = TxHelpers.setScript(dApp, script)
    val issue       = TxHelpers.issue(invoker)
    val asset       = IssuedAsset(issue.id())
    val Seq(ciWithAlias, ciWithFakeAlias) = Seq(dAppAlias, fakeAlias).map(
      TxHelpers
        .invoke(
          _,
          if (isCIDefaultFunc) None else Some("func"),
          List(CONST_BYTESTR(ByteStr.fromBytes(1, 2, 3)).explicitGet()),
          payment.toSeq,
          feeAssetId = if (sponsored) asset else Waves
        )
    )
    (List(genesis, genesis2), dApp, setContract, ciWithAlias, ciWithFakeAlias, aliasTx)
  }

  property("invoking contract results contract's state") {
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(s => dataContract(s))
    assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci), Block.ProtoBlockVersion), fs) {
      case (blockDiff, newState) =>
        blockDiff.scriptsRun shouldBe 1
        newState.accountData(genesis(0).recipient, "sender") shouldBe Some(BinaryDataEntry("sender", ByteStr(ci.sender.toAddress.bytes)))
        newState.accountData(genesis(0).recipient, "argument") shouldBe Some(
          BinaryDataEntry("argument", ci.funcCallOpt.get.args.head.asInstanceOf[CONST_BYTESTR].bs)
        )
        blockDiff.transactions(ci.id()).affected.contains(setScript.sender.toAddress) shouldBe true
    }
  }

  property("can't use empty keys in v2") {
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(dataContract(_, emptyData = true), txVersion = TxVersion.V1)
    assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fsWithV5) {
      _.explicitGet()
    }

    val (genesis2, setScript2, ci2, _, _, _) = preconditionsAndSetContract(dataContract(_, emptyData = true), txVersion = TxVersion.V2)
    assertDiffEi(Seq(TestBlock.create(genesis2 ++ Seq(setScript2))), TestBlock.create(Seq(ci2)), fsWithV5) {
      _ should produce("Empty keys aren't allowed")
    }
  }

  property("invoking payment contract results in accounts state") {
    val acc                               = TxHelpers.signer(5)
    val amount                            = 123
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(transferContract(_, acc.toAddress, amount))

    assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci), Block.ProtoBlockVersion), fs) {
      case (blockDiff, newState) =>
        blockDiff.scriptsRun shouldBe 1
        newState.balance(acc.toAddress, Waves) shouldBe amount
        blockDiff.transactions should contain key ci.id()
    }
  }

  property("invoking default func payment contract results in accounts state") {
    val acc                               = TxHelpers.signer(5)
    val amount                            = 123
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(defaultTransferContract(_, acc.toAddress, amount), isCIDefaultFunc = true)

    assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci), Block.ProtoBlockVersion), fs) {
      case (blockDiff, newState) =>
        blockDiff.scriptsRun shouldBe 1
        newState.balance(acc.toAddress, Waves) shouldBe amount
        blockDiff.transactions should contain key ci.id()
    }
  }

  property("invoking default func payment to alias contract results in accounts state") {
    val acc                               = TxHelpers.signer(5)
    val alias                             = Alias.create("alias").explicitGet()
    val createAlias                       = TxHelpers.createAlias("alias", acc)
    val amount                            = 123
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(defaultTransferContract(_, alias, amount), isCIDefaultFunc = true)
    val features                          = fs.copy(preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0))

    assertDiffAndState(
      Seq(TestBlock.create(genesis ++ Seq(TxHelpers.genesis(acc.toAddress), setScript, createAlias))),
      TestBlock.create(Seq(ci), Block.ProtoBlockVersion),
      features
    ) {
      case (blockDiff, _) =>
        blockDiff.scriptsRun shouldBe 1
        blockDiff.portfolios(acc.toAddress) shouldBe Portfolio.waves(amount)
        blockDiff.transactions should contain key ci.id()
    }
    assertDiffEi(
      Seq(TestBlock.create(genesis ++ Seq(TxHelpers.genesis(acc.toAddress), setScript, createAlias))),
      TestBlock.create(Seq(ci), Block.ProtoBlockVersion),
      fs
    ) {
      _ should produce(
        """'Alias(
          |	alias = "alias"
          |)' instead of recipient""".stripMargin
      )
    }
  }

  property("suitable verifier error message on incorrect proofs number") {
    Seq(0, 2).foreach { proofCount =>
      val acc                               = TxHelpers.signer(5)
      val amount                            = 123
      val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(transferContract(_, acc.toAddress, amount))
      val proofs                            = Proofs(List.fill(proofCount)(ByteStr.fromBytes(1, 1)))

      assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci.copy(proofs = proofs)), Block.ProtoBlockVersion), fs) {
        _ should produce("Transactions from non-scripted accounts must have exactly 1 proof")
      }
    }
  }

  property("suitable verifier error message on incorrect proof") {
    val acc                               = TxHelpers.signer(5)
    val amount                            = 123
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(transferContract(_, acc.toAddress, amount))
    val proofs                            = Proofs(List(ByteStr.fromBytes(1, 1)))

    assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci.copy(proofs = proofs)), Block.ProtoBlockVersion), fs) {
      _ should produce("Proof doesn't validate as signature")
    }
  }

  property("invoke script by alias") {
    val acc                                              = TxHelpers.signer(5)
    val amount                                           = 123
    val (genesis, _, setScript, ci, fakeCi, createAlias) = preconditionsAndSetContractWithAlias(transferContract(_, acc.toAddress, amount))
1
    assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript, createAlias))), TestBlock.create(Seq(ci), Block.ProtoBlockVersion), fs) {
      case (blockDiff, newState) =>
        blockDiff.scriptsRun shouldBe 1
        newState.balance(acc.toAddress, Waves) shouldBe amount
        blockDiff.transactions should contain key ci.id()
    }
    assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(fakeCi), Block.ProtoBlockVersion), fs) {
      _ should produce("does not exist")
    }
  }

  property("can't make more than 10 payments") {
    val acc                               = TxHelpers.signer(5)
    val amount                            = 123
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(transferContract(_, acc.toAddress, amount, assets = List.fill(11)(Waves)))

    assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
      _ should produce("Actions count limit is exceeded")
    }
  }

  property("invoking contract receive payment") {
    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V3)
      .foreach { version =>
        val recipient = TxHelpers.signer(5)
        val amount    = 123
        val invoker   = TxHelpers.signer(0)
        val asset     = TxHelpers.issue()
        val (genesis, setScript, ci, dAppAddress, _, _) = preconditionsAndSetContract(
          transferContract(_, recipient.toAddress, amount, version = version),
          payment = Some(Payment(1, IssuedAsset(asset.id()))),
          version = version
        )
        assertDiffAndState(
          Seq(TestBlock.create(genesis ++ Seq(asset, setScript))),
          TestBlock.create(Seq(ci), Block.ProtoBlockVersion),
          if (version < V4) fs else fsWithV5
        ) {
          case (blockDiff, newState) =>
            blockDiff.scriptsRun shouldBe 2
            newState.balance(recipient.toAddress, Waves) shouldBe amount
            newState.balance(invoker.toAddress, IssuedAsset(asset.id())) shouldBe (asset.quantity - 1)
            newState.balance(dAppAddress.toAddress, IssuedAsset(asset.id())) shouldBe 1
        }
      }
  }

  property("successfully invoked contract trace should contain both attached and transferring asset script info") {
    val dApp              = TxHelpers.signer(1)
    val transferringAsset = TxHelpers.issue()
    val attachedAsset     = TxHelpers.issue(name = "test2")

    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(
      transferContract(_, dApp.toAddress, assets = List(IssuedAsset(transferringAsset.id()))),
      payment = Some(Payment(1, IssuedAsset(attachedAsset.id())))
    )
    assertDiffEiTraced(
      Seq(TestBlock.create(genesis ++ Seq(transferringAsset, attachedAsset, setScript))),
      TestBlock.create(Seq(ci)),
      fs
    ) { blockDiffEi =>
      blockDiffEi.resultE.explicitGet().scriptsRun shouldBe 3
      inside(blockDiffEi.trace) {
        case List(
            InvokeScriptTrace(_, _, _, Right(ScriptResultV3(_, transfers, _)), _, _),
            AssetVerifierTrace(transferringAssetId, None, _),
            AssetVerifierTrace(attachedAssetId, None, _)
            ) =>
          attachedAssetId shouldBe attachedAsset.id()
          transferringAssetId shouldBe transferringAsset.id()
          transfers.head.assetId.get shouldBe transferringAsset.id()
      }
    }
  }

  property("asset script ban invoking contract with payment and produce trace") {
    val asset = TxHelpers.issue()
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(
      transferContract(_),
      payment = Some(Payment(1, IssuedAsset(asset.id())))
    )
    assertDiffEiTraced(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci)), fs) { blockDiffEi =>
      blockDiffEi.resultE should produce("TransactionNotAllowedByScript")
      inside(blockDiffEi.trace) {
        case List(_, AssetVerifierTrace(assetId, Some(tne: TransactionNotAllowedByScript), _)) =>
          assetId shouldBe asset.id()
          tne.isAssetScript shouldBe true
      }
    }
  }

  property("invoking contract make payment by asset") {
    val recipient = TxHelpers.signer(5).toAddress
    val quantity  = 1000000
    val amount    = quantity / 2
    val dApp      = TxHelpers.signer(0)
    val issue     = TxHelpers.issue()
    val asset     = IssuedAsset(issue.id())

    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(transferContract(_, recipient, assets = List(asset)))

    assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(issue, ci), Block.ProtoBlockVersion), fs) {
      case (blockDiff, newState) =>
        blockDiff.scriptsRun shouldBe 3
        newState.balance(dApp.toAddress, asset) shouldBe (issue.quantity - amount)
        newState.balance(recipient, asset) shouldBe amount
    }
  }

  property("invoking contract disable by payment smart asset") {
    val issue                             = TxHelpers.issue(script = Some(assetBanned))
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(transferContract(_, assets = List(IssuedAsset(issue.id()))))

    assertDiffEiTraced(Seq(TestBlock.create(genesis ++ Seq(issue, setScript))), TestBlock.create(Seq(ci)), fs) { blockDiffEi =>
      blockDiffEi.resultE should produce("Transaction is not allowed by script")
    }
  }

  property("invoking contract disable by one of payment smart asset with trace") {
    val dApp   = TxHelpers.signer(1)
    val issue1 = TxHelpers.issue(issuer = dApp)
    val issue2 = TxHelpers.issue(issuer = dApp, name = "test2", script = Some(assetBanned))

    val contract                          = transferContract(_: String, assets = List(IssuedAsset(issue1.id()), IssuedAsset(issue2.id())))
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(contract)

    assertDiffEiTraced(Seq(TestBlock.create(genesis ++ Seq(issue1, issue2, setScript))), TestBlock.create(Seq(ci)), fs) { blockDiffEi =>
      blockDiffEi.resultE should produce("Transaction is not allowed by script")
      inside(blockDiffEi.trace) {
        case List(
            InvokeScriptTrace(_, dAppAddress, functionCall, Right(ScriptResultV3(_, transfers, _)), _, _),
            AssetVerifierTrace(allowedAssetId, None, _),
            AssetVerifierTrace(bannedAssetId, Some(_: FailedTransactionError), _)
            ) =>
          dAppAddress shouldBe ci.dAppAddressOrAlias
          functionCall shouldBe ci.funcCall

          allowedAssetId shouldBe issue1.id()
          bannedAssetId shouldBe issue2.id()

          transfers.flatMap(_.assetId.toList) shouldBe List(allowedAssetId, bannedAssetId)
      }
    }
  }

  property("trace not contains attached asset script invocation result when transferring asset script produce error") {
    val attachedAsset     = TxHelpers.issue()
    val transferringAsset = TxHelpers.issue(name = "test2", script = Some(throwingAsset))

    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(
      transferContract(_, assets = List(IssuedAsset(transferringAsset.id()))),
      payment = Some(Payment(1, IssuedAsset(attachedAsset.id())))
    )

    assertDiffEiTraced(
      Seq(TestBlock.create(genesis ++ Seq(transferringAsset, attachedAsset, setScript))),
      TestBlock.create(Seq(ci)),
      fs
    ) { blockDiffEi =>
      blockDiffEi.resultE should produce("TransactionValidationError")
      inside(blockDiffEi.trace) {
        case List(
            InvokeScriptTrace(_, _, _, Right(ScriptResultV3(_, transfers, _)), _, _),
            AssetVerifierTrace(transferringAssetId, Some(_), _)
            ) =>
          transferringAssetId shouldBe transferringAsset.id()
          transfers.head.assetId.get shouldBe transferringAsset.id()
      }
    }
  }

  property("Contract payment should be positive") {
    val asset                             = TxHelpers.issue()
    val contract                          = transferContract(_: String, recipientAmount = -1, assets = List(IssuedAsset(asset.id())))
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(contract)

    assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci)), fs) { blockDiffEi =>
      blockDiffEi should produce("Negative amount")
    }
  }

  property("payment should be positive") {
    val dApp = TxHelpers.signer(1).toAddress
    Try(TxHelpers.invoke(dApp, payments = Seq(Payment(-1, Waves)))).toEither should produce("NonPositiveAmount")
  }

  property("smart asset payment require extra fee") {
    val dApp                              = TxHelpers.signer(0)
    val asset                             = TxHelpers.issue()
    val contract                          = transferContract(_: String, assets = List(IssuedAsset(asset.id())))
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(contract)

    assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci)), fs) { blockDiffEi =>
      blockDiffEi should produce("does not exceed minimal value")
    }
  }

  property("contract with payment of smart asset require extra fee") {
    val issue                             = TxHelpers.issue()
    val payment                           = Payment(1, IssuedAsset(issue.id()))
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(transferContract(_), payment = Some(payment))
    assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(issue, setScript))), TestBlock.create(Seq(ci)), fs) { blockDiffEi =>
      blockDiffEi should produce("does not exceed minimal value")
    }
  }

  property("can't overflow payment + fee") {
    val invoker                           = TxHelpers.signer(0)
    val payment                           = Some(Payment(Long.MaxValue, Waves))
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(transferContract(_), payment = payment)
    assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
      _ should produce("Attempt to transfer unavailable funds")
    }
  }

  property("can't overflow sum of payment in contract") {
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(
      transferContract(_, recipientAmount = Long.MaxValue / 2 + 2, assets = List.fill(4)(Waves)),
      payment = Some(Payment(1, Waves))
    )
    assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
      _ should produce("Attempt to transfer unavailable funds")
    }
  }

  property("invoking contract with sponsored fee") {
    val recipient                                                      = TxHelpers.signer(5)
    val amount                                                         = 123
    val (genesis, setScript, ci, dApp, sponsoredAsset, setSponsorship) = preconditionsAndSetContract(transferContract(_), sponsored = true)
    val t                                                              = TxHelpers.transfer()

    assertDiffAndState(
      Seq(TestBlock.create(genesis ++ Seq[Transaction](sponsoredAsset, t, setSponsorship, setScript))),
      TestBlock.create(Seq(ci), Block.ProtoBlockVersion),
      fs
    ) {
      case (blockDiff, newState) =>
        blockDiff.scriptsRun shouldBe 1
        newState.balance(recipient.toAddress, Waves) shouldBe amount
        newState.balance(ci.sender.toAddress, IssuedAsset(sponsoredAsset.id())) shouldBe (sponsoredAsset.quantity / 10 - ci.fee)
        newState.balance(dApp.toAddress, IssuedAsset(sponsoredAsset.id())) shouldBe (sponsoredAsset.quantity - sponsoredAsset.quantity / 10 + ci.fee)
    }
  }

  property("argument passed to callable function has wrong type") {
    val (genesis, setScript, ci, _, _, _) = simplePreconditionsAndSetContract(invocationParamsCount = 2)
    assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
      _ should produce("Can't apply (CONST_BOOLEAN) to 'parseInt(str: String)'")
    }
  }

  property("can't write more than 100 entries") {
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(s => writeSet(s, ContractLimits.MaxWriteSetSize(V4) + 1))
    assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
      _ should produce("Stored data count limit is exceeded")
    }
  }

  property("can write 100 entries") {
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(s => writeSet(s, ContractLimits.MaxWriteSetSize(V4)))
    assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
      _.explicitGet()
    }
  }

  property("can't write entry with key size greater than limit") {
    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V3)
      .foreach { version =>
        val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(
          s => writeSetWithKeyLength(s, ContractLimits.MaxKeySizeInBytesByVersion(version) + 1, version),
          version = version
        )

        val settings =
          if (version == V3) fs
          else fs.copy(preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0))

        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), settings) {
          _ should produce(
            s"Data entry key size = ${ContractLimits.MaxKeySizeInBytesByVersion(version) + 1} bytes " +
              s"must be less than ${ContractLimits.MaxKeySizeInBytesByVersion(version)}"
          )
        }
      }
  }

  property("can write entry with key size equals limit") {
    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V3)
      .foreach { version =>
        val (genesis, setScript, ci, _, _, _) =
          preconditionsAndSetContract(s => writeSetWithKeyLength(s, ContractLimits.MaxKeySizeInBytesByVersion(version), version), version = version)
        val settings =
          if (version == V3) fs
          else fs.copy(preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0))

        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), settings) {
          _.explicitGet()
        }
      }
  }

  property("can't write entry with empty key from V4") {
    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V3)
      .foreach { version =>
        val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(
          s => writeSetWithKeyLength(s, length = 0, version = version),
          version = version
        )
        val settings =
          version match {
            case V3 => fs
            case V4 => fs.copy(preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0))
            case V5 =>
              fs.copy(
                preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0) + (BlockchainFeatures.SynchronousCalls.id -> 0)
              )
            case v =>
              throw new TestFailedException(s"Unexpected $v", 0)
          }
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), settings) {
          if (version == V3)
            _ shouldBe Symbol("right")
          else
            _ should produce("Data entry key should not be empty")
        }
      }
  }

  property("Function call args count should be equal @Callable func one") {
    Seq(0, 3)
      .foreach { invocationArgsCount =>
        val (genesis, setScript, ci, _, _, _) = simplePreconditionsAndSetContract(invocationParamsCount = invocationArgsCount)
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          _ should produce(s"takes 2 args but $invocationArgsCount were(was) given")
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

    (2 to 8).foreach { proofsCount =>
      val (genesis, setVerifier, setContract, ci, _, _, _) =
        preconditionsAndSetContractWithVerifier(multiSigCheckDApp(proofsCount), writeSetWithKeyLength(_))

      val proof         = ci.proofs
      val multiSigProof = ci.proofs.copy(proofs = List.fill(proofsCount)(proof.proofs.head))
      val multiSigCi    = ci.copy(1.toByte, proofs = multiSigProof)

      assertDiffEi(
        Seq(TestBlock.create(genesis ++ Seq(setVerifier, setContract))),
        TestBlock.create(Seq(multiSigCi)),
        fs
      )(_)
    }
  }

  property("Default function invocation should produce error if contract default function has arguments") {
    val contract                          = transferContract(_: String, funcName = "default", assets = List(Waves))
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(contract, isCIDefaultFunc = true)

    assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
      _ should produce(s"takes 1 args but 0 were(was) given")
    }
  }

  property("Default function invocation should produce error if contract does't have default function") {
    val contract                          = transferContract(_: String, funcName = "other", assets = List(Waves))
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(contract, isCIDefaultFunc = true)

    assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
      _ should produce("Cannot find callable function `default`, address = ")
    }
  }

  property("self-payment and self-transfer V3") {
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(
      transferContract(_, assets = List(Waves)),
      dApp = TxHelpers.signer(0),
      payment = Some(Payment(1, Waves))
    )
    val features = fs.copy(preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0))
    assertDiffEi(Seq(TestBlock.create(Seq(genesis.head, setScript))), TestBlock.create(Seq(ci)), features) {
      _.explicitGet()
    }
  }

  property("self-payment V4") {
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(
      transferContract(_, version = V4),
      payment = Some(Payment(1, Waves)),
      dApp = TxHelpers.signer(0),
      version = V4
    )
    val features = fs.copy(preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0))
    assertDiffEi(Seq(TestBlock.create(Seq(genesis.head, setScript))), TestBlock.create(Seq(ci)), features) {
      _ should produce("DApp self-payment is forbidden since V4")
    }
  }

  property("self-transfer V4") {
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(
      transferContract(_, assets = List(Waves), version = V4),
      dApp = TxHelpers.signer(0),
      version = V4
    )
    val features = fs.copy(preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0))
    assertDiffEi(Seq(TestBlock.create(Seq(genesis.head, setScript))), TestBlock.create(Seq(ci)), features) {
      _ should produce("DApp self-transfer is forbidden since V4")
    }
  }

  property("transferring asset this value") {
    val recipient = TxHelpers.signer(5)
    val quantity  = 1000000
    val amount    = quantity / 2
    val dApp      = TxHelpers.signer(1)

    val issue                             = TxHelpers.issue(dApp).signWith(dApp.privateKey)
    val contract                          = transferContract(_: String, assets = List(IssuedAsset(issue.id())))
    val (genesis, setScript, ci, _, _, _) = preconditionsAndSetContract(contract)

    val features = fs.copy(
      preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0)
    )
    assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(issue, ci), Block.ProtoBlockVersion), features) {
      case (blockDiff, newState) =>
        blockDiff.scriptsRun shouldBe 3
        newState.balance(dApp.toAddress, IssuedAsset(issue.id())) shouldBe (issue.quantity - amount)
        newState.balance(recipient.toAddress, IssuedAsset(issue.id())) shouldBe amount
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
           |func $funcName() = {
           |  let check = ${"sigVerify(base58'', base58'', base58'') ||" * 10} true
           |  if (check)
           |    then throw("bad news")
           |    else throw("bad news")
           |}
           |""".stripMargin
      Parser.parseContract(script).get.value
    }
    compileContractFromExpr(expr, V4)
  }

  property("issuing asset with existed id should produce error") {
    val dApp = TxHelpers.signer(1)

    val contract = issueContract("func")
    val script   = ContractScript(V4, contract).explicitGet()
    val invoke   = TxHelpers.invoke(dApp.toAddress, Some("func"))

    val blockchain: Blockchain = mock[Blockchain]
    (() => blockchain.settings)
      .expects()
      .returning(TestSettings.Default.blockchainSettings)
      .anyNumberOfTimes()
    (blockchain.assetScript _)
      .expects(*)
      .returning(None)
      .anyNumberOfTimes() // XXX Why?
    (blockchain.accountScript _)
      .expects(dApp.toAddress)
      .returning(Some(AccountScriptInfo(dApp.publicKey, script, 10L, Map(1 -> Map("func" -> 10L)))))
      .anyNumberOfTimes()
    (blockchain.accountScript _).expects(invoke.sender.toAddress).returning(None).anyNumberOfTimes()
    (blockchain.hasAccountScript _).expects(invoke.sender.toAddress).returning(false).anyNumberOfTimes()
    (() => blockchain.activatedFeatures)
      .expects()
      .returning(Map(BlockchainFeatures.Ride4DApps.id -> 0))
      .anyNumberOfTimes()
    (() => blockchain.height).expects().returning(1).anyNumberOfTimes()
    (blockchain.blockHeader _)
      .expects(*)
      .returning(
        Some(
          SignedBlockHeader(
            BlockHeader(1, 1, ByteStr.empty, 1, ByteStr.empty, PublicKey(new Array[Byte](32)), Seq(), 1, ByteStr.empty),
            ByteStr.empty
          )
        )
      )
      .anyNumberOfTimes()
    (blockchain.blockHeader _)
      .expects(*)
      .returning(
        Some(
          SignedBlockHeader(
            BlockHeader(1, 1, ByteStr.empty, 1, ByteStr.empty, PublicKey(new Array[Byte](32)), Seq(), 1, ByteStr.empty),
            ByteStr.empty
          )
        )
      )
      .anyNumberOfTimes()
    (blockchain.assetDescription _)
      .expects(*)
      .returning(
        Some(
          AssetDescription(
            ByteStr.fromBytes(1, 2, 3),
            dApp.publicKey,
            ByteString.EMPTY,
            ByteString.EMPTY,
            1,
            false,
            BigInt(1),
            Height(1),
            None,
            0L,
            false
          )
        )
      )
    InvokeScriptTransactionDiff
      .apply(blockchain, invoke.timestamp, limitedExecution = false)(invoke)
      .resultE should produce("is already issued")
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
           |func $funcName() = [Reissue(base58'$asset', 1, false), Reissue(base58'$asset', 4, true)]
           |""".stripMargin
      Parser.parseContract(script).get.value
    }

    compileContractFromExpr(expr, V4)
  }

  property("Reissuing unreissued asset should produce error") {
    val invoker     = TxHelpers.signer(0)
    val dApp        = TxHelpers.signer(1)
    val genesis1Tx  = TxHelpers.genesis(dApp.toAddress)
    val genesis2Tx  = TxHelpers.genesis(invoker.toAddress)
    val assetTx     = TxHelpers.issue(dApp)
    val contract    = reissueContract("func", assetTx.id())
    val script      = ContractScript(V4, contract).explicitGet()
    val setScriptTx = TxHelpers.setScript(dApp, script)

    val invoke = TxHelpers.invoke(dApp.toAddress, Some("func"))
    tempDb { _ =>
      val features = fs.copy(
        preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0)
      )
      assertDiffEi(Seq(TestBlock.create(Seq(genesis1Tx, genesis2Tx, setScriptTx))), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), features) {
        ei =>
          ei should produce("Asset is not reissuable")
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

  property("issued asset can be transferred") {
    val invoker = TxHelpers.signer(0)
    val dApp    = TxHelpers.signer(1)

    val genesis1Tx  = TxHelpers.genesis(dApp.toAddress)
    val genesis2Tx  = TxHelpers.genesis(invoker.toAddress)
    val contract    = transferIssueContract("func")
    val script      = ContractScript(V4, contract).explicitGet()
    val setScriptTx = TxHelpers.setScript(dApp, script)

    val invoke = TxHelpers.invoke(dApp.toAddress, Some("func"))

    tempDb { _ =>
      val features = fs.copy(
        preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0)
      )
      assertDiffEi(Seq(TestBlock.create(Seq(genesis1Tx, genesis2Tx, setScriptTx))), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), features) {
        ei =>
          ei
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

  property("nonissued asset cann't be transfered") {
    val invoker = TxHelpers.signer(0)
    val dApp    = TxHelpers.signer(1)

    val genesis1Tx  = TxHelpers.genesis(dApp.toAddress)
    val genesis2Tx  = TxHelpers.genesis(invoker.toAddress)
    val contract    = transferNonIssueContract("func")
    val script      = ContractScript(V4, contract).explicitGet()
    val setScriptTx = TxHelpers.setScript(dApp, script)

    val invoke = TxHelpers.invoke(dApp.toAddress, Some("func"))

    tempDb { _ =>
      val features = fs.copy(
        preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0)
      )
      assertDiffEi(Seq(TestBlock.create(Seq(genesis1Tx, genesis2Tx, setScriptTx))), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), features) {
        ei =>
          ei should produce("negative asset balance")
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

  property("duplicate issuing asset should produce diff error") {
    val invoker = TxHelpers.signer(0)
    val dApp    = TxHelpers.signer(1)

    val genesis1Tx  = TxHelpers.genesis(dApp.toAddress)
    val genesis2Tx  = TxHelpers.genesis(invoker.toAddress)
    val contract    = doubleIssueContract("func")
    val script      = ContractScript(V4, contract).explicitGet()
    val setScriptTx = TxHelpers.setScript(dApp, script)
    val invoke      = TxHelpers.invoke(dApp.toAddress, Some("func"))

    tempDb { _ =>
      val features = fs.copy(
        preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0),
        syncDAppCheckTransfersHeight = 999
      )
      assertDiffEi(Seq(TestBlock.create(Seq(genesis1Tx, genesis2Tx, setScriptTx))), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), features) {
        ei =>
          inside(ei) {
            case Right(diff) => diff.scriptResults(invoke.id()).error.get.text should include("is already issued")
          }
      }
    }
  }

  property("correctly counts sponsored fee") {
    val (genesis, setScript, invoke, _, issue, sponsorFee) = preconditionsAndSetContract(s => writeSet(s, 1), sponsored = true, selfSend = true)

    assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(issue, sponsorFee, setScript))), TestBlock.create(Seq(invoke)), fsWithV5) { diff =>
      invoke.feeAssetId shouldBe sponsorFee.asset
      invoke.dAppAddressOrAlias shouldBe invoke.sender.toAddress
      diff.explicitGet().portfolios(invoke.sender.toAddress).balanceOf(sponsorFee.asset) shouldBe 0L
    }
  }

  property(s"accepts failed transactions after ${BlockchainFeatures.BlockV5} activation") {
    val funcBinding = "func"

    val invoker = TxHelpers.signer(0)
    val dApp    = TxHelpers.signer(1)
    val other   = TxHelpers.signer(2)

    val sponsorIssue = TxHelpers.issue(other)
    val sponsorAsset = IssuedAsset(sponsorIssue.id())
    val sponsorTx    = TxHelpers.sponsor(sponsorAsset, sender = other)

    val issueTx = TxHelpers.issue()

    val feeInWaves = FeeConstants(InvokeScriptTransaction.typeId) * FeeValidation.FeeUnit
    val feeInAsset = Sponsorship.fromWaves(FeeConstants(InvokeScriptTransaction.typeId) * FeeValidation.FeeUnit, sponsorTx.minSponsoredAssetFee.get)

    Seq(
      (feeInWaves, Waves, issueContract(funcBinding), List.empty[EXPR]),        // insufficient fee
      (feeInAsset, sponsorAsset, issueContract(funcBinding), List.empty[EXPR]), // insufficient fee
      (feeInWaves, Waves, throwContract(funcBinding), List.empty[EXPR]),        // DApp script execution
      (feeInAsset, sponsorAsset, throwContract(funcBinding), List.empty[EXPR]), // DApp script execution
      {
        val contract = transferContract(funcBinding, assets = List(issueTx.asset), version = V4)
        val fee      = feeInWaves + ScriptExtraFee
        (fee, Waves, contract, List(CONST_BYTESTR(ByteStr.fromBytes(1, 2, 3)).explicitGet()))
      }
    ).foreach {
      case (fee, feeAsset, contract, args) =>
        val g1Tx = TxHelpers.genesis(dApp.toAddress, ENOUGH_AMT)
        val g2Tx = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)
        val g3Tx = TxHelpers.genesis(other.toAddress, ENOUGH_AMT)

        val tTx = TxHelpers.transfer(other, invoker.toAddress, sponsorIssue.quantity, sponsorAsset)

        val wavesBalance     = ENOUGH_AMT - enoughFee
        val sponsoredBalance = sponsorIssue.quantity

        val script = ContractScript(V4, contract).explicitGet()
        val ssTx   = TxHelpers.setScript(dApp, script)
        val invoke = TxHelpers.invoke(dApp.toAddress, Some(funcBinding), args, fee = fee, feeAssetId = feeAsset)

        assertDiffAndState(
          Seq(TestBlock.create(Seq(g1Tx, g2Tx, g3Tx, sponsorIssue, issueTx, sponsorTx, tTx, ssTx))),
          TestBlock.create(Seq(invoke), Block.ProtoBlockVersion),
          fsWithV5
        ) {
          case (diff, state) =>
            diff.scriptsRun shouldBe 0
            diff.portfolios(invoke.sender.toAddress).balanceOf(invoke.feeAssetId)
            state.balance(invoke.sender.toAddress, invoke.feeAssetId) shouldBe invoke.feeAssetId.fold(wavesBalance)(_ => sponsoredBalance) - invoke.fee
            state.transactionInfo(invoke.id()).map(r => r._2 -> r._1.succeeded) shouldBe Some((invoke, false))
        }
    }
  }

  property(
    s"rejects withdrawal of fee from the funds received as a result of the script call execution after ${BlockchainFeatures.BlockV5} activation"
  ) {
    val invoker = TxHelpers.signer(0)
    val dApp    = TxHelpers.signer(1)
    val other   = TxHelpers.signer(2)

    val g1Tx           = TxHelpers.genesis(dApp.toAddress)
    val g2Tx           = TxHelpers.genesis(other.toAddress)
    val iTx            = TxHelpers.issue(other)
    val sponsoredAsset = IssuedAsset(iTx.assetId)
    val sTx            = TxHelpers.sponsor(sponsoredAsset, sender = other)
    val tTx            = TxHelpers.transfer(other, dApp.toAddress, iTx.quantity / 1)

    val wavesFee     = ???
    val sponsoredFee = Sponsorship.fromWaves(wavesFee, sTx.minSponsoredAssetFee.get)

    Seq((Waves, wavesFee), (sponsoredAsset, sponsoredFee))
      .foreach {
        case (feeAsset, fee) =>
          val contract = transferContract("func", assets = List(feeAsset), version = V4)
          val script   = ContractScript(V4, contract).explicitGet()
          val ssTx     = TxHelpers.setScript(dApp, script)
          val invoke = TxHelpers.invoke(
            dApp.toAddress,
            Some("func"),
            args = List(CONST_BYTESTR(ByteStr.fromBytes(1, 2, 3)).explicitGet()),
            fee = fee,
            feeAssetId = feeAsset
          )

          assertDiffEi(Seq(TestBlock.create(Seq(g1Tx, g2Tx, iTx, sTx, tTx, ssTx))), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) {
            _ should produce("AccountBalanceError")
          }
      }
  }

  property("counts complexity correctly for failed transactions (validation fails)") {
    def contract(asset: String): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 4 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             |let a = base58'$asset'
             |
             |@Callable(inv)
             |func sameComplexity(i: String) = {
             | let check = ${"sigVerify(base58'', base58'', base58'') ||" * 10} true
             | if (i == "throw" && check) then
             |   throw("Some error")
             | else if (i == "insufficient fee" && check) then
             |   [ ${(1 to ContractLimits.MaxCallableActionsAmount(V4))
               .map(i => s"""Issue("Asset $i", "", 100, 8, true, unit, $i)""")
               .mkString(",")} ]
             | else if (i == "negative amount" && check) then
             |   [ ScriptTransfer(inv.caller, -1, a) ]
             | else if (i == "overflow amount" && check) then
             |   [ ScriptTransfer(inv.caller, ${Long.MaxValue / 2}, a), ScriptTransfer(inv.caller, ${Long.MaxValue / 2 + 1}, a) ]
             | else if (i == "self payment" && check) then
             |   [ ScriptTransfer(this, 10, unit) ]
             | else if (i == "max actions" && check) then
             |   [ ${(0 to ContractLimits.MaxCallableActionsAmount(V4)).map(_ => "ScriptTransfer(inv.caller, 10, a)").mkString(",")} ]
             | else if (i == "invalid data entries" && check) then
             |   [ ${(0 to ContractLimits.MaxWriteSetSize(V4))
               .map(x => s"""IntegerEntry("val", $x)""")
               .mkString(",")},ScriptTransfer(inv.caller, 10, a)]
             | else []
             |}
             |
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V4)
    }

    val invoker = TxHelpers.signer(0)
    val dApp    = TxHelpers.signer(1)

    val gTx1             = TxHelpers.genesis(dApp.toAddress, ENOUGH_AMT)
    val gTx2             = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)
    val (assetScript, _) = ScriptCompiler.compile("false", ScriptEstimatorV3(fixOverflow = true)).explicitGet()
    val iTx              = TxHelpers.issue(dApp, script = Some(assetScript))

    val script = ContractScript(V4, contract(iTx.assetId.toString)).explicitGet()
    val ssTx   = TxHelpers.setScript(dApp, script)

    Seq("throw", "insufficient fee", "negative amount", "overflow amount", "self payment", "max actions", "invalid data entries", "ok").foreach {
      arg =>
        val invoke = TxHelpers.invoke(dApp.toAddress, Some("sameComplexity"), args = List(CONST_STRING(arg).explicitGet()))
        assertDiffAndState(Seq(TestBlock.create(Seq(gTx1, gTx2, ssTx, iTx))), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) {
          case (diff, _) =>
            if (arg == "ok")
              diff.errorMessage(invoke.id()) shouldBe empty
            else
              diff.errorMessage(invoke.id()) shouldBe defined
        }
    }
  }

  property("counts complexity correctly for failed transactions (asset script fails)") {
    val (trueScript, trueComplexity) = {
      val script = """
         |{-# STDLIB_VERSION 4 #-}
         |{-# CONTENT_TYPE EXPRESSION #-}
         |
         |true""".stripMargin
      ScriptCompiler.compile(script, ScriptEstimatorV3(fixOverflow = true)).explicitGet()
    }

    val (falseScript, falseComplexity) = {
      val script = """
         |{-# STDLIB_VERSION 4 #-}
         |{-# CONTENT_TYPE EXPRESSION #-}
         |
         |false""".stripMargin
      ScriptCompiler.compile(script, ScriptEstimatorV3(fixOverflow = true)).explicitGet()
    }

    def contract(assets: Seq[String]): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 4 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             |@Callable(inv)
             |func foo() = {
             | [ ${assets.map(a => s"""ScriptTransfer(inv.caller, 10, base58'$a')""").mkString(",")} ]
             |}
             |
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V4)
    }
    val invoker = TxHelpers.signer(0)
    val dApp    = TxHelpers.signer(1)

    val gTx1 = TxHelpers.genesis(dApp.toAddress, ENOUGH_AMT)
    val gTx2 = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)

    Seq(true, false)
      .foreach { isAccountScripted =>
        val invokerScriptTx =
          if (isAccountScripted) Seq(TxHelpers.setScript(invoker, trueScript))
          else Seq.empty

        val failAsset    = Random.nextInt(6) + 1
        val assetScripts = (1 to 6).map(i => if (i == failAsset) falseScript else trueScript)
        val iTxs = (1 to 6).map { i =>
          TxHelpers.issue(dApp, script = Some(trueScript))
        }
        val tTxs = iTxs.takeRight(3).map { tx =>
          TxHelpers.transfer(dApp, invoker.toAddress, ENOUGH_AMT / 2, IssuedAsset(tx.assetId))
        }
        val saTxs = assetScripts.zipWithIndex.map {
          case (sc, i) => TxHelpers.setAssetScript(dApp, IssuedAsset(iTxs(i).id()), sc)
        }
        val script = ContractScript(V4, contract(iTxs.take(4).map(_.assetId.toString))).explicitGet()
        val ssTx   = TxHelpers.setScript(dApp, script)

        val payments = iTxs.takeRight(2).map(tx => Payment(10, IssuedAsset(tx.assetId)))
        val invoke   = TxHelpers.invoke(dApp.toAddress, Some("foo"), payments = payments)

        val genesisTxs = Seq(gTx1, gTx2) ++ invokerScriptTx ++ iTxs ++ tTxs ++ saTxs :+ ssTx

        assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) {
          case (diff, _) =>
            diff.errorMessage(invoke.id()) shouldBe defined
            diff.scriptsComplexity should be > 0L
        }
      }
  }

  property("scriptHash") {
    def contract(): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func foo() = {
             |  if scriptHash(i.caller) == unit && scriptHash(Alias("unexisting")) == unit
             |  then
             |    [
             |      BinaryEntry("hash1", scriptHash(this).value()),
             |      BinaryEntry("hash2", scriptHash(Alias("alias")).value())
             |    ]
             |  else
             |    throw("Unexpected script was found.")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }
    val invoker = TxHelpers.signer(0)
    val dApp    = TxHelpers.signer(1)

    val gTx1 = TxHelpers.genesis(dApp.toAddress, ENOUGH_AMT)
    val gTx2 = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)

    val alias  = TxHelpers.createAlias("alias")
    val script = ContractScript(V5, contract()).explicitGet()
    val ssTx   = TxHelpers.setScript(dApp, script)

    val payments = List(Payment(10, Waves))
    val invoke   = TxHelpers.invoke(dApp.toAddress, Some("foo"), payments = payments)

    assertDiffAndState(Seq(TestBlock.create(Seq(gTx1, gTx2, alias, ssTx))), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) {
      case (diff, bc) =>
        diff.errorMessage(invoke.id()) shouldBe None
        val hash = ByteStr(com.wavesplatform.lang.Global.blake2b256(script.bytes().arr))
        bc.accountData(dApp.toAddress, "hash1").get.value shouldBe hash
        bc.accountData(dApp.toAddress, "hash2").get.value shouldBe hash
    }
  }

  property("Crosscontract call (same accaunt)") {
    def contract(): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func default() = {
             |   ([IntegerEntry("bar", 1)], "return")
             | }
             |
             | @Callable(i)
             | func foo() = {
             |  let r = invoke(this, unit, [], [])
             |  if r == "return"
             |  then
             |   let data = getIntegerValue(this, "bar")
             |   if data == 1
             |   then
             |    [
             |     IntegerEntry("key", 1)
             |    ]
             |   else
             |    throw("Bad state")
             |  else
             |   throw("Bad returned value")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }
    val invoker = TxHelpers.signer(0)
    val dApp    = TxHelpers.signer(1)

    val gTx1 = TxHelpers.genesis(dApp.toAddress, ENOUGH_AMT)
    val gTx2 = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)

    val script   = ContractScript(V5, contract()).explicitGet()
    val ssTx     = TxHelpers.setScript(dApp, script)
    val payments = List(Payment(10, Waves))
    val invoke   = TxHelpers.invoke(dApp.toAddress, Some("foo"), Nil, payments)

    assertDiffAndState(Seq(TestBlock.create(Seq(gTx1, gTx2, ssTx))), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) {
      case (diff, bc) =>
        diff.errorMessage(invoke.id()) shouldBe None
        bc.accountData(dApp.toAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
        bc.accountData(dApp.toAddress, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
    }
  }

  property("Crosscontract call doesn't require extra fee") {
    def contract(): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func default() = {
             |   ([IntegerEntry("bar", 1)], "return")
             | }
             |
             | @Callable(i)
             | func foo() = {
             |  let r = invoke(this, unit, [], [])
             |  if r == "return"
             |  then
             |   let data = getIntegerValue(this, "bar")
             |   if data == 1
             |   then
             |    [
             |     IntegerEntry("key", 1)
             |    ]
             |   else
             |    throw("Bad state")
             |  else
             |   throw("Bad returned value")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }
    val invoker = TxHelpers.signer(0)
    val dApp    = TxHelpers.signer(1)
    val gTx1    = TxHelpers.genesis(dApp.toAddress, ENOUGH_AMT)
    val gTx2    = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)

    val script   = ContractScript(V5, contract()).explicitGet()
    val ssTx     = TxHelpers.setScript(dApp, script)
    val payments = List(Payment(10, Waves))
    val invoke   = TxHelpers.invoke(dApp.toAddress, Some("foo"), Nil, payments)

    assertDiffAndState(Seq(TestBlock.create(Seq(gTx1, gTx2, ssTx))), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) {
      case (diff, _) =>
        diff.errorMessage(invoke.id()) shouldBe None
        diff.scriptsComplexity shouldBe 108
        diff.scriptsRun shouldBe 2
    }
  }

  property("Crosscontract call (two accaunts)") {
    def contract(): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func bar(a: ByteVector) = {
             |   if i.caller.bytes == a && addressFromPublicKey(i.callerPublicKey).bytes == a
             |   then
             |     let n = Issue("barAsset", "bar asset", 1, 0, false, unit, 0)
             |     ([IntegerEntry("bar", 1), ScriptTransfer(Address(a), 3, unit), BinaryEntry("asset", n.calculateAssetId()), n, ScriptTransfer(Address(a), 1, n.calculateAssetId())], 17)
             |   else
             |     throw("Bad caller")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    def contract1(otherAcc: Address, alias: Alias): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func foo() = {
             |  let b1 = wavesBalance(this)
             |  let ob1 = wavesBalance(Address(base58'$otherAcc'))
             |  if b1 == b1 && ob1 == ob1
             |  then
             |    let r = invoke(Alias("${alias.name}"), "bar", [this.bytes], [AttachedPayment(unit, 17)])
             |    if r == 17
             |    then
             |     let data = getIntegerValue(Address(base58'$otherAcc'), "bar")
             |     let b2 = wavesBalance(this)
             |     let ob2 = wavesBalance(Address(base58'$otherAcc'))
             |     let ab = assetBalance(this, getBinaryValue(Address(base58'$otherAcc'), "asset"))
             |     if data == 1
             |     then
             |      if ob1.regular+14 == ob2.regular && b1.regular == b2.regular+14 && ab == 1
             |      then
             |       let l = Lease(Address(base58'$otherAcc'), 23)
             |       [
             |        IntegerEntry("key", 1),
             |        Lease(Address(base58'$otherAcc'), 13),
             |        l,
             |        LeaseCancel(l.calculateLeaseId())
             |       ]
             |      else
             |       throw("Balance check failed")
             |    else
             |     throw("Bad state")
             |   else
             |    throw("Bad returned value")
             |  else
             |   throw("Impossible")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    val invoker = TxHelpers.signer(0)
    val dApp    = TxHelpers.signer(1)
    val service = TxHelpers.signer(2)

    val gTx1 = TxHelpers.genesis(dApp.toAddress, ENOUGH_AMT)
    val gTx2 = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)
    val gTx3 = TxHelpers.genesis(service.toAddress, ENOUGH_AMT)

    val alias      = Alias.create("alias").explicitGet()
    val aliasTx    = TxHelpers.createAlias("alias", service)
    val script1    = ContractScript(V5, contract1(service.toAddress, alias)).explicitGet()
    val script     = ContractScript(V5, contract()).explicitGet()
    val ssTx       = TxHelpers.setScript(dApp, script1)
    val ssTx1      = TxHelpers.setScript(service, script)
    val payments   = List(Payment(10L, Waves))
    val invoke     = TxHelpers.invoke(dApp.toAddress, Some("foo"), Nil, payments)
    val genesisTxs = Seq(gTx1, gTx2, gTx3, aliasTx, ssTx1, ssTx)

    assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) {
      case (diff, bc) =>
        diff.scriptResults(invoke.id()).error shouldBe None
        val l  = diff.scriptResults(invoke.id()).leases(0)
        val l1 = diff.scriptResults(invoke.id()).leases(1)
        val l2 = diff.scriptResults(invoke.id()).leaseCancels(0)
        l.amount shouldBe 13
        l.recipient shouldBe service
        l1.amount shouldBe 23
        l1.recipient shouldBe service
        l1.id shouldBe l2.id
        bc.accountData(dApp.toAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
        bc.accountData(service.toAddress, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
    }
  }

  property("originCaller and originCallerPublicKey fields") {
    def contract(): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func bar(a: ByteVector, o: ByteVector) = {
             |   if i.caller.bytes == a && addressFromPublicKey(i.callerPublicKey).bytes == a && i.originCaller.bytes == o && addressFromPublicKey(i.originCallerPublicKey).bytes == o
             |   then
             |     let n = Issue("barAsset", "bar asset", 1, 0, false, unit, 0)
             |     ([IntegerEntry("bar", 1), ScriptTransfer(Address(a), 3, unit), BinaryEntry("asset", n.calculateAssetId()), n, ScriptTransfer(Address(a), 1, n.calculateAssetId())], 17)
             |   else
             |     throw("Bad caller")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    def contract1(otherAcc: Address, alias: Alias): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func foo() = {
             |  let b1 = wavesBalance(this)
             |  let ob1 = wavesBalance(Address(base58'$otherAcc'))
             |  if b1 == b1 && ob1 == ob1 && i.caller == i.originCaller && i.callerPublicKey == i.originCallerPublicKey
             |  then
             |    let r = invoke(Alias("${alias.name}"), "bar", [this.bytes, i.caller.bytes], [AttachedPayment(unit, 17)])
             |    if r == 17
             |    then
             |     let data = getIntegerValue(Address(base58'$otherAcc'), "bar")
             |     let b2 = wavesBalance(this)
             |     let ob2 = wavesBalance(Address(base58'$otherAcc'))
             |     let ab = assetBalance(this, getBinaryValue(Address(base58'$otherAcc'), "asset"))
             |     if data == 1
             |     then
             |      if ob1.regular+14 == ob2.regular && b1.regular == b2.regular+14 && ab == 1
             |      then
             |       let l = Lease(Address(base58'$otherAcc'), 23)
             |       [
             |        IntegerEntry("key", 1),
             |        Lease(Address(base58'$otherAcc'), 13),
             |        l,
             |        LeaseCancel(l.calculateLeaseId())
             |       ]
             |      else
             |       throw("Balance check failed")
             |    else
             |     throw("Bad state")
             |   else
             |    throw("Bad returned value")
             |  else
             |   throw("Imposible")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }
    val invoker = TxHelpers.signer(0)
    val dApp    = TxHelpers.signer(1)
    val service = TxHelpers.signer(2)

    val gTx1 = TxHelpers.genesis(dApp.toAddress, ENOUGH_AMT)
    val gTx2 = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)
    val gTx3 = TxHelpers.genesis(service.toAddress, ENOUGH_AMT)

    val alias      = Alias.create("alias").explicitGet()
    val aliasTx    = TxHelpers.createAlias("alias", service)
    val script1    = ContractScript(V5, contract1(service.toAddress, alias)).explicitGet()
    val script     = ContractScript(V5, contract()).explicitGet()
    val ssTx       = TxHelpers.setScript(dApp, script1)
    val ssTx1      = TxHelpers.setScript(service, script)
    val payments   = List(Payment(10L, Waves))
    val invoke     = TxHelpers.invoke(dApp.toAddress, Some("foo"), payments = payments)
    val genesisTxs = Seq(gTx1, gTx2, gTx3, aliasTx, ssTx1, ssTx)

    assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) {
      case (diff, bc) =>
        diff.scriptResults(invoke.id()).error shouldBe None
        val l  = diff.scriptResults(invoke.id()).leases(0)
        val l1 = diff.scriptResults(invoke.id()).leases(1)
        val l2 = diff.scriptResults(invoke.id()).leaseCancels(0)
        l.amount shouldBe 13
        l.recipient shouldBe service
        l1.amount shouldBe 23
        l1.recipient shouldBe service
        l1.id shouldBe l2.id
        bc.accountData(dApp.toAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
        bc.accountData(service.toAddress, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
    }
  }

  property("non-NFT issue require extra fee") {
    def contract(): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func bar(a: ByteVector) = {
             |   if i.caller.bytes == a && addressFromPublicKey(i.callerPublicKey).bytes == a
             |   then
             |     let n = Issue("barAsset", "bar asset", 2, 0, false, unit, 0)
             |     ([IntegerEntry("bar", 1), ScriptTransfer(Address(a), 3, unit), BinaryEntry("asset", n.calculateAssetId()), n, ScriptTransfer(Address(a), 1, n.calculateAssetId())], 17)
             |   else
             |     throw("Bad caller")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    def contract1(otherAcc: Address, alias: Alias): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func foo() = {
             |  let b1 = wavesBalance(this)
             |  let ob1 = wavesBalance(Address(base58'$otherAcc'))
             |  if b1 == b1 && ob1 == ob1
             |  then
             |    let r = invoke(Alias("${alias.name}"), "bar", [this.bytes], [AttachedPayment(unit, 17)])
             |    if r == 17
             |    then
             |     let data = getIntegerValue(Address(base58'$otherAcc'), "bar")
             |     let b2 = wavesBalance(this)
             |     let ob2 = wavesBalance(Address(base58'$otherAcc'))
             |     let ab = assetBalance(this, getBinaryValue(Address(base58'$otherAcc'), "asset"))
             |     if data == 1
             |     then
             |      if ob1.regular+14 == ob2.regular && b1.regular == b2.regular+14 && ab == 1
             |      then
             |       [
             |        IntegerEntry("key", 1)
             |       ]
             |      else
             |       throw("Balance check failed")
             |    else
             |     throw("Bad state")
             |   else
             |    throw("Bad returned value")
             |  else
             |   throw("Impossible")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    val invoker = TxHelpers.signer(0)
    val dApp    = TxHelpers.signer(1)
    val service = TxHelpers.signer(2)

    val gTx1 = TxHelpers.genesis(dApp.toAddress, ENOUGH_AMT)
    val gTx2 = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)
    val gTx3 = TxHelpers.genesis(service.toAddress, ENOUGH_AMT)

    val alias      = Alias.create("alias").explicitGet()
    val aliasTx    = TxHelpers.createAlias("alias")
    val script1    = ContractScript(V5, contract1(service.toAddress, alias)).explicitGet()
    val script     = ContractScript(V5, contract()).explicitGet()
    val ssTx       = TxHelpers.setScript(dApp, script1)
    val ssTx1      = TxHelpers.setScript(service, script)
    val payments   = List(Payment(10L, Waves))
    val invoke     = TxHelpers.invoke(dApp.toAddress, Some("foo"), payments = payments)
    val genesisTxs = Seq(gTx1, gTx2, gTx3, aliasTx, ssTx1, ssTx)
    assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) {
      case (diff, bc) =>
        val err = diff.scriptResults(invoke.id()).error.get
        err.code shouldBe FailedTransactionError.Cause.FeeForActions.code
        bc.accountData(dApp.toAddress, "key") shouldBe None
        bc.accountData(service.toAddress, "bar") shouldBe None
    }
  }

  property("non-NFT issue work") {
    def contract(): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func bar(a: ByteVector) = {
             |   if i.caller.bytes == a && addressFromPublicKey(i.callerPublicKey).bytes == a
             |   then
             |     let n = Issue("barAsset", "bar asset", 2, 0, false, unit, 0)
             |     ([IntegerEntry("bar", 1), ScriptTransfer(Address(a), 3, unit), BinaryEntry("asset", n.calculateAssetId()), n, ScriptTransfer(Address(a), 1, n.calculateAssetId())], 17)
             |   else
             |     throw("Bad caller")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    def contract1(otherAcc: Address, alias: Alias): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func foo() = {
             |  let b1 = wavesBalance(this)
             |  let ob1 = wavesBalance(Address(base58'$otherAcc'))
             |  if b1 == b1 && ob1 == ob1
             |  then
             |    let r = invoke(Alias("${alias.name}"), "bar", [this.bytes], [AttachedPayment(unit, 17)])
             |    if r == 17
             |    then
             |     let data = getIntegerValue(Address(base58'$otherAcc'), "bar")
             |     let b2 = wavesBalance(this)
             |     let ob2 = wavesBalance(Address(base58'$otherAcc'))
             |     let ab = assetBalance(this, getBinaryValue(Address(base58'$otherAcc'), "asset"))
             |     if data == 1
             |     then
             |      if ob1.regular+14 == ob2.regular && b1.regular == b2.regular+14 && ab == 1
             |      then
             |       [
             |        IntegerEntry("key", 1)
             |       ]
             |      else
             |       throw("Balance check failed")
             |    else
             |     throw("Bad state")
             |   else
             |    throw("Bad returned value")
             |  else
             |   throw("Impossible")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }
    val invoker = TxHelpers.signer(0)
    val dApp    = TxHelpers.signer(1)
    val service = TxHelpers.signer(2)

    val gTx1 = TxHelpers.genesis(dApp.toAddress, ENOUGH_AMT)
    val gTx2 = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)
    val gTx3 = TxHelpers.genesis(service.toAddress, ENOUGH_AMT)

    val alias   = Alias.create("alias").explicitGet()
    val aliasTx = TxHelpers.createAlias("alias", service)
    val script1 = ContractScript(V5, contract1(service.toAddress, alias)).explicitGet()
    val script  = ContractScript(V5, contract()).explicitGet()
    val ssTx    = TxHelpers.setScript(dApp, script1)
    val ssTx1   = TxHelpers.setScript(service, script)

    val payments   = List(Payment(10L, Waves))
    val invoke     = TxHelpers.invoke(dApp.toAddress, Some("foo"), payments = payments)
    val genesisTxs = Seq(gTx1, gTx2, gTx3, aliasTx, ssTx1, ssTx)

    assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) {
      case (diff, bc) =>
        bc.accountData(dApp.toAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
        bc.accountData(service.toAddress, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
    }
  }

  property("Crosscontract call (two accaunts, double call)") {
    def contract(): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func bar(a: ByteVector) = {
             |   ([IntegerEntry("bar", 1), ScriptTransfer(Address(a), 3, unit)], 17)
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    def contract1(otherAcc: Address): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func foo() = {
             |  let b1 = wavesBalance(this)
             |  let ob1 = wavesBalance(Address(base58'$otherAcc'))
             |  if b1 == b1 && ob1 == ob1
             |  then
             |    let r = invoke(Address(base58'$otherAcc'), "bar", [this.bytes], [AttachedPayment(unit, 17)])
             |    if r == 17
             |    then
             |     let data = getIntegerValue(Address(base58'$otherAcc'), "bar")
             |     let b2 = wavesBalance(this)
             |     let ob2 = wavesBalance(Address(base58'$otherAcc'))
             |     if data == 1
             |     then
             |      if ob1.regular+14 == ob2.regular && b1.regular == b2.regular+14
             |      then
             |       let r1 = invoke(Address(base58'$otherAcc'), "bar", [this.bytes], [AttachedPayment(unit, 18)])
             |       if r1 == r1
             |       then
             |        let b3 = wavesBalance(this)
             |        let ob3 = wavesBalance(Address(base58'$otherAcc'))
             |        if ob2.regular+15 == ob3.regular && b2.regular == b3.regular+15
             |        then
             |         [
             |          IntegerEntry("key", 1)
             |         ]
             |        else
             |         throw("Bad balance after second invoke")
             |      else
             |       throw("Impossible")
             |     else
             |      throw("Balance check failed")
             |    else
             |     throw("Bad state")
             |  else
             |   throw("Bad returned value")
             |   else
             |    throw("Impossible")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    val dApp    = TxHelpers.signer(0)
    val invoker = TxHelpers.signer(1)
    val service = TxHelpers.signer(2)

    val gTx1 = TxHelpers.genesis(dApp.toAddress, ENOUGH_AMT)
    val gTx2 = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)
    val gTx3 = TxHelpers.genesis(service.toAddress, ENOUGH_AMT)

    val script1 = ContractScript(V5, contract1(service.toAddress)).explicitGet()
    val script  = ContractScript(V5, contract()).explicitGet()
    val ssTx    = TxHelpers.setScript(dApp, script1)
    val ssTx1   = TxHelpers.setScript(service, script)

    val payments = List(Payment(10, Waves))
    val invoke   = TxHelpers.invoke(dApp.toAddress, Some("foo"), payments = payments)

    val genesisTxs = Seq(gTx1, gTx2, gTx3, ssTx1, ssTx)

    assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) {
      case (diff, bc) =>
        bc.accountData(dApp.toAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
        bc.accountData(service.toAddress, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
    }
  }

  property("Crosscontract nested call (two accounts)") {
    def contract(): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func bar(a: ByteVector) = {
             |   ([IntegerEntry("bar", 1), ScriptTransfer(Address(a), 3, unit)], 17)
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    def contract1(otherAcc: Address): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func back() = {
             |   [IntegerEntry("key", 0), ScriptTransfer(Address(base58'$otherAcc'), 2, unit)]
             | }
             |
             | @Callable(i)
             | func foo() = {
             |  let b1 = wavesBalance(this)
             |  let ob1 = wavesBalance(Address(base58'$otherAcc'))
             |  if b1 == b1 && ob1 == ob1
             |  then
             |    let r = invoke(Address(base58'$otherAcc'), "bar", [this.bytes], [AttachedPayment(unit, 17)])
             |    if r == 17
             |    then
             |     let data = getIntegerValue(Address(base58'$otherAcc'), "bar")
             |     let b2 = wavesBalance(this)
             |     let ob2 = wavesBalance(Address(base58'$otherAcc'))
             |     if data == 1
             |     then
             |      if ob1.regular + 14 == ob2.regular && b1.regular == b2.regular + 14
             |      then
             |       [
             |        IntegerEntry("key", 1)
             |       ]
             |      else
             |       throw("Balance check failed")
             |    else
             |     throw("Bad state")
             |   else
             |    throw("Bad returned value")
             |  else
             |   throw("Impossible")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    val dApp    = TxHelpers.signer(0)
    val invoker = TxHelpers.signer(0)
    val service = TxHelpers.signer(0)

    val gTx1 = TxHelpers.genesis(dApp.toAddress, ENOUGH_AMT)
    val gTx2 = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)
    val gTx3 = TxHelpers.genesis(service.toAddress, ENOUGH_AMT)

    val script1 = ContractScript(V5, contract1(service.toAddress)).explicitGet()
    val script  = ContractScript(V5, contract()).explicitGet()
    val ssTx    = TxHelpers.setScript(dApp, script1)
    val ssTx1   = TxHelpers.setScript(service, script)

    val payments   = List(Payment(10, Waves))
    val invoke     = TxHelpers.invoke(dApp.toAddress, Some("foo"), Nil, payments = payments)
    val genesisTxs = Seq(gTx1, gTx2, gTx3, ssTx1, ssTx)

    assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) {
      case (_, bc) =>
        bc.accountData(dApp.toAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
        bc.accountData(service.toAddress, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
    }
  }

  property("Crosscontract with payment") {
    def contract(): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func bar(a: ByteVector) = {
             |   ([IntegerEntry("bar", 1), ScriptTransfer(Address(a), 3, unit)], 17)
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    def contract1(otherAcc: Address): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func back() = {
             |   [ScriptTransfer(Address(base58'$otherAcc'), 2, unit)]
             | }
             |
             | @Callable(i)
             | func foo() = {
             |  let b1 = wavesBalance(this)
             |  let ob1 = wavesBalance(Address(base58'$otherAcc'))
             |  if b1 == b1 && ob1 == ob1
             |  then
             |    let r = invoke(Address(base58'$otherAcc'), "bar", [this.bytes], [AttachedPayment(unit, 17)])
             |    if r == 17
             |    then
             |     let data = getIntegerValue(Address(base58'$otherAcc'), "bar")
             |     let b2 = wavesBalance(this)
             |     let ob2 = wavesBalance(Address(base58'$otherAcc'))
             |     if data == 1
             |     then
             |      if ob1.regular + 14 == ob2.regular && b1.regular == b2.regular + 14
             |      then
             |       [
             |        IntegerEntry("key", 1)
             |       ]
             |      else
             |       throw("Balance check failed: " + ob1.regular.toString() + " " + ob2.regular.toString())
             |    else
             |     throw("Bad state")
             |   else
             |    throw("Bad returned value")
             |  else
             |   throw("Impossible")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    val invoker = TxHelpers.signer(0)
    val dApp    = TxHelpers.signer(1)
    val service = TxHelpers.signer(2)

    val fee  = ciFee()
    val gTx1 = TxHelpers.genesis(dApp.toAddress, ENOUGH_AMT)
    val gTx2 = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)
    val gTx3 = TxHelpers.genesis(service.toAddress, ENOUGH_AMT)

    val script1 = ContractScript(V5, contract1(service.toAddress)).explicitGet()
    val script  = ContractScript(V5, contract()).explicitGet()
    val ssTx    = TxHelpers.setScript(dApp, script1)
    val ssTx1   = TxHelpers.setScript(service, script)

    val payments = List(Payment(10, Waves))
    val invoke   = TxHelpers.invoke(dApp.toAddress, Some("foo"), payments = payments)

    val genesisTxs = Seq(gTx1, gTx2, gTx3, ssTx1, ssTx)

    assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) {
      case (_, bc) =>
        bc.accountData(dApp.toAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
        bc.accountData(service.toAddress, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
    }
  }

  property("Infinite recursive crosscontract call") {
    val recursiveContract: DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func foo() = {
             |  let r = invoke(this, "foo", [], [])
             |  if r == r
             |  then
             |    [
             |    ]
             |  else
             |   throw("Impossible")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    val dApp    = TxHelpers.signer(0)
    val invoker = TxHelpers.signer(0)

    val gTx1 = TxHelpers.genesis(dApp.toAddress, ENOUGH_AMT)
    val gTx2 = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)

    val script = ContractScript(V5, recursiveContract).explicitGet()
    val ssTx   = TxHelpers.setScript(dApp, script)

    val payments = List(Payment(10, Waves))
    val invoke   = TxHelpers.invoke(dApp.toAddress, Some("foo"), payments = payments)

    assertDiffEi(Seq(TestBlock.create(Seq(gTx1, gTx2, ssTx))), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) { ei =>
      ei should produce(s"DApp calls limit = 100 is exceeded")
    }
  }

  property("Smart asset transfer by nested contract actions") {
    val (assetScript, _) = {
      val script = """
         |{-# STDLIB_VERSION 5 #-}
         |{-# CONTENT_TYPE EXPRESSION #-}
         |
         |true""".stripMargin
      ScriptCompiler.compile(script, ScriptEstimatorV3(fixOverflow = true)).explicitGet()
    }

    def contract(asset: ByteStr): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func bar(a: ByteVector) = {
             |   ([IntegerEntry("bar", 1), ScriptTransfer(Address(a), 3, base58'$asset')], 17)
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    def contract1(otherAcc: Address, asset: ByteStr): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func back() = {
             |   [IntegerEntry("key", 0), ScriptTransfer(Address(base58'$otherAcc'), 2, unit)]
             | }
             |
             | @Callable(i)
             | func foo() = {
             |  let b1 = wavesBalance(this)
             |  let ob1 = wavesBalance(Address(base58'$otherAcc'))
             |  if b1 == b1 && ob1 == ob1
             |  then
             |    let r = invoke(Address(base58'$otherAcc'), "bar", [this.bytes], [AttachedPayment(unit, 17)])
             |    if r == 17
             |    then
             |     let data = getIntegerValue(Address(base58'$otherAcc'), "bar")
             |     let b2 = wavesBalance(this)
             |     let ob2 = wavesBalance(Address(base58'$otherAcc'))
             |     let ab = assetBalance(this, base58'$asset')
             |     if data == 1
             |     then
             |      if ob1.regular + 17 == ob2.regular && b1.regular == b2.regular + 17 && ab == 3
             |      then
             |       [
             |        IntegerEntry("key", 1)
             |       ]
             |      else
             |       throw("Balance check failed " + ob1.regular.toString() + " " + ob2.regular.toString())
             |    else
             |     throw("Bad state")
             |   else
             |    throw("Bad returned value")
             |  else
             |   throw("Impossible")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    val invoker = TxHelpers.signer(0)
    val dApp    = TxHelpers.signer(1)
    val service = TxHelpers.signer(2)

    val iTx = TxHelpers.issue(service, script = Some(assetScript))

    val gTx1 = TxHelpers.genesis(dApp.toAddress, ENOUGH_AMT)
    val gTx2 = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)
    val gTx3 = TxHelpers.genesis(service.toAddress, ENOUGH_AMT)

    val script1    = ContractScript(V5, contract1(service.toAddress, iTx.id())).explicitGet()
    val script     = ContractScript(V5, contract(iTx.id())).explicitGet()
    val ssTx       = TxHelpers.setScript(dApp, script1)
    val ssTx1      = TxHelpers.setScript(service, script)
    val payments   = List(Payment(10, Waves))
    val invoke     = TxHelpers.invoke(dApp.toAddress, Some("foo"), payments = payments)
    val genesisTxs = Seq(gTx1, gTx2, gTx3, ssTx1, ssTx, iTx)

    assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) {
      case (_, bc) =>
        bc.accountData(dApp.toAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
        bc.accountData(service.toAddress, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
    }
  }

  property("Asset transfer disabled in nested contract actions") {
    val (assetScript, _) = {
      val script = """
         |{-# STDLIB_VERSION 5 #-}
         |{-# CONTENT_TYPE EXPRESSION #-}
         |
         |false""".stripMargin
      ScriptCompiler.compile(script, ScriptEstimatorV3(fixOverflow = true)).explicitGet()
    }

    def contract(asset: ByteStr): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func bar(a: ByteVector) = {
             |   ([IntegerEntry("bar", 1), ScriptTransfer(Address(a), 3, base58'$asset')], 17)
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    def contract1(otherAcc: Address): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func back() = {
             |   [IntegerEntry("key", 0), ScriptTransfer(Address(base58'$otherAcc'), 2, unit)]
             | }
             |
             | @Callable(i)
             | func foo() = {
             |  let b1 = wavesBalance(this)
             |  let ob1 = wavesBalance(Address(base58'$otherAcc'))
             |  if b1 == b1 && ob1 == ob1
             |  then
             |    let r = invoke(Address(base58'$otherAcc'), "bar", [this.bytes], [AttachedPayment(unit, 17)])
             |    if r == 17
             |    then
             |     let data = getIntegerValue(Address(base58'$otherAcc'), "bar")
             |     let tdata = getIntegerValue(this, "key")
             |     let b2 = wavesBalance(this)
             |     let ob2 = wavesBalance(Address(base58'$otherAcc'))
             |     if data == 1 && tdata == 0
             |     then
             |      if ob1.regular+16 == ob2.regular && b1.regular == b2.regular+16
             |      then
             |       [
             |        IntegerEntry("key", 1)
             |       ]
             |      else
             |       throw("Balance check failed")
             |    else
             |     throw("Bad state")
             |   else
             |    throw("Bad returned value")
             |  else
             |   throw("Impossible")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    val invoker = TxHelpers.signer(0)
    val dApp    = TxHelpers.signer(1)
    val service = TxHelpers.signer(2)

    val iTx = TxHelpers.issue(service, script = Some(assetScript))

    val gTx1 = TxHelpers.genesis(dApp.toAddress, ENOUGH_AMT)
    val gTx2 = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)
    val gTx3 = TxHelpers.genesis(service.toAddress, ENOUGH_AMT)

    val script1    = ContractScript(V5, contract1(service.toAddress)).explicitGet()
    val script     = ContractScript(V5, contract(iTx.id())).explicitGet()
    val ssTx       = TxHelpers.setScript(dApp, script1)
    val ssTx1      = TxHelpers.setScript(service, script)
    val payments   = List(Payment(10, Waves))
    val invoke     = TxHelpers.invoke(dApp.toAddress, Some("foo"), payments = payments)
    val genesisTxs = Seq(gTx1, gTx2, gTx3, ssTx1, ssTx, iTx)

    assertDiffEi(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) { ei =>
      ei should produce(s"Transaction is not allowed by script of the asset ${iTx.id()}")
    }
  }

  property("Asset payment disabled by asset script") {
    val (assetScript, _) = {
      val script = """
         |{-# STDLIB_VERSION 5 #-}
         |{-# CONTENT_TYPE EXPRESSION #-}
         |
         |false""".stripMargin
      ScriptCompiler.compile(script, ScriptEstimatorV3(fixOverflow = true)).explicitGet()
    }

    def contract: DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func bar(a: ByteVector) = {
             |   let r = invoke(Address(a), "back", [], [])
             |   if r == r
             |   then
             |    ([IntegerEntry("bar", 1)], 17)
             |   else
             |    throw("Impossible")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    def contract1(otherAcc: Address, asset: ByteStr): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func back() = {
             |   [IntegerEntry("key", 0), ScriptTransfer(Address(base58'$otherAcc'), 2, unit)]
             | }
             |
             | @Callable(i)
             | func foo() = {
             |  let b1 = wavesBalance(this)
             |  let ob1 = wavesBalance(Address(base58'$otherAcc'))
             |  if b1 == b1 && ob1 == ob1
             |  then
             |    let r = invoke(Address(base58'$otherAcc'), "bar", [this.bytes], [AttachedPayment(base58'$asset', 1)])
             |    if r == 17
             |    then
             |     let data = getIntegerValue(Address(base58'$otherAcc'), "bar")
             |     let tdata = getIntegerValue(this, "key")
             |     let b2 = wavesBalance(this)
             |     let ob2 = wavesBalance(Address(base58'$otherAcc'))
             |     if data == 1 && tdata == 0
             |     then
             |      if ob1.regular+16 == ob2.regular && b1.regular == b2.regular+16
             |      then
             |       [
             |        IntegerEntry("key", 1)
             |       ]
             |      else
             |       throw("Balance check failed")
             |    else
             |     throw("Bad state")
             |   else
             |    throw("Bad returned value")
             |  else
             |   throw("Impossible")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    val invoker = TxHelpers.signer(0)
    val dApp    = TxHelpers.signer(1)
    val service = TxHelpers.signer(2)

    val iTx = TxHelpers.issue(dApp, script = Some(assetScript))

    val gTx1 = TxHelpers.genesis(dApp.toAddress, ENOUGH_AMT)
    val gTx2 = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)
    val gTx3 = TxHelpers.genesis(service.toAddress, ENOUGH_AMT)

    val script1    = ContractScript(V5, contract1(service.toAddress, iTx.id())).explicitGet()
    val script     = ContractScript(V5, contract).explicitGet()
    val ssTx       = TxHelpers.setScript(dApp, script1)
    val ssTx1      = TxHelpers.setScript(service, script)
    val payments   = List(Payment(10, Waves))
    val invoke     = TxHelpers.invoke(dApp.toAddress, Some("foo"), payments = payments)
    val genesisTxs = Seq(gTx1, gTx2, gTx3, ssTx1, ssTx, iTx)

    assertDiffEi(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) { ei =>
      ei should produce(s"Transaction is not allowed by script of the asset ${iTx.id()}")
    }
  }

  property("Payment in transaction process after Invoke") {

    def contract(asset: ByteStr): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func bar(a: ByteVector) = {
             |   let r = invoke(Address(a), "back", [], [])
             |   if r == r
             |   then
             |    ([IntegerEntry("bar", 1), ScriptTransfer(Address(a), 3, base58'$asset')], 17)
             |   else
             |    throw("Impossible")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    def contract1(otherAcc: Address): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func back() = {
             |   [IntegerEntry("key", 0), ScriptTransfer(Address(base58'$otherAcc'), 2, unit)]
             | }
             |
             | @Callable(i)
             | func foo() = {
             |  let b1 = wavesBalance(this)
             |  let ob1 = wavesBalance(Address(base58'$otherAcc'))
             |  if b1 == b1 && ob1 == ob1
             |  then
             |    let r = invoke(Address(base58'$otherAcc'), "bar", [this.bytes], i.payments)
             |    if r == 17
             |    then
             |     let data = getIntegerValue(Address(base58'$otherAcc'), "bar")
             |     let tdata = getIntegerValue(this, "key")
             |     let b2 = wavesBalance(this)
             |     let ob2 = wavesBalance(Address(base58'$otherAcc'))
             |     if data == 1 && tdata == 0
             |     then
             |      if ob1.regular+16 == ob2.regular && b1.regular == b2.regular+16
             |      then
             |       [
             |        IntegerEntry("key", 1)
             |       ]
             |      else
             |       throw("Balance check failed")
             |    else
             |     throw("Bad state")
             |   else
             |    throw("Bad returned value")
             |  else
             |   throw("Impossible")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    val invoker = TxHelpers.signer(0)
    val dApp    = TxHelpers.signer(1)
    val service = TxHelpers.signer(2)

    val iTx = TxHelpers.issue(dApp)

    val gTx1 = TxHelpers.genesis(dApp.toAddress, ENOUGH_AMT)
    val gTx2 = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)
    val gTx3 = TxHelpers.genesis(service.toAddress, ENOUGH_AMT)

    val script1    = ContractScript(V5, contract1(service.toAddress)).explicitGet()
    val script     = ContractScript(V5, contract(iTx.id())).explicitGet()
    val ssTx       = TxHelpers.setScript(dApp, script1)
    val ssTx1      = TxHelpers.setScript(service, script)
    val payments   = List(Payment(20, IssuedAsset(iTx.id())))
    val invoke     = TxHelpers.invoke(dApp.toAddress, Some("foo"), payments = payments)
    val genesisTxs = Seq(gTx1, gTx2, gTx3, ssTx1, ssTx, iTx)

    assertDiffEi(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) { ei =>
      ei should produce(
        s"Attempt to transfer unavailable funds: " +
          s"Transaction application leads to negative asset '${iTx.id()}' balance to (at least) temporary negative state, current balance is 0"
      )
    }
  }

  property("Check balances in payment and asset scripts") {
    val transferAmount              = 3
    val paymentFromClientDAppAmount = 5
    val paymentFromInvokerAmount    = 10
    val returnValue                 = 17

    val invoker = TxHelpers.signer(0)
    val fee     = ciFee(3).sample.get

    val paymentScript = {
      val script = s"""
                      | {-# STDLIB_VERSION 5        #-}
                      | {-# SCRIPT_TYPE ASSET       #-}
                      | {-# CONTENT_TYPE EXPRESSION #-}
                      | assetBalance(this.issuer, this.id) == $ENOUGH_AMT
                    """.stripMargin
      ScriptCompiler.compile(script, ScriptEstimatorV3(fixOverflow = true)).explicitGet()._1
    }

    val transferScript = {
      val script = s"""
                      | {-# STDLIB_VERSION 5        #-}
                      | {-# SCRIPT_TYPE ASSET       #-}
                      | {-# CONTENT_TYPE EXPRESSION #-}
                      |
                      | let paymentAsset = this.issuer.getBinaryValue("paymentAsset")
                      | let startWavesBalance = this.issuer.getIntegerValue("startWavesBalance")
                      | let startInvokerBalance = this.issuer.getIntegerValue("startInvokerBalance")
                      | let resultInvokerBalance = wavesBalance(Address(base58'${invoker.toAddress.stringRepr}')).regular
                      | let issuerBalance = wavesBalance(this.issuer)
                      |
                      | assetBalance(this.issuer, this.id) == $ENOUGH_AMT                                     &&
                      | assetBalance(this.issuer, paymentAsset) == $ENOUGH_AMT - $paymentFromClientDAppAmount &&
                      | issuerBalance.regular == startWavesBalance                                            &&
                      | resultInvokerBalance == startInvokerBalance - $fee
                    """.stripMargin
      ScriptCompiler.compile(script, ScriptEstimatorV3(fixOverflow = true)).explicitGet()._1
    }

    def serviceDApp(): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5   #-}
             |{-# CONTENT_TYPE DAPP  #-}
             |{-#SCRIPT_TYPE ACCOUNT #-}
             |
             | @Callable(i)
             | func bar(startInvokerBalance: Int, startWavesBalance: Int, startPaymentAssetBalance: Int, paymentAsset: ByteVector) = {
             |   let resultInvokerBalance = wavesBalance(Address(base58'${invoker.toAddress.stringRepr}')).regular
             |   let paymentAssetBalance = assetBalance(i.caller, paymentAsset)
             |
             |   if (
             |     startInvokerBalance == resultInvokerBalance         &&
             |     startWavesBalance == wavesBalance(i.caller).regular &&
             |     startPaymentAssetBalance == paymentAssetBalance + i.payments[0].amount
             |   )
             |     then
             |       ([IntegerEntry("bar", 1)], $returnValue)
             |     else
             |       throw("Balance check failed")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }
      compileContractFromExpr(expr, V5)
    }

    def clientDApp(serviceDAppAddress: Address, transferAsset: ByteStr, paymentAsset: ByteStr): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5    #-}
             |{-# CONTENT_TYPE DAPP   #-}
             |{-# SCRIPT_TYPE ACCOUNT #-}
             |
             | @Callable(i)
             | func foo() = {
             |  strict startInvokerBalance = wavesBalance(Address(base58'${invoker.toAddress.stringRepr}')).regular
             |  strict startWavesBalance = wavesBalance(this).regular
             |  strict startPaymentAssetBalance = assetBalance(this, base58'$paymentAsset')
             |
             |  strict r = invoke(
             |    Address(base58'$serviceDAppAddress'),
             |    "bar",
             |    [startInvokerBalance, startWavesBalance, startPaymentAssetBalance, base58'$paymentAsset'],
             |    [AttachedPayment(base58'$paymentAsset', $paymentFromClientDAppAmount)]
             |  )
             |
             |  strict resultWavesBalance = wavesBalance(this).regular
             |  strict resultPaymentAssetBalance = assetBalance(this, base58'$paymentAsset')
             |
             |  if (
             |    startWavesBalance == resultWavesBalance &&
             |    resultPaymentAssetBalance == startPaymentAssetBalance - $paymentFromClientDAppAmount
             |  )
             |  then
             |   [
             |     BinaryEntry("paymentAsset", base58'$paymentAsset'),
             |     IntegerEntry("startInvokerBalance", startInvokerBalance),
             |     IntegerEntry("startWavesBalance", startWavesBalance),
             |     IntegerEntry("startPaymentAssetBalance", startPaymentAssetBalance),
             |     ScriptTransfer(Address(base58'$serviceDAppAddress'), $transferAmount, base58'$transferAsset'),
             |     IntegerEntry("key", 1)
             |   ]
             |  else
             |   throw("Balance check failed")
             | }
           """.stripMargin
        Parser.parseContract(script).get.value
      }
      compileContractFromExpr(expr, V5)
    }

    val clientDAppAcc  = TxHelpers.signer(1)
    val serviceDAppAcc = TxHelpers.signer(2)

    val paymentIssue  = TxHelpers.issue(clientDAppAcc, script = Some(paymentScript))
    val transferIssue = TxHelpers.issue(clientDAppAcc, script = Some(transferScript))

    val gTx1 = TxHelpers.genesis(clientDAppAcc.toAddress, ENOUGH_AMT - 1)
    val gTx2 = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)
    val gTx3 = TxHelpers.genesis(serviceDAppAcc.toAddress, ENOUGH_AMT)

    val clientDAppScript  = ContractScript(V5, clientDApp(serviceDAppAcc.toAddress, transferIssue.id(), paymentIssue.id())).explicitGet()
    val serviceDAppScript = ContractScript(V5, serviceDApp()).explicitGet()
    val setClientDApp     = TxHelpers.setScript(clientDAppAcc, clientDAppScript)
    val setServiceDApp    = TxHelpers.setScript(serviceDAppAcc, serviceDAppScript)
    val payments          = List(Payment(paymentFromInvokerAmount, Waves))
    val invoke            = TxHelpers.invoke(clientDAppAcc.toAddress, Some("foo"), payments = payments)

    val genesisTxs = Seq(gTx1, gTx2, gTx3, setServiceDApp, setClientDApp, paymentIssue, transferIssue)
    assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) {
      case (diff, bc) =>
        diff.errorMessage(invoke.id()) shouldBe None

        bc.accountData(clientDAppAcc.toAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
        bc.accountData(serviceDAppAcc.toAddress, "bar") shouldBe Some(IntegerDataEntry("bar", 1))

        bc.balance(clientDAppAcc.toAddress, IssuedAsset(transferIssue.id())) shouldBe ENOUGH_AMT - transferAmount
        bc.balance(serviceDAppAcc.toAddress, IssuedAsset(transferIssue.id())) shouldBe 3
    }
  }
}
