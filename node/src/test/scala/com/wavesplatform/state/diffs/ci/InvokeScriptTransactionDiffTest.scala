package com.wavesplatform.state.diffs.ci

import cats.kernel.Monoid
import com.google.protobuf.ByteString
import com.wavesplatform.TestValues
import com.wavesplatform.account._
import com.wavesplatform.block.{Block, BlockHeader, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithDomain}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.values.{DApp => DAppType, _}
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.{Terms, TestCompiler}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.evaluator.FunctionIds.CREATE_LIST
import com.wavesplatform.lang.v1.evaluator.ScriptResultV3
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{FieldNames, WavesContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.{ContractLimits, compiler}
import com.wavesplatform.lang.{Global, ValidationError, utils}
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.settings.TestSettings
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionDiff
import com.wavesplatform.state.diffs.{ENOUGH_AMT, produce}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.script.trace.{AssetVerifierTrace, InvokeScriptTrace, TracedResult}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{Asset, _}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{EitherValues, Inside}

import scala.collection.immutable
import scala.util.{Random, Try}

class InvokeScriptTransactionDiffTest extends PropSpec with WithDomain with DBCacheSettings with EitherValues with Inside with MockFactory {
  import DomainPresets._

  private val allVersions = DirectiveDictionary[StdLibVersion].all
  private val lastVersion = allVersions.last

  private def testDomain(assertion: (StdLibVersion, Domain) => Unit): Unit =
    allVersions
      .filter(v => v >= V3 && v <= lastVersion)
      .foreach(v => withDomain(settingsFor(v))(assertion(v, _)))

  private def testDiffTraced(preconditions: Seq[Block], block: Block, from: StdLibVersion = V3, to: StdLibVersion = lastVersion)(
    assertion: ((StdLibVersion, TracedResult[ValidationError, Diff])) => Unit
  ): Unit =
    allVersions
      .filter(v => v >= from && v <= to)
      .foreach(v => assertDiffEiTraced(preconditions, block, settingsFor(v).blockchainSettings.functionalitySettings)(r => assertion((v, r))))

  private def testDiff(preconditions: Seq[Block], block: Block, from: StdLibVersion = V3, to: StdLibVersion = lastVersion)(
    assertion: Either[ValidationError, Diff] => Unit
  ): Unit =
    testDiffTraced(preconditions, block, from, to)(assertion.compose(_._2.resultE))

  private def testDiffAndState(preconditions: Seq[Block], block: Block, from: StdLibVersion = V3)(assertion: (Diff, Blockchain) => Unit): Unit =
    allVersions
      .filter(_ >= from)
      .foreach(v => assertDiffAndState(preconditions, block, settingsFor(v).blockchainSettings.functionalitySettings)(assertion))

  private val assetAllowed   = TestCompiler(V3).compileAsset("tx.fee > -1")
  private val assetUsingThis = TestCompiler(V3).compileAsset("this == this")
  private val assetBanned    = TestCompiler(V3).compileAsset("false")
  private val throwingAsset  = TestCompiler(V3).compileAsset("throw()")

  private val invoker        = TxHelpers.signer(0)
  private val dApp           = TxHelpers.signer(1)
  private val thirdAcc       = TxHelpers.signer(2)
  private val invokerAddress = invoker.toAddress
  private val dAppAddress    = dApp.toAddress
  private val thirdAddress   = thirdAcc.toAddress

  private val amount = 123

  private def dataContract(
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
          FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("argument").explicitGet(), REF("a"))),
          FUNCTION_CALL(
            Native(1100),
            List(
              FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender").explicitGet(), GETTER(GETTER(REF("i"), "caller"), "bytes"))),
              REF("nil")
            )
          )
        )

    DApp(
      DAppMeta(),
      List.empty,
      List(
        CallableFunction(
          CallableAnnotation("i"),
          Terms.FUNC(
            "f",
            List("a"),
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

  private def dAppWithTransfers(
      recipientAddress: Address = thirdAddress,
      recipientAmount: Long = amount,
      argName: String = "a",
      funcName: String = "f",
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
          CallableAnnotation("i"),
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

  private def defaultTransferContract(
      recipientAddress: AddressOrAlias,
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
            CONST_LONG(amount),
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
          CallableAnnotation("i"),
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

  private def writeSet(count: Int): DApp = {
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
           | func f(b: ByteVector) = {
           |    WriteSet([
           |    $DataEntries
           |        ])
           |}
           |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compileContractFromExpr(expr)
  }

  private def writeSetWithKeyLength(length: Int = 1, version: StdLibVersion = V3): DApp = {
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
           | func f(b: ByteVector) =
           |    $body
           |
        """.stripMargin
      Parser.parseContract(script).get.value
    }

    compileContractFromExpr(expr, version)
  }

  private def compileContractFromExpr(expr: Expressions.DAPP, stdLibVersion: StdLibVersion = V3): DApp = {
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

  private def simpleContract =
    TestCompiler(V3).compileContract(
      s"""
         |@Callable(i)
         |func funcForTesting(str: String, num: Int) =
         |  if (parseInt(str) == num) then throw() else throw()
         |
         |@Verifier(tx)
         |func verify() =
         |  false
       """.stripMargin
    )

  private def simplePreconditionsAndSetContract(
      invocationParamsCount: Int
  ): (List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction) = {
    val genesis     = TxHelpers.genesis(dAppAddress)
    val genesis2    = TxHelpers.genesis(invokerAddress)
    val setContract = TxHelpers.setScript(dApp, simpleContract)
    val ci          = TxHelpers.invoke(dAppAddress, Some("funcForTesting"), List.fill(invocationParamsCount)(FALSE), version = TxVersion.V1)
    (List(genesis, genesis2), setContract, ci)
  }

  private def preconditionsAndSetContract(
      contract: DApp,
      dApp: KeyPair = dApp,
      payment: Option[Payment] = None,
      sponsored: Option[SponsorFeeTransaction] = None,
      isCIDefaultFunc: Boolean = false,
      version: StdLibVersion = V3,
      txVersion: TxVersion = TxVersion.V1,
      selfSend: Boolean = false,
      fee: Long = TestValues.invokeFee
  ): (List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction) = {
    val genesis     = TxHelpers.genesis(dApp.toAddress)
    val genesis2    = TxHelpers.genesis(invokerAddress)
    val script      = ContractScript(version, contract).explicitGet()
    val setContract = TxHelpers.setScript(dApp, script)
    val ci = TxHelpers.invoke(
      dApp.toAddress,
      if (isCIDefaultFunc) None else Some("f"),
      List(CONST_BYTESTR(ByteStr.fromBytes(1, 2, 3)).explicitGet()),
      payment.toSeq,
      if (selfSend) dApp else invoker,
      sponsored.map(s => Sponsorship.fromWaves(fee, s.minSponsoredAssetFee.get)).getOrElse(fee),
      sponsored.map(_.asset).getOrElse(Waves),
      txVersion
    )
    (if (selfSend) List(genesis) else List(genesis, genesis2), setContract, ci)
  }

  private def preconditionsAndSetContractWithVerifier(verifier: DApp, senderBindingToContract: DApp): (
      List[GenesisTransaction],
      SetScriptTransaction,
      SetScriptTransaction,
      InvokeScriptTransaction,
      KeyPair,
      IssueTransaction,
      SponsorFeeTransaction
  ) = {
    val genesis     = TxHelpers.genesis(dAppAddress)
    val genesis2    = TxHelpers.genesis(invokerAddress)
    val setVerifier = TxHelpers.setScript(invoker, ContractScript(V3, verifier).explicitGet())
    val setContract = TxHelpers.setScript(dApp, ContractScript(V3, senderBindingToContract).explicitGet())
    val issue       = TxHelpers.issue(invoker)
    val asset       = IssuedAsset(issue.id())
    val sponsor     = TxHelpers.sponsor(asset)
    val ci = TxHelpers.invoke(
      dAppAddress,
      Some("f"),
      List(CONST_BYTESTR(ByteStr.fromBytes(1, 2, 3)).explicitGet())
    )
    (List(genesis, genesis2), setVerifier, setContract, ci, dApp, issue, sponsor)
  }

  private def preconditionsAndSetContractWithAlias(
      senderBindingToContract: DApp
  ): (List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, InvokeScriptTransaction, CreateAliasTransaction) = {
    val genesis     = TxHelpers.genesis(dAppAddress)
    val genesis2    = TxHelpers.genesis(invokerAddress)
    val dAppAlias   = Alias.create("alias").explicitGet()
    val fakeAlias   = Alias.create("fakealias").explicitGet()
    val aliasTx     = TxHelpers.createAlias("alias", dApp)
    val script      = ContractScript(V3, senderBindingToContract).explicitGet()
    val setContract = TxHelpers.setScript(dApp, script)
    val invokes = Seq(dAppAlias, fakeAlias).map(
      TxHelpers.invoke(_, Some("f"), List(CONST_BYTESTR(ByteStr.fromBytes(1, 2, 3)).explicitGet()), version = TxVersion.V1)
    )
    (List(genesis, genesis2), setContract, invokes(0), invokes(1), aliasTx)
  }

  property("invoking contract results contract's state") {
    val (genesis, setScript, ci) = preconditionsAndSetContract(dataContract())
    testDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci), Block.ProtoBlockVersion)) {
      case (blockDiff, newState) =>
        blockDiff.scriptsRun shouldBe 1
        newState.accountData(dAppAddress, "sender").get.value shouldBe ByteStr(ci.sender.toAddress.bytes)
        newState.accountData(dAppAddress, "argument").get.value shouldBe ci.funcCallOpt.get.args.head.asInstanceOf[CONST_BYTESTR].bs
        blockDiff.transactions(ci.id()).affected.contains(setScript.sender.toAddress) shouldBe true
    }
  }

  property("can't use empty keys in v2") {
    val (genesis, setScript, ci) = preconditionsAndSetContract(dataContract(emptyData = true), txVersion = TxVersion.V1)
    testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci))) {
      _.explicitGet()
    }

    val (genesis2, setScript2, ci2) = preconditionsAndSetContract(dataContract(emptyData = true), txVersion = TxVersion.V2)
    testDiff(Seq(TestBlock.create(genesis2 ++ Seq(setScript2))), TestBlock.create(Seq(ci2)), from = V4) {
      _ should produce("Empty keys aren't allowed")
    }
  }

  property("invoking payment contract results in accounts state") {
    val (genesis, setScript, ci) = preconditionsAndSetContract(dAppWithTransfers())

    testDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci), Block.ProtoBlockVersion)) {
      case (blockDiff, newState) =>
        blockDiff.scriptsRun shouldBe 1
        newState.balance(thirdAddress, Waves) shouldBe amount
        blockDiff.transactions should contain key ci.id()
    }
  }

  property("invoking default func payment contract results in accounts state") {
    val (genesis, setScript, ci) = preconditionsAndSetContract(defaultTransferContract(thirdAddress), isCIDefaultFunc = true)

    testDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci), Block.ProtoBlockVersion)) {
      case (blockDiff, newState) =>
        blockDiff.scriptsRun shouldBe 1
        newState.balance(thirdAddress, Waves) shouldBe amount
        blockDiff.transactions should contain key ci.id()
    }
  }

  property("invoking default func payment to alias contract results in accounts state") {
    val alias                    = Alias.create("alias").explicitGet()
    val createAlias              = TxHelpers.createAlias("alias", thirdAcc)
    val (genesis, setScript, ci) = preconditionsAndSetContract(defaultTransferContract(alias), isCIDefaultFunc = true)

    testDiffAndState(
      Seq(TestBlock.create(genesis ++ Seq(TxHelpers.genesis(thirdAddress), setScript, createAlias))),
      TestBlock.create(Seq(ci), Block.ProtoBlockVersion),
      from = V4
    ) {
      case (blockDiff, _) =>
        blockDiff.scriptsRun shouldBe 1
        blockDiff.portfolios(thirdAddress) shouldBe Portfolio.waves(amount)
        blockDiff.transactions should contain key ci.id()
    }
  }

  property("disallow ScriptTransfer by alias before RIDE V4 activation") {
    val alias                    = Alias.create("alias").explicitGet()
    val createAlias              = TxHelpers.createAlias("alias", thirdAcc)
    val (genesis, setScript, ci) = preconditionsAndSetContract(defaultTransferContract(alias), isCIDefaultFunc = true)

    testDiff(
      Seq(TestBlock.create(genesis ++ Seq(TxHelpers.genesis(thirdAddress), setScript, createAlias))),
      TestBlock.create(Seq(ci), Block.ProtoBlockVersion),
      to = V3
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
      val (genesis, setScript, ci) = preconditionsAndSetContract(dAppWithTransfers())
      val proofs                   = Proofs(List.fill(proofCount)(ByteStr.fromBytes(1, 1)))

      testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci.copy(proofs = proofs)), Block.ProtoBlockVersion)) {
        _ should produce("Transactions from non-scripted accounts must have exactly 1 proof")
      }
    }
  }

  property("suitable verifier error message on incorrect proof") {
    val (genesis, setScript, ci) = preconditionsAndSetContract(dAppWithTransfers())
    val proofs                   = Proofs(List(ByteStr.fromBytes(1, 1)))

    testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci.copy(proofs = proofs)), Block.ProtoBlockVersion)) {
      _ should produce("Proof doesn't validate as signature")
    }
  }

  property("invoke script by alias") {
    val (genesis, setScript, ci, fakeCi, createAlias) = preconditionsAndSetContractWithAlias(dAppWithTransfers())

    testDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript, createAlias))), TestBlock.create(Seq(ci), Block.ProtoBlockVersion)) {
      case (blockDiff, newState) =>
        blockDiff.scriptsRun shouldBe 1
        newState.balance(thirdAddress, Waves) shouldBe amount
        blockDiff.transactions should contain key ci.id()
    }
    testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(fakeCi), Block.ProtoBlockVersion)) {
      _ should produce("does not exist")
    }
  }

  property("can't make more than 10 payments") {
    val (genesis, setScript, ci) = preconditionsAndSetContract(dAppWithTransfers(assets = List.fill(11)(Waves)))

    testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci))) {
      _ should produce("Actions count limit is exceeded")
    }
  }

  property("invoking contract receive payment") {
    testDomain { (version, d) =>
      val issue = TxHelpers.issue(script = Some(assetAllowed))
      val asset = IssuedAsset(issue.id())
      val (genesis, setScript, ci) = preconditionsAndSetContract(
        dAppWithTransfers(version = version),
        payment = Some(Payment(1, asset)),
        version = version,
        fee = TestValues.invokeFee(1)
      )
      d.appendBlock(genesis: _*)
      d.appendBlock(issue, setScript)
      d.appendBlock(ci)
      d.liquidDiff.scriptsRun shouldBe 2
      d.blockchain.balance(thirdAddress, Waves) shouldBe amount
      d.blockchain.balance(invokerAddress, asset) shouldBe (issue.quantity - 1)
      d.blockchain.balance(dAppAddress, asset) shouldBe 1
    }
  }

  property("successfully invoked contract trace should contain both attached and transferring asset script info") {
    val transferringAsset = TxHelpers.issue(script = Some(assetAllowed))
    val attachedAsset     = TxHelpers.issue(name = "test2", script = Some(assetAllowed))

    val (genesis, setScript, ci) = preconditionsAndSetContract(
      dAppWithTransfers(dAppAddress, assets = List(IssuedAsset(transferringAsset.id()))),
      payment = Some(Payment(1, IssuedAsset(attachedAsset.id()))),
      fee = TestValues.invokeFee(2)
    )
    testDiffTraced(
      Seq(TestBlock.create(genesis ++ Seq(transferringAsset, attachedAsset, setScript))),
      TestBlock.create(Seq(ci))
    ) {
      case (_, blockDiffEi) =>
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
    val asset = TxHelpers.issue(script = Some(assetBanned))
    val (genesis, setScript, ci) = preconditionsAndSetContract(
      dAppWithTransfers(),
      payment = Some(Payment(1, IssuedAsset(asset.id()))),
      fee = TestValues.invokeFee(1)
    )
    testDiffTraced(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci))) {
      case (version, blockDiffEi) =>
        if (version == V3)
          blockDiffEi.resultE should produce("TransactionNotAllowedByScript")
        else
          blockDiffEi.resultE.explicitGet().errorMessage(ci.id()).get.text should include("Transaction is not allowed by script of the asset")
        inside(blockDiffEi.trace) {
          case List(_, AssetVerifierTrace(assetId, Some(tne: TransactionNotAllowedByScript), _)) =>
            assetId shouldBe asset.id()
            tne.isAssetScript shouldBe true
        }
    }
  }

  property("invoking contract make payment by asset") {
    val issue = TxHelpers.issue(dApp, script = Some(assetAllowed))
    val asset = IssuedAsset(issue.id())

    val (genesis, setScript, ci) = preconditionsAndSetContract(
      dAppWithTransfers(assets = List(asset)),
      fee = TestValues.invokeFee(1)
    )

    testDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(issue, ci), Block.ProtoBlockVersion)) {
      case (blockDiff, newState) =>
        blockDiff.scriptsRun shouldBe 3
        newState.balance(dAppAddress, asset) shouldBe (issue.quantity - amount)
        newState.balance(thirdAddress, asset) shouldBe amount
    }
  }

  property("invoking contract disable by payment smart asset") {
    val issue = TxHelpers.issue(script = Some(assetBanned))
    val (genesis, setScript, ci) =
      preconditionsAndSetContract(dAppWithTransfers(assets = List(IssuedAsset(issue.id()))), fee = TestValues.invokeFee(1))

    testDiffTraced(Seq(TestBlock.create(genesis ++ Seq(issue, setScript))), TestBlock.create(Seq(ci))) { blockDiffEi =>
      blockDiffEi._2.resultE should produce("Transaction is not allowed by script")
    }
  }

  property("invoking contract disable by one of payment smart asset with trace") {
    val issue1 = TxHelpers.issue(issuer = dApp, script = Some(assetAllowed))
    val issue2 = TxHelpers.issue(issuer = dApp, name = "test2", script = Some(assetBanned))

    val contract                 = dAppWithTransfers(assets = List(IssuedAsset(issue1.id()), IssuedAsset(issue2.id())))
    val (genesis, setScript, ci) = preconditionsAndSetContract(contract, fee = TestValues.invokeFee(2))

    testDiffTraced(Seq(TestBlock.create(genesis ++ Seq(issue1, issue2, setScript))), TestBlock.create(Seq(ci))) {
      case (version, blockDiffEi) =>
        if (version == V3)
          blockDiffEi.resultE should produce("Transaction is not allowed by script")
        else
          blockDiffEi.resultE.explicitGet().errorMessage(ci.id()).get.text should include("Transaction is not allowed by script")
        inside(blockDiffEi.trace) {
          case List(
              InvokeScriptTrace(_, `dAppAddress`, functionCall, Right(ScriptResultV3(_, transfers, _)), _, _),
              AssetVerifierTrace(allowedAssetId, None, _),
              AssetVerifierTrace(bannedAssetId, Some(_: FailedTransactionError), _)
              ) =>
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

    val (genesis, setScript, ci) = preconditionsAndSetContract(
      dAppWithTransfers(assets = List(IssuedAsset(transferringAsset.id()))),
      payment = Some(Payment(1, IssuedAsset(attachedAsset.id()))),
      fee = TestValues.invokeFee(1)
    )

    testDiffTraced(
      Seq(TestBlock.create(genesis ++ Seq(transferringAsset, attachedAsset, setScript))),
      TestBlock.create(Seq(ci))
    ) {
      case (version, blockDiffEi) =>
        if (version == V3)
          blockDiffEi.resultE should produce(s"Transaction is not allowed by script of the asset ${transferringAsset.id()}")
        else
          blockDiffEi.resultE.explicitGet().errorMessage(ci.id()).get.text should include(
            s"Transaction is not allowed by script of the asset ${transferringAsset.id()}"
          )
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
    val asset                    = TxHelpers.issue(dApp)
    val contract                 = dAppWithTransfers(recipientAmount = -1, assets = List(IssuedAsset(asset.id())))
    val (genesis, setScript, ci) = preconditionsAndSetContract(contract)

    testDiff(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci))) { blockDiffEi =>
      blockDiffEi should produce("Negative amount")
    }
  }

  property("payment should be positive") {
    Try(TxHelpers.invoke(dAppAddress, payments = Seq(Payment(-1, Waves)))).toEither should produce("NonPositiveAmount")
  }

  property("smart asset payment require extra fee only before V5 activation") {
    val issue                    = TxHelpers.issue(dApp, script = Some(assetAllowed))
    val contract                 = dAppWithTransfers(assets = List(IssuedAsset(issue.id())))
    val (genesis, setScript, ci) = preconditionsAndSetContract(contract)
    testDiff(Seq(TestBlock.create(genesis ++ Seq(issue, setScript))), TestBlock.create(Seq(ci)), to = V4)(
      _ should produce("does not exceed minimal value")
    )
    testDiff(Seq(TestBlock.create(genesis ++ Seq(issue, setScript))), TestBlock.create(Seq(ci)), from = V5)(
      _.explicitGet().errorMessage(ci.id()) shouldBe None
    )
  }

  property("contract with payment of smart asset require extra fee only before V5 activation") {
    val issue                    = TxHelpers.issue(script = Some(assetAllowed))
    val payment                  = Payment(1, IssuedAsset(issue.id()))
    val (genesis, setScript, ci) = preconditionsAndSetContract(dAppWithTransfers(), payment = Some(payment))
    testDiff(Seq(TestBlock.create(genesis ++ Seq(issue, setScript))), TestBlock.create(Seq(ci)), to = V4)(
      _ should produce("does not exceed minimal value")
    )
    testDiff(Seq(TestBlock.create(genesis ++ Seq(issue, setScript))), TestBlock.create(Seq(ci)), from = V5)(
      _.explicitGet().errorMessage(ci.id()) shouldBe None
    )
  }

  property("can't overflow payment + fee") {
    val payment                  = Some(Payment(ENOUGH_AMT, Waves))
    val (genesis, setScript, ci) = preconditionsAndSetContract(dAppWithTransfers(), payment = payment)
    testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci))) {
      _ should produce("Attempt to transfer unavailable funds")
    }
  }

  property("can't overflow sum of payment in contract") {
    val (genesis, setScript, ci) = preconditionsAndSetContract(
      dAppWithTransfers(recipientAmount = Long.MaxValue / 2 + 2, assets = List.fill(4)(Waves)),
      payment = Some(Payment(1, Waves))
    )
    testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)))(
      _ should produce("Attempt to transfer unavailable funds")
    )
  }

  property("invoking contract with sponsored fee") {
    val sponsorIssue             = TxHelpers.issue(dApp, amount = ENOUGH_AMT)
    val sponsorAsset             = IssuedAsset(sponsorIssue.id())
    val sponsor                  = TxHelpers.sponsor(sponsorAsset, sender = dApp)
    val (genesis, setScript, ci) = preconditionsAndSetContract(dAppWithTransfers(), sponsored = Some(sponsor))
    val t                        = TxHelpers.transfer(dApp, invokerAddress, sponsorIssue.quantity / 2, sponsorAsset)

    testDiffAndState(
      Seq(TestBlock.create(genesis ++ Seq(sponsorIssue, t, sponsor, setScript))),
      TestBlock.create(Seq(ci), Block.ProtoBlockVersion)
    ) {
      case (blockDiff, newState) =>
        blockDiff.scriptsRun shouldBe 1
        blockDiff.errorMessage(ci.id()) shouldBe None
        newState.balance(thirdAddress, Waves) shouldBe amount
        newState.balance(ci.sender.toAddress, sponsorAsset) shouldBe (sponsorIssue.quantity / 2 - ci.fee)
        newState.balance(dAppAddress, sponsorAsset) shouldBe (sponsorIssue.quantity - sponsorIssue.quantity / 2 + ci.fee)
    }
  }

  property("argument passed to callable function has wrong type") {
    val (genesis, setScript, ci) = simplePreconditionsAndSetContract(invocationParamsCount = 2)
    testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci))) {
      _ should produce("Can't apply (CONST_BOOLEAN) to 'parseInt(str: String)'")
    }
  }

  property("can't write more than 100 entries") {
    val (genesis, setScript, ci) = preconditionsAndSetContract(writeSet(ContractLimits.MaxWriteSetSize(V4) + 1))
    testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci))) {
      _ should produce("Stored data count limit is exceeded")
    }
  }

  property("can write 100 entries") {
    val (genesis, setScript, ci) = preconditionsAndSetContract(writeSet(ContractLimits.MaxWriteSetSize(V4)))
    testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci))) {
      _.explicitGet()
    }
  }

  property("can't write entry with key size greater than limit") {
    testDomain { (version, d) =>
      val (genesis, setScript, ci) = preconditionsAndSetContract(
        writeSetWithKeyLength(ContractLimits.MaxKeySizeInBytesByVersion(version) + 1, version),
        version = version
      )
      d.appendBlock(genesis: _*)
      d.appendBlock(setScript)
      val error =
        s"Data entry key size = ${ContractLimits.MaxKeySizeInBytesByVersion(version) + 1} bytes " +
          s"must be less than ${ContractLimits.MaxKeySizeInBytesByVersion(version)}"
      if (version == V3)
        d.appendBlockE(ci) should produce(error)
      else {
        d.appendBlock(ci)
        d.liquidDiff.errorMessage(ci.id()).get.text shouldBe error
      }
    }
  }

  property("can write entry with key size equals limit") {
    testDomain { (version, d) =>
      val (genesis, setScript, ci) =
        preconditionsAndSetContract(
          writeSetWithKeyLength(ContractLimits.MaxKeySizeInBytesByVersion(version), version),
          version = version
        )
      d.appendBlock(genesis: _*)
      d.appendBlock(setScript)
      d.appendBlock(ci)
      d.liquidDiff.errorMessage(ci.id()) shouldBe None
    }
  }

  property("can't write entry with empty key from V4") {
    testDomain { (version, d) =>
      val (genesis, setScript, ci) = preconditionsAndSetContract(
        writeSetWithKeyLength(length = 0, version = version),
        version = version
      )
      d.appendBlock(genesis: _*)
      d.appendBlock(setScript, ci)
      if (version == V3)
        d.liquidDiff.errorMessage(ci.id()) shouldBe None
      else
        d.liquidDiff.errorMessage(ci.id()).get.text shouldBe "Data entry key should not be empty"
    }
  }

  property("Function call args count should be equal @Callable func one") {
    Seq(0, 3)
      .foreach { invocationArgsCount =>
        val (genesis, setScript, ci) = simplePreconditionsAndSetContract(invocationArgsCount)
        testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci))) {
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
        preconditionsAndSetContractWithVerifier(multiSigCheckDApp(proofsCount), writeSetWithKeyLength())

      val proof         = ci.proofs
      val multiSigProof = ci.proofs.copy(proofs = List.fill(proofsCount)(proof.proofs.head))
      val multiSigCi    = ci.copy(1.toByte, proofs = multiSigProof)

      testDiff(
        Seq(TestBlock.create(genesis ++ Seq(setVerifier, setContract))),
        TestBlock.create(Seq(multiSigCi))
      )(_)
    }
  }

  property("Default function invocation should produce error if contract default function has arguments") {
    val contract                 = dAppWithTransfers(funcName = "default", assets = List(Waves))
    val (genesis, setScript, ci) = preconditionsAndSetContract(contract, isCIDefaultFunc = true)

    testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci))) {
      _ should produce(s"takes 1 args but 0 were(was) given")
    }
  }

  property("Default function invocation should produce error if contract does't have default function") {
    val contract                 = dAppWithTransfers(funcName = "other", assets = List(Waves))
    val (genesis, setScript, ci) = preconditionsAndSetContract(contract, isCIDefaultFunc = true)

    testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci))) {
      _ should produce("Cannot find callable function `default`, address = ")
    }
  }

  property("self-payment and self-transfer V3") {
    val (genesis, setScript, ci) = preconditionsAndSetContract(
      dAppWithTransfers(),
      dApp = invoker,
      payment = Some(Payment(1, Waves)),
      fee = TestValues.invokeFee(1)
    )
    testDiff(Seq(TestBlock.create(Seq(genesis.head, setScript))), TestBlock.create(Seq(ci))) {
      _.explicitGet()
    }
  }

  property("self-payment V4") {
    val (genesis, setScript, ci) = preconditionsAndSetContract(
      dAppWithTransfers(version = V4),
      payment = Some(Payment(1, Waves)),
      dApp = invoker,
      version = V4,
      fee = TestValues.invokeFee(1)
    )
    testDiff(Seq(TestBlock.create(Seq(genesis.head, setScript))), TestBlock.create(Seq(ci)), from = V4) {
      _ should produce("DApp self-payment is forbidden since V4")
    }
  }

  property("self-transfer V4") {
    val (genesis, setScript, ci) = preconditionsAndSetContract(
      dAppWithTransfers(recipientAddress = invokerAddress, assets = List(Waves), version = V4),
      dApp = invoker,
      version = V4,
      fee = TestValues.invokeFee(1)
    )
    testDiff(Seq(TestBlock.create(Seq(genesis.head, setScript))), TestBlock.create(Seq(ci)), from = V4) {
      _ should produce("DApp self-transfer is forbidden since V4")
    }
  }

  property("transferring asset this value") {
    val issue                    = TxHelpers.issue(dApp, script = Some(assetUsingThis))
    val contract                 = dAppWithTransfers(assets = List(IssuedAsset(issue.id())))
    val (genesis, setScript, ci) = preconditionsAndSetContract(contract, fee = TestValues.invokeFee(1))

    testDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(issue, ci), Block.ProtoBlockVersion), from = V4) {
      case (blockDiff, newState) =>
        blockDiff.scriptsRun shouldBe 3
        newState.balance(dAppAddress, IssuedAsset(issue.id())) shouldBe (issue.quantity - amount)
        newState.balance(thirdAddress, IssuedAsset(issue.id())) shouldBe amount
    }
  }

  private val issueContract: DApp = {
    val expr = {
      val script =
        s"""
           |{-# STDLIB_VERSION 4 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-#SCRIPT_TYPE ACCOUNT#-}
           |
           |@Callable(i)
           |func f() = [Issue("InvokeAsset", "InvokeDesc", 100, 0, true, unit, 0)]
           |""".stripMargin
      Parser.parseContract(script).get.value
    }

    compileContractFromExpr(expr, V4)
  }

  private val throwContract: DApp = {
    val expr = {
      val script =
        s"""
           |{-# STDLIB_VERSION 4 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-#SCRIPT_TYPE ACCOUNT#-}
           |
           |@Callable(i)
           |func f() = {
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
    val script = ContractScript(V4, issueContract).explicitGet()
    val invoke = TxHelpers.invoke(dAppAddress, Some("f"), fee = TestValues.invokeFee(issues = 1))

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
      .expects(dAppAddress)
      .returning(Some(AccountScriptInfo(dApp.publicKey, script, 10L, Map(1 -> Map("f" -> 10L)))))
      .anyNumberOfTimes()
    (blockchain.accountScript _).expects(invoke.sender.toAddress).returning(None).anyNumberOfTimes()
    (blockchain.hasAccountScript _).expects(invoke.sender.toAddress).returning(false).anyNumberOfTimes()
    (blockchain.balance _).expects(*, Waves).returning(ENOUGH_AMT)
    (blockchain.leaseBalance _).expects(*).returning(LeaseBalance.empty)
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
            reissuable = false,
            BigInt(1),
            Height(1),
            None,
            0L,
            nft = false
          )
        )
      )
      .anyNumberOfTimes()
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
    val genesis1Tx  = TxHelpers.genesis(dAppAddress)
    val genesis2Tx  = TxHelpers.genesis(invokerAddress)
    val assetTx     = TxHelpers.issue(dApp)
    val contract    = reissueContract("f", assetTx.id())
    val script      = ContractScript(V4, contract).explicitGet()
    val setScriptTx = TxHelpers.setScript(dApp, script)

    val invoke = TxHelpers.invoke(dAppAddress, Some("f"))
    testDiff(
      Seq(TestBlock.create(Seq(genesis1Tx, genesis2Tx, setScriptTx, assetTx))),
      TestBlock.create(Seq(invoke), Block.ProtoBlockVersion),
      from = V4
    ) {
      _ should produce("Asset is not reissuable")
    }
  }

  private val transferIssueContract: DApp = {
    val expr = {
      val script =
        s"""
           |{-# STDLIB_VERSION 4 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-#SCRIPT_TYPE ACCOUNT#-}
           |
           |@Callable(i)
           |func f() = {
           | let v = Issue("InvokeAsset", "InvokeDesc", 100, 0, true, unit, 0)
           | [v, ScriptTransfer(i.caller, 1, v.calculateAssetId())]
           |}
           |""".stripMargin
      Parser.parseContract(script).get.value
    }
    compileContractFromExpr(expr, V4)
  }

  property("issued asset can be transferred") {
    val genesis1Tx  = TxHelpers.genesis(dAppAddress)
    val genesis2Tx  = TxHelpers.genesis(invokerAddress)
    val script      = ContractScript(V4, transferIssueContract).explicitGet()
    val setScriptTx = TxHelpers.setScript(dApp, script)

    val invoke = TxHelpers.invoke(dAppAddress, Some("f"))

    testDiff(Seq(TestBlock.create(Seq(genesis1Tx, genesis2Tx, setScriptTx))), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), from = V4) {
      _.explicitGet()
    }
  }

  private val transferNonIssueContract: DApp = {
    val expr = {
      val script =
        s"""
           |{-# STDLIB_VERSION 4 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-#SCRIPT_TYPE ACCOUNT#-}
           |
           |@Callable(i)
           |func f() = {
           | let v = Issue("InvokeAsset", "InvokeDesc", 100, 0, true, unit, 0)
           | [ScriptTransfer(i.caller, 1, v.calculateAssetId())]
           |}
           |""".stripMargin
      Parser.parseContract(script).get.value
    }

    compileContractFromExpr(expr, V4)
  }

  property("nonissued asset can't be transferred") {
    val genesis1Tx  = TxHelpers.genesis(dAppAddress)
    val genesis2Tx  = TxHelpers.genesis(invokerAddress)
    val script      = ContractScript(V4, transferNonIssueContract).explicitGet()
    val setScriptTx = TxHelpers.setScript(dApp, script)

    val invoke = TxHelpers.invoke(dAppAddress, Some("f"))

    testDiff(
      Seq(TestBlock.create(Seq(genesis1Tx, genesis2Tx, setScriptTx))),
      TestBlock.create(Seq(invoke), Block.ProtoBlockVersion),
      from = V4,
      to = V4
    ) {
      _ should produce("negative asset balance")
    }
    testDiffAndState(
      Seq(TestBlock.create(Seq(genesis1Tx, genesis2Tx, setScriptTx))),
      TestBlock.create(Seq(invoke), Block.ProtoBlockVersion),
      from = V5
    ) { (diff, _) =>
      diff.errorMessage(invoke.id()).get.text should include("is not found on the blockchain")
    }
  }

  private val doubleIssueContract: DApp = {
    val expr = {
      val script =
        s"""
           |@Callable(i)
           |func f() = {
           | let v = Issue("InvokeAsset", "InvokeDesc", 100, 0, true, unit, 0)
           | [v, v]
           |}
           |""".stripMargin
      Parser.parseContract(script).get.value
    }

    compileContractFromExpr(expr, V4)
  }

  property("duplicate issuing asset should produce diff error") {
    val genesis1Tx  = TxHelpers.genesis(dAppAddress)
    val genesis2Tx  = TxHelpers.genesis(invokerAddress)
    val script      = ContractScript(V4, doubleIssueContract).explicitGet()
    val setScriptTx = TxHelpers.setScript(dApp, script)
    val invoke      = TxHelpers.invoke(dAppAddress, Some("f"), fee = TestValues.invokeFee(issues = 2))

    testDiff(Seq(TestBlock.create(Seq(genesis1Tx, genesis2Tx, setScriptTx))), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), from = V4) {
      inside(_) {
        case Right(diff) => diff.scriptResults(invoke.id()).error.get.text should include("is already issued")
      }
    }
  }

  property("correctly counts sponsored fee") {
    val issue        = TxHelpers.issue(dApp, amount = ENOUGH_AMT)
    val sponsorAsset = IssuedAsset(issue.id())
    val sponsor      = TxHelpers.sponsor(sponsorAsset, sender = dApp)
    val (genesis, setScript, invoke) =
      preconditionsAndSetContract(writeSet(1), sponsored = Some(sponsor), selfSend = true, fee = TestValues.invokeFee(1))

    testDiff(Seq(TestBlock.create(genesis ++ Seq(issue, sponsor, setScript))), TestBlock.create(Seq(invoke))) { diff =>
      invoke.feeAssetId shouldBe sponsorAsset
      invoke.dAppAddressOrAlias shouldBe invoke.sender.toAddress
      diff.explicitGet().portfolios(invoke.sender.toAddress).balanceOf(sponsorAsset) shouldBe 0L
    }
  }

  property(s"accepts failed transactions after ${BlockchainFeatures.BlockV5} activation") {
    val sponsorIssue = TxHelpers.issue(thirdAcc, ENOUGH_AMT)
    val sponsorAsset = IssuedAsset(sponsorIssue.id())
    val sponsorTx    = TxHelpers.sponsor(sponsorAsset, sender = thirdAcc)

    val issueTx = TxHelpers.issue(dApp, script = Some(throwingAsset))

    val feeInWaves = TestValues.invokeFee
    val feeInAsset = Sponsorship.fromWaves(feeInWaves, sponsorTx.minSponsoredAssetFee.get)

    Seq(
      (feeInWaves, Waves, issueContract, List.empty[EXPR]),        // insufficient fee
      (feeInAsset, sponsorAsset, issueContract, List.empty[EXPR]), // insufficient fee
      (feeInWaves, Waves, throwContract, List.empty[EXPR]),        // DApp script execution
      (feeInAsset, sponsorAsset, throwContract, List.empty[EXPR]), // DApp script execution
      {                                                            // smart asset script execution
        val contract = dAppWithTransfers(assets = List(issueTx.asset), version = V4)
        val fee      = TestValues.invokeFee(1)
        (fee, Waves, contract, List(CONST_BYTESTR(ByteStr.fromBytes(1, 2, 3)).explicitGet()))
      }
    ).foreach {
      case (fee, feeAsset, contract, args) =>
        val g1Tx = TxHelpers.genesis(dAppAddress)
        val g2Tx = TxHelpers.genesis(invokerAddress)
        val g3Tx = TxHelpers.genesis(thirdAddress)

        val tTx = TxHelpers.transfer(thirdAcc, invokerAddress, sponsorIssue.quantity, sponsorAsset)

        val script = ContractScript(V4, contract).explicitGet()
        val ssTx   = TxHelpers.setScript(dApp, script)
        val invoke = TxHelpers.invoke(dAppAddress, Some("f"), args, fee = fee, feeAssetId = feeAsset)

        testDiffAndState(
          Seq(TestBlock.create(Seq(g1Tx, g2Tx, g3Tx, sponsorIssue, issueTx, sponsorTx, tTx, ssTx))),
          TestBlock.create(Seq(invoke), Block.ProtoBlockVersion),
          from = V4
        ) {
          case (diff, state) =>
            diff.scriptsRun shouldBe 0
            diff.portfolios(invoke.sender.toAddress).balanceOf(invoke.feeAssetId)
            state.balance(invoke.sender.toAddress, invoke.feeAssetId) shouldBe invoke.feeAssetId.fold(g2Tx.amount)(_ => sponsorIssue.quantity) - invoke.fee
            state.transactionInfo(invoke.id()).map(r => r._2 -> r._1.succeeded) shouldBe Some((invoke, false))
        }
    }
  }

  property(
    s"rejects withdrawal of fee from the funds received as a result of the script call execution after ${BlockchainFeatures.BlockV5} activation"
  ) {
    val g1Tx           = TxHelpers.genesis(dAppAddress)
    val g2Tx           = TxHelpers.genesis(thirdAddress)
    val iTx            = TxHelpers.issue(thirdAcc)
    val sponsoredAsset = IssuedAsset(iTx.assetId)
    val sTx            = TxHelpers.sponsor(sponsoredAsset, sender = thirdAcc)
    val tTx            = TxHelpers.transfer(thirdAcc, dAppAddress, iTx.quantity / 1)

    val wavesFee     = TestValues.invokeFee(1)
    val sponsoredFee = Sponsorship.fromWaves(wavesFee, sTx.minSponsoredAssetFee.get)

    Seq((Waves, wavesFee), (sponsoredAsset, sponsoredFee))
      .foreach {
        case (feeAsset, fee) =>
          val contract = dAppWithTransfers(assets = List(feeAsset), version = V4)
          val script   = ContractScript(V4, contract).explicitGet()
          val ssTx     = TxHelpers.setScript(dApp, script)
          val invoke = TxHelpers.invoke(
            dAppAddress,
            Some("f"),
            args = List(CONST_BYTESTR(ByteStr.fromBytes(1, 2, 3)).explicitGet()),
            fee = fee,
            feeAssetId = feeAsset
          )

          testDiff(Seq(TestBlock.create(Seq(g1Tx, g2Tx, iTx, sTx, tTx, ssTx))), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), from = V4) {
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

    val gTx1             = TxHelpers.genesis(dAppAddress)
    val gTx2             = TxHelpers.genesis(invokerAddress)
    val (assetScript, _) = ScriptCompiler.compile("false", ScriptEstimatorV3(fixOverflow = true)).explicitGet()
    val iTx              = TxHelpers.issue(dApp, script = Some(assetScript))

    val script = ContractScript(V4, contract(iTx.assetId.toString)).explicitGet()
    val ssTx   = TxHelpers.setScript(dApp, script)

    Seq("throw", "insufficient fee", "negative amount", "overflow amount", "self payment", "max actions", "invalid data entries", "ok").foreach {
      arg =>
        val invoke = TxHelpers.invoke(dAppAddress, Some("sameComplexity"), args = List(CONST_STRING(arg).explicitGet()))
        testDiffAndState(Seq(TestBlock.create(Seq(gTx1, gTx2, ssTx, iTx))), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), from = V4) {
          case (diff, _) =>
            if (arg == "ok")
              diff.errorMessage(invoke.id()) shouldBe empty
            else
              diff.errorMessage(invoke.id()) shouldBe defined
        }
    }
  }

  property("counts complexity correctly for failed transactions (asset script fails)") {
    val trueScript  = TestCompiler(V4).compileExpression("true")
    val falseScript = TestCompiler(V4).compileExpression("false")

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

    val gTx1 = TxHelpers.genesis(dAppAddress)
    val gTx2 = TxHelpers.genesis(invokerAddress)

    Seq(true, false)
      .foreach { isAccountScripted =>
        val invokerScriptTx =
          if (isAccountScripted) Seq(TxHelpers.setScript(invoker, trueScript))
          else Seq.empty

        val failAsset    = Random.nextInt(6) + 1
        val assetScripts = (1 to 6).map(i => if (i == failAsset) falseScript else trueScript)
        val iTxs = (1 to 6).map { _ =>
          TxHelpers.issue(dApp, ENOUGH_AMT, script = Some(trueScript))
        }
        val tTxs = iTxs.takeRight(3).map { tx =>
          TxHelpers.transfer(dApp, invokerAddress, ENOUGH_AMT / 2, IssuedAsset(tx.assetId))
        }
        val saTxs = assetScripts.zipWithIndex.map {
          case (sc, i) => TxHelpers.setAssetScript(dApp, IssuedAsset(iTxs(i).id()), sc)
        }
        val script = ContractScript(V4, contract(iTxs.take(4).map(_.assetId.toString))).explicitGet()
        val ssTx   = TxHelpers.setScript(dApp, script)

        val payments = iTxs.takeRight(2).map(tx => Payment(10, IssuedAsset(tx.assetId)))
        val invoke   = TxHelpers.invoke(dAppAddress, Some("foo"), payments = payments)

        val genesisTxs = Seq(gTx1, gTx2) ++ invokerScriptTx ++ iTxs ++ tTxs ++ saTxs :+ ssTx

        testDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), from = V4) {
          case (diff, _) =>
            diff.errorMessage(invoke.id()) shouldBe defined
            diff.scriptsComplexity should be > 0L
        }
      }
  }

  property("scriptHash") {
    val script = TestCompiler(V5).compileContract(
      s"""
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
       """.stripMargin
    )

    val gTx1 = TxHelpers.genesis(dAppAddress)
    val gTx2 = TxHelpers.genesis(invokerAddress)

    val alias = TxHelpers.createAlias("alias", dApp)
    val ssTx  = TxHelpers.setScript(dApp, script)

    val payments = List(Payment(10, Waves))
    val invoke   = TxHelpers.invoke(dAppAddress, Some("foo"), payments = payments)

    testDiffAndState(Seq(TestBlock.create(Seq(gTx1, gTx2, alias, ssTx))), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), from = V5) {
      case (diff, bc) =>
        diff.errorMessage(invoke.id()) shouldBe None
        val hash = ByteStr(com.wavesplatform.lang.Global.blake2b256(script.bytes().arr))
        bc.accountData(dAppAddress, "hash1").get.value shouldBe hash
        bc.accountData(dAppAddress, "hash2").get.value shouldBe hash
    }
  }
}
