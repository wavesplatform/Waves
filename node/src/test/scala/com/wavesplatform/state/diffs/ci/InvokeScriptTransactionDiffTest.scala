package com.wavesplatform.state.diffs.ci

import scala.collection.immutable
import com.google.protobuf.ByteString
import com.wavesplatform.TestValues
import com.wavesplatform.TestValues.invokeFee
import com.wavesplatform.account.*
import com.wavesplatform.block.{Block, BlockHeader, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithDomain}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.{Terms, TestCompiler}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.evaluator.FunctionIds.CREATE_LIST
import com.wavesplatform.lang.v1.evaluator.ScriptResultV3
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.FieldNames
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.settings.TestSettings
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.FeeValidation.FeeConstants
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionDiff
import com.wavesplatform.state.diffs.{ENOUGH_AMT, FeeValidation, produceRejectOrFailedDiff}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.*
import com.wavesplatform.transaction.assets.*
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.script.trace.{AssetVerifierTrace, InvokeScriptTrace, TracedResult}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{Asset, utils as _, *}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{EitherValues, Inside}

import scala.util.{Random, Try}

class InvokeScriptTransactionDiffTest extends PropSpec with WithDomain with DBCacheSettings with EitherValues with Inside with MockFactory {
  import DomainPresets.*

  private val allVersions = DirectiveDictionary[StdLibVersion].all
  private val lastVersion = allVersions.last

  private def testDomain(assertion: (StdLibVersion, Domain) => Unit): Unit =
    allVersions
      .filter(v => v >= V3 && v <= lastVersion)
      .foreach(v => withDomain(settingsForRide(v))(assertion(v, _)))

  private def testDiffTraced(preconditions: Seq[Block], block: Block, from: StdLibVersion = V3, to: StdLibVersion = lastVersion)(
      assertion: ((StdLibVersion, TracedResult[ValidationError, Diff])) => Unit
  ): Unit =
    allVersions
      .filter(v => v >= from && v <= to)
      .foreach(v => assertDiffEiTraced(preconditions, block, settingsForRide(v).blockchainSettings.functionalitySettings)(r => assertion((v, r))))

  private def testDiff(preconditions: Seq[Block], block: Block, from: StdLibVersion = V3, to: StdLibVersion = lastVersion)(
      assertion: Either[ValidationError, Diff] => Unit
  ): Unit =
    testDiffTraced(preconditions, block, from, to)(assertion.compose(_._2.resultE))

  private def testDiffAndState(preconditions: Seq[Block], block: Block, from: StdLibVersion = V3, to: StdLibVersion = lastVersion)(
      assertion: (Diff, Blockchain) => Unit
  ): Unit =
    allVersions
      .filter(v => v >= from && v <= to)
      .foreach(v => assertDiffAndState(preconditions, block, settingsForRide(v).blockchainSettings.functionalitySettings)(assertion))

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

  private def dataContract(bigData: Boolean = false, emptyData: Boolean = false): Script = {
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

    ContractScriptImpl(
      V3,
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
    )
  }

  private def dAppWithTransfers(
      recipientAddress: Address = thirdAddress,
      recipientAmount: Long = amount,
      argName: String = "a",
      funcName: String = "f",
      assets: List[Asset] = List(Waves),
      version: StdLibVersion = V3
  ): Script = {
    val transfers: immutable.Seq[FUNCTION_CALL] = assets.map(a =>
      FUNCTION_CALL(
        User(FieldNames.ScriptTransfer),
        List(
          FUNCTION_CALL(User("Address"), List(CONST_BYTESTR(ByteStr(recipientAddress.bytes)).explicitGet())),
          CONST_LONG(recipientAmount),
          a.fold(REF("unit"): EXPR)(asset => CONST_BYTESTR(asset.id).explicitGet())
        )
      )
    )

    val payments: EXPR = transfers.foldRight(REF("nil"): EXPR) { case (elem, tail) =>
      FUNCTION_CALL(Native(CREATE_LIST), List(elem, tail))
    }

    ContractScriptImpl(
      version,
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
    )
  }

  private def defaultTransferContract(recipientAddress: AddressOrAlias, assets: List[Asset] = List(Waves)): Script = {
    val transfers: immutable.Seq[FUNCTION_CALL] = assets.map(a =>
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

    val payments: EXPR = transfers.foldRight(REF("nil"): EXPR) { case (elem, tail) =>
      FUNCTION_CALL(Native(CREATE_LIST), List(elem, tail))
    }

    ContractScriptImpl(
      V3,
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
    )
  }

  private def writeSet(count: Int): Script = {
    val DataEntries = Array.tabulate(count)(i => s"""DataEntry("$i", $i)""").mkString(",")
    TestCompiler(V3).compileContract(s"""
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
        """.stripMargin)
  }

  private def writeSetWithKeyLength(length: Int = 1, version: StdLibVersion = V3): Script = {
    val keyName = Array.fill(length)("a").mkString

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

    TestCompiler(version).compileContract(script)
  }

  private val simpleContract =
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
      contract: Script,
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
    val setContract = TxHelpers.setScript(dApp, contract)
    val ci = TxHelpers.invoke(
      dApp.toAddress,
      if (isCIDefaultFunc) None else Some("f"),
      List(CONST_BYTESTR(ByteStr.fromBytes(1, 2, 3)).explicitGet()),
      payment.toSeq,
      if (selfSend) dApp else invoker,
      sponsored.map(s => Sponsorship.fromWaves(fee, s.minSponsoredAssetFee.get.value)).getOrElse(fee),
      sponsored.map(_.asset).getOrElse(Waves),
      txVersion
    )
    (if (selfSend) List(genesis) else List(genesis, genesis2), setContract, ci)
  }

  private def preconditionsAndSetContractWithVerifier(verifier: Script, senderBindingToContract: Script): (
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
    val setVerifier = TxHelpers.setScript(invoker, verifier)
    val setContract = TxHelpers.setScript(dApp, senderBindingToContract)
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
      senderBindingToContract: Script
  ): (List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, InvokeScriptTransaction, CreateAliasTransaction) = {
    val genesis     = TxHelpers.genesis(dAppAddress)
    val genesis2    = TxHelpers.genesis(invokerAddress)
    val dAppAlias   = Alias.create("alias").explicitGet()
    val fakeAlias   = Alias.create("fakealias").explicitGet()
    val aliasTx     = TxHelpers.createAlias("alias", dApp)
    val setContract = TxHelpers.setScript(dApp, senderBindingToContract)
    val invokes = Seq(dAppAlias, fakeAlias).map(
      TxHelpers.invoke(_, Some("f"), List(CONST_BYTESTR(ByteStr.fromBytes(1, 2, 3)).explicitGet()), version = TxVersion.V1)
    )
    (List(genesis, genesis2), setContract, invokes(0), invokes(1), aliasTx)
  }

  property("doesnt validate intermediate action balance before V6")(withDomain(DomainPresets.RideV5) { d =>
    val dApp = TxHelpers.defaultSigner

    d.helpers.creditWavesToDefaultSigner()
    val asset = d.helpers.issueAsset()
    d.helpers.transferAll(dApp, TxHelpers.address(3), asset)
    d.helpers.setScript(
      dApp,
      TxHelpers.script(s"""
                          |{-# STDLIB_VERSION 4 #-}
                          |{-# CONTENT_TYPE DAPP #-}
                          |
                          |@Callable(i)
                          |func test(asset: ByteVector) = {
                          |   [
                          |     ScriptTransfer(Address(base58'${TxHelpers.secondAddress}'), 100, asset),
                          |     Reissue(asset, 100, true)
                          |   ]
                          |}
                          |""".stripMargin)
    )

    val invoke = TxHelpers.invoke(dApp.toAddress, Some("test"), Seq(CONST_BYTESTR(asset.id).explicitGet()))
    d.appendAndAssertSucceed(invoke)
    d.blockchain.balance(dApp.toAddress, asset) shouldBe 0L
    d.blockchain.balance(TxHelpers.secondAddress, asset) shouldBe 100L
  })

  property("validates intermediate action balance after V6")(
    withDomain(DomainPresets.RideV6.configure(fs => fs.copy(enforceTransferValidationAfter = 0))) { d =>
      val dApp = TxHelpers.defaultSigner

      d.helpers.creditWavesToDefaultSigner()
      val asset = d.helpers.issueAsset()
      d.helpers.transferAll(dApp, TxHelpers.address(3), asset)

      withClue("simple script") {
        d.helpers.setScript(
          dApp,
          TxHelpers.scriptV5(s"""
                                |@Callable(i)
                                |func test(asset: ByteVector) = {
                                |   [
                                |     ScriptTransfer(Address(base58'${TxHelpers.secondAddress}'), 100, asset),
                                |     Reissue(asset, 100, true)
                                |   ]
                                |}
                                |""".stripMargin)
        )

        val invoke = TxHelpers.invoke(dApp.toAddress, Some("test"), Seq(CONST_BYTESTR(asset.id).explicitGet()))
        d.appendAndCatchError(invoke).toString should include("negative asset balance")
        d.blockchain.balance(dApp.toAddress, asset) shouldBe 0L
        d.blockchain.balance(TxHelpers.secondAddress, asset) shouldBe 0L
      }

      withClue("complex script") {
        d.helpers.setScript(
          dApp,
          TxHelpers.scriptV5(s"""
                                |@Callable(i)
                                |func test(asset: ByteVector) = {
                                |   strict test1 = ${"sigVerify(base58'', base58'', base58'') ||" * 16} true
                                |   [
                                |     ScriptTransfer(Address(base58'${TxHelpers.secondAddress}'), 100, asset),
                                |     Reissue(asset, 100, true)
                                |   ]
                                |}
                                |""".stripMargin)
        )

        val invoke = TxHelpers.invoke(dApp.toAddress, Some("test"), Seq(CONST_BYTESTR(asset.id).explicitGet()))
        d.appendAndAssertFailed(invoke)
        d.blockchain.balance(dApp.toAddress, asset) shouldBe 0L
        d.blockchain.balance(TxHelpers.secondAddress, asset) shouldBe 0L
      }
    }
  )

  property("nested script failure") {
    val firstDApp  = TxHelpers.defaultSigner
    val secondDApp = TxHelpers.secondSigner

    val firstScript =
      s"""
         |@Callable(i)
         |func test() = {
         |  strict r = invoke(Address(base58'${secondDApp.toAddress}'), "test", [], [])
         |  ([], r)
         |}
         |""".stripMargin
    val secondScript =
      """
        |@Callable(i)
        |func test() = throw("test error")
        |""".stripMargin

    withClue("before V6")(withDomain(DomainPresets.RideV5) { d =>
      d.helpers.creditWavesToDefaultSigner()
      d.helpers.creditWavesFromDefaultSigner(secondDApp.toAddress)
      d.helpers.setScript(firstDApp, TxHelpers.scriptV5(firstScript))
      d.helpers.setScript(secondDApp, TxHelpers.scriptV5(secondScript))

      val invoke = TxHelpers.invoke(firstDApp.toAddress, Some("test"))
      d.appendAndCatchError(invoke).toString should include("test error")
    })

    withClue("after V6")(withDomain(DomainPresets.RideV6) { d =>
      d.helpers.creditWavesToDefaultSigner()
      d.helpers.creditWavesFromDefaultSigner(secondDApp.toAddress)
      d.helpers.setScript(firstDApp, TxHelpers.scriptV5(firstScript))
      d.helpers.setScript(secondDApp, TxHelpers.scriptV5(secondScript))

      val invoke = TxHelpers.invoke(firstDApp.toAddress, Some("test"))
      d.appendAndCatchError(invoke).toString should include("test error")
    })
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

  property("can't more than 5kb of data") {
    val (genesis, setScript, ci) = preconditionsAndSetContract(dataContract(bigData = true))
    testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), from = V5) {
      _ should produceRejectOrFailedDiff("WriteSet size can't exceed")

    }
  }

  property("can't use empty keys in v2") {
    /*forAll(for {
      r <- preconditionsAndSetContract(s => dataContractGen(s, emptyData = true), txVersion = TxVersion.V1)
    } yield (r._1, r._2, r._3)) {
      case (genesis, setScript, ci) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fsWithV5) {
          _.explicitGet()
        }
    }*/

    val (genesis, setScript, ci) = preconditionsAndSetContract(dataContract(emptyData = true), txVersion = TxVersion.V2)
    testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), from = V4) {
      _ should produceRejectOrFailedDiff("Empty keys aren't allowed")

    }
  }

  property("invoking ScriptTransfer contract results in accounts state") {
    val (genesis, setScript, ci) = preconditionsAndSetContract(dAppWithTransfers())
    testDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci), Block.ProtoBlockVersion)) { case (blockDiff, _) =>
      blockDiff.scriptsRun shouldBe 1
      blockDiff.portfolios(thirdAddress).balance shouldBe amount
      blockDiff.portfolios(setScript.sender.toAddress).balance shouldBe -amount
      blockDiff.transactions should contain key ci.id()
    }
  }

  property("invoking default func ScriptTransfer contract results in accounts state") {
    val (genesis, setScript, ci) = preconditionsAndSetContract(defaultTransferContract(thirdAddress), isCIDefaultFunc = true)

    testDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci), Block.ProtoBlockVersion)) { case (blockDiff, _) =>
      blockDiff.scriptsRun shouldBe 1
      blockDiff.portfolios(thirdAddress).balance shouldBe amount
      blockDiff.portfolios(setScript.sender.toAddress).balance shouldBe -amount
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
    ) { case (blockDiff, _) =>
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
      val proofs = Proofs(
        List.fill(proofCount)(ByteStr.fromBytes(1, 1))
      )

      testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci.copy(proofs = proofs)), Block.ProtoBlockVersion)) {
        _ should produceRejectOrFailedDiff("Transactions from non-scripted accounts must have exactly 1 proof")
      }
    }
  }

  property("suitable verifier error message on incorrect proof") {
    val (genesis, setScript, ci) = preconditionsAndSetContract(dAppWithTransfers())
    val proofs                   = Proofs(List(ByteStr.fromBytes(1, 1)))

    testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci.copy(proofs = proofs)), Block.ProtoBlockVersion)) {
      _ should produceRejectOrFailedDiff("Proof doesn't validate as signature")

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
      _ should produceRejectOrFailedDiff("does not exist")

    }
  }

  Seq(V3, V4, V5, V6).foreach { version =>
    val limit =
      if (version == V6)
        ContractLimits.MaxBalanceScriptActionsAmountV6
      else ContractLimits.MaxCallableActionsAmountBeforeV6(version)
    property(s"can't make more than $limit ScriptTransfers for V${version.id}") {
      val (genesis, setScript, ci) =
        preconditionsAndSetContract(
          dAppWithTransfers(
            assets = List.fill(limit + 1)(Waves),
            version = version
          ),
          version = version
        )

      val errMsg =
        if (version == V6)
          "ScriptTransfer, Lease, LeaseCancel actions count limit is exceeded"
        else
          "Actions count limit is exceeded"

      testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), from = version) {
        _ should produceRejectOrFailedDiff(errMsg)
      }
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
      d.appendBlock(genesis*)
      d.appendBlock(issue, setScript)
      d.appendBlock(ci)
      d.liquidDiff.scriptsRun shouldBe 2
      d.blockchain.balance(thirdAddress, Waves) shouldBe amount
      d.blockchain.balance(invokerAddress, asset) shouldBe (issue.quantity.value - 1)
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
    ) { case (_, blockDiffEi) =>
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
    testDiffTraced(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci))) { case (version, blockDiffEi) =>
      if (version == V3)
        blockDiffEi.resultE should produceRejectOrFailedDiff("TransactionNotAllowedByScript")
      else
        blockDiffEi.resultE should produceRejectOrFailedDiff("Transaction is not allowed by script of the asset")
      inside(blockDiffEi.trace) { case List(_, AssetVerifierTrace(assetId, Some(tne: TransactionNotAllowedByScript), _)) =>
        assetId shouldBe asset.id()
        tne.isAssetScript shouldBe true
      }

    }
  }

  property("invoking contract make payment by asset") {
    val issue = TxHelpers.issue(dApp, script = Some(assetAllowed), fee = 1.004.waves)
    val asset = IssuedAsset(issue.id())

    val (genesis, setScript, ci) = preconditionsAndSetContract(
      dAppWithTransfers(assets = List(asset)),
      fee = TestValues.invokeFee(1)
    )

    testDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(issue, ci), Block.ProtoBlockVersion)) {
      case (blockDiff, newState) =>
        blockDiff.scriptsRun shouldBe 3
        newState.balance(dAppAddress, asset) shouldBe (issue.quantity.value - amount)
        newState.balance(thirdAddress, asset) shouldBe amount

    }
  }

  property("invoking contract disable by payment smart asset") {
    val issue = TxHelpers.issue(dApp, script = Some(assetBanned))
    val (genesis, setScript, ci) =
      preconditionsAndSetContract(dAppWithTransfers(assets = List(IssuedAsset(issue.id()))), fee = TestValues.invokeFee(1))

    testDiffTraced(Seq(TestBlock.create(genesis ++ Seq(issue, setScript))), TestBlock.create(Seq(ci))) { blockDiffEi =>
      blockDiffEi._2.resultE should produceRejectOrFailedDiff("Transaction is not allowed by script")

    }
  }

  property("invoking contract disable by one of payment smart asset with trace") {
    val issue1 = TxHelpers.issue(issuer = dApp, script = Some(assetAllowed))
    val issue2 = TxHelpers.issue(issuer = dApp, name = "test2", script = Some(assetBanned))

    val contract                 = dAppWithTransfers(assets = List(IssuedAsset(issue1.id()), IssuedAsset(issue2.id())))
    val (genesis, setScript, ci) = preconditionsAndSetContract(contract, fee = TestValues.invokeFee(2))

    testDiffTraced(Seq(TestBlock.create(genesis ++ Seq(issue1, issue2, setScript))), TestBlock.create(Seq(ci))) { case (version, blockDiffEi) =>
      if (version == V3)
        blockDiffEi.resultE should produceRejectOrFailedDiff("Transaction is not allowed by script")
      else
        blockDiffEi.resultE should produceRejectOrFailedDiff("Transaction is not allowed by script")
      inside(blockDiffEi.trace) {
        case List(
              InvokeScriptTrace(_, `dAppAddress`, functionCall, Right(ScriptResultV3(_, transfers, _)), _, _),
              AssetVerifierTrace(allowedAssetId, None, _),
              AssetVerifierTrace(bannedAssetId, Some(_: FailedTransactionError), _)
            ) =>
          dAppAddress shouldBe ci.dApp
          functionCall shouldBe ci.funcCall

          allowedAssetId shouldBe issue1.id()
          bannedAssetId shouldBe issue2.id()

          transfers.flatMap(_.assetId.toList) shouldBe List(allowedAssetId, bannedAssetId)
      }

    }
  }

  property("trace not contains attached asset script invocation result when transferring asset script produce error") {
    val attachedAsset     = TxHelpers.issue()
    val transferringAsset = TxHelpers.issue(dApp, name = "test2", script = Some(throwingAsset))

    val (genesis, setScript, ci) = preconditionsAndSetContract(
      dAppWithTransfers(assets = List(IssuedAsset(transferringAsset.id()))),
      payment = Some(Payment(1, IssuedAsset(attachedAsset.id()))),
      fee = TestValues.invokeFee(1)
    )

    testDiffTraced(
      Seq(TestBlock.create(genesis ++ Seq(transferringAsset, attachedAsset, setScript))),
      TestBlock.create(Seq(ci))
    ) { case (version, blockDiffEi) =>
      if (version == V3)
        blockDiffEi.resultE should produceRejectOrFailedDiff(s"Transaction is not allowed by script of the asset ${transferringAsset.id()}")
      else
        blockDiffEi.resultE should produceRejectOrFailedDiff(s"Transaction is not allowed by script of the asset ${transferringAsset.id()}")
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
      blockDiffEi should produceRejectOrFailedDiff("Negative amount")

    }
  }

  property("payment should be positive") {
    Try(TxHelpers.invoke(dAppAddress, payments = Seq(Payment(-1, Waves)))).toEither should produceRejectOrFailedDiff("NonPositiveAmount")
  }

  property("smart asset payment require extra fee only before V5 activation") {
    val issue                    = TxHelpers.issue(dApp, script = Some(assetAllowed))
    val contract                 = dAppWithTransfers(assets = List(IssuedAsset(issue.id())))
    val (genesis, setScript, ci) = preconditionsAndSetContract(contract)
    testDiff(Seq(TestBlock.create(genesis ++ Seq(issue, setScript))), TestBlock.create(Seq(ci)), to = V4)(
      _ should produceRejectOrFailedDiff("does not exceed minimal value")
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
      _ should produceRejectOrFailedDiff("does not exceed minimal value")
    )
    testDiff(Seq(TestBlock.create(genesis ++ Seq(issue, setScript))), TestBlock.create(Seq(ci)), from = V5)(
      _.explicitGet().errorMessage(ci.id()) shouldBe None
    )
  }

  property("can't overflow payment + fee") {
    val payment                  = Some(Payment(ENOUGH_AMT, Waves))
    val (genesis, setScript, ci) = preconditionsAndSetContract(dAppWithTransfers(), payment = payment)
    testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci))) {
      _ should produceRejectOrFailedDiff("Attempt to transfer unavailable funds")

    }
  }

  property("invoking contract with sponsored fee") {
    val sponsorIssue             = TxHelpers.issue(dApp, amount = ENOUGH_AMT)
    val sponsorAsset             = IssuedAsset(sponsorIssue.id())
    val sponsor                  = TxHelpers.sponsor(sponsorAsset, sender = dApp)
    val (genesis, setScript, ci) = preconditionsAndSetContract(dAppWithTransfers(), sponsored = Some(sponsor))
    val t                        = TxHelpers.transfer(dApp, invokerAddress, sponsorIssue.quantity.value / 2, sponsorAsset)

    testDiffAndState(
      Seq(TestBlock.create(genesis ++ Seq(sponsorIssue, t, sponsor, setScript))),
      TestBlock.create(Seq(ci), Block.ProtoBlockVersion)
    ) { case (blockDiff, newState) =>
      blockDiff.scriptsRun shouldBe 1
      blockDiff.errorMessage(ci.id()) shouldBe None
      newState.balance(thirdAddress, Waves) shouldBe amount
      newState.balance(ci.sender.toAddress, sponsorAsset) shouldBe (sponsorIssue.quantity.value / 2 - ci.fee.value)
      newState.balance(
        dAppAddress,
        sponsorAsset
      ) shouldBe (sponsorIssue.quantity.value - sponsorIssue.quantity.value / 2 + ci.fee.value)
    }
  }

  property("argument passed to callable function has wrong type") {
    val (genesis, setScript, ci) = simplePreconditionsAndSetContract(invocationParamsCount = 2)
    testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci))) {
      _ should produceRejectOrFailedDiff("Can't apply (CONST_BOOLEAN) to 'parseInt(str: String)'")

    }
  }

  property("can't write more than 100 entries") {
    val (genesis, setScript, ci) = preconditionsAndSetContract(writeSet(ContractLimits.MaxWriteSetSize + 1))
    testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci))) {
      _ should produceRejectOrFailedDiff("Stored data count limit is exceeded")

    }
  }

  property("can write 100 entries") {
    val (genesis, setScript, ci) = preconditionsAndSetContract(writeSet(ContractLimits.MaxWriteSetSize))
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

      d.appendBlock(genesis*)
      d.appendBlock(setScript)
      val error =
        s"Data entry key size = ${ContractLimits.MaxKeySizeInBytesByVersion(version) + 1} bytes " +
          s"must be less than ${ContractLimits.MaxKeySizeInBytesByVersion(version)}"

      if (version == V3)
        d.appendBlockE(ci) should produce(error)
      else if (version == V6) {
        d.appendBlockE(ci) should produceRejectOrFailedDiff(error)
      } else {
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
      d.appendBlock(genesis*)
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
      d.appendBlock(genesis*)
      if (version == V3) {
        d.appendBlock(setScript, ci)
        d.liquidDiff.errorMessage(ci.id()) shouldBe None
      } else if (version == V6) {
        d.appendBlockE(setScript, ci) should produceRejectOrFailedDiff("Data entry key should not be empty")
      } else {
        d.appendBlock(setScript, ci)
        d.liquidDiff.errorMessage(ci.id()).map(_.text) shouldBe Some("Data entry key should not be empty")
      }
    }
  }

  property("Function call args count should be equal @Callable func one") {
    Seq(0, 3)
      .foreach { invocationArgsCount =>
        val (genesis, setScript, ci) = simplePreconditionsAndSetContract(invocationArgsCount)
        testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci))) {
          _ should produceRejectOrFailedDiff(s"takes 2 args but $invocationArgsCount were(was) given")
        }
      }
  }

  property("dApp multisig verify") {
    def multiSigCheckDApp(proofs: Int): Script = {
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

      TestCompiler(V3).compileContract(script)
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
      _ should produceRejectOrFailedDiff(s"takes 1 args but 0 were(was) given")
    }
  }

  property("Default function invocation should produce error if contract does't have default function") {
    val contract                 = dAppWithTransfers(funcName = "other", assets = List(Waves))
    val (genesis, setScript, ci) = preconditionsAndSetContract(contract, isCIDefaultFunc = true)

    testDiff(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci))) {
      _ should produceRejectOrFailedDiff("Cannot find callable function `default`, address = ")

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
      _ should produceRejectOrFailedDiff("DApp self-payment is forbidden since V4")

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
      _ should produceRejectOrFailedDiff("DApp self-transfer is forbidden since V4")

    }
  }

  property("transferring asset this value") {
    val issue                    = TxHelpers.issue(dApp, script = Some(assetUsingThis), fee = 1.004.waves)
    val contract                 = dAppWithTransfers(assets = List(IssuedAsset(issue.id())))
    val (genesis, setScript, ci) = preconditionsAndSetContract(contract, fee = TestValues.invokeFee(1))

    testDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(issue, ci), Block.ProtoBlockVersion), from = V4) {
      case (blockDiff, newState) =>
        blockDiff.scriptsRun shouldBe 3
        newState.balance(dAppAddress, IssuedAsset(issue.id())) shouldBe (issue.quantity.value - amount)
        newState.balance(thirdAddress, IssuedAsset(issue.id())) shouldBe amount

    }
  }

  private val issueContract: Script = {
    val script =
      s"""
         |{-# STDLIB_VERSION 4 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-#SCRIPT_TYPE ACCOUNT#-}
         |
         |@Callable(i)
         |func f() = [Issue("InvokeAsset", "InvokeDesc", 100, 0, true, unit, 0)]
         |""".stripMargin

    TestCompiler(V4).compileContract(script)
  }

  property("issuing asset with existed id should produce error") {
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
      .returning(Some(AccountScriptInfo(dApp.publicKey, issueContract, 10L, Map(1 -> Map("f" -> 10L)))))
      .anyNumberOfTimes()
    (blockchain.accountScript _).expects(invoke.sender.toAddress).returning(None).anyNumberOfTimes()
    (blockchain.hasAccountScript _).expects(invoke.sender.toAddress).returning(false).anyNumberOfTimes()
    (blockchain.balance _).expects(*, Waves).returning(ENOUGH_AMT).anyNumberOfTimes()
    (blockchain.leaseBalance _).expects(*).returning(LeaseBalance.empty).anyNumberOfTimes()
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
      .resultE should produceRejectOrFailedDiff("is already issued")

  }

  def reissueContract(funcName: String, asset: ByteStr): Script = {
    val script =
      s"""
         |{-# STDLIB_VERSION 4 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-#SCRIPT_TYPE ACCOUNT#-}
         |
         |@Callable(i)
         |func $funcName() = [Reissue(base58'$asset', 1, false), Reissue(base58'$asset', 4, true)]
         |""".stripMargin

    TestCompiler(V4).compileContract(script)
  }

  property("Reissuing unreissued asset should produce error") {
    val genesis1Tx  = TxHelpers.genesis(dAppAddress)
    val genesis2Tx  = TxHelpers.genesis(invokerAddress)
    val assetTx     = TxHelpers.issue(dApp, fee = 1.004.waves)
    val contract    = reissueContract("f", assetTx.id())
    val setScriptTx = TxHelpers.setScript(dApp, contract)

    val invoke = TxHelpers.invoke(dAppAddress, Some("f"))
    testDiff(
      Seq(TestBlock.create(Seq(genesis1Tx, genesis2Tx, setScriptTx, assetTx))),
      TestBlock.create(Seq(invoke), Block.ProtoBlockVersion),
      from = V4
    ) {
      _ should produceRejectOrFailedDiff("Asset is not reissuable")

    }
  }

  private val transferIssueContract: Script = {
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

    TestCompiler(V4).compileContract(script)
  }

  property("issued asset can be transferred") {
    val genesis1Tx  = TxHelpers.genesis(dAppAddress)
    val genesis2Tx  = TxHelpers.genesis(invokerAddress)
    val setScriptTx = TxHelpers.setScript(dApp, transferIssueContract)

    val invoke = TxHelpers.invoke(dAppAddress, Some("f"), fee = 100500000)

    testDiff(Seq(TestBlock.create(Seq(genesis1Tx, genesis2Tx, setScriptTx))), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), from = V4) {
      _.explicitGet()

    }
  }

  private def transferNonIssueContract(issue: Boolean) =
    TestCompiler(V4).compileContract(
      s"""
         |@Callable(i)
         |func f() = {
         |  let issue = Issue("asset", "", 100, 0, true, unit, 0)
         |  [ScriptTransfer(i.caller, 1, issue.calculateAssetId())${if (issue) ", issue" else ""}]
         |}
       """.stripMargin
    )

  property("non-issued asset can't be transferred") {
    Seq(true, false).foreach { issue =>
      val genesis1Tx  = TxHelpers.genesis(dAppAddress)
      val genesis2Tx  = TxHelpers.genesis(invokerAddress)
      val setScriptTx = TxHelpers.setScript(dApp, transferNonIssueContract(issue))
      val invoke      = TxHelpers.invoke(dAppAddress, Some("f"), fee = invokeFee(issues = 1))
      testDiff(
        Seq(TestBlock.create(Seq(genesis1Tx, genesis2Tx, setScriptTx))),
        TestBlock.create(Seq(invoke), Block.ProtoBlockVersion),
        from = V4,
        to = V4
      )(
        if (issue)
          _ shouldBe Symbol("right")
        else
          _ should produce("negative asset balance")
      )
      testDiff(
        Seq(TestBlock.create(Seq(genesis1Tx, genesis2Tx, setScriptTx))),
        TestBlock.create(Seq(invoke), Block.ProtoBlockVersion),
        from = V5
      ) {
        _ should produceRejectOrFailedDiff("is not found on the blockchain")
      }
    }
  }

  private val doubleIssueContract: Script = {
    val script =
      s"""
         |@Callable(i)
         |func f() = {
         | let v = Issue("InvokeAsset", "InvokeDesc", 100, 0, true, unit, 0)
         | [v, v]
         |}
         |""".stripMargin

    TestCompiler(V4).compileContract(script)
  }

  property("duplicate issuing asset should produce diff error") {
    val genesis1Tx  = TxHelpers.genesis(dAppAddress)
    val genesis2Tx  = TxHelpers.genesis(invokerAddress)
    val setScriptTx = TxHelpers.setScript(dApp, doubleIssueContract)
    val invoke      = TxHelpers.invoke(dAppAddress, Some("f"), fee = TestValues.invokeFee(issues = 2))
    testDiff(Seq(TestBlock.create(Seq(genesis1Tx, genesis2Tx, setScriptTx))), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), from = V4) {
      inside(_) {
        case Right(diff) =>
          diff.scriptResults(invoke.id()).error.get.text should include("is already issued")
        case Left(TransactionValidationError(ScriptExecutionError(error, _, _), _)) => error should include("is already issued")
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
      invoke.dApp shouldBe invoke.sender.toAddress
      diff.explicitGet().portfolios(invoke.sender.toAddress).balanceOf(sponsorAsset) shouldBe 0L
    }
  }

  private val throwContract: Script = {
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

    TestCompiler(V4).compileContract(script)
  }

  property(s"accepts failed transactions after ${BlockchainFeatures.BlockV5} activation") {
    val sponsorIssue = TxHelpers.issue(thirdAcc, ENOUGH_AMT)
    val sponsorAsset = IssuedAsset(sponsorIssue.id())
    val sponsorTx    = TxHelpers.sponsor(sponsorAsset, sender = thirdAcc)

    val issueTx = TxHelpers.issue(dApp, script = Some(throwingAsset))

    val feeInWaves = FeeConstants(TransactionType.InvokeScript) * FeeValidation.FeeUnit
    val feeInAsset = Sponsorship.fromWaves(feeInWaves, sponsorTx.minSponsoredAssetFee.get.value)

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
    ).foreach { case (fee, feeAsset, contract, args) =>
      val g1Tx = TxHelpers.genesis(dAppAddress)
      val g2Tx = TxHelpers.genesis(invokerAddress)
      val g3Tx = TxHelpers.genesis(thirdAddress)

      val tTx = TxHelpers.transfer(thirdAcc, invokerAddress, sponsorIssue.quantity.value, sponsorAsset)

      val ssTx   = TxHelpers.setScript(dApp, contract)
      val invoke = TxHelpers.invoke(dAppAddress, Some("f"), args, fee = fee, feeAssetId = feeAsset)
      testDiffAndState(
        Seq(TestBlock.create(Seq(g1Tx, g2Tx, g3Tx, sponsorIssue, issueTx, sponsorTx, tTx, ssTx))),
        TestBlock.create(Seq(invoke), Block.ProtoBlockVersion),
        from = V4,
        to = V5
      ) { case (diff, state) =>
        diff.scriptsRun shouldBe 0
        diff.portfolios(invoke.sender.toAddress).balanceOf(invoke.feeAssetId)
        state.balance(invoke.sender.toAddress, invoke.feeAssetId) shouldBe invoke.feeAssetId.fold(g2Tx.amount.value)(_ =>
          sponsorIssue.quantity.value
        ) - invoke.fee.value
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
    val tTx            = TxHelpers.transfer(thirdAcc, dAppAddress, iTx.quantity.value / 1)

    val wavesFee     = TestValues.invokeFee(1)
    val sponsoredFee = Sponsorship.fromWaves(wavesFee, sTx.minSponsoredAssetFee.get.value)

    Seq((Waves, wavesFee), (sponsoredAsset, sponsoredFee))
      .foreach { case (feeAsset, fee) =>
        val contract = dAppWithTransfers(assets = List(feeAsset), version = V4)
        val ssTx     = TxHelpers.setScript(dApp, contract)
        val invoke = TxHelpers.invoke(
          dAppAddress,
          Some("f"),
          args = List(CONST_BYTESTR(ByteStr.fromBytes(1, 2, 3)).explicitGet()),
          fee = fee,
          feeAssetId = feeAsset
        )

        testDiff(Seq(TestBlock.create(Seq(g1Tx, g2Tx, iTx, sTx, tTx, ssTx))), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), from = V4) {
          _ should produceRejectOrFailedDiff("AccountBalanceError")
        }
      }
  }

  property("counts complexity correctly for failed transactions (validation fails)") {
    def contract(asset: String) = TestCompiler(V4)
      .compileContract(s"""
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
                          |   [ ${(1 to ContractLimits.MaxCallableActionsAmountBeforeV6(V4))
        .map(i => s"""Issue("Asset $i", "", 100, 8, true, unit, $i)""")
        .mkString(",")} ]
                          | else if (i == "negative amount" && check) then
                          |   [ ScriptTransfer(inv.caller, -1, a) ]
                          | else if (i == "overflow amount" && check) then
                          |   [ ScriptTransfer(inv.caller, ${Long.MaxValue / 2}, a), ScriptTransfer(inv.caller, ${Long.MaxValue / 2 + 1}, a) ]
                          | else if (i == "self payment" && check) then
                          |   [ ScriptTransfer(this, 10, unit) ]
                          | else if (i == "max actions" && check) then
                          |   [ ${(0 to ContractLimits.MaxCallableActionsAmountBeforeV6(V4))
        .map(_ => "ScriptTransfer(inv.caller, 10, a)")
        .mkString(",")} ]
                          | else if (i == "invalid data entries" && check) then
                          |   [ ${(0 to ContractLimits.MaxWriteSetSize)
        .map(x => s"""IntegerEntry("val", $x)""")
        .mkString(",")},ScriptTransfer(inv.caller, 10, a)]
                          | else []
                          |}
                          |
                          |""".stripMargin)

    val gTx1             = TxHelpers.genesis(dAppAddress)
    val gTx2             = TxHelpers.genesis(invokerAddress)
    val (assetScript, _) = ScriptCompiler.compile("false", ScriptEstimatorV3(fixOverflow = true, overhead = true)).explicitGet()
    val iTx              = TxHelpers.issue(dApp, amount = Long.MaxValue, script = Some(assetScript), fee = 1.004.waves)

    val ssTx = TxHelpers.setScript(dApp, contract(iTx.assetId.toString))
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

    def contract(assets: Seq[String]) = TestCompiler(V4)
      .compileContract(s"""{-# STDLIB_VERSION 4 #-}
                          |{-# CONTENT_TYPE DAPP #-}
                          |{-#SCRIPT_TYPE ACCOUNT#-}
                          |
                          |@Callable(inv)
                          |func foo() = {
                          | [ ${assets.map(a => s"""ScriptTransfer(inv.caller, 10, base58'$a')""").mkString(",")} ]
                          |}
                          |
                          |""".stripMargin)

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
          TxHelpers.issue(dApp, ENOUGH_AMT, script = Some(trueScript), fee = 1.004.waves)
        }
        val tTxs = iTxs.takeRight(3).map { tx =>
          TxHelpers.transfer(dApp, invokerAddress, ENOUGH_AMT / 2, IssuedAsset(tx.assetId))
        }
        val saTxs = assetScripts.zipWithIndex.map { case (sc, i) =>
          TxHelpers.setAssetScript(dApp, IssuedAsset(iTxs(i).id()), sc, fee = 1.waves)
        }
        val ssTx = TxHelpers.setScript(dApp, contract(iTxs.take(4).map(_.assetId.toString)))

        val payments = iTxs.takeRight(2).map(tx => Payment(10, IssuedAsset(tx.assetId)))
        val invoke   = TxHelpers.invoke(dAppAddress, Some("foo"), payments = payments, fee = 0.017.waves)

        val genesisTxs = Seq(gTx1, gTx2) ++ invokerScriptTx ++ iTxs ++ tTxs ++ saTxs :+ ssTx
        testDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), from = V4, to = V5) {
          case (diff, _) =>
            diff.errorMessage(invoke.id()) shouldBe defined
            diff.scriptsComplexity should be > 0L
        }
        testDiff(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), from = V6) {
          _ should produce("TransactionValidationError")
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

  def paymentContract(
      senderBinding: String,
      argName: String,
      funcName: String,
      recipientAddress: Address,
      recipientAmount: Long,
      assets: List[Asset] = List(Waves),
      version: StdLibVersion = V3
  ): DApp = {

    val transfers: immutable.Seq[FUNCTION_CALL] = assets.map(a =>
      FUNCTION_CALL(
        User(FieldNames.ScriptTransfer),
        List(
          FUNCTION_CALL(User("Address"), List(CONST_BYTESTR(ByteStr(recipientAddress.bytes)).explicitGet())),
          CONST_LONG(recipientAmount),
          a.fold(REF("unit"): EXPR)(asset => CONST_BYTESTR(asset.id).explicitGet())
        )
      )
    )

    val payments: EXPR = transfers.foldRight(REF("nil"): EXPR) { case (elem, tail) =>
      FUNCTION_CALL(Native(CREATE_LIST), List(elem, tail))
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

    val transfers: immutable.Seq[FUNCTION_CALL] = assets.map(a =>
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

    val payments: EXPR = transfers.foldRight(REF("nil"): EXPR) { case (elem, tail) =>
      FUNCTION_CALL(Native(CREATE_LIST), List(elem, tail))
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

  def writeSet(funcName: String, count: Int): Script = {
    val DataEntries = Array.tabulate(count)(i => s"""DataEntry("$i", $i)""").mkString(",")

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

    TestCompiler(V3).compileContract(script)
  }
}
