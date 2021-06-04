package com.wavesplatform.state.diffs.ci

import cats.kernel.Monoid
import com.wavesplatform.account._
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithState}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.values.{DApp => DAppType, _}
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.FunctionIds.{CREATE_LIST, THROW}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{FieldNames, WavesContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.{FunctionHeader, compiler}
import com.wavesplatform.lang.{Global, utils}
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.{ENOUGH_AMT, produce}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{Asset, _}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.{EitherValues, Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.collection.immutable

class InvokeScriptV5LimitsTest 
    extends PropSpec
    with PropertyChecks
    with Matchers
    with TransactionGen
    with NoShrink
    with Inside
    with WithState
    with DBCacheSettings
    with MockFactory
    with EitherValues {

  private val fsWithV5 = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id    -> 0,
      BlockchainFeatures.SmartAssets.id      -> 0,
      BlockchainFeatures.Ride4DApps.id       -> 0,
      BlockchainFeatures.FeeSponsorship.id   -> 0,
      BlockchainFeatures.DataTransaction.id  -> 0,
      BlockchainFeatures.BlockV5.id          -> 0,
      BlockchainFeatures.SynchronousCalls.id -> 0
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
            PureContext.build(stdLibVersion, fixUnicodeFunctions = true).withEnvironment[Environment],
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
      (issueTx, sponsorTx, _, _) <- sponsorFeeCancelSponsorFeeGen(master)
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

  property("Allow not more 30 non-data actions") {
    def contract(): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func bar(a: ByteVector, an: String) = {
             |   if i.caller.bytes == a && addressFromPublicKey(i.callerPublicKey).bytes == a
             |   then
             |     let n = Issue(an, an, 1, 0, false, unit, 0)
             |     ([IntegerEntry("bar", 1), ${"ScriptTransfer(Address(a), 1, unit), " * 13} BinaryEntry("asset", n.calculateAssetId()), n, ScriptTransfer(Address(a), 1, n.calculateAssetId())], 17)
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
             |    let r = invoke(Alias("${alias.name}"), "bar", [this.bytes, "aaaaaaaa"], [AttachedPayment(unit, 17)])
             |    let r1 = invoke(Alias("${alias.name}"), "bar", [this.bytes, "bbbbbbbb"], [AttachedPayment(unit, 17)])
             |    if r == r1
             |    then
             |     let data = getIntegerValue(Address(base58'$otherAcc'), "bar")
             |     let b2 = wavesBalance(this)
             |     let ob2 = wavesBalance(Address(base58'$otherAcc'))
             |     let ab = assetBalance(this, getBinaryValue(Address(base58'$otherAcc'), "asset"))
             |     if data == 1
             |     then
             |      if ob1.regular+8 == ob2.regular && b1.regular == b2.regular+8 && ab == 1
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
    val scenario =
      for {
        master  <- accountGen
        invoker <- accountGen
        service <- accountGen
        ts      <- timestampGen
        fee     <- ciFee(sc = 1, nonNftIssue = 2)
        gTx1 = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx2 = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx3 = GenesisTransaction.create(service.toAddress, ENOUGH_AMT, ts).explicitGet()

        alias = Alias.create("alias").explicitGet()
        aliasTx <- createAliasGen(service, alias, fee, ts)
        script1  = ContractScript(V5, contract1(service.toAddress, alias))
        script   = ContractScript(V5, contract())
        ssTx     = SetScriptTransaction.selfSigned(1.toByte, master, script1.toOption, fee, ts + 5).explicitGet()
        ssTx1    = SetScriptTransaction.selfSigned(1.toByte, service, script.toOption, fee, ts + 5).explicitGet()
        fc       = Terms.FUNCTION_CALL(FunctionHeader.User("foo"), List.empty)
        payments = List(Payment(10L, Waves))
        invokeTx = InvokeScriptTransaction
          .selfSigned(TxVersion.V3, invoker, master.toAddress, Some(fc), payments, fee, Waves, ts + 6)
          .explicitGet()
      } yield (Seq(gTx1, gTx2, gTx3, aliasTx, ssTx1, ssTx), invokeTx, master.toAddress, service.toAddress)

    forAll(scenario) {
      case (genesisTxs, invokeTx, dApp, service) =>
        assertDiffEi(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invokeTx), Block.ProtoBlockVersion), fsWithV5) {
          ei => ei should produce("Actions count limit is exceeded")
        }
    }
  }

  property("Allow 30 non-data actions") {
    def contract(): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func bar(a: ByteVector, an: String) = {
             |   if i.caller.bytes == a && addressFromPublicKey(i.callerPublicKey).bytes == a
             |   then
             |     let n = Issue(an, an, 1, 0, false, unit, 0)
             |     ([IntegerEntry("bar", 1), ${"ScriptTransfer(Address(a), 1, unit), " * 13} BinaryEntry("asset", n.calculateAssetId()), n, ScriptTransfer(Address(a), 1, n.calculateAssetId())], 17)
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
             |    let r = invoke(Alias("${alias.name}"), "bar", [this.bytes, "aaaaaaaa"], [AttachedPayment(unit, 17)])
             |    let r1 = invoke(Alias("${alias.name}"), "bar", [this.bytes, "bbbbbbbb"], [AttachedPayment(unit, 17)])
             |    if r == r1
             |    then
             |     let data = getIntegerValue(Address(base58'$otherAcc'), "bar")
             |     let b2 = wavesBalance(this)
             |     let ob2 = wavesBalance(Address(base58'$otherAcc'))
             |     let ab = assetBalance(this, getBinaryValue(Address(base58'$otherAcc'), "asset"))
             |     if data == 1
             |     then
             |      if ob1.regular+8 == ob2.regular && b1.regular == b2.regular+8 && ab == 1
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
             |   throw("Imposible")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }
    val scenario =
      for {
        master  <- accountGen
        invoker <- accountGen
        service <- accountGen
        ts      <- timestampGen
        fee     <- ciFee(sc = 1, nonNftIssue = 2)
        gTx1 = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx2 = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx3 = GenesisTransaction.create(service.toAddress, ENOUGH_AMT, ts).explicitGet()

        alias = Alias.create("alias").explicitGet()
        aliasTx <- createAliasGen(service, alias, fee, ts)
        script1  = ContractScript(V5, contract1(service.toAddress, alias))
        script   = ContractScript(V5, contract())
        ssTx     = SetScriptTransaction.selfSigned(1.toByte, master, script1.toOption, fee, ts + 5).explicitGet()
        ssTx1    = SetScriptTransaction.selfSigned(1.toByte, service, script.toOption, fee, ts + 5).explicitGet()
        fc       = Terms.FUNCTION_CALL(FunctionHeader.User("foo"), List.empty)
        payments = List(Payment(10L, Waves))
        invokeTx = InvokeScriptTransaction
          .selfSigned(TxVersion.V3, invoker, master.toAddress, Some(fc), payments, fee, Waves, ts + 6)
          .explicitGet()
      } yield (Seq(gTx1, gTx2, gTx3, aliasTx, ssTx1, ssTx), invokeTx, master.toAddress, service.toAddress)

    forAll(scenario) {
      case (genesisTxs, invokeTx, dApp, service) =>
        assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invokeTx), Block.ProtoBlockVersion), fsWithV5) {
          case (diff, bc) =>
            diff.scriptResults(invokeTx.id()).error shouldBe None
            bc.accountData(dApp, "key") shouldBe Some(IntegerDataEntry("key", 1))
            bc.accountData(service, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
        }
    }
  }
}
