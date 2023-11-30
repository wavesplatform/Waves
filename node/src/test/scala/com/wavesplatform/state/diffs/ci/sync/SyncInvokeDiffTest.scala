package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.TestValues
import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithDomain}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.state.diffs.produceRejectOrFailedDiff
import com.wavesplatform.state.{IntegerDataEntry, StringDataEntry}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.TxValidationError.FailedTransactionError
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.{EitherValues, Inside}

class SyncInvokeDiffTest extends PropSpec with WithDomain with DBCacheSettings with EitherValues with Inside {
  import DomainPresets.*

  private val fsWithV5 = RideV5.blockchainSettings.functionalitySettings

  private val invoker        = TxHelpers.signer(0)
  private val dApp           = TxHelpers.signer(1)
  private val thirdAcc       = TxHelpers.signer(2)
  private val invokerAddress = invoker.toAddress
  private val dAppAddress    = dApp.toAddress
  private val thirdAddress   = thirdAcc.toAddress

  property("Crosscontract call (same account)") {
    val script =
      TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() =
           |   ([IntegerEntry("bar", 1)], "return")
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
           """.stripMargin
      )

    val gTx1 = TxHelpers.genesis(dAppAddress)
    val gTx2 = TxHelpers.genesis(invokerAddress)

    val ssTx     = TxHelpers.setScript(dApp, script)
    val payments = List(Payment(10, Waves))
    val invoke   = TxHelpers.invoke(dAppAddress, Some("foo"), Nil, payments)

    assertDiffAndState(Seq(TestBlock.create(Seq(gTx1, gTx2, ssTx))), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) {
      case (snapshot, bc) =>
        snapshot.errorMessage(invoke.id()) shouldBe None
        bc.accountData(dAppAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
        bc.accountData(dAppAddress, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
    }
  }

  property("Crosscontract call doesn't require extra fee") {
    val script =
      TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() =
           |   ([IntegerEntry("bar", 1)], "return")
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
           """.stripMargin
      )
    val gTx1 = TxHelpers.genesis(dAppAddress)
    val gTx2 = TxHelpers.genesis(invokerAddress)

    val ssTx     = TxHelpers.setScript(dApp, script)
    val payments = List(Payment(10, Waves))
    val invoke   = TxHelpers.invoke(dAppAddress, Some("foo"), Nil, payments)

    assertDiffAndState(Seq(TestBlock.create(Seq(gTx1, gTx2, ssTx))), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) {
      case (snapshot, _) =>
        snapshot.errorMessage(invoke.id()) shouldBe None
        snapshot.scriptsComplexity shouldBe 108
        inside(snapshot.scriptResults.toSeq) { case Seq((_, call1)) =>
          inside(call1.invokes) { case Seq(call2) =>
            call2.stateChanges.error shouldBe empty
            call2.stateChanges.invokes shouldBe empty
          }
        }
    }
  }

  property("Crosscontract call (two accounts)") {
    val script =
      TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func bar(a: ByteVector) = {
           |   if i.caller.bytes == a && addressFromPublicKey(i.callerPublicKey).bytes == a
           |   then
           |     let n = Issue("barAsset", "bar asset", 1, 0, false, unit, 0)
           |     ([IntegerEntry("bar", 1), ScriptTransfer(Address(a), 3, unit), BinaryEntry("asset", n.calculateAssetId()), n, ScriptTransfer(Address(a), 1, n.calculateAssetId())], 17)
           |   else
           |     throw("Bad caller")
           | }
           """.stripMargin
      )

    val script1 =
      TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func foo() = {
           |  let b1 = wavesBalance(this)
           |  let ob1 = wavesBalance(Address(base58'$thirdAddress'))
           |  if b1 == b1 && ob1 == ob1
           |  then
           |    let r = invoke(Alias("alias"), "bar", [this.bytes], [AttachedPayment(unit, 17)])
           |    if r == 17
           |    then
           |     let data = getIntegerValue(Address(base58'$thirdAddress'), "bar")
           |     let b2 = wavesBalance(this)
           |     let ob2 = wavesBalance(Address(base58'$thirdAddress'))
           |     let ab = assetBalance(this, getBinaryValue(Address(base58'$thirdAddress'), "asset"))
           |     if data == 1
           |     then
           |      if ob1.regular+14 == ob2.regular && b1.regular == b2.regular+14 && ab == 1
           |      then
           |       let l = Lease(Address(base58'$thirdAddress'), 23)
           |       [
           |        IntegerEntry("key", 1),
           |        Lease(Address(base58'$thirdAddress'), 13),
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
           """.stripMargin
      )

    val gTx1 = TxHelpers.genesis(dAppAddress)
    val gTx2 = TxHelpers.genesis(invokerAddress)
    val gTx3 = TxHelpers.genesis(thirdAcc.toAddress)

    val aliasTx    = TxHelpers.createAlias("alias", thirdAcc)
    val ssTx       = TxHelpers.setScript(dApp, script1)
    val ssTx1      = TxHelpers.setScript(thirdAcc, script)
    val payments   = List(Payment(10L, Waves))
    val invoke     = TxHelpers.invoke(dAppAddress, Some("foo"), Nil, payments, fee = TestValues.invokeFee(issues = 1))
    val genesisTxs = Seq(gTx1, gTx2, gTx3, aliasTx, ssTx1, ssTx)

    assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) { case (snapshot, bc) =>
      snapshot.scriptResults(invoke.id()).error shouldBe None
      val l  = snapshot.scriptResults(invoke.id()).leases(0)
      val l1 = snapshot.scriptResults(invoke.id()).leases(1)
      val l2 = snapshot.scriptResults(invoke.id()).leaseCancels(0)
      l.amount shouldBe 13
      l.recipient shouldBe thirdAcc.toAddress
      l1.amount shouldBe 23
      l1.recipient shouldBe thirdAcc.toAddress
      l1.id shouldBe l2.id
      bc.accountData(dAppAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
      bc.accountData(thirdAcc.toAddress, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
    }
  }

  property("originCaller and originCallerPublicKey fields") {
    val script = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func bar(a: ByteVector, o: ByteVector) = {
         |   if i.caller.bytes == a && addressFromPublicKey(i.callerPublicKey).bytes == a && i.originCaller.bytes == o && addressFromPublicKey(i.originCallerPublicKey).bytes == o
         |   then
         |     let n = Issue("barAsset", "bar asset", 1, 0, false, unit, 0)
         |     ([IntegerEntry("bar", 1), ScriptTransfer(Address(a), 3, unit), BinaryEntry("asset", n.calculateAssetId()), n, ScriptTransfer(Address(a), 1, n.calculateAssetId())], 17)
         |   else
         |     throw("Bad caller")
         | }
           """.stripMargin
    )

    val script1 = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func foo() = {
         |  let b1 = wavesBalance(this)
         |  let ob1 = wavesBalance(Address(base58'$thirdAddress'))
         |  if b1 == b1 && ob1 == ob1 && i.caller == i.originCaller && i.callerPublicKey == i.originCallerPublicKey
         |  then
         |    let r = invoke(Alias("alias"), "bar", [this.bytes, i.caller.bytes], [AttachedPayment(unit, 17)])
         |    if r == 17
         |    then
         |     let data = getIntegerValue(Address(base58'$thirdAddress'), "bar")
         |     let b2 = wavesBalance(this)
         |     let ob2 = wavesBalance(Address(base58'$thirdAddress'))
         |     let ab = assetBalance(this, getBinaryValue(Address(base58'$thirdAddress'), "asset"))
         |     if data == 1
         |     then
         |      if ob1.regular+14 == ob2.regular && b1.regular == b2.regular+14 && ab == 1
         |      then
         |       let l = Lease(Address(base58'$thirdAddress'), 23)
         |       [
         |        IntegerEntry("key", 1),
         |        Lease(Address(base58'$thirdAddress'), 13),
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
           """.stripMargin
    )

    val gTx1 = TxHelpers.genesis(dAppAddress)
    val gTx2 = TxHelpers.genesis(invokerAddress)
    val gTx3 = TxHelpers.genesis(thirdAcc.toAddress)

    val aliasTx    = TxHelpers.createAlias("alias", thirdAcc)
    val ssTx       = TxHelpers.setScript(dApp, script1)
    val ssTx1      = TxHelpers.setScript(thirdAcc, script)
    val payments   = List(Payment(10L, Waves))
    val invoke     = TxHelpers.invoke(dAppAddress, Some("foo"), payments = payments, fee = TestValues.invokeFee(issues = 1))
    val genesisTxs = Seq(gTx1, gTx2, gTx3, aliasTx, ssTx1, ssTx)

    assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) { case (snapshot, bc) =>
      snapshot.scriptResults(invoke.id()).error shouldBe None
      val l  = snapshot.scriptResults(invoke.id()).leases(0)
      val l1 = snapshot.scriptResults(invoke.id()).leases(1)
      val l2 = snapshot.scriptResults(invoke.id()).leaseCancels(0)
      l.amount shouldBe 13
      l.recipient shouldBe thirdAcc.toAddress
      l1.amount shouldBe 23
      l1.recipient shouldBe thirdAcc.toAddress
      l1.id shouldBe l2.id
      bc.accountData(dAppAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
      bc.accountData(thirdAcc.toAddress, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
    }
  }

  property("non-NFT issue require extra fee") {
    val script = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func bar(a: ByteVector) = {
         |   if i.caller.bytes == a && addressFromPublicKey(i.callerPublicKey).bytes == a
         |   then
         |     let n = Issue("barAsset", "bar asset", 2, 0, false, unit, 0)
         |     ([IntegerEntry("bar", 1), ScriptTransfer(Address(a), 3, unit), BinaryEntry("asset", n.calculateAssetId()), n, ScriptTransfer(Address(a), 1, n.calculateAssetId())], 17)
         |   else
         |     throw("Bad caller")
         | }
           """.stripMargin
    )

    val script1 = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func foo() = {
         |  let b1 = wavesBalance(this)
         |  let ob1 = wavesBalance(Address(base58'$thirdAddress'))
         |  if b1 == b1 && ob1 == ob1
         |  then
         |    let r = invoke(Alias("alias"), "bar", [this.bytes], [AttachedPayment(unit, 17)])
         |    if r == 17
         |    then
         |     let data = getIntegerValue(Address(base58'$thirdAddress'), "bar")
         |     let b2 = wavesBalance(this)
         |     let ob2 = wavesBalance(Address(base58'$thirdAddress'))
         |     let ab = assetBalance(this, getBinaryValue(Address(base58'$thirdAddress'), "asset"))
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
           """.stripMargin
    )

    val gTx1 = TxHelpers.genesis(dAppAddress)
    val gTx2 = TxHelpers.genesis(invokerAddress)
    val gTx3 = TxHelpers.genesis(thirdAcc.toAddress)

    val aliasTx    = TxHelpers.createAlias("alias", thirdAcc)
    val ssTx       = TxHelpers.setScript(dApp, script1)
    val ssTx1      = TxHelpers.setScript(thirdAcc, script)
    val payments   = List(Payment(10L, Waves))
    val invoke     = TxHelpers.invoke(dAppAddress, Some("foo"), payments = payments, fee = TestValues.invokeFee(2))
    val genesisTxs = Seq(gTx1, gTx2, gTx3, aliasTx, ssTx1, ssTx)
    assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) { case (snapshot, bc) =>
      val err = snapshot.scriptResults(invoke.id()).error.get
      err.code shouldBe FailedTransactionError.Cause.FeeForActions.code
      bc.accountData(dAppAddress, "key") shouldBe None
      bc.accountData(thirdAcc.toAddress, "bar") shouldBe None
    }
  }

  property("non-NFT issue work") {
    val script = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func bar(a: ByteVector) = {
         |   if i.caller.bytes == a && addressFromPublicKey(i.callerPublicKey).bytes == a
         |   then
         |     let n = Issue("barAsset", "bar asset", 2, 0, false, unit, 0)
         |     ([IntegerEntry("bar", 1), ScriptTransfer(Address(a), 3, unit), BinaryEntry("asset", n.calculateAssetId()), n, ScriptTransfer(Address(a), 1, n.calculateAssetId())], 17)
         |   else
         |     throw("Bad caller")
         | }
           """.stripMargin
    )

    val script1 = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func foo() = {
         |  let b1 = wavesBalance(this)
         |  let ob1 = wavesBalance(Address(base58'$thirdAddress'))
         |  if b1 == b1 && ob1 == ob1
         |  then
         |    let r = invoke(Alias("alias"), "bar", [this.bytes], [AttachedPayment(unit, 17)])
         |    if r == 17
         |    then
         |     let data = getIntegerValue(Address(base58'$thirdAddress'), "bar")
         |     let b2 = wavesBalance(this)
         |     let ob2 = wavesBalance(Address(base58'$thirdAddress'))
         |     let ab = assetBalance(this, getBinaryValue(Address(base58'$thirdAddress'), "asset"))
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
           """.stripMargin
    )

    val gTx1 = TxHelpers.genesis(dAppAddress)
    val gTx2 = TxHelpers.genesis(invokerAddress)
    val gTx3 = TxHelpers.genesis(thirdAcc.toAddress)

    val aliasTx = TxHelpers.createAlias("alias", thirdAcc)
    val ssTx    = TxHelpers.setScript(dApp, script1)
    val ssTx1   = TxHelpers.setScript(thirdAcc, script)

    val payments   = List(Payment(10L, Waves))
    val invoke     = TxHelpers.invoke(dAppAddress, Some("foo"), payments = payments, fee = TestValues.invokeFee(issues = 1))
    val genesisTxs = Seq(gTx1, gTx2, gTx3, aliasTx, ssTx1, ssTx)

    assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) { case (snapshot, bc) =>
      bc.accountData(dAppAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
      bc.accountData(thirdAcc.toAddress, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
    }
  }

  property("Crosscontract call (two accounts, double call)") {
    val script = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func bar(a: ByteVector) = {
         |   ([IntegerEntry("bar", 1), ScriptTransfer(Address(a), 3, unit)], 17)
         | }
           """.stripMargin
    )

    val script1 = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func foo() = {
         |  let b1 = wavesBalance(this)
         |  let ob1 = wavesBalance(Address(base58'$thirdAddress'))
         |  if b1 == b1 && ob1 == ob1
         |  then
         |    let r = invoke(Address(base58'$thirdAddress'), "bar", [this.bytes], [AttachedPayment(unit, 17)])
         |    if r == 17
         |    then
         |     let data = getIntegerValue(Address(base58'$thirdAddress'), "bar")
         |     let b2 = wavesBalance(this)
         |     let ob2 = wavesBalance(Address(base58'$thirdAddress'))
         |     if data == 1
         |     then
         |      if ob1.regular+14 == ob2.regular && b1.regular == b2.regular+14
         |      then
         |       let r1 = invoke(Address(base58'$thirdAddress'), "bar", [this.bytes], [AttachedPayment(unit, 18)])
         |       if r1 == r1
         |       then
         |        let b3 = wavesBalance(this)
         |        let ob3 = wavesBalance(Address(base58'$thirdAddress'))
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
           """.stripMargin
    )

    val gTx1 = TxHelpers.genesis(dAppAddress)
    val gTx2 = TxHelpers.genesis(invokerAddress)
    val gTx3 = TxHelpers.genesis(thirdAcc.toAddress)

    val ssTx  = TxHelpers.setScript(dApp, script1)
    val ssTx1 = TxHelpers.setScript(thirdAcc, script)

    val payments = List(Payment(10, Waves))
    val invoke   = TxHelpers.invoke(dAppAddress, Some("foo"), payments = payments, fee = TestValues.invokeFee(3))

    val genesisTxs = Seq(gTx1, gTx2, gTx3, ssTx1, ssTx)

    assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) { case (snapshot, bc) =>
      bc.accountData(dAppAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
      bc.accountData(thirdAcc.toAddress, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
    }
  }

  property("Crosscontract nested call (two accounts)") {
    val script = TestCompiler(V5).compileContract(
      s"""
         |{-# STDLIB_VERSION 5 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-#SCRIPT_TYPE ACCOUNT#-}
         |
         | @Callable(i)
         | func bar(a: ByteVector) = {
         |   ([IntegerEntry("bar", 1), ScriptTransfer(Address(a), 3, unit)], 17)
         | }
           """.stripMargin
    )

    val script1 = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func back() = {
         |   [IntegerEntry("key", 0), ScriptTransfer(Address(base58'$thirdAddress'), 2, unit)]
         | }
         |
         | @Callable(i)
         | func foo() = {
         |  let b1 = wavesBalance(this)
         |  let ob1 = wavesBalance(Address(base58'$thirdAddress'))
         |  if b1 == b1 && ob1 == ob1
         |  then
         |    let r = invoke(Address(base58'$thirdAddress'), "bar", [this.bytes], [AttachedPayment(unit, 17)])
         |    if r == 17
         |    then
         |     let data = getIntegerValue(Address(base58'$thirdAddress'), "bar")
         |     let b2 = wavesBalance(this)
         |     let ob2 = wavesBalance(Address(base58'$thirdAddress'))
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
           """.stripMargin
    )

    val gTx1 = TxHelpers.genesis(dAppAddress)
    val gTx2 = TxHelpers.genesis(invokerAddress)
    val gTx3 = TxHelpers.genesis(thirdAcc.toAddress)

    val ssTx  = TxHelpers.setScript(dApp, script1)
    val ssTx1 = TxHelpers.setScript(thirdAcc, script)

    val payments   = List(Payment(10, Waves))
    val invoke     = TxHelpers.invoke(dAppAddress, Some("foo"), Nil, payments = payments)
    val genesisTxs = Seq(gTx1, gTx2, gTx3, ssTx1, ssTx)

    assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) { case (_, bc) =>
      bc.accountData(dAppAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
      bc.accountData(thirdAcc.toAddress, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
    }
  }

  property("Crosscontract with payment") {
    val script = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func bar(a: ByteVector) = {
         |   ([IntegerEntry("bar", 1), ScriptTransfer(Address(a), 3, unit)], 17)
         | }
           """.stripMargin
    )

    val script1 = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func back() = {
         |   [ScriptTransfer(Address(base58'$thirdAddress'), 2, unit)]
         | }
         |
         | @Callable(i)
         | func foo() = {
         |  let b1 = wavesBalance(this)
         |  let ob1 = wavesBalance(Address(base58'$thirdAddress'))
         |  if b1 == b1 && ob1 == ob1
         |  then
         |    let r = invoke(Address(base58'$thirdAddress'), "bar", [this.bytes], [AttachedPayment(unit, 17)])
         |    if r == 17
         |    then
         |     let data = getIntegerValue(Address(base58'$thirdAddress'), "bar")
         |     let b2 = wavesBalance(this)
         |     let ob2 = wavesBalance(Address(base58'$thirdAddress'))
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
           """.stripMargin
    )

    val gTx1 = TxHelpers.genesis(dAppAddress)
    val gTx2 = TxHelpers.genesis(invokerAddress)
    val gTx3 = TxHelpers.genesis(thirdAcc.toAddress)

    val ssTx  = TxHelpers.setScript(dApp, script1)
    val ssTx1 = TxHelpers.setScript(thirdAcc, script)

    val payments = List(Payment(10, Waves))
    val invoke   = TxHelpers.invoke(dAppAddress, Some("foo"), payments = payments)

    val genesisTxs = Seq(gTx1, gTx2, gTx3, ssTx1, ssTx)

    assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) { case (_, bc) =>
      bc.accountData(dAppAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
      bc.accountData(thirdAcc.toAddress, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
    }
  }

  property("Infinite recursive crosscontract call") {
    val recursiveContract = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func foo() = {
         |   strict r = invoke(this, "foo", [], [])
         |   []
         | }
           """.stripMargin
    )

    val gTx1 = TxHelpers.genesis(dAppAddress)
    val gTx2 = TxHelpers.genesis(invokerAddress)

    val ssTx = TxHelpers.setScript(dApp, recursiveContract)

    val payments = List(Payment(10, Waves))
    val invoke   = TxHelpers.invoke(dAppAddress, Some("foo"), payments = payments)

    assertDiffEi(Seq(TestBlock.create(Seq(gTx1, gTx2, ssTx))), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) { ei =>
      ei should produceRejectOrFailedDiff(s"DApp calls limit = 100 is exceeded")
    }
  }

  property("Smart asset transfer by nested contract actions") {
    val (assetScript, _) = {
      val script = """
                     |{-# STDLIB_VERSION 5 #-}
                     |{-# CONTENT_TYPE EXPRESSION #-}
                     |
                     |true""".stripMargin
      ScriptCompiler.compile(script, ScriptEstimatorV3(fixOverflow = true, overhead = true)).explicitGet()
    }

    def contract(asset: ByteStr): Script = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func bar(a: ByteVector) = {
         |   ([IntegerEntry("bar", 1), ScriptTransfer(Address(a), 3, base58'$asset')], 17)
         | }
           """.stripMargin
    )

    def contract1(asset: ByteStr): Script = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func back() = {
         |   [IntegerEntry("key", 0), ScriptTransfer(Address(base58'$thirdAddress'), 2, unit)]
         | }
         |
         | @Callable(i)
         | func foo() = {
         |  let b1 = wavesBalance(this)
         |  let ob1 = wavesBalance(Address(base58'$thirdAddress'))
         |  if b1 == b1 && ob1 == ob1
         |  then
         |    let r = invoke(Address(base58'$thirdAddress'), "bar", [this.bytes], [AttachedPayment(unit, 17)])
         |    if r == 17
         |    then
         |     let data = getIntegerValue(Address(base58'$thirdAddress'), "bar")
         |     let b2 = wavesBalance(this)
         |     let ob2 = wavesBalance(Address(base58'$thirdAddress'))
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
           """.stripMargin
    )

    val iTx = TxHelpers.issue(thirdAcc, script = Some(assetScript))

    val gTx1 = TxHelpers.genesis(dAppAddress)
    val gTx2 = TxHelpers.genesis(invokerAddress)
    val gTx3 = TxHelpers.genesis(thirdAcc.toAddress)

    val ssTx       = TxHelpers.setScript(dApp, contract1(iTx.id()))
    val ssTx1      = TxHelpers.setScript(thirdAcc, contract(iTx.id()))
    val payments   = List(Payment(10, Waves))
    val invoke     = TxHelpers.invoke(dAppAddress, Some("foo"), payments = payments)
    val genesisTxs = Seq(gTx1, gTx2, gTx3, ssTx1, ssTx, iTx)

    assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) { case (_, bc) =>
      bc.accountData(dAppAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
      bc.accountData(thirdAcc.toAddress, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
    }
  }

  property("Asset transfer disabled in nested contract actions") {
    val (assetScript, _) = {
      val script = """
                     |{-# STDLIB_VERSION 5 #-}
                     |{-# CONTENT_TYPE EXPRESSION #-}
                     |
                     |false""".stripMargin
      ScriptCompiler.compile(script, ScriptEstimatorV3(fixOverflow = true, overhead = true)).explicitGet()
    }

    def script(asset: ByteStr) = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func bar(a: ByteVector) = {
         |   ([IntegerEntry("bar", 1), ScriptTransfer(Address(a), 3, base58'$asset')], 17)
         | }
           """.stripMargin
    )

    val script1 = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func back() = {
         |   [IntegerEntry("key", 0), ScriptTransfer(Address(base58'$thirdAddress'), 2, unit)]
         | }
         |
         | @Callable(i)
         | func foo() = {
         |  let b1 = wavesBalance(this)
         |  let ob1 = wavesBalance(Address(base58'$thirdAddress'))
         |  if b1 == b1 && ob1 == ob1
         |  then
         |    let r = invoke(Address(base58'$thirdAddress'), "bar", [this.bytes], [AttachedPayment(unit, 17)])
         |    if r == 17
         |    then
         |     let data = getIntegerValue(Address(base58'$thirdAddress'), "bar")
         |     let tdata = getIntegerValue(this, "key")
         |     let b2 = wavesBalance(this)
         |     let ob2 = wavesBalance(Address(base58'$thirdAddress'))
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
           """.stripMargin
    )

    val service = thirdAcc

    val iTx = TxHelpers.issue(service, script = Some(assetScript))

    val gTx1 = TxHelpers.genesis(dAppAddress)
    val gTx2 = TxHelpers.genesis(invokerAddress)
    val gTx3 = TxHelpers.genesis(thirdAcc.toAddress)

    val ssTx       = TxHelpers.setScript(dApp, script1)
    val ssTx1      = TxHelpers.setScript(thirdAcc, script(iTx.id()))
    val payments   = List(Payment(10, Waves))
    val invoke     = TxHelpers.invoke(dAppAddress, Some("foo"), payments = payments)
    val genesisTxs = Seq(gTx1, gTx2, gTx3, ssTx1, ssTx, iTx)

    assertDiffEi(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) { ei =>
      ei should produceRejectOrFailedDiff(s"Transaction is not allowed by script of the asset ${iTx.id()}")
    }
  }

  property("Asset payment disabled by asset script") {
    val (assetScript, _) = {
      val script = """
                     |{-# STDLIB_VERSION 5 #-}
                     |{-# CONTENT_TYPE EXPRESSION #-}
                     |
                     |false""".stripMargin
      ScriptCompiler.compile(script, ScriptEstimatorV3(fixOverflow = true, overhead = true)).explicitGet()
    }

    val contract = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func bar(a: ByteVector) = {
         |   let r = invoke(Address(a), "back", [], [])
         |   if r == r
         |   then
         |    ([IntegerEntry("bar", 1)], 17)
         |   else
         |    throw("Impossible")
         | }
           """.stripMargin
    )

    def contract1(asset: ByteStr) = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func back() = {
         |   [IntegerEntry("key", 0), ScriptTransfer(Address(base58'$thirdAddress'), 2, unit)]
         | }
         |
         | @Callable(i)
         | func foo() = {
         |  let b1 = wavesBalance(this)
         |  let ob1 = wavesBalance(Address(base58'$thirdAddress'))
         |  if b1 == b1 && ob1 == ob1
         |  then
         |    let r = invoke(Address(base58'$thirdAddress'), "bar", [this.bytes], [AttachedPayment(base58'$asset', 1)])
         |    if r == 17
         |    then
         |     let data = getIntegerValue(Address(base58'$thirdAddress'), "bar")
         |     let tdata = getIntegerValue(this, "key")
         |     let b2 = wavesBalance(this)
         |     let ob2 = wavesBalance(Address(base58'$thirdAddress'))
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
           """.stripMargin
    )

    val iTx = TxHelpers.issue(dApp, script = Some(assetScript))

    val gTx1 = TxHelpers.genesis(dAppAddress)
    val gTx2 = TxHelpers.genesis(invokerAddress)
    val gTx3 = TxHelpers.genesis(thirdAcc.toAddress)

    val ssTx       = TxHelpers.setScript(dApp, contract1(iTx.id()))
    val ssTx1      = TxHelpers.setScript(thirdAcc, contract)
    val payments   = List(Payment(10, Waves))
    val invoke     = TxHelpers.invoke(dAppAddress, Some("foo"), payments = payments)
    val genesisTxs = Seq(gTx1, gTx2, gTx3, ssTx1, ssTx, iTx)

    assertDiffEi(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) { ei =>
      ei should produceRejectOrFailedDiff(s"Transaction is not allowed by script of the asset ${iTx.id()}")
    }
  }

  property("Payment in transaction process after Invoke") {
    def script(asset: ByteStr) = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func bar(a: ByteVector) = {
         |   let r = invoke(Address(a), "back", [], [])
         |   if r == r
         |   then
         |    ([IntegerEntry("bar", 1), ScriptTransfer(Address(a), 3, base58'$asset')], 17)
         |   else
         |    throw("Impossible")
         | }
           """.stripMargin
    )

    val script1 = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func back() = {
         |   [IntegerEntry("key", 0), ScriptTransfer(Address(base58'$thirdAddress'), 2, unit)]
         | }
         |
         | @Callable(i)
         | func foo() = {
         |  let b1 = wavesBalance(this)
         |  let ob1 = wavesBalance(Address(base58'$thirdAddress'))
         |  if b1 == b1 && ob1 == ob1
         |  then
         |    let r = invoke(Address(base58'$thirdAddress'), "bar", [this.bytes], i.payments)
         |    if r == 17
         |    then
         |     let data = getIntegerValue(Address(base58'$thirdAddress'), "bar")
         |     let tdata = getIntegerValue(this, "key")
         |     let b2 = wavesBalance(this)
         |     let ob2 = wavesBalance(Address(base58'$thirdAddress'))
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
           """.stripMargin
    )

    val iTx = TxHelpers.issue(dApp)

    val gTx1 = TxHelpers.genesis(dAppAddress)
    val gTx2 = TxHelpers.genesis(invokerAddress)
    val gTx3 = TxHelpers.genesis(thirdAcc.toAddress)

    val ssTx       = TxHelpers.setScript(dApp, script1)
    val ssTx1      = TxHelpers.setScript(thirdAcc, script(iTx.id()))
    val payments   = List(Payment(20, IssuedAsset(iTx.id())))
    val invoke     = TxHelpers.invoke(dAppAddress, Some("foo"), payments = payments)
    val genesisTxs = Seq(gTx1, gTx2, gTx3, ssTx1, ssTx, iTx)

    assertDiffEi(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) { ei =>
      ei should produceRejectOrFailedDiff(
        s"Attempt to transfer unavailable funds: " +
          s"Transaction application leads to negative asset '${iTx.id()}' balance to (at least) temporary negative state, current balance is 0"
      )
    }
  }

  property("Check balances in payment and asset scripts") {
    val startBalance                = 1000
    val transferAmount              = 3
    val paymentFromClientDAppAmount = 5
    val paymentFromInvokerAmount    = 10
    val returnValue                 = 17

    val fee = TestValues.invokeFee(3)

    val paymentScript = {
      val script = s"""
                      | {-# STDLIB_VERSION 5        #-}
                      | {-# SCRIPT_TYPE ASSET       #-}
                      | {-# CONTENT_TYPE EXPRESSION #-}
                      | assetBalance(this.issuer, this.id) == $startBalance
                    """.stripMargin
      ScriptCompiler.compile(script, ScriptEstimatorV3(fixOverflow = true, overhead = true)).explicitGet()._1
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
                      | let resultInvokerBalance = wavesBalance(Address(base58'${invokerAddress.toString}')).regular
                      | let issuerBalance = wavesBalance(this.issuer)
                      |
                      | assetBalance(this.issuer, this.id) == $startBalance                                     &&
                      | assetBalance(this.issuer, paymentAsset) == $startBalance - $paymentFromClientDAppAmount &&
                      | issuerBalance.regular == startWavesBalance                                              &&
                      | resultInvokerBalance == startInvokerBalance - $fee
                    """.stripMargin
      ScriptCompiler.compile(script, ScriptEstimatorV3(fixOverflow = true, overhead = true)).explicitGet()._1
    }

    val serviceDApp = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func bar(startInvokerBalance: Int, startWavesBalance: Int, startPaymentAssetBalance: Int, paymentAsset: ByteVector) = {
         |   let resultInvokerBalance = wavesBalance(Address(base58'${invokerAddress.toString}')).regular
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
           """.stripMargin
    )

    def clientDApp(serviceDAppAddress: Address, transferAsset: ByteStr, paymentAsset: ByteStr) =
      TestCompiler(V5).compileContract {
        s"""
           | @Callable(i)
           | func foo() = {
           |  strict startInvokerBalance = wavesBalance(Address(base58'${invokerAddress.toString}')).regular
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
      }

    val paymentIssue  = TxHelpers.issue(dApp, startBalance, script = Some(paymentScript))
    val transferIssue = TxHelpers.issue(dApp, startBalance, script = Some(transferScript))

    val gTx1 = TxHelpers.genesis(dAppAddress)
    val gTx2 = TxHelpers.genesis(invokerAddress)
    val gTx3 = TxHelpers.genesis(thirdAcc.toAddress)

    val clientDAppScript = clientDApp(thirdAcc.toAddress, transferIssue.id(), paymentIssue.id())
    val setClientDApp    = TxHelpers.setScript(dApp, clientDAppScript)
    val setServiceDApp   = TxHelpers.setScript(thirdAcc, serviceDApp)
    val payments         = List(Payment(paymentFromInvokerAmount, Waves))
    val invoke           = TxHelpers.invoke(dAppAddress, Some("foo"), payments = payments, fee = fee)

    val genesisTxs = Seq(gTx1, gTx2, gTx3, setServiceDApp, setClientDApp, paymentIssue, transferIssue)
    assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), fsWithV5) { case (snapshot, bc) =>
      snapshot.errorMessage(invoke.id()) shouldBe None

      bc.accountData(dAppAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
      bc.accountData(thirdAcc.toAddress, "bar") shouldBe Some(IntegerDataEntry("bar", 1))

      bc.balance(dAppAddress, IssuedAsset(transferIssue.id())) shouldBe startBalance - transferAmount
      bc.balance(thirdAcc.toAddress, IssuedAsset(transferIssue.id())) shouldBe 3
    }
  }

  property("Crosscontract call - internal invoke state update") {
    val (invokeEntry1Key, invokeEntry1Val)    = ("entry1", 42)
    val (invokeEntry2Key, invokeEntry2NewVal) = ("entry2", 100500)
    val invokeEntry3Key                       = "entry3"

    val contractMain = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func foo() = {
         |    let nextDAppAddr = Address(base58'$thirdAddress')
         |
         |    strict invEntry3BeforeIsDefined = isDefined(getString(nextDAppAddr, "$invokeEntry3Key"))
         |
         |    strict invResult = invoke(nextDAppAddr, "bar", [], [])
         |
         |    let invEntry1ValIsOK = getIntegerValue(nextDAppAddr, "$invokeEntry1Key") == $invokeEntry1Val
         |    let invEntry2IsNotString = isDefined(getString(nextDAppAddr, "$invokeEntry2Key")) == false
         |    let invEntry2ValIsOK = getIntegerValue(nextDAppAddr, "$invokeEntry2Key") == $invokeEntry2NewVal
         |    let invEntry3AfterIsDeleted = isDefined(getString(nextDAppAddr, "$invokeEntry3Key")) == false
         |
         |
         |    if invEntry1ValIsOK && invEntry2IsNotString && invEntry2ValIsOK && invEntry3BeforeIsDefined && invEntry3AfterIsDeleted
         |    then
         |      ([], unit)
         |    else
         |      throw("Internal invoke state update error")
         | }
    """.stripMargin
    )

    val contractSecond = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func bar() = {
         |    (
         |      [IntegerEntry("$invokeEntry1Key", $invokeEntry1Val),
         |       IntegerEntry("$invokeEntry2Key", $invokeEntry2NewVal),
         |       DeleteEntry("$invokeEntry3Key")
         |      ],
         |      unit
         |    )
         | }
    """.stripMargin
    )

    val gTx1 = TxHelpers.genesis(dAppAddress)
    val gTx2 = TxHelpers.genesis(invokerAddress)
    val gTx3 = TxHelpers.genesis(thirdAddress)

    val ssTxMain   = TxHelpers.setScript(dApp, contractMain)
    val ssTxSecond = TxHelpers.setScript(thirdAcc, contractSecond)

    val dataEntry    = StringDataEntry(invokeEntry2Key, "strData")
    val dataTxSecond = TxHelpers.data(thirdAcc, Seq(dataEntry))

    val dataEntry2    = StringDataEntry(invokeEntry3Key, "deleted entry")
    val dataTxSecond2 = TxHelpers.data(thirdAcc, Seq(dataEntry2))

    val payments     = List(Payment(10L, Waves))
    val invokeTx     = TxHelpers.invoke(dAppAddress, Some("foo"), payments = payments)
    val preparingTxs = Seq(gTx1, gTx2, gTx3, ssTxMain, ssTxSecond, dataTxSecond, dataTxSecond2)

    assertDiffAndState(Seq(TestBlock.create(preparingTxs)), TestBlock.create(Seq(invokeTx), Block.ProtoBlockVersion), fsWithV5) { case (snapshot, bc) =>
      snapshot.errorMessage(invokeTx.id()) shouldBe None
      bc.accountData(thirdAddress, invokeEntry1Key) shouldBe Some(IntegerDataEntry(invokeEntry1Key, invokeEntry1Val))
      bc.accountData(thirdAddress, invokeEntry2Key) shouldBe Some(IntegerDataEntry(invokeEntry2Key, invokeEntry2NewVal))
    }
  }

  property("Crosscontract call - same contract internal invoke - state update") {
    val (invokeEntry1Key, invokeEntry1Val)    = ("entry1", 42)
    val (invokeEntry2Key, invokeEntry2NewVal) = ("entry2", 100500)
    val invokeEntry3Key                       = "entry3"

    val contractMain = TestCompiler(V5).compileContract(s"""
                                                           | @Callable(i)
                                                           | func foo() = {
                                                           |    strict invEntry3BeforeIsDefined = isDefined(getString(this, "$invokeEntry3Key"))
                                                           |
                                                           |    strict invResult = invoke(this, "bar", [], [])
                                                           |
                                                           |    let invEntry1ValIsOK = getIntegerValue(this, "$invokeEntry1Key") == $invokeEntry1Val
                                                           |    let invEntry2IsNotString = isDefined(getString(this, "$invokeEntry2Key")) == false
                                                           |    let invEntry2ValIsOK = getIntegerValue(this, "$invokeEntry2Key") == $invokeEntry2NewVal
                                                           |    let invEntry3AfterIsDeleted = isDefined(getString(this, "$invokeEntry3Key")) == false
                                                           |
                                                           |
                                                           |    if invEntry1ValIsOK && invEntry2IsNotString && invEntry2ValIsOK && invEntry3BeforeIsDefined && invEntry3AfterIsDeleted
                                                           |    then
                                                           |      ([], unit)
                                                           |    else
                                                           |      throw("Internal invoke state update error")
                                                           | }
                                                           |
                                                           | @Callable(i)
                                                           | func bar() = {
                                                           |   (
                                                           |      [IntegerEntry("$invokeEntry1Key", $invokeEntry1Val),
                                                           |       IntegerEntry("$invokeEntry2Key", $invokeEntry2NewVal),
                                                           |       DeleteEntry("$invokeEntry3Key")
                                                           |      ],
                                                           |      unit
                                                           |   )
                                                           | }
                                                           |""".stripMargin)

    val gTx1 = TxHelpers.genesis(dAppAddress)
    val gTx2 = TxHelpers.genesis(invokerAddress)

    val ssTxMain = TxHelpers.setScript(dApp, contractMain)

    val dataEntry  = StringDataEntry(invokeEntry2Key, "strData")
    val dataTxMain = TxHelpers.data(dApp, Seq(dataEntry))

    val dataEntry2  = StringDataEntry(invokeEntry3Key, "deleted entry")
    val dataTxMain2 = TxHelpers.data(dApp, Seq(dataEntry2))

    val payments     = List(Payment(10L, Waves))
    val invokeTx     = TxHelpers.invoke(dAppAddress, Some("foo"), payments = payments)
    val preparingTxs = Seq(gTx1, gTx2, ssTxMain, dataTxMain, dataTxMain2)

    assertDiffAndState(Seq(TestBlock.create(preparingTxs)), TestBlock.create(Seq(invokeTx), Block.ProtoBlockVersion), fsWithV5) { case (snapshot, bc) =>
      snapshot.errorMessage(invokeTx.id()) shouldBe None
      bc.accountData(dAppAddress, invokeEntry1Key) shouldBe Some(IntegerDataEntry(invokeEntry1Key, invokeEntry1Val))
      bc.accountData(dAppAddress, invokeEntry2Key) shouldBe Some(IntegerDataEntry(invokeEntry2Key, invokeEntry2NewVal))
    }
  }

  property("Crosscontract call - multiple internal invokes - state update") {
    val (invokeEntry1Key, invokeEntry1Val) = ("entry1", 42)
    val transferAssetAmount                = 123
    val paymentAssetAmount                 = 456
    val fourthAcc                          = TxHelpers.signer(3)

    val paymentAssetScript =
      TestCompiler(V5).compileExpression(
        s"""
           | getIntegerValue(Address(base58'$thirdAddress'), "$invokeEntry1Key") == $invokeEntry1Val
         """.stripMargin
      )

    val transferAssetScript =
      TestCompiler(V5).compileExpression(
        s"""
           | getIntegerValue(Address(base58'$thirdAddress'), "$invokeEntry1Key") == $invokeEntry1Val
         """.stripMargin
      )

    def contractMain(paymentAsset: ByteStr) =
      TestCompiler(V5).compileContract(s"""
                                          | @Callable(i)
                                          | func foo() = {
                                          |    let secondDAppAddr = Address(base58'${fourthAcc.toAddress}')
                                          |    let thirdDAppAddr = Address(base58'$thirdAddress')
                                          |
                                          |    strict invBarResult = invoke(secondDAppAddr, "bar", [], [])
                                          |
                                          |    let thirdDAppDataEntryIsOK = getIntegerValue(thirdDAppAddr, "$invokeEntry1Key") == $invokeEntry1Val
                                          |
                                          |    strict invAnotherBazResult = invoke(
                                          |      thirdDAppAddr,
                                          |      "anotherBaz",
                                          |      [],
                                          |      [AttachedPayment(base58'$paymentAsset', $paymentAssetAmount)])
                                          |
                                          |    if thirdDAppDataEntryIsOK
                                          |    then
                                          |      ([], unit)
                                          |    else
                                          |      throw("Internal invoke chain state update error")
                                          | }
    """.stripMargin)

    def contractSecond(transferAsset: ByteStr): Script =
      TestCompiler(V5).compileContract(s"""
                                          | @Callable(i)
                                          | func bar() = {
                                          |    let thirdDAppAddr = Address(base58'$thirdAddress')
                                          |
                                          |    strict invBazResult = invoke(thirdDAppAddr, "baz", [], [])
                                          |
                                          |    let thirdDAppDataEntryIsOK = getIntegerValue(thirdDAppAddr, "$invokeEntry1Key") == $invokeEntry1Val
                                          |
                                          |    if thirdDAppDataEntryIsOK
                                          |    then
                                          |      ([ScriptTransfer(thirdDAppAddr, $transferAssetAmount, base58'$transferAsset')], unit)
                                          |    else
                                          |      throw("Internal invoke chain state update error")
                                          | }
                                          |""".stripMargin)

    val contractThird =
      TestCompiler(V5).compileContract(s"""
                                          | @Callable(i)
                                          | func baz() = {
                                          |    (
                                          |      [
                                          |        IntegerEntry("$invokeEntry1Key", $invokeEntry1Val)
                                          |      ],
                                          |      unit
                                          |    )
                                          | }
                                          |
                                          | @Callable(i)
                                          | func anotherBaz() = {
                                          |    ([], unit)
                                          | }
    """.stripMargin)

    val paymentIssue  = TxHelpers.issue(dApp, script = Some(paymentAssetScript))
    val transferIssue = TxHelpers.issue(fourthAcc, script = Some(transferAssetScript))

    val gTx1 = TxHelpers.genesis(dAppAddress)
    val gTx2 = TxHelpers.genesis(invokerAddress)
    val gTx3 = TxHelpers.genesis(fourthAcc.toAddress)
    val gTx4 = TxHelpers.genesis(thirdAcc.toAddress)

    val ssTxMain   = TxHelpers.setScript(dApp, contractMain(paymentIssue.id()))
    val ssTxSecond = TxHelpers.setScript(fourthAcc, contractSecond(transferIssue.id()))
    val ssTxThird  = TxHelpers.setScript(thirdAcc, contractThird)

    val payments     = List(Payment(10L, Waves))
    val invokeTx     = TxHelpers.invoke(dAppAddress, Some("foo"), payments = payments)
    val preparingTxs = Seq(gTx1, gTx2, gTx3, gTx4, ssTxMain, ssTxSecond, ssTxThird, paymentIssue, transferIssue)

    assertDiffAndState(Seq(TestBlock.create(preparingTxs)), TestBlock.create(Seq(invokeTx), Block.ProtoBlockVersion), fsWithV5) { case (snapshot, bc) =>
      snapshot.errorMessage(invokeTx.id()) shouldBe None
      bc.balance(thirdAcc.toAddress, IssuedAsset(transferIssue.id())) shouldBe transferAssetAmount
      bc.balance(thirdAcc.toAddress, IssuedAsset(paymentIssue.id())) shouldBe paymentAssetAmount
    }
  }
}
