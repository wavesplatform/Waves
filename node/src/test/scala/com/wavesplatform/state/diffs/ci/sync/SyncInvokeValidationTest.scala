package com.wavesplatform.state.diffs.ci.sync
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.{PropSpec, produce}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment

class SyncInvokeValidationTest extends PropSpec with WithDomain {
  import DomainPresets.*

  private val dApp1Signer  = secondSigner
  private val dApp1Address = secondAddress
  private val dApp2Signer  = signer(2)
  private val dApp2Address = signer(2).toAddress
  private val dApp3Signer  = signer(3)
  private val dApp3Address = signer(3).toAddress

  property("invoke function with arguments named 'default' as default function") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer, dApp2Signer)) { d =>
      val dApp1 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict r = Address(base58'$dApp2Address').invoke("default", [], [])
           |   []
           | }
         """.stripMargin
      )
      val dApp2 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default(arg: Int) = []
         """.stripMargin
      )
      d.appendBlock(setScript(dApp1Signer, dApp1), setScript(dApp2Signer, dApp2))
      d.appendBlockE(invoke(dApp1Address)) should produce("function 'default takes 1 args but 0 were(was) given")
    }
  }

  property("invoke dApp without default function as with default function") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer, dApp2Signer)) { d =>
      val dApp1 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict r = Address(base58'$dApp2Address').invoke("default", [], [])
           |   []
           | }
         """.stripMargin
      )
      val dApp2 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func nonDefault(arg: Int) = []
         """.stripMargin
      )
      d.appendBlock(setScript(dApp1Signer, dApp1), setScript(dApp2Signer, dApp2))
      d.appendBlockE(invoke(dApp1Address)) should produce("@Callable function 'default' doesn't exist in the script")
    }
  }

  property("invoke unexisting function") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer, dApp2Signer)) { d =>
      val dApp1 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict r = Address(base58'$dApp2Address').invoke("f", [], [])
           |   []
           | }
         """.stripMargin
      )
      val dApp2 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func g(arg: Int) = []
         """.stripMargin
      )
      d.appendBlock(setScript(dApp1Signer, dApp1), setScript(dApp2Signer, dApp2))
      d.appendBlockE(invoke(dApp1Address)) should produce("@Callable function 'f' doesn't exist in the script")
    }
  }

  property("defined arguments should be equal passed arguments to sync invoke") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer, dApp2Signer)) { d =>
      val dApp1 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict r = Address(base58'$dApp2Address').invoke("default", [1, 2], [])
           |   []
           | }
         """.stripMargin
      )
      val dApp2 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default(arg: Int) = []
         """.stripMargin
      )
      d.appendBlock(setScript(dApp1Signer, dApp1), setScript(dApp2Signer, dApp2))
      d.appendBlockE(invoke(dApp1Address)) should produce("function 'default takes 1 args but 2 were(was) given")
    }
  }

  property("max arguments limit") {
    def dApp(arguments: Int) =
      TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default(${(1 to arguments).map(i => s"a$i: Int").mkString(",")}) = []
         """.stripMargin
      )
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer, dApp2Signer)) { d =>
      val dApp1 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict r = Address(base58'$dApp2Address').invoke("default", [${List.fill(22)(1).mkString(",")}], [])
           |   []
           | }
         """.stripMargin
      )
      val dApp2 = dApp(22)
      d.appendBlock(setScript(dApp1Signer, dApp1), setScript(dApp2Signer, dApp2))
      d.appendAndAssertSucceed(invoke(dApp1Address))
    }
    (the[Exception] thrownBy dApp(23)).getMessage should include("Script functions can have no more than 22 arguments")
  }

  property("callable function argument overlaps global variable") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer, dApp2Signer)) { d =>
      val dApp1 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict r = Address(base58'$dApp2Address').invoke("default", [1], [])
           |   []
           | }
         """.stripMargin
      )
      val dApp2 = TestCompiler(V5).compileContract(
        s"""
           | let arg = 123
           |
           | @Callable(i)
           | func default(arg: Int) = if (arg == 123) then throw() else []
           |
           | @Callable(i)
           | func recursive() = {
           |   strict r = if (arg != 123) then throw() else this.invoke("default", [1], [])
           |   []
           | }
         """.stripMargin
      )
      d.appendBlock(setScript(dApp1Signer, dApp1), setScript(dApp2Signer, dApp2))
      d.appendAndAssertSucceed(invoke(dApp1Address))
      d.appendAndAssertSucceed(invoke(dApp2Address, Some("recursive")))
    }
  }

  property("supported arguments type") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer, dApp2Signer)) { d =>
      val dApp1 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   let args = [1, "str", true, base58'']
           |   strict r = Address(base58'$dApp2Address').invoke("default", args :+ args, [])
           |   []
           | }
         """.stripMargin
      )
      val dApp2 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default(
           |   arg1: Int,
           |   arg2: String,
           |   arg3: Boolean,
           |   arg4: ByteVector,
           |   arg5: List[Int|String|Boolean|ByteVector]
           | ) = {
           |   let check =
           |     arg1 == 1        &&
           |     arg2 == "str"    &&
           |     arg3 == true     &&
           |     arg4 == base58'' &&
           |     arg5 == [1, "str", true, base58'']
           |   if (check) then [] else throw()
           | }
         """.stripMargin
      )
      d.appendBlock(setScript(dApp1Signer, dApp1), setScript(dApp2Signer, dApp2))
      d.appendAndAssertSucceed(invoke(dApp1Address))
    }
  }

  property("tx belongs to all call participants") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer, dApp2Signer, dApp3Signer)) { d =>
      val dApp1 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict r = Address(base58'$dApp2Address').invoke("default", [], [])
           |   []
           | }
         """.stripMargin
      )
      val dApp2 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict r = Address(base58'$dApp3Address').invoke("default", [], [])
           |   []
           | }
         """.stripMargin
      )
      val dApp3 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = []
         """.stripMargin
      )
      d.appendBlock(setScript(dApp1Signer, dApp1), setScript(dApp2Signer, dApp2), setScript(dApp3Signer, dApp3))
      d.appendAndAssertSucceed(invoke(dApp1Address))

      d.liquidDiff.transactions.head.affected shouldBe Set(dApp1Address, dApp2Address, dApp3Address, defaultAddress)
    }
  }

  property("callable name length limit") {
    def dApp(length: Int) =
      TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func ${"a" * length}() = []
         """.stripMargin
      )
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer, dApp2Signer)) { d =>
      val dApp1 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict r = Address(base58'$dApp2Address').invoke("${"a" * 255}", [], [])
           |   []
           | }
         """.stripMargin
      )
      val dApp2 = dApp(255)
      d.appendBlock(setScript(dApp1Signer, dApp1), setScript(dApp2Signer, dApp2))
      d.appendAndAssertSucceed(invoke(dApp1Address))
    }
    (the[Exception] thrownBy dApp(256)).getMessage should include("size = 256 bytes exceeds 255")
  }

  property("invoke address without script") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer)) { d =>
      val dApp1 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict r = Address(base58'$dApp2Address').invoke("f", [], [])
           |   []
           | }
         """.stripMargin
      )
      d.appendBlock(setScript(dApp1Signer, dApp1))
      d.appendBlockE(invoke(dApp1Address)) should produce(s"No contract at address $dApp2Address")
    }
  }

  property("payments count limit") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer, dApp2Signer)) { d =>
      val dApp1 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict r = Address(base58'$dApp2Address').invoke("default", [], [])
           |   []
           | }
         """.stripMargin
      )
      val dApp2 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = []
         """.stripMargin
      )
      d.appendBlock(setScript(dApp1Signer, dApp1), setScript(dApp2Signer, dApp2))
      d.appendAndAssertSucceed(invoke(dApp1Address, payments = Seq.fill(10)(Payment(1, Waves))))
      d.appendBlockE(invoke(dApp1Address, payments = Seq.fill(11)(Payment(1, Waves)))) should produce("Script payment amount=11 should not exceed 10")
    }
  }
}
