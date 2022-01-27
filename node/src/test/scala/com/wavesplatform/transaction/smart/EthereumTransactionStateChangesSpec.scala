package com.wavesplatform.transaction.smart

import com.wavesplatform.api.common.TransactionMeta
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.values.StdLibVersion.V6
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.StringDataEntry
import com.wavesplatform.test.FlatSpec
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.utils.EthTxGenerator
import com.wavesplatform.transaction.{EthereumTransaction, TxHelpers}
import com.wavesplatform.utils.{EthHelpers, JsonMatchers}
import play.api.libs.json.Json

//noinspection NotImplementedCode
class EthereumTransactionStateChangesSpec extends FlatSpec with WithDomain with EthHelpers with JsonMatchers {
  "Failed ethereum invoke" should "preserve meta with payload" in withDomain(DomainPresets.RideV6) { d =>
    val dApp = TxHelpers.secondSigner

    d.helpers.creditWavesToDefaultSigner()
    d.helpers.creditWavesFromDefaultSigner(dApp.toAddress, 1_000_000)
    d.helpers.setScript(
      dApp,
      TxHelpers.scriptV5(
        s"""@Callable(i)
           |func deposit() = {
           |  if ((${(1 to 15).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}) || true) then throw("err")
           |  else [StringEntry("test", "foo")]
           |}""".stripMargin
      )
    )

    val invoke = EthTxGenerator.generateEthInvoke(
      TxHelpers.defaultEthSigner,
      dApp.toAddress,
      "deposit",
      Nil,
      Seq(InvokeScriptTransaction.Payment(100, Waves))
    )

    d.appendAndAssertFailed(invoke)
    d.commonApi.transactionMeta(invoke.id()) match {
      case e: TransactionMeta.Ethereum =>
        withClue("meta should be defined")(e.meta should not be empty)
        e.meta.get.toProtoString shouldBe
          """invocation {
            |  function_call: "\t\001\000\000\000\adeposit\000\000\000\000"
            |  payments {
            |    asset_id: ""
            |    amount: 100
            |  }
            |}
            |""".stripMargin
      case _ =>
        ???
    }

    val invokeExpression = EthTxGenerator.generateEthInvokeExpression(
      TxHelpers.defaultEthSigner,
      TestCompiler(V6).compileFreeCall(
        s"""
           |if ((${(1 to 15).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}) || true) then throw("err")
           |else [StringEntry("test", "foo")]
         """.stripMargin
      )
    )

    d.appendAndAssertFailed(invokeExpression)
    d.commonApi.transactionMeta(invokeExpression.id()) match {
      case e: TransactionMeta.Ethereum =>
        withClue("meta should be defined")(e.meta should not be empty)
        e.meta.get.toProtoString shouldBe
          """invoke_expression {
            |  expression: "\006\001\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\t\000\364\003\003\001\000\001\000\001\000\006\t\000\364\003\003\001\000\001\000\001\000\006\t\000\364\003\003\001\000\001\000\001\000\006\t\000\364\003\003\001\000\001\000\001\000\006\t\000\364\003\003\001\000\001\000\001\000\006\t\000\364\003\003\001\000\001\000\001\000\006\t\000\364\003\003\001\000\001\000\001\000\006\t\000\364\003\003\001\000\001\000\001\000\006\t\000\364\003\003\001\000\001\000\001\000\006\t\000\364\003\003\001\000\001\000\001\000\006\t\000\364\003\003\001\000\001\000\001\000\006\t\000\364\003\003\001\000\001\000\001\000\006\t\000\364\003\003\001\000\001\000\001\000\006\t\000\364\003\003\001\000\001\000\001\000\006\t\000\364\003\003\001\000\001\000\001\000\006\006\t\000\002\001\002\003err\t\000\314\b\002\t\001\vStringEntry\002\002\004test\002\003foo\005\003nil\305S}\304"
            |}
            |""".stripMargin
      case _ =>
        ???
    }
  }

  "Ethereum invoke with complexity>1000" should "handle error" in withDomain(DomainPresets.RideV6) { d =>
    val dApp = TxHelpers.secondSigner

    d.helpers.creditWavesToDefaultSigner()
    d.helpers.creditWavesFromDefaultSigner(dApp.toAddress, 1_000_000)
    d.helpers.setScript(
      dApp,
      TxHelpers.scriptV5(
        s"""@Callable(i)
           |func deposit() = {
           |  if ((${(1 to 15).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}) || true) then throw("err")
           |  else [StringEntry("test", "foo")]
           |}""".stripMargin
      )
    )

    def check(invoke: EthereumTransaction): Unit = {
      d.appendBlock(invoke)
      d.liquidAndSolidAssert { () =>
        d.commonApi.transactions.transactionById(invoke.id()) match {
          case Some(meta: TransactionMeta.Ethereum) =>
            assert(!meta.succeeded, "should fail")
            Json.toJson(meta.invokeScriptResult) should matchJson(
              """
                |{
                |  "data" : [ ],
                |  "transfers" : [ ],
                |  "issues" : [ ],
                |  "reissues" : [ ],
                |  "burns" : [ ],
                |  "sponsorFees" : [ ],
                |  "leases" : [ ],
                |  "leaseCancels" : [ ],
                |  "invokes" : [ ],
                |  "error" : {
                |    "code" : 1,
                |    "text" : "err"
                |  }
                |}
              """.stripMargin
            )
          case _ => ???
        }
      }
    }

    check(
      EthTxGenerator.generateEthInvoke(
        TxHelpers.defaultEthSigner,
        dApp.toAddress,
        "deposit",
        Nil,
        Seq(InvokeScriptTransaction.Payment(100, Waves))
      )
    )

    check(
      EthTxGenerator.generateEthInvokeExpression(
        TxHelpers.defaultEthSigner,
        TestCompiler(V6).compileFreeCall(
          s"""
             |if ((${(1 to 15).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}) || true) then throw("err")
             |else [StringEntry("test", "foo")]
           """.stripMargin
        )
      )
    )
  }

  it should "handle success" in withDomain(DomainPresets.RideV6) { d =>
    val dApp = TxHelpers.signer(10)

    d.helpers.creditWavesToDefaultSigner()
    d.helpers.creditWavesFromDefaultSigner(dApp.toAddress, 1_000_000)

    d.helpers.setScript(
      dApp,
      TxHelpers.scriptV5(
        s"""@Callable(i)
           |func deposit() = {
           |  if ((${(1 to 15).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}) || false) then throw("err")
           |  else [StringEntry("test", "foo")]
           |}""".stripMargin
      )
    )

    def check(invoke: EthereumTransaction): Unit = {
      d.appendBlock(invoke)
      d.liquidAndSolidAssert { () =>
        d.blockchain.accountData(dApp.toAddress, "test") shouldBe Some(StringDataEntry("test", "foo"))
        d.commonApi.transactions.transactionById(invoke.id()) match {
          case Some(meta: TransactionMeta.Ethereum) =>
            assert(meta.succeeded, "should succeed")
            Json.toJson(meta.invokeScriptResult) should matchJson(
              """
                |{
                |  "data" : [ {
                |    "key" : "test",
                |    "type" : "string",
                |    "value" : "foo"
                |  } ],
                |  "transfers" : [ ],
                |  "issues" : [ ],
                |  "reissues" : [ ],
                |  "burns" : [ ],
                |  "sponsorFees" : [ ],
                |  "leases" : [ ],
                |  "leaseCancels" : [ ],
                |  "invokes" : [ ]
                |}
              """.stripMargin
            )
          case _ => ???
        }
      }
    }

    check(
      EthTxGenerator.generateEthInvoke(
        TxHelpers.defaultEthSigner,
        dApp.toAddress,
        "deposit",
        Nil,
        Seq(InvokeScriptTransaction.Payment(100, Waves))
      )
    )

    check(
      EthTxGenerator.generateEthInvokeExpression(
        TxHelpers.defaultEthSigner,
        TestCompiler(V6).compileFreeCall(
          s"""
             |if ((${(1 to 15).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}) || false) then throw("err")
             |else [StringEntry("test", "foo")]
         """.stripMargin
        )
      )
    )
  }

  it should "handle nested error" in withDomain(DomainPresets.RideV6) { d =>
    val dApp       = TxHelpers.signer(10)
    val nestedDApp = TxHelpers.signer(11)

    d.helpers.creditWavesToDefaultSigner()
    d.helpers.creditWavesFromDefaultSigner(dApp.toAddress, 1_000_000)
    d.helpers.creditWavesFromDefaultSigner(nestedDApp.toAddress, 1_000_000)

    d.helpers.setScript(
      dApp,
      TxHelpers.scriptV5(
        s"""@Callable(i)
           |func deposit() = {
           |  strict res1 = invoke(Address(base58'${nestedDApp.toAddress}'), "test", nil, [AttachedPayment(unit, 100)])
           |  strict res2 = ((${(1 to 15).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}) || true)
           |  [StringEntry("test", "foo")]
           |}""".stripMargin
      )
    )

    d.helpers.setScript(
      nestedDApp,
      TxHelpers.scriptV5(
        s"""@Callable(i)
           |func test() = {
           |  if ((${(1 to 15).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}) || true) then throw("err")
           |  else [StringEntry("test1", "bar")]
           |}""".stripMargin
      )
    )

    def check(invoke: EthereumTransaction): Unit = {
      d.appendBlock(invoke)
      d.liquidAndSolidAssert { () =>
        d.commonApi.transactions.transactionById(invoke.id()) match {
          case Some(meta: TransactionMeta.Ethereum) =>
            assert(!meta.succeeded, "should fail")
            Json.toJson(meta.invokeScriptResult) should matchJson(
              """
                |{
                |  "data" : [ ],
                |  "transfers" : [ ],
                |  "issues" : [ ],
                |  "reissues" : [ ],
                |  "burns" : [ ],
                |  "sponsorFees" : [ ],
                |  "leases" : [ ],
                |  "leaseCancels" : [ ],
                |  "invokes" : [ {
                |    "dApp" : "3N1aSGxVmMSUQbnporHpeX5X34X7Gec6kg3",
                |    "call" : {
                |      "function" : "test",
                |      "args" : [ ]
                |    },
                |    "payment" : [ {
                |      "assetId" : null,
                |      "amount" : 100
                |    } ],
                |    "stateChanges" : {
                |      "data" : [ ],
                |      "transfers" : [ ],
                |      "issues" : [ ],
                |      "reissues" : [ ],
                |      "burns" : [ ],
                |      "sponsorFees" : [ ],
                |      "leases" : [ ],
                |      "leaseCancels" : [ ],
                |      "invokes" : [ ],
                |      "error" : {
                |        "code" : 1,
                |        "text" : "err"
                |      }
                |    }
                |  } ],
                |  "error" : {
                |    "code" : 1,
                |    "text" : "FailedTransactionError(code = 1, error = err, log =)"
                |  }
                |}
              """.stripMargin
            )
          case _ => ???
        }
      }
    }

    check(
      EthTxGenerator.generateEthInvoke(
        TxHelpers.defaultEthSigner,
        dApp.toAddress,
        "deposit",
        Nil,
        Seq(InvokeScriptTransaction.Payment(100, Waves))
      )
    )

    check(
      EthTxGenerator.generateEthInvokeExpression(
        TxHelpers.defaultEthSigner,
        TestCompiler(V6).compileFreeCall(
          s"""
             |strict res1 = invoke(Address(base58'${nestedDApp.toAddress}'), "test", nil, [AttachedPayment(unit, 100)])
             |strict res2 = ((${(1 to 15).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}) || true)
             |[StringEntry("test", "foo")]
           """.stripMargin
        )
      )
    )
  }

  it should "success with nested calls" in withDomain(DomainPresets.RideV6) { d =>
    val dApp       = TxHelpers.signer(10)
    val nestedDApp = TxHelpers.signer(11)

    d.helpers.creditWavesToDefaultSigner()
    d.helpers.creditWavesFromDefaultSigner(dApp.toAddress, 1_000_000)
    d.helpers.creditWavesFromDefaultSigner(nestedDApp.toAddress, 1_000_000)

    d.helpers.setScript(
      dApp,
      TxHelpers.scriptV5(
        s"""@Callable(i)
           |func deposit() = {
           |  strict res1 = invoke(Address(base58'${nestedDApp.toAddress}'), "test", nil, [AttachedPayment(unit, 100)])
           |  strict res2 = ((${(1 to 15).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}) || true)
           |  [StringEntry("test", "foo")]
           |}""".stripMargin
      )
    )

    d.helpers.setScript(
      nestedDApp,
      TxHelpers.scriptV5(
        s"""@Callable(i)
           |func test() = {
           |  if ((${(1 to 15).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}) || false) then throw("err")
           |  else [StringEntry("test1", "bar")]
           |}""".stripMargin
      )
    )

    def check(invoke: EthereumTransaction): Unit = {
      d.appendBlock(invoke)
      d.liquidAndSolidAssert { () =>
        d.blockchain.accountData(dApp.toAddress, "test") shouldBe Some(StringDataEntry("test", "foo"))
        d.blockchain.accountData(nestedDApp.toAddress, "test1") shouldBe Some(StringDataEntry("test1", "bar"))

        d.commonApi.transactions.transactionById(invoke.id()) match {
          case Some(meta: TransactionMeta.Ethereum) =>
            assert(meta.succeeded, "should succeed")
            Json.toJson(meta.invokeScriptResult) should matchJson(
              """{
                |  "data" : [ {
                |    "key" : "test",
                |    "type" : "string",
                |    "value" : "foo"
                |  } ],
                |  "transfers" : [ ],
                |  "issues" : [ ],
                |  "reissues" : [ ],
                |  "burns" : [ ],
                |  "sponsorFees" : [ ],
                |  "leases" : [ ],
                |  "leaseCancels" : [ ],
                |  "invokes" : [ {
                |    "dApp" : "3N1aSGxVmMSUQbnporHpeX5X34X7Gec6kg3",
                |    "call" : {
                |      "function" : "test",
                |      "args" : [ ]
                |    },
                |    "payment" : [ {
                |      "assetId" : null,
                |      "amount" : 100
                |    } ],
                |    "stateChanges" : {
                |      "data" : [ {
                |        "key" : "test1",
                |        "type" : "string",
                |        "value" : "bar"
                |      } ],
                |      "transfers" : [ ],
                |      "issues" : [ ],
                |      "reissues" : [ ],
                |      "burns" : [ ],
                |      "sponsorFees" : [ ],
                |      "leases" : [ ],
                |      "leaseCancels" : [ ],
                |      "invokes" : [ ]
                |    }
                |  } ]
                |}
              """.stripMargin
            )
          case _ => ???
        }
      }
    }

    check(
      EthTxGenerator.generateEthInvoke(
        TxHelpers.defaultEthSigner,
        dApp.toAddress,
        "deposit",
        Nil,
        Seq(InvokeScriptTransaction.Payment(100, Waves))
      )
    )

    check(
      EthTxGenerator.generateEthInvokeExpression(
        TxHelpers.defaultEthSigner,
        TestCompiler(V6).compileFreeCall(
          s"""
             |strict res1 = invoke(Address(base58'${nestedDApp.toAddress}'), "test", nil, [AttachedPayment(unit, 100)])
             |strict res2 = ((${(1 to 15).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}) || true)
             |[StringEntry("test", "foo")]
           """.stripMargin
        )
      )
    )
  }
}
