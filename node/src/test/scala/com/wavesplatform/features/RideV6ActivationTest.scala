package com.wavesplatform.features

import cats.syntax.option.*
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.lang.v1.compiler.{Terms, TestCompiler}
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers.defaultSigner
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{Proofs, TxPositiveAmount, TxVersion}
import org.scalatest.OptionValues

class RideV6ActivationTest extends FreeSpec with WithDomain with OptionValues {
  "After RideV6 activation" - {
    "NODE-540 If a transaction exceeds the limit of writes number" - {
      "<=1000 complexity - rejected" - Seq(110, ContractLimits.FailFreeInvokeComplexity).foreach { complexity =>
        s"complexity = $complexity" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(defaultSigner)) { d =>
          val setScriptTx = SetScriptTransaction
            .selfSigned(TxVersion.V2, defaultSigner, Some(mkDApp(complexity)), 0.01.waves, System.currentTimeMillis())
            .explicitGet()
          d.appendBlock(setScriptTx)

          d.createDiffE(invokeTx) should produce("Stored data count limit is exceeded")
        }
      }

      ">1000 complexity - failed" in {
        withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(defaultSigner)) { d =>
          val complexity = ContractLimits.FailFreeInvokeComplexity + 1
          val setScriptTx = SetScriptTransaction
            .selfSigned(TxVersion.V3, defaultSigner, Some(mkDApp(complexity)), 0.01.waves, System.currentTimeMillis())
            .explicitGet()
          d.appendBlock(setScriptTx)

          d.appendBlock(invokeTx)
          val invokeTxMeta = d.transactionsApi.transactionById(invokeTx.id()).value
          invokeTxMeta.spentComplexity shouldBe complexity
          invokeTxMeta.succeeded shouldBe false
        }
      }

      def mkDApp(complexity: Int): ContractScript.ContractScriptImpl = {
        val baseComplexity = 101 + 101 // Because we spend 101 on IntegerEntry and 101 on a list construction
        TestCompiler(V6).compileContract(
          s"""
             |{-#STDLIB_VERSION 6 #-}
             |{-#SCRIPT_TYPE ACCOUNT #-}
             |{-#CONTENT_TYPE DAPP #-}
             |
             |@Callable(inv)
             |func foo() = {
             |  let x = ${mkExprWithComplexity(complexity - baseComplexity)}
             |  ${(1 to 101).map(i => s"""IntegerEntry("i$i", x)""").mkString("[", ", ", "]")}
             |}
        """.stripMargin
        )
      }
    }

    "NODE-542 If a transaction exceeds the limit of writes size" - {
      "<=1000 complexity - rejected" - Seq(110, ContractLimits.FailFreeInvokeComplexity).foreach { complexity =>
        s"complexity = $complexity" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(defaultSigner)) { d =>
          val setScriptTx = SetScriptTransaction
            .selfSigned(TxVersion.V2, defaultSigner, Some(mkDApp(complexity)), 0.011.waves, System.currentTimeMillis())
            .explicitGet()
          d.appendBlock(setScriptTx)

          d.createDiffE(invokeTx) should produce("WriteSet size can't exceed 5120 bytes, actual: 5121 bytes")
        }
      }

      ">1000 complexity - failed" in {
        withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(defaultSigner)) { d =>
          val complexity = ContractLimits.FailFreeInvokeComplexity + 1
          val setScriptTx = SetScriptTransaction
            .selfSigned(TxVersion.V3, defaultSigner, Some(mkDApp(complexity)), 0.011.waves, System.currentTimeMillis())
            .explicitGet()
          d.appendBlock(setScriptTx)

          d.appendBlock(invokeTx)
          val invokeTxMeta = d.transactionsApi.transactionById(invokeTx.id()).value
          invokeTxMeta.spentComplexity shouldBe complexity
          invokeTxMeta.succeeded shouldBe false
        }
      }

      def mkDApp(complexity: Int): ContractScript.ContractScriptImpl = {
        val baseComplexity = 2 + 2 // Because we spend 2 on BinaryEntry and IntegerEntry and 101 on a list construction
        // See DataTxValidator.realUserPayloadSize: IntegerDataEntry - 8, "b" and "i" keys - 2
        val limitedBinaryEntrySize = ContractLimits.MaxWriteSetSizeInBytes - 8 - 2
        TestCompiler(V6).compileContract(
          s"""
             |{-#STDLIB_VERSION 6 #-}
             |{-#SCRIPT_TYPE ACCOUNT #-}
             |{-#CONTENT_TYPE DAPP #-}
             |
             |@Callable(inv)
             |func foo() = {
             |  let x = ${mkExprWithComplexity(complexity - baseComplexity)}
             |  [
             |    BinaryEntry("b", base64'${Base64.encode(Array.fill[Byte](limitedBinaryEntrySize + 1)(0))}'),
             |    IntegerEntry("i", x)
             |  ]
             |}
        """.stripMargin
        )
      }
    }

    "NODE-544 If a transaction exceeds the limit of writes number through WriteSet" - {
      "<=1000 complexity - rejected" - Seq(110, ContractLimits.FailFreeInvokeComplexity).foreach { complexity =>
        s"complexity = $complexity" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(defaultSigner)) { d =>
          val setScriptTx = SetScriptTransaction
            .selfSigned(TxVersion.V2, defaultSigner, Some(mkDApp(complexity)), 0.021.waves, System.currentTimeMillis())
            .explicitGet()
          d.appendBlock(setScriptTx)

          d.createDiffE(invokeTx) should produce("Stored data count limit is exceeded")
        }
      }

      ">1000 complexity - failed" in {
        withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(defaultSigner)) { d =>
          val complexity = ContractLimits.FailFreeInvokeComplexity + 1
          val setScriptTx = SetScriptTransaction
            .selfSigned(TxVersion.V3, defaultSigner, Some(mkDApp(complexity)), 0.021.waves, System.currentTimeMillis())
            .explicitGet()
          d.appendBlock(setScriptTx)

          d.appendBlock(invokeTx)
          val invokeTxMeta = d.transactionsApi.transactionById(invokeTx.id()).value
          invokeTxMeta.spentComplexity shouldBe complexity
          invokeTxMeta.succeeded shouldBe false
        }
      }

      def mkDApp(complexity: Int): ContractScript.ContractScriptImpl = {
        val baseComplexity = 2 * 101 + 101 + 1 // Because we spend 2*101 on DataEntry and 101 on a list construction and 1 for WriteSet
        TestCompiler(V3).compileContract(
          s"""
             |{-#STDLIB_VERSION 3 #-}
             |{-#SCRIPT_TYPE ACCOUNT #-}
             |{-#CONTENT_TYPE DAPP #-}
             |
             |@Callable(inv)
             |func foo() = {
             |  let x = ${mkExprWithComplexity(complexity - baseComplexity)}
             |  ${(1 to 101).map(i => s"""DataEntry("i$i", x)""").mkString("WriteSet([", ", ", "])")}
             |}
        """.stripMargin
        )
      }
    }
  }

  private def mkExprWithComplexity(complexity: Int): String = s"1 ${"+ 1" * complexity}"

  private lazy val invokeTx: InvokeScriptTransaction = {
    val chainId = DomainPresets.SettingsFromDefaultConfig.blockchainSettings.addressSchemeCharacter.toByte
    InvokeScriptTransaction(
      chainId = chainId,
      version = TxVersion.V3,
      sender = defaultSigner.publicKey,
      fee = TxPositiveAmount.unsafeFrom(1000000L),
      feeAssetId = Waves,
      dApp = defaultSigner.toAddress(chainId),
      funcCallOpt = Terms.FUNCTION_CALL(FunctionHeader.User("foo"), Nil).some,
      payments = Seq.empty,
      timestamp = System.currentTimeMillis(),
      proofs = Proofs.empty
    ).signWith(defaultSigner.privateKey)
  }
}
