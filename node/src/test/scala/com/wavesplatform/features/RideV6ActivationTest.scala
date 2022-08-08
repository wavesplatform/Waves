package com.wavesplatform.features

import cats.syntax.option.*
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.{Terms, TestCompiler}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers.defaultSigner
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{Proofs, TxPositiveAmount, TxVersion}
import org.scalatest.OptionValues

class RideV6ActivationTest extends FreeSpec with WithDomain with OptionValues {
  "After RideV6 activation" - {
    "NODE-540 If a transaction exceeds the limit of writes " - {
      "<=1000 complexity - rejected" - Seq(110, 1000).foreach { complexity =>
        s"complexity = $complexity" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(defaultSigner)) { d =>
          val setScriptTx = SetScriptTransaction
            .selfSigned(TxVersion.V2, defaultSigner, Some(mkDApp(complexity)), 0.01.waves, System.currentTimeMillis())
            .explicitGet()
          d.appendBlock(setScriptTx)

          val invokeTx = mkInvoke(d.settings.blockchainSettings.addressSchemeCharacter.toByte)
          d.createDiffE(invokeTx) should produce("Stored data count limit is exceeded")
        }
      }

      ">1000 complexity - failed" in {
        withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(defaultSigner)) { d =>
          val setScriptTx = SetScriptTransaction
            .selfSigned(TxVersion.V3, defaultSigner, Some(mkDApp(1001)), 0.01.waves, System.currentTimeMillis())
            .explicitGet()
          d.appendBlock(setScriptTx)

          val invokeTx = mkInvoke(d.settings.blockchainSettings.addressSchemeCharacter.toByte)
          d.appendBlock(invokeTx)

          val invokeTxMeta = d.transactionsApi.transactionById(invokeTx.id()).value
          invokeTxMeta.succeeded shouldBe false
        }
      }
    }
  }

  // Because we spend 101 on IntegerEntry and 101 on a list construction
  private val baseComplexity = 101 + 101

  private def mkDApp(complexity: Int): ContractScript.ContractScriptImpl = TestCompiler(V6).compileContract(
    s"""
       |{-#STDLIB_VERSION 6 #-}
       |{-#SCRIPT_TYPE ACCOUNT #-}
       |{-#CONTENT_TYPE DAPP #-}
       |
       |@Callable(inv)
       |func foo() = {
       |  let x = ${mkExprWithComplexity(complexity - baseComplexity)}
       |  ${(1 to 101).map(i => s"""IntegerEntry("k$i", x)""").mkString("[", ", ", "]")}
       |}
    """.stripMargin
  )

  private def mkExprWithComplexity(complexity: Int): String = s"1 ${"+ 1" * complexity}"

  private def mkInvoke(chainId: Byte): InvokeScriptTransaction =
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
