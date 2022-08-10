package com.wavesplatform.features

import cats.syntax.option.*
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.{Terms, TestCompiler}
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers.{defaultSigner, secondSigner}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{Proofs, TxPositiveAmount, TxVersion}
import org.scalatest.OptionValues

// TODO Tx version?
class RideV6ActivationTest extends FreeSpec with WithDomain with OptionValues {
  private val chainId = DomainPresets.SettingsFromDefaultConfig.blockchainSettings.addressSchemeCharacter.toByte

  "After RideV6 activation" - {
    val cases = Seq(
      Case(
        "NODE-540 If a transaction exceeds the limit of writes number",
        "Stored data count limit is exceeded",
        { complexity =>
          val baseComplexity = 101 + 101 // Because we spend 101 on IntegerEntry and 101 on a list construction
          mkV6ContractScript(
            s""" let x = ${mkExprWithComplexity(complexity - baseComplexity)}
               | ${(1 to 101).map(i => s"""IntegerEntry("i$i", x)""").mkString("[", ", ", "]")}
               | """.stripMargin
          )
        }
      ),
      Case(
        "NODE-542 If a transaction exceeds the limit of writes size",
        "WriteSet size can't exceed 5120 bytes, actual: 5121 bytes",
        { complexity =>
          val baseComplexity = 2 + 2 // Because we spend 2 on BinaryEntry and IntegerEntry and 101 on a list construction
          // See DataTxValidator.realUserPayloadSize: IntegerDataEntry - 8, "b" and "i" keys - 2
          val limitedBinaryEntrySize = ContractLimits.MaxWriteSetSizeInBytes - 8 - 2
          mkV6ContractScript(
            s""" let x = ${mkExprWithComplexity(complexity - baseComplexity)}
               | [
               |   BinaryEntry("b", base64'${Base64.encode(Array.fill[Byte](limitedBinaryEntrySize + 1)(0))}'),
               |   IntegerEntry("i", x)
               | ]""".stripMargin
          )
        }
      ),
      Case(
        "NODE-544 If a transaction exceeds the limit of writes number through WriteSet",
        "Stored data count limit is exceeded",
        { complexity =>
          val baseComplexity = 2 * 101 + 101 + 1 // Because we spend 2*101 on DataEntry and 101 on a list construction and 1 for WriteSet
          mkV3ContractScript(
            s""" let x = ${mkExprWithComplexity(complexity - baseComplexity)}
               | ${(1 to 101).map(i => s"""DataEntry("i$i", x)""").mkString("WriteSet([", ", ", "])")}
               | """.stripMargin
          )
        }
      ),
      Case(
        "NODE-546 If a transaction tries to write an empty key to the state",
        "Empty keys aren't allowed in tx version >= 2",
        { complexity =>
          val baseComplexity = 1 + 1 // Because we spend 1 on IntegerEntry and 1 on a list construction
          mkV6ContractScript(
            s"""
               | let x = ${mkExprWithComplexity(complexity - baseComplexity)}
               | [IntegerEntry("", x)]
               |""".stripMargin
          )
        }
      ),
      Case(
        "NODE-548 If a transaction exceeds the limit of non-data actions",
        "ScriptTransfer, Lease, LeaseCancel actions count limit is exceeded",
        { complexity =>
          val baseComplexity = 1 + 101 + 101 // Because we spend 1 for Address, 101 on ScriptTransfer and 101 on a list construction
          mkV6ContractScript(
            s""" let to = Address(base58'${secondSigner.toAddress(chainId)}')
               | let x = ${mkExprWithComplexity(complexity - baseComplexity)}
               | ${(1 to 101).map(i => s"""ScriptTransfer(to, x, unit)""").mkString("[", ", ", "]")}
               | """.stripMargin
          )
        }
      ),
      Case(
        "NODE-550 If a transaction has a self-payment",
        "DApp self-payment is forbidden since V4",
        { complexity =>
          val baseComplexity = 1 + 1 // Because we spend 1 on IntegerEntry, 1 on a list construction
          mkV6ContractScript(
            s""" let x = ${mkExprWithComplexity(complexity - baseComplexity)}
               | [ IntegerEntry("i", x) ]
               | """.stripMargin
          )
        },
        invokeTx = invokeWithSelfPaymentTx
      ),
      Case(
        "NODE-552 If a sender sends a ScriptTransfer himself in a transaction",
        "DApp self-transfer is forbidden since V4",
        { complexity =>
          val baseComplexity = 1 + 1 // Because we spend 1 on ScriptTransfer, 1 on a list construction
          mkV6ContractScript(
            s""" let x = ${mkExprWithComplexity(complexity - baseComplexity)}
               | [ ScriptTransfer(inv.caller, x, unit) ]
               | """.stripMargin
          )
        }
      ),
      Case(
        "NODE-554 If a transaction sends a negative amount",
        "Negative transfer amount",
        { complexity =>
          // Because we spend 1 on Address, 1 on ScriptTransfer, 1 on a list construction and 1 for "-x"
          val baseComplexity = 1 + 1 + 1 + 1
          mkV6ContractScript(
            s""" let to = Address(base58'${secondSigner.toAddress(chainId)}')
               | let x = ${mkExprWithComplexity(complexity - baseComplexity)}
               | [ ScriptTransfer(to, -x, unit) ]
               | """.stripMargin
          )
        }
      ),
      Case(
        "NODE-558 If a transaction issues a token with invalid name",
        "Invalid asset name",
        { complexity =>
          // Because we spend 1 on Issue, 1 on a list construction
          val baseComplexity = 1 + 1
          mkV6ContractScript(
            s""" let x = ${mkExprWithComplexity(complexity - baseComplexity)}
               | [ Issue("n", "Test token", x, 2, true) ]
               | """.stripMargin
          )
        }
      ),
      Case(
        "NODE-560 If a transaction issues a token with invalid description",
        "Invalid asset description",
        { complexity =>
          // Because we spend 1 on Issue, 1 on a list construction
          val baseComplexity = 1 + 1
          mkV6ContractScript(
            s""" let x = ${mkExprWithComplexity(complexity - baseComplexity)}
               | [ Issue("Token", "${"a" * (IssueTransaction.MaxAssetDescriptionLength + 1)}", x, 2, true) ]
               | """.stripMargin
          )
        }
      )
    )

    cases.foreach { testCase =>
      testCase.title - {
        "<=1000 complexity - rejected" - Seq(110, ContractLimits.FailFreeInvokeComplexity).foreach { complexity =>
          s"complexity = $complexity" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(defaultSigner)) { d =>
            val setScriptTx = SetScriptTransaction
              .selfSigned(TxVersion.V3, defaultSigner, Some(testCase.mkDApp(complexity)), 1.waves, System.currentTimeMillis())
              .explicitGet()
            d.appendBlock(setScriptTx)
            d.createDiffE(testCase.invokeTx) should produce(testCase.rejectError)
          }
        }
        ">1000 complexity - failed" in {
          withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(defaultSigner)) { d =>
            val complexity = ContractLimits.FailFreeInvokeComplexity + 1
            val setScriptTx = SetScriptTransaction
              .selfSigned(TxVersion.V3, defaultSigner, Some(testCase.mkDApp(complexity)), 1.waves, System.currentTimeMillis())
              .explicitGet()
            d.appendBlock(setScriptTx)
            d.appendBlock(testCase.invokeTx)
            val invokeTxMeta = d.transactionsApi.transactionById(testCase.invokeTx.id()).value
            invokeTxMeta.spentComplexity shouldBe complexity
            invokeTxMeta.succeeded shouldBe false
          }
        }
      }
    }
  }

  private case class Case(title: String, rejectError: String, mkDApp: Int => Script, invokeTx: InvokeScriptTransaction = invokeTx)

  private def mkV3ContractScript(funBody: String): Script = mkContractScript(V3, funBody)
  private def mkV6ContractScript(funBody: String): Script = mkContractScript(V6, funBody)
  private def mkContractScript(v: StdLibVersion, funBody: String): Script =
    TestCompiler(v).compileContract(
      s"""
         |{-#STDLIB_VERSION ${v.id} #-}
         |{-#SCRIPT_TYPE ACCOUNT #-}
         |{-#CONTENT_TYPE DAPP #-}
         |
         |@Callable(inv)
         |func foo() = {
         |  $funBody
         |}
      """.stripMargin
    )

  private def mkExprWithComplexity(complexity: Int): String = s"1 ${"+ 1" * complexity}"

  private lazy val invokeTx = InvokeScriptTransaction(
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

  private lazy val invokeWithSelfPaymentTx = InvokeScriptTransaction(
    chainId = chainId,
    version = TxVersion.V3,
    sender = defaultSigner.publicKey,
    fee = TxPositiveAmount.unsafeFrom(1000000L),
    feeAssetId = Waves,
    dApp = defaultSigner.toAddress(chainId),
    funcCallOpt = Terms.FUNCTION_CALL(FunctionHeader.User("foo"), Nil).some,
    payments = Seq(InvokeScriptTransaction.Payment(1, Waves)),
    timestamp = System.currentTimeMillis(),
    proofs = Proofs.empty
  ).signWith(defaultSigner.privateKey)
}
