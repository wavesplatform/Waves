package com.wavesplatform.features

import cats.syntax.option.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.{Terms, TestCompiler}
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxHelpers.{defaultSigner, secondSigner}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{Asset, Proofs, TxPositiveAmount, TxVersion}
import org.scalatest.OptionValues

import java.nio.charset.StandardCharsets

// TODO Tx version?
class RideV6ActivationTest extends FreeSpec with WithDomain with OptionValues {
  private val chainId      = DomainPresets.SettingsFromDefaultConfig.blockchainSettings.addressSchemeCharacter.toByte
  private val otherChainId = (chainId + 1).toByte

  private val unknownAssetId = IssuedAsset(ByteStr.decodeBase58("F9th5zKSTwtKaKEjf28A2Fj6Z6KMKoDX9jmZce6fsAhS").get)
  private val unknownLeaseId = ByteStr(Array.fill[Byte](DigestLength)(0))

  private val secondSignedAddr = secondSigner.toAddress(chainId)

  "After RideV6 activation" - {
    val cases = Seq(
      Case(
        "NODE-540 If a transaction exceeds the limit of writes number",
        "Stored data count limit is exceeded",
        { complexity =>
          val baseComplexity = 101 + 101 // 101 on IntegerEntry and 101 on a list construction
          mkV6Script(
            s""" let complexInt = ${mkExprWithComplexity(complexity - baseComplexity)}
               | ${(1 to 101).map(i => s"""IntegerEntry("i$i", complexInt)""").mkString("[", ", ", "]")}
               | """.stripMargin
          )
        }
      ),
      Case(
        "NODE-542 If a transaction exceeds the limit of writes size",
        "WriteSet size can't exceed 5120 bytes, actual: 5121 bytes",
        { complexity =>
          val baseComplexity = 2 + 2 // 2 on BinaryEntry and IntegerEntry and 101 on a list construction
          // See DataTxValidator.realUserPayloadSize: IntegerDataEntry - 8, "b" and "i" keys - 2
          val limitedBinaryEntrySize = ContractLimits.MaxWriteSetSizeInBytes - 8 - 2
          mkV6Script(
            s""" let complexInt = ${mkExprWithComplexity(complexity - baseComplexity)}
               | [
               |   BinaryEntry("b", base64'${Base64.encode(Array.fill[Byte](limitedBinaryEntrySize + 1)(0))}'),
               |   IntegerEntry("i", complexInt)
               | ]""".stripMargin
          )
        }
      ),
      Case(
        "NODE-544 If a transaction exceeds the limit of writes number through WriteSet",
        "Stored data count limit is exceeded",
        { complexity =>
          val baseComplexity = 2 * 101 + 101 + 1 // 2*101 on DataEntry and 101 on a list construction and 1 for WriteSet
          mkV3Script(
            s""" let complexInt = ${mkExprWithComplexity(complexity - baseComplexity)}
               | ${(1 to 101).map(i => s"""DataEntry("i$i", complexInt)""").mkString("WriteSet([", ", ", "])")}
               | """.stripMargin
          )
        }
      )
    ) ++
      Seq(
        "Binary"  -> s"""BinaryEntry("", base58'${Base58.encode("test".getBytes(StandardCharsets.UTF_8))}')""",
        "Boolean" -> """BooleanEntry("", false)""",
        "Delete"  -> """DeleteEntry("")""",
        "Integer" -> """IntegerEntry("", 0)""",
        "String"  -> """StringEntry("", "lie")"""
      ).map { case (entryType, entry) =>
        Case(
          s"NODE-546 If a transaction tries to write an empty $entryType key to the state",
          "Empty keys aren't allowed in tx version >= 2",
          { complexity =>
            val baseComplexity = 1 + 1 + 2 // 1 on IntegerEntry, 1 on other entry and 2 on a list construction
            mkV6Script(
              s"""
                 | let complexInt = ${mkExprWithComplexity(complexity - baseComplexity)}
                 | [ IntegerEntry("k", complexInt), $entry ]
                 |""".stripMargin
            )
          }
        )
      } ++
      Seq(
        "ScriptTransfer" -> "ScriptTransfer(to, 1, unit)",
        "Lease"          -> "Lease(to, 1)",
        "LeaseCancel"    -> s"LeaseCancel(base58'$unknownLeaseId')"
      ).map { case (actionType, v) =>
        Case(
          s"NODE-548 If a transaction exceeds the limit of $actionType actions",
          "ScriptTransfer, Lease, LeaseCancel actions count limit is exceeded",
          { complexity =>
            // 1 for Address, 101 on actions, 101 on a list construction, 10 on getInteger,
            // 2 on valueOrElse, and 1 on "+" and 1 for what? TODO
            val baseComplexity = 1 + 101 + 101 + 10 + 2 + 1 + 1
            mkV6Script(
              s""" let to = Address(base58'$secondSignedAddr')
                 | let complexInt = ${mkExprWithComplexity(complexity - baseComplexity)}
                 | (
                 |   [ ${(1 to 101).map(_ => v).mkString(", ")} ],
                 |   # otherwise a script with LeaseCancel will be with a different complexity
                 |   valueOrElse(getInteger(to, "force-use-of-to"), 0) + complexInt
                 | )
                 | """.stripMargin
            )
          }
        )
      } ++
      Seq(
        Case(
          "NODE-548 If a transaction exceeds the limit of mixed non-data actions",
          "ScriptTransfer, Lease, LeaseCancel actions count limit is exceeded",
          { complexity =>
            val baseComplexity = 1 + 101 + 101 // 1 for Address, 101 on actions and 101 on a list construction
            mkV6Script(
              s""" let to = Address(base58'$secondSignedAddr')
                 | let complexInt = ${mkExprWithComplexity(complexity - baseComplexity)}
                 | [
                 |   ${(1 to 34).map(_ => s"""ScriptTransfer(to, complexInt, unit)""").mkString(", ")},
                 |   ${(1 to 34).map(_ => s"""Lease(to, 1)""").mkString(", ")},
                 |   ${(1 to 33).map(_ => s"""LeaseCancel(base58'$unknownLeaseId')""").mkString(", ")}
                 | ]
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-550 If a transaction has a self-payment",
          "DApp self-payment is forbidden since V4",
          mkScriptWithOneAction("""IntegerEntry("i", 1)"""),
          invokeTx = invokeTxWithPayment(1, Waves) // self-payment
        ),
        Case(
          "NODE-552 If a sender sends a ScriptTransfer himself in a transaction",
          "DApp self-transfer is forbidden since V4",
          mkScriptWithOneAction("ScriptTransfer(this, 1, unit)")
        ),
        Case(
          "NODE-554 If a transaction sends a negative amount",
          "Negative transfer amount",
          { complexity =>
            // 1 on Address, 1 on ScriptTransfer, 1 on a list construction and 1 for "-complexInt"
            val baseComplexity = 1 + 1 + 1 + 1
            mkV6Script(
              s""" let to = Address(base58'$secondSignedAddr')
                 | let complexInt = ${mkExprWithComplexity(complexity - baseComplexity)}
                 | [ ScriptTransfer(to, -complexInt, unit) ]
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-558 If a transaction issues a token with invalid name",
          "Invalid asset name",
          mkScriptWithOneAction("""Issue("n", "Test token", 1, 2, true)""")
        ),
        Case(
          "NODE-560 If a transaction issues a token with invalid description",
          "Invalid asset description",
          mkScriptWithOneAction(s"""Issue("Token", "${"a" * (IssueTransaction.MaxAssetDescriptionLength + 1)}", 1, 2, true)""")
        ),
        Case(
          "NODE-566 If a transaction sends a ScriptTransfer with invalid assetId",
          "invalid asset ID",
          { complexity =>
            // 1 on Address, 1 on ScriptTransfer, 1 on a list construction
            val baseComplexity = 1 + 1 + 1
            mkV6Script(
              s""" let to = Address(base58'$secondSignedAddr')
                 | let complexInt = ${mkExprWithComplexity(complexity - baseComplexity)}
                 | [ ScriptTransfer(to, complexInt, base58'${Base58.encode("test".getBytes(StandardCharsets.UTF_8))}') ]
                 | """.stripMargin
            )
          }
        ),
        // TODO Can't create a test with invalid payment. Probably in node-it?
        // Case(
        //   "NODE-566 If a transaction contains a payment with invalid assetId",
        //   "invalid asset ID",
        //   { complexity =>
        //     // 1 on Address, 1 on ScriptTransfer, 1 on a list construction
        //     val baseComplexity = 1 + 1 + 1
        //     mkV6Script(
        //       s""" let to = Address(base58'${secondSigner.toAddress(chainId)}')
        //          | let x = ${mkExprWithComplexity(complexity - baseComplexity)}
        //          | [ ScriptTransfer(to, x, unit) ]
        //          | """.stripMargin
        //     )
        //   }
        // ),
        Case(
          "NODE-568 If a transaction sends a ScriptTransfer with unknown assetId",
          s"Transfer error: asset '$unknownAssetId' is not found on the blockchain",
          { complexity =>
            // 1 on Address, 1 on ScriptTransfer, 1 on a list construction
            val baseComplexity = 1 + 1 + 1
            mkV6Script(
              s""" let to = Address(base58'$secondSignedAddr')
                 | let complexInt = ${mkExprWithComplexity(complexity - baseComplexity)}
                 | [ ScriptTransfer(to, complexInt, base58'$unknownAssetId') ]
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-568 If a transaction sends an inner invoke with a payment with unknown assetId",
          s"Transfer error: asset '$unknownAssetId' is not found on the blockchain",
          { complexity =>
            // 1 on Address, 1 on ScriptTransfer, 1 on a list construction, 75 on invoke, 1 on throw (exactAs)
            val baseComplexity = 1 + 1 + 1 + 75 + 1
            TestCompiler(V6).compileContract(
              s""" {-#STDLIB_VERSION 6 #-}
                 | {-#SCRIPT_TYPE ACCOUNT #-}
                 | {-#CONTENT_TYPE DAPP #-}
                 |
                 | @Callable(inv)
                 | func foo() = {
                 |   strict res = invoke(this, "bar", [], [])
                 |   ([], res.exactAs[Int])
                 | }
                 |
                 | @Callable(inv)
                 | func bar() = {
                 |   let to = Address(base58'$secondSignedAddr')
                 |   let complexInt = ${mkExprWithComplexity(complexity - baseComplexity)}
                 |   ([ ScriptTransfer(to, complexInt, base58'$unknownAssetId') ], 1)
                 | }
                 | """.stripMargin
            )
          },
          invokeTx
        ),
        Case(
          s"NODE-576 If a transaction sends a Burn with unknown assetId",
          "Referenced assetId not found",
          mkScriptWithOneAction(s"Burn(base58'$unknownAssetId', 1)")
        ),
        Case(
          s"NODE-576 If a transaction sends a Reissue with unknown assetId",
          "Referenced assetId not found",
          mkScriptWithOneAction(s"Reissue(base58'$unknownAssetId', 1, true)")
        ),
        Case(
          s"NODE-576 If a transaction sends a SponsorFee with unknown assetId",
          "was not issued from address of current dApp",
          mkScriptWithOneAction(s"SponsorFee(base58'$unknownAssetId', 1)")
        )
      ) ++ Seq(
        "ScriptTransfer" -> "ScriptTransfer(to, complexInt, unit)",
        "Lease"          -> "Lease(to, complexInt)"
      ).map { case (actionType, actionSrc) =>
        Case(
          s"NODE-578 If a transaction sends a $actionType with address of another network",
          "Address belongs to another network",
          { complexity =>
            // 1 on Address, 1 on action, 1 on a list construction
            val baseComplexity = 1 + 1 + 1
            mkV6Script(
              s""" let to = Address(base58'${secondSigner.toAddress(otherChainId)}')
                 | let complexInt = ${mkExprWithComplexity(complexity - baseComplexity)}
                 | [ $actionSrc ]
                 | """.stripMargin
            )
          }
        )
      } ++ Seq(
        Case(
          "NODE-578 If a transaction sends an invoke with address of another network",
          "Address belongs to another network",
          { complexity =>
            // 1 on Address, 75 on invoke, 1 on throw (exactAs)
            val baseComplexity = 1 + 75 + 1
            mkV6Script(
              s""" let to = Address(base58'${secondSigner.toAddress(otherChainId)}')
                 | let complexInt = ${mkExprWithComplexity(complexity - baseComplexity)}
                 | strict res = invoke(to, "bar", [complexInt], [])
                 | ([], res.exactAs[Int])
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-580 If a transaction invokes a script with invalid address",
          "Wrong addressBytes length",
          { complexity =>
            // 1 on Address, 1 on tuple, 1 on a list construction, 1 on ScriptTransfer
            val baseComplexity = 1 + 1 + 1 + 1
            mkV6Script(
              s""" let to = Address(base58'${secondSignedAddr.toString.take(5)}')
                 | let complexInt = ${mkExprWithComplexity(complexity - baseComplexity)}
                 | ([ScriptTransfer(to, 1, unit)], complexInt)
                 | """.stripMargin
            )
          }
        )
      ) ++ Seq(
        ("negative", -1, "Negative lease amount = -1"),
        ("zero", 0, "NonPositiveAmount(0,waves)")
      ).map { case (tpe, leaseAmount, rejectError) =>
        Case(
          s"NODE-584 If a transaction does a $tpe leasing",
          rejectError,
          { complexity =>
            // 1 on Address, 1 on tuple, 1 on a list construction, 1 on Lease
            val baseComplexity = 1 + 1 + 1 + 1
            mkV6Script(
              s""" let to = Address(base58'$secondSignedAddr')
                 | let complexInt = ${mkExprWithComplexity(complexity - baseComplexity)}
                 | ([Lease(to, $leaseAmount)], complexInt)
                 | """.stripMargin
            )
          }
        )
      }

    // NODE-562 После активации Ride V6 при установке спонсорства для чужого ассета транзакция отклоняется до 1000 комплексити и фейлится после
    // NODE-564 После активации Ride V6 при установке спонсорства для смарт-ассета транзакция отклоняется до 1000 комплексити и фейлится после
    // NODE-570 После активации Ride V6 при переполнении в Reissue транзакция отклоняется до 1000 комплексити и фейлится после
    // NODE-572 После активации Ride V6 при попытке Reissue ассета с reissuable=false транзакция отклоняется до 1000 комплексити и фейлится после
    // NODE-574 После активации Ride V6 при попытке Reissue ассета, выпущенного другим адресом, транзакция отклоняется до 1000 комплексити и фейлится после

    // NODE-586 После активации Ride V6 при лизинге самому себе транзакция отклоняется до 1000 комплексити и фейлится после
    // NODE-588 После активации Ride V6 при создании лизинга с существующим leaseId транзакция отклоняется до 1000 комплексити и фейлится после
    // NODE-590 После активации Ride V6 при попытке лизинга средств, отсутствующих на балансе, транзакция отклоняется до 1000 комплексити и фейлится после
    // NODE-592 После активации Ride V6 при отмене лизинга с leaseId невалидной длины транзакция отклоняется до 1000 комплексити и фейлится после
    // NODE-594 После активации Ride V6 при отмене несуществующего лизинга транзакция отклоняется до 1000 комплексити и фейлится после
    // NODE-596 После активации Ride V6 при отмене уже отмененного лизинга транзакция отклоняется до 1000 комплексити и фейлится после
    // NODE-598 После активации Ride V6 при попытке отмены чужого лизинга транзакция отклоняется до 1000 комплексити и фейлится после
    // NODE-600 После активации Ride V6 если верификатор вернул не boolean, транзакция отклоняется до 1000 комплексити и фейлится после
    // NODE-604 После активации Ride V6 при ошибке в скрипте ассета транзакция отклоняется до 1000 комплексити и фейлится после
    // NODE-606 После активации Ride V6 при отрицательном вложенном пэйменте транзакция отклоняется до 1000 комплексити и фейлится после
    // NODE-608 После активации Ride V6 при попытке выпустить ассет с существующим id транзакция отклоняется до 1000 комплексити и фейлится после
    // NODE-610 После активации Ride V6 при отрицательном Reissue транзакция отклоняется до 1000 комплексити и фейлится после
    // NODE-612 После активации Ride V6 при отрицательном Burn транзакция отклоняется до 1000 комплексити и фейлится после
    // NODE-614 После активации Ride V6 при отрицательном ScriptTransfer (Negative transfer amount = ${t.amount}) транзакция отклоняется до 1000 комплексити и фейлится после
    // NODE-616 После активации Ride V6 при отрицательном Lease транзакция отклоняется до 1000 комплексити и фейлится после
    // NODE-618 После активации Ride V6 при отрицательном спонсировании транзакция отклоняется до 1000 комплексити и фейлится после
    // NODE-620 После активации Ride V6 при возникновении промежуточного отрицательного баланса транзакция отклоняется до 1000 комплексити и фейлится после
    // NODE-698 После активации Ride V6 при invoke адреса без установленного скрипта dapp транзакция отклоняется до 1000 комплексити и фейлится после

    // ??? NODE-556 После активации Ride V6 при переполнении в наборе ScriptTransfer в одном массиве экшенов транзакция отклоняется до 1000 комплексити и фейлится после

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

  private def mkScriptWithOneAction(actionSrc: String): Int => Script = { targetComplexity =>
    // 1 on tuple, 1 on action, 1 on a list construction
    val baseComplexity = 1 + 1 + 1
    mkV6Script(
      s""" let complexInt = ${mkExprWithComplexity(targetComplexity - baseComplexity)}
         | ([ $actionSrc ], complexInt)
         | """.stripMargin
    )
  }

  private def mkV3Script(funBody: String): Script = mkScript(V3, funBody)
  private def mkV6Script(funBody: String): Script = mkScript(V6, funBody)
  private def mkScript(v: StdLibVersion, funBody: String): Script =
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

  private def mkExprWithComplexity(complexity: Int): String = s"1${" + 1" * complexity}"

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

  private def invokeTxWithPayment(amount: Long, assetId: Asset): InvokeScriptTransaction =
    invokeTx
      .copy(payments = Seq(InvokeScriptTransaction.Payment(amount, assetId)))
      .signWith(defaultSigner.privateKey)
}
