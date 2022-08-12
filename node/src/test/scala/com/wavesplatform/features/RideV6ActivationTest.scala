package com.wavesplatform.features

import com.wavesplatform.common.utils.{Base58, Base64}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers.{defaultSigner, secondSigner}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.{Asset, Transaction, TxHelpers}
import org.scalatest.OptionValues

import java.nio.charset.StandardCharsets

// TODO Version of contracts
class RideV6ActivationTest extends FreeSpec with WithDomain with OptionValues {
  private val chainId      = DomainPresets.SettingsFromDefaultConfig.blockchainSettings.addressSchemeCharacter.toByte
  private val otherChainId = (chainId + 1).toByte

  private val regularAssetTx       = TxHelpers.issue(amount = Long.MaxValue - 1)
  private val notReIssuableAssetTx = TxHelpers.issue(amount = 100, reissuable = false)
  private val smartAssetTx = TxHelpers.issue(
    script = Some(
      TestCompiler(V5).compileAsset(
        s""" {-# STDLIB_VERSION 5 #-}
           | {-# CONTENT_TYPE EXPRESSION #-}
           | {-# SCRIPT_TYPE ASSET #-}
           |
           | true
           |""".stripMargin
      )
    )
  )

  private val leasingTx = TxHelpers.lease()
  private val leasingId = leasingTx.id()

  private val secondSignedAddr = secondSigner.toAddress(chainId)

  private val secondSignerAssetTx = TxHelpers.issue(issuer = secondSigner)
  private val secondSignerAssetId = secondSignerAssetTx.id()

  private val secondSignerLeasingTx = TxHelpers.lease(sender = secondSigner, recipient = defaultSigner.toAddress(chainId))
  private val secondSignerLeasingId = secondSignerLeasingTx.id()

  "After RideV6 activation" - {
    val cases = Seq(
      Case(
        "NODE-540 If a transaction exceeds the limit of writes number",
        "Stored data count limit is exceeded",
        { targetComplexity =>
          val baseComplexity = 101 + 101 // 101 on IntegerEntry and 101 on a list construction
          mkV6Script(
            s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
               | ${(1 to 101).map(i => s"""IntegerEntry("i$i", complexInt)""").mkString("[", ", ", "]")}
               | """.stripMargin
          )
        }
      ),
      Case(
        "NODE-542 If a transaction exceeds the limit of writes size",
        "WriteSet size can't exceed 5120 bytes, actual: 5121 bytes",
        { targetComplexity =>
          val baseComplexity = 2 + 2 // 2 on BinaryEntry and IntegerEntry and 101 on a list construction
          // See DataTxValidator.realUserPayloadSize: IntegerDataEntry - 8, "b" and "i" keys - 2
          val limitedBinaryEntrySize = ContractLimits.MaxWriteSetSizeInBytes - 8 - 2
          mkV6Script(
            s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
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
        { targetComplexity =>
          val baseComplexity = 2 * 101 + 101 + 1 // 2*101 on DataEntry and 101 on a list construction and 1 for WriteSet
          mkV3Script(
            s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
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
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 2 // 1 on IntegerEntry, 1 on other entry and 2 on a list construction
            mkV6Script(
              s"""
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | [ IntegerEntry("k", complexInt), $entry ]
                 |""".stripMargin
            )
          }
        )
      } ++
      Seq(
        "ScriptTransfer" -> "ScriptTransfer(to, 1, unit)",
        "Lease"          -> "Lease(to, 1)",
        "LeaseCancel"    -> s"LeaseCancel(base58'$secondSignerLeasingId')"
      ).map { case (actionType, v) =>
        Case(
          s"NODE-548 If a transaction exceeds the limit of $actionType actions",
          "ScriptTransfer, Lease, LeaseCancel actions count limit is exceeded",
          { targetComplexity =>
            // 1 for Address, 101 on actions, 101 on a list construction, 10 on getInteger,
            // 2 on valueOrElse, and 1 on "+" and 1 for what? TODO
            val baseComplexity = 1 + 101 + 101 + 10 + 2 + 1 + 1
            mkV6Script(
              s""" let to = Address(base58'$secondSignedAddr')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
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
          { targetComplexity =>
            val baseComplexity = 1 + 101 + 101 // 1 for Address, 101 on actions and 101 on a list construction
            mkV6Script(
              s""" let to = Address(base58'$secondSignedAddr')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | [
                 |   ${(1 to 34).map(_ => s"""ScriptTransfer(to, complexInt, unit)""").mkString(", ")},
                 |   ${(1 to 34).map(_ => s"""Lease(to, 1)""").mkString(", ")},
                 |   ${(1 to 33).map(_ => s"""LeaseCancel(base58'$secondSignerLeasingId')""").mkString(", ")}
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
          "NODE-554 If a transaction sends a ScriptTransfer with negative amount",
          "Negative transfer amount",
          { targetComplexity =>
            // 1 on Address, 1 on ScriptTransfer, 1 on a list construction and 1 for "-complexInt"
            val baseComplexity = 1 + 1 + 1 + 1
            mkV6Script(
              s""" let to = Address(base58'$secondSignedAddr')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
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
          "NODE-562 If a script tries to set a sponsor fee for another's account asset",
          "was not issued from address of current dApp",
          { targetComplexity =>
            // 1 on tuple, 1 on SponsorFee, 1 on list
            val baseComplexity = 1 + 1 + 1
            mkV6Script(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([SponsorFee(base58'$secondSignerAssetId', 1)], complexInt)
                 | """.stripMargin
            )
          },
          knownTxs = Seq(secondSignerAssetTx)
        ),
        Case(
          "NODE-564 If a script tries to set a sponsor fee for smart asset",
          "Sponsorship smart assets is disabled",
          { targetComplexity =>
            // 1 on tuple, 1 on SponsorFee, 1 on list
            val baseComplexity = 1 + 1 + 1
            mkV6Script(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([SponsorFee(base58'${smartAssetTx.id()}', 1)], complexInt)
                 | """.stripMargin
            )
          },
          knownTxs = Seq(smartAssetTx)
        ),
        Case(
          "NODE-566 If a transaction sends a ScriptTransfer with invalid assetId",
          "invalid asset ID",
          { targetComplexity =>
            // 1 on Address, 1 on ScriptTransfer, 1 on a list construction
            val baseComplexity = 1 + 1 + 1
            mkV6Script(
              s""" let to = Address(base58'$secondSignedAddr')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | [ ScriptTransfer(to, complexInt, base58'${Base58.encode("test".getBytes(StandardCharsets.UTF_8))}') ]
                 | """.stripMargin
            )
          }
        ),
        // TODO Can't create a test with invalid payment. Probably in node-it?
        // Case(
        //   "NODE-566 If a transaction contains a payment with invalid assetId",
        //   "invalid asset ID",
        //   { targetComplexity =>
        //     // 1 on Address, 1 on ScriptTransfer, 1 on a list construction
        //     val baseComplexity = 1 + 1 + 1
        //     mkV6Script(
        //       s""" let to = Address(base58'$secondSignedAddr')
        //          | let x = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
        //          | [ ScriptTransfer(to, x, unit) ]
        //          | """.stripMargin
        //     )
        //   }
        // ),
        Case(
          "NODE-568 If a transaction sends a ScriptTransfer with unknown assetId",
          s"Transfer error: asset '$secondSignerAssetId' is not found on the blockchain",
          { targetComplexity =>
            // 1 on Address, 1 on ScriptTransfer, 1 on a list construction
            val baseComplexity = 1 + 1 + 1
            mkV6Script(
              s""" let to = Address(base58'$secondSignedAddr')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | [ ScriptTransfer(to, complexInt, base58'$secondSignerAssetId') ]
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-568 If a transaction sends an inner invoke with a payment with unknown assetId",
          s"Transfer error: asset '$secondSignerAssetId' is not found on the blockchain",
          { targetComplexity =>
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
                 |   let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 |   ([ ScriptTransfer(to, complexInt, base58'$secondSignerAssetId') ], 1)
                 | }
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-570 If a script tries to overflow the amount of assets through Reissue",
          "Asset total value overflow",
          { targetComplexity =>
            // 1 on tuple, 1 on Reissue, 1 on list
            val baseComplexity = 1 + 1 + 1
            mkV6Script(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([Reissue(base58'${regularAssetTx.id()}', 2, true)], complexInt) # Amount will be (Long.MaxValue - 1) + 2
                 | """.stripMargin
            )
          },
          knownTxs = Seq(regularAssetTx)
        ),
        Case(
          "NODE-572 If a script tries to reissue a not reissuable asset",
          "Asset is not reissuable",
          { targetComplexity =>
            // 1 on tuple, 1 on Reissue, 1 on list
            val baseComplexity = 1 + 1 + 1
            mkV6Script(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([Reissue(base58'${notReIssuableAssetTx.id()}', 1, true)], complexInt)
                 | """.stripMargin
            )
          },
          knownTxs = Seq(notReIssuableAssetTx)
        ),
        Case(
          "NODE-574 If a script tries to reissue an asset issued by other address",
          "Asset was issued by other address",
          { targetComplexity =>
            // 1 on tuple, 1 on Reissue, 1 on list
            val baseComplexity = 1 + 1 + 1
            mkV6Script(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([Reissue(base58'$secondSignerAssetId', 1, true)], complexInt)
                 | """.stripMargin
            )
          },
          knownTxs = Seq(secondSignerAssetTx)
        ),
        Case(
          "NODE-576 If a transaction sends a Burn with unknown assetId",
          "Referenced assetId not found",
          mkScriptWithOneAction(s"Burn(base58'$secondSignerAssetId', 1)")
        ),
        Case(
          "NODE-576 If a transaction sends a Reissue with unknown assetId",
          "Referenced assetId not found",
          mkScriptWithOneAction(s"Reissue(base58'$secondSignerAssetId', 1, true)")
        ),
        Case(
          "NODE-576 If a transaction sends a SponsorFee with unknown assetId",
          "was not issued from address of current dApp",
          mkScriptWithOneAction(s"SponsorFee(base58'$secondSignerAssetId', 1)")
        )
      ) ++ Seq(
        "ScriptTransfer" -> "ScriptTransfer(to, complexInt, unit)",
        "Lease"          -> "Lease(to, complexInt)"
      ).map { case (actionType, actionSrc) =>
        Case(
          s"NODE-578 If a transaction sends a $actionType with address of another network",
          "Address belongs to another network",
          { targetComplexity =>
            // 1 on Address, 1 on action, 1 on a list construction
            val baseComplexity = 1 + 1 + 1
            mkV6Script(
              s""" let to = Address(base58'${secondSigner.toAddress(otherChainId)}')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | [ $actionSrc ]
                 | """.stripMargin
            )
          }
        )
      } ++ Seq(
        Case(
          "NODE-578 If a transaction sends an invoke with address of another network",
          "Address belongs to another network",
          { targetComplexity =>
            // 1 on Address, 75 on invoke, 1 on throw (exactAs)
            val baseComplexity = 1 + 75 + 1
            mkV6Script(
              s""" let to = Address(base58'${secondSigner.toAddress(otherChainId)}')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | strict res = invoke(to, "bar", [complexInt], [])
                 | ([], res.exactAs[Int])
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-580 If a transaction invokes a script with invalid address",
          "Wrong addressBytes length",
          { targetComplexity =>
            // 1 on Address, 1 on tuple, 1 on a list construction, 1 on ScriptTransfer
            val baseComplexity = 1 + 1 + 1 + 1
            mkV6Script(
              s""" let to = Address(base58'${secondSignedAddr.toString.take(5)}')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
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
          { targetComplexity =>
            // 1 on Address, 1 on tuple, 1 on a list construction, 1 on Lease
            val baseComplexity = 1 + 1 + 1 + 1
            mkV6Script(
              s""" let to = Address(base58'$secondSignedAddr')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([Lease(to, $leaseAmount)], complexInt)
                 | """.stripMargin
            )
          }
        )
      } ++ Seq(
        Case(
          "NODE-586 If a transaction does a self-leasing",
          "Cannot lease to self",
          { targetComplexity =>
            // 1 on tuple, 1 on a list construction, 1 on Lease
            val baseComplexity = 1 + 1 + 1
            mkV6Script(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([Lease(this, 1)], complexInt)
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-588 If a transaction does same leasing multiple times",
          "is already in the state",
          { targetComplexity =>
            // 1 on Address, 1 on tuple, 1+1 on a list construction, 1+1 on Lease
            val baseComplexity = 1 + 1 + 1 + 1 + 1 + 1
            mkV6Script(
              s""" let to = Address(base58'$secondSignedAddr')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([Lease(to, 1, 3), Lease(to, 1, 3)], complexInt) # Leasing id is a hash of its data and invoke tx id
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-596 If a transaction tries to cancel a leasing twice in a script",
          "Duplicate LeaseCancel id(s)",
          { targetComplexity =>
            // 1 on tuple, 1+1 on a list construction, 1+1 on LeaseCancel
            val baseComplexity = 1 + 1 + 1 + 1 + 1
            mkV6Script(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([LeaseCancel(base58'$leasingId'), LeaseCancel(base58'$leasingId')], complexInt)
                 | """.stripMargin
            )
          },
          knownTxs = Seq(leasingTx)
        ),
        Case(
          "NODE-596 If a transaction in a script tries to cancel already cancelled leasing",
          "Cannot cancel already cancelled lease",
          { targetComplexity =>
            // 1 on tuple, 1 on a list construction, 1 on LeaseCancel
            val baseComplexity = 1 + 1 + 1
            mkV6Script(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([LeaseCancel(base58'$leasingId')], complexInt)
                 | """.stripMargin
            )
          },
          knownTxs = Seq(leasingTx, TxHelpers.leaseCancel(leasingId))
        ),
        Case(
          "NODE-598 If a transaction tries to cancel another account leasing",
          "LeaseTransaction was leased by other sender and time",
          { targetComplexity =>
            // 1 on tuple, 1 on a list construction, 1 on LeaseCancel
            val baseComplexity = 1 + 1 + 1
            mkV6Script(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([LeaseCancel(base58'$secondSignerLeasingId')], complexInt)
                 | """.stripMargin
            )
          },
          knownTxs = Seq(secondSignerLeasingTx)
        ),
        Case(
          "NODE-590 If a transaction does a leasing with nonexistent funds",
          "Cannot lease more than own",
          // TODO ENOUGH_AMT + 1
          { targetComplexity =>
            // 1 on Address, 1 on tuple, 1 on a list construction, 1 on Lease
            val baseComplexity = 1 + 1 + 1 + 1
            mkV6Script(
              s""" let to = Address(base58'$secondSignedAddr')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([Lease(to, ${ENOUGH_AMT * 2})], complexInt)
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-592 If a transaction cancels a leasing with wrong lease id",
          s"Lease id=${secondSignerLeasingId.toString.take(5)} has invalid length",
          { targetComplexity =>
            // 1 on tuple, 1 on a list construction, 1 on LeaseCancel
            val baseComplexity = 1 + 1 + 1
            mkV6Script(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([LeaseCancel(base58'${secondSignerLeasingId.toString.take(5)}')], complexInt)
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-594 If a transaction cancels an unknown leasing",
          s"Lease with id=$secondSignerLeasingId not found",
          { targetComplexity =>
            // 1 on tuple, 1 on a list construction, 1 on LeaseCancel
            val baseComplexity = 1 + 1 + 1
            mkV6Script(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([LeaseCancel(base58'$secondSignerLeasingId')], complexInt)
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-604 If a transaction failed by a script",
          "test error",
          { targetComplexity =>
            // 1 on compare, 1 on tuple
            val baseComplexity = 1 + 1
            mkV6Script(
              s""" let to = Address(base58'$secondSignedAddr')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | if (complexInt > 0) then {
                 |   throw("test error")
                 | } else {
                 |   ([], 1)
                 | }
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-608 If a script tries to issue the same asset multiple times",
          "is already issued",
          { targetComplexity =>
            // 1 on tuple, 1+1 on Issue, 1+1 on list
            val baseComplexity = 1 + 1 + 1 + 1 + 1
            mkV6Script(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([Issue("bucks", "Test token", 1, 2, true), Issue("bucks", "Test token", 1, 2, true)], complexInt)
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-620 If a negative balance happens during invoke",
          "negative waves balance",
          { targetComplexity =>
            // 75 for invoke, 1 for Address, 1 for list, 500 for secondSigner.foo() call
            val baseComplexity = 75 + 1 + 1 + 500
            TestCompiler(V6).compileContract(
              s""" {-#STDLIB_VERSION 6 #-}
                 | {-#SCRIPT_TYPE ACCOUNT #-}
                 | {-#CONTENT_TYPE DAPP #-}
                 |
                 | @Callable(inv)
                 | func foo() = {
                 |   let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 |   strict res = invoke(
                 |     Address(base58'$secondSignedAddr'), 
                 |     "bar",
                 |     [ complexInt ],
                 |     [ ]
                 |   )
                 |   ([], res.exactAs[Int])
                 | }
                 | """.stripMargin
            )
          },
          knownTxs = Seq(
            TxHelpers.setScript(
              secondSigner, {
                // 1 for Address, 1 for tuple, 1 for a list construction 1 for ScriptTransfer
                val baseComplexity = 1 + 1 + 1 + 1
                TestCompiler(V6).compileContract(
                  s""" {-#STDLIB_VERSION 6 #-}
                     | {-#SCRIPT_TYPE ACCOUNT #-}
                     | {-#CONTENT_TYPE DAPP #-}
                     |
                     | @Callable(inv)
                     | func bar(n: Int) = {
                     |   let to = Address(base58'${defaultSigner.toAddress(chainId)}')
                     |   (
                     |     [ScriptTransfer(to, ${ENOUGH_AMT + 100}, unit)],
                     |     ${mkIntExprWithComplexity(500 - baseComplexity)}
                     |   )
                     | }
                     | """.stripMargin
                )
              }
            )
          )
        )
      )

    cases.foreach { testCase =>
      testCase.title - {
        "<=1000 complexity - rejected" - Seq(110, ContractLimits.FailFreeInvokeComplexity).foreach { complexity =>
          s"complexity = $complexity" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(defaultSigner, secondSigner)) { d =>
            val setScriptTx = TxHelpers.setScript(defaultSigner, testCase.mkDApp(complexity), 1.waves)
            d.appendBlock((testCase.knownTxs :+ setScriptTx)*)
            d.createDiffE(testCase.invokeTx) should produce(testCase.rejectError)
          }
        }
        ">1000 complexity - failed" in {
          withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(defaultSigner, secondSigner)) { d =>
            val complexity  = ContractLimits.FailFreeInvokeComplexity + 1
            val setScriptTx = TxHelpers.setScript(defaultSigner, testCase.mkDApp(complexity), 1.waves)
            d.appendBlock((testCase.knownTxs :+ setScriptTx)*)
            d.appendBlock(testCase.invokeTx)
            val invokeTxMeta = d.transactionsApi.transactionById(testCase.invokeTx.id()).value
            invokeTxMeta.spentComplexity shouldBe complexity
            invokeTxMeta.succeeded shouldBe false
          }
        }
      }
    }
  }

  private case class Case(
      title: String,
      rejectError: String,
      mkDApp: Int => Script,
      invokeTx: InvokeScriptTransaction = invokeTx,
      knownTxs: Seq[Transaction] = Seq.empty
  )

  private def mkScriptWithOneAction(actionSrc: String): Int => Script = { targetComplexity =>
    // 1 on tuple, 1 on action, 1 on a list construction
    val baseComplexity = 1 + 1 + 1
    mkV6Script(
      s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
         | ([ $actionSrc ], complexInt)
         | """.stripMargin
    )
  }

  private def mkV3Script(fooBody: String): Script = mkScript(V3, fooBody)
  private def mkV6Script(fooBody: String): Script = mkScript(V6, fooBody)
  private def mkScript(v: StdLibVersion, fooBody: String): Script =
    TestCompiler(v).compileContract(
      s""" {-#STDLIB_VERSION ${v.id} #-}
         | {-#SCRIPT_TYPE ACCOUNT #-}
         | {-#CONTENT_TYPE DAPP #-}
         |
         | @Callable(inv)
         | func foo() = {
         |   $fooBody
         | }
      """.stripMargin
    )

  private def mkIntExprWithComplexity(targetComplexity: Int): String = s"1${" + 1" * targetComplexity}"

  private lazy val invokeTx = TxHelpers.invoke(
    dApp = defaultSigner.toAddress(chainId),
    func = Some("foo"),
    fee = 3.waves
  )

  private def invokeTxWithPayment(amount: Long, assetId: Asset): InvokeScriptTransaction =
    invokeTx
      .copy(payments = Seq(InvokeScriptTransaction.Payment(amount, assetId)))
      .signWith(defaultSigner.privateKey)
}
