package com.wavesplatform.features

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.{Transaction, TxHelpers}
import org.scalatest.OptionValues

import java.nio.charset.StandardCharsets

// TODO Version of contracts
class RideV6ActivationTest extends FreeSpec with WithDomain with OptionValues {
  private val chainId      = DomainPresets.SettingsFromDefaultConfig.blockchainSettings.addressSchemeCharacter.toByte
  private val otherChainId = (chainId + 1).toByte

  private val invalidAssetId = IssuedAsset(ByteStr(("1" * 5).getBytes(StandardCharsets.UTF_8)))

  private val alice     = TxHelpers.signer(1) // signer(0) forges blocks and this affects the balance
  private val aliceAddr = alice.toAddress(chainId)

  private val bob               = TxHelpers.signer(2)
  private val bobAddr           = bob.toAddress(chainId)
  private val bobOtherChainAddr = bob.toAddress(otherChainId)

  private val aliceRegularAssetTx       = TxHelpers.issue(issuer = alice, amount = Long.MaxValue - 1)
  private val aliceNotReIssuableAssetTx = TxHelpers.issue(issuer = alice, amount = 100, reissuable = false)
  private val aliceSmartAssetTx = TxHelpers.issue(
    issuer = alice,
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
  private val aliceLeasingTx = TxHelpers.lease(sender = alice, recipient = bobAddr)
  private val aliceLeasingId = aliceLeasingTx.id()
  private val aliceInvokeTx = TxHelpers.invoke(
    dApp = aliceAddr,
    invoker = alice,
    func = Some("foo"),
    fee = 3.waves
  )

  private val bobAssetTx   = TxHelpers.issue(issuer = bob)
  private val bobAssetId   = bobAssetTx.id()
  private val bobLeasingTx = TxHelpers.lease(sender = bob, recipient = aliceAddr)
  private val bobLeasingId = bobLeasingTx.id()

  "After RideV6 activation" - {
    val cases = Seq(
      Case(
        "NODE-540 If a transaction exceeds the limit of writes number",
        "Stored data count limit is exceeded",
        { targetComplexity =>
          val baseComplexity = 101 + 101 // 101 for list, 101 for IntegerEntry
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
          val baseComplexity = 2 + 1 + 1 // 2 for list, 1 for BinaryEntry, 1 for IntegerEntry
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
          val baseComplexity = 101 + 2 * 101 + 1 // 101 for list, 2 * 101 for DataEntry, 1 for WriteSet
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
            val baseComplexity = 2 + 1 + 1 // 2 for list (two elements), 1 for IntegerEntry, 1 for test entry
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
        "LeaseCancel"    -> s"LeaseCancel(base58'$bobLeasingId')"
      ).map { case (actionType, v) =>
        Case(
          s"NODE-548 If a transaction exceeds the limit of $actionType actions",
          "ScriptTransfer, Lease, LeaseCancel actions count limit is exceeded",
          { targetComplexity =>
            // 1 for Address, 1 for tuple, 101 for list, 101 for actions, 10 for getInteger, 2 for valueOrElse, 1 for "+"
            val baseComplexity = 1 + 1 + 101 + 101 + 10 + 2 + 1
            mkV6Script(
              s""" let to = Address(base58'$bobAddr')
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
            val baseComplexity = 1 + 101 + 101 // 1 for Address, 101 for list, 101 for actions
            mkV6Script(
              s""" let to = Address(base58'$bobAddr')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | [
                 |   ${(1 to 34).map(_ => s"""ScriptTransfer(to, complexInt, unit)""").mkString(", ")},
                 |   ${(1 to 34).map(_ => s"""Lease(to, 1)""").mkString(", ")},
                 |   ${(1 to 33).map(_ => s"""LeaseCancel(base58'$bobLeasingId')""").mkString(", ")}
                 | ]
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-550 If a transaction has a self-payment",
          "DApp self-payment is forbidden since V4",
          mkScriptWithOneAction("""IntegerEntry("i", 1)"""),
          // self-payment, because this is a self-invoke
          invokeTx = aliceInvokeTx
            .copy(payments = Seq(InvokeScriptTransaction.Payment(1, Waves)))
            .signWith(alice.privateKey)
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
            val baseComplexity = 1 + 1 + 1 + 1 // 1 for Address, 1 for list, 1 for ScriptTransfer, 1 for "-complexInt"
            mkV6Script(
              s""" let to = Address(base58'$bobAddr')
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
            val baseComplexity = 1 + 1 + 1 // 1 for tuple, 1 for list, 1 for SponsorFee
            mkV6Script(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([SponsorFee(base58'$bobAssetId', 1)], complexInt)
                 | """.stripMargin
            )
          },
          knownTxs = Seq(bobAssetTx)
        ),
        Case(
          "NODE-564 If a script tries to set a sponsor fee for smart asset",
          "Sponsorship smart assets is disabled",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 // 1 for tuple, 1 for list, 1 for SponsorFee
            mkV6Script(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([SponsorFee(base58'${aliceSmartAssetTx.id()}', 1)], complexInt)
                 | """.stripMargin
            )
          },
          knownTxs = Seq(aliceSmartAssetTx)
        ),
        Case(
          "NODE-566 If a transaction sends a ScriptTransfer with invalid assetId",
          "invalid asset ID",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 // 1 for Address, 1 for list, 1 for ScriptTransfer
            mkV6Script(
              s""" let to = Address(base58'$bobAddr')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | [ ScriptTransfer(to, complexInt, base58'$invalidAssetId') ]
                 | """.stripMargin
            )
          }
        ),
        mkInnerPayment(
          "NODE-566 If a transaction contains an inner payment with invalid assetId",
          s"invalid asset ID '$invalidAssetId'",
          s"AttachedPayment(base58'$invalidAssetId', 1)"
        ),
        Case(
          "NODE-568 If a transaction sends a ScriptTransfer with unknown assetId",
          s"Transfer error: asset '$bobAssetId' is not found on the blockchain",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 // 1 for Address, 1 for list, 1 for ScriptTransfer
            mkV6Script(
              s""" let to = Address(base58'$bobAddr')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | [ ScriptTransfer(to, complexInt, base58'$bobAssetId') ]
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-568 If a transaction sends an inner invoke with a payment with unknown assetId",
          s"Transfer error: asset '$bobAssetId' is not found on the blockchain",
          { targetComplexity =>
            // bar: 1 for Address, 1 for tuple, 1 for list, 1 for ScriptTransfer
            // foo: 75 for invoke
            val baseComplexity = 1 + 1 + 1 + 1 + 75
            TestCompiler(V6).compileContract(
              s""" {-#STDLIB_VERSION 6 #-}
                 | {-#SCRIPT_TYPE ACCOUNT #-}
                 | {-#CONTENT_TYPE DAPP #-}
                 | 
                 | @Callable(inv)
                 | func bar() = {
                 |   let to = Address(base58'$bobAddr')
                 |   let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 |   ([ ScriptTransfer(to, complexInt, base58'$bobAssetId') ], 1)
                 | }
                 |
                 | @Callable(inv)
                 | func foo() = {
                 |   strict res = invoke(this, "bar", [], [])
                 |   ([], res.exactAs[Int])
                 | }
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-570 If a script tries to overflow the amount of assets through Reissue",
          "Asset total value overflow",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 // 1 for tuple, 1 for list, 1 for Reissue
            mkV6Script(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([Reissue(base58'${aliceRegularAssetTx.id()}', 2, true)], complexInt) # Amount will be (Long.MaxValue - 1) + 2
                 | """.stripMargin
            )
          },
          knownTxs = Seq(aliceRegularAssetTx)
        ),
        Case(
          "NODE-572 If a script tries to reissue a not reissuable asset",
          "Asset is not reissuable",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 // 1 for tuple, 1 for list, 1 for Reissue
            mkV6Script(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([Reissue(base58'${aliceNotReIssuableAssetTx.id()}', 1, true)], complexInt)
                 | """.stripMargin
            )
          },
          knownTxs = Seq(aliceNotReIssuableAssetTx)
        ),
        Case(
          "NODE-574 If a script tries to reissue an asset issued by other address",
          "Asset was issued by other address",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 // 1 for tuple, 1 for list, 1 for Reissue
            mkV6Script(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([Reissue(base58'$bobAssetId', 1, true)], complexInt)
                 | """.stripMargin
            )
          },
          knownTxs = Seq(bobAssetTx)
        ),
        Case(
          "NODE-576 If a transaction sends a Burn with unknown assetId",
          "Referenced assetId not found",
          mkScriptWithOneAction(s"Burn(base58'$bobAssetId', 1)")
        ),
        Case(
          "NODE-576 If a transaction sends a Reissue with unknown assetId",
          "Referenced assetId not found",
          mkScriptWithOneAction(s"Reissue(base58'$bobAssetId', 1, true)")
        ),
        Case(
          "NODE-576 If a transaction sends a SponsorFee with unknown assetId",
          "was not issued from address of current dApp",
          mkScriptWithOneAction(s"SponsorFee(base58'$bobAssetId', 1)")
        )
      ) ++ Seq(
        "ScriptTransfer" -> "ScriptTransfer(to, complexInt, unit)",
        "Lease"          -> "Lease(to, complexInt)"
      ).map { case (actionType, actionSrc) =>
        Case(
          s"NODE-578 If a transaction sends a $actionType with address of another network",
          "Address belongs to another network",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 // 1 for Address, 1 for list, 1 for action
            mkV6Script(
              s""" let to = Address(base58'$bobOtherChainAddr')
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
            val baseComplexity = 1 + 75 + 1 // 1 for Address, 75 for invoke, 1 for list ([complexInt])
            mkV6Script(
              s""" let to = Address(base58'$bobOtherChainAddr')
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
            val baseComplexity = 1 + 1 + 1 + 1 // 1 for Address, 1 for tuple, 1 for list, 1 for ScriptTransfer
            mkV6Script(
              s""" let to = Address(base58'${bobAddr.toString.take(5)}')
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
            val baseComplexity = 1 + 1 + 1 + 1 // 1 for Address, 1 for tuple, 1 for list, 1 for Lease
            mkV6Script(
              s""" let to = Address(base58'$bobAddr')
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
            val baseComplexity = 1 + 1 + 1 // 1 for tuple, 1 for list, 1 for Lease
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
            val baseComplexity = 1 + 1 + 2 + 1 + 1 // 1 for Address, 1 for tuple, 2 for list, 1+1 for Lease
            mkV6Script(
              s""" let to = Address(base58'$bobAddr')
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
            val baseComplexity = 1 + 2 + 1 // 1 for tuple, 2 for list, 1 for LeaseCancel
            mkV6Script(
              s""" let cancel = LeaseCancel(base58'$aliceLeasingId')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([cancel, cancel], complexInt)
                 | """.stripMargin
            )
          },
          knownTxs = Seq(aliceLeasingTx)
        ),
        Case(
          "NODE-596 If a transaction in a script tries to cancel already cancelled leasing",
          "Cannot cancel already cancelled lease",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 // 1 for tuple, 1 for list, 1 for LeaseCancel
            mkV6Script(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([LeaseCancel(base58'$aliceLeasingId')], complexInt)
                 | """.stripMargin
            )
          },
          knownTxs = Seq(aliceLeasingTx, TxHelpers.leaseCancel(aliceLeasingId, sender = alice))
        ),
        Case(
          "NODE-598 If a transaction tries to cancel another account leasing",
          "LeaseTransaction was leased by other sender and time",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 // 1 for tuple, 1 for list, 1 for LeaseCancel
            mkV6Script(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([LeaseCancel(base58'$bobLeasingId')], complexInt)
                 | """.stripMargin
            )
          },
          knownTxs = Seq(bobLeasingTx)
        ),
        Case(
          "NODE-590 If a transaction does a leasing with nonexistent funds",
          "Cannot lease more than own",
          { targetComplexity =>
            // 1 for Address(bob), 1 for tuple, 1 for list, 1 for Lease, 10 for wavesBalance, 1 for Address(alice),
            // and 1 for "+"
            val baseComplexity = 1 + 1 + 1 + 1 + 10 + 1 + 1
            mkV6Script(
              s""" let to = Address(base58'$bobAddr')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([Lease(to, wavesBalance(Address(base58'$aliceAddr')).available + 1)], complexInt)
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-592 If a transaction cancels a leasing with wrong lease id",
          s"Lease id=${bobLeasingId.toString.take(5)} has invalid length",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 // 1 for tuple, 1 for list, 1 for LeaseCancel
            mkV6Script(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([LeaseCancel(base58'${bobLeasingId.toString.take(5)}')], complexInt)
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-594 If a transaction cancels an unknown leasing",
          s"Lease with id=$bobLeasingId not found",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 // 1 for tuple, 1 for list, 1 for LeaseCancel
            mkV6Script(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([LeaseCancel(base58'$bobLeasingId')], complexInt)
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-604 If a transaction failed by a script",
          "test error",
          { targetComplexity =>
            val baseComplexity = 1 + 1 // 1 for compare, 1 for tuple / throw
            mkV6Script(
              s""" let to = Address(base58'$bobAddr')
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
        mkInnerPayment(
          "NODE-606 If an inner invoke contains a negative payment",
          "with attached WAVES amount = -1",
          "AttachedPayment(unit, -1)"
        ),
        Case(
          "NODE-608 If a script tries to issue the same asset multiple times",
          "is already issued",
          { targetComplexity =>
            val baseComplexity = 1 + 2 + 1 + 1 // 1 for tuple, 2 for list, 1+1 for Issue
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
            // 75 for invoke, 1 for Address, 1 for list, 500 for bob.foo() body
            val baseComplexity = 75 + 1 + 1 + 500
            TestCompiler(V6).compileContract(
              s""" {-#STDLIB_VERSION 6 #-}
                 | {-#SCRIPT_TYPE ACCOUNT #-}
                 | {-#CONTENT_TYPE DAPP #-}
                 |
                 | @Callable(inv)
                 | func foo() = {
                 |   let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 |   strict res = invoke(Address(base58'$bobAddr'), "bar", [ complexInt ], [])
                 |   ([], res.exactAs[Int])
                 | }
                 | """.stripMargin
            )
          },
          knownTxs = Seq(
            TxHelpers.setScript(
              bob, {
                // 1 for Address(alice), 1 for tuple, 1 for list, 1 for ScriptTransfer, 10 for wavesBalance,
                // 1 for Address(bob) and 1 for "+"
                val baseComplexity = 1 + 1 + 1 + 1 + 10 + 1 + 1
                TestCompiler(V6).compileContract(
                  s""" {-#STDLIB_VERSION 6 #-}
                     | {-#SCRIPT_TYPE ACCOUNT #-}
                     | {-#CONTENT_TYPE DAPP #-}
                     |
                     | @Callable(inv)
                     | func bar(n: Int) = {
                     |   let to = Address(base58'$aliceAddr')
                     |   (
                     |     [ScriptTransfer(to, wavesBalance(Address(base58'$bobAddr')).available + 1, unit)],
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
          s"complexity = $complexity" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(alice, bob)) { d =>
            val setScriptTx = TxHelpers.setScript(alice, testCase.mkDApp(complexity), 1.waves)
            d.appendBlock((testCase.knownTxs :+ setScriptTx)*)
            d.createDiffE(testCase.invokeTx) should produce(testCase.rejectError)
          }
        }
        ">1000 complexity - failed" in {
          withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(alice, bob)) { d =>
            val complexity  = ContractLimits.FailFreeInvokeComplexity + 1
            val setScriptTx = TxHelpers.setScript(alice, testCase.mkDApp(complexity), 1.waves)
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

  private def mkInnerPayment(title: String, rejectError: String, payment: String): Case = Case(
    title,
    rejectError,
    { targetComplexity =>
      // 75 for invoke, 1 for Address, 1 for list, 1 for second list, 1 for bob.foo() body
      val baseComplexity = 75 + 1 + 1 + 1 + 1
      TestCompiler(V6).compileContract(
        s""" {-#STDLIB_VERSION 6 #-}
           | {-#SCRIPT_TYPE ACCOUNT #-}
           | {-#CONTENT_TYPE DAPP #-}
           |
           | @Callable(inv)
           | func foo() = {
           |   let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
           |   strict res = invoke(
           |     Address(base58'$bobAddr'),
           |     "bar",
           |     [ complexInt ],
           |     [ $payment ]
           |   )
           |   ([], res.exactAs[Int])
           | }
           | """.stripMargin
      )
    },
    knownTxs = Seq(
      TxHelpers.setScript(
        bob, {
          val baseComplexity = 1 // 1 for tuple
          TestCompiler(V6).compileContract(
            s""" {-#STDLIB_VERSION 6 #-}
               | {-#SCRIPT_TYPE ACCOUNT #-}
               | {-#CONTENT_TYPE DAPP #-}
               |
               | @Callable(inv)
               | func bar(n: Int) = {
               |   ([], ${mkIntExprWithComplexity(500 - baseComplexity)})
               | }
               | """.stripMargin
          )
        }
      )
    )
  )

  /** @param mkDApp
    *   Int is the targetComplexity of the script
    */
  private case class Case(
      title: String,
      rejectError: String,
      mkDApp: Int => Script,
      invokeTx: InvokeScriptTransaction = aliceInvokeTx,
      knownTxs: Seq[Transaction] = Seq.empty
  )

  private def mkScriptWithOneAction(actionSrc: String): Int => Script = { targetComplexity =>
    // 1 for tuple, 1 for action, 1 for list
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
}
