package com.wavesplatform.features

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.{Transaction, TxHelpers}
import org.scalatest.{EitherValues, OptionValues}

import java.nio.charset.StandardCharsets

// TODO Version of contracts
// TODO estimate compiled expr
class RideV6ActivationTest extends FreeSpec with WithDomain with OptionValues with EitherValues {
  private val chainId      = DomainPresets.SettingsFromDefaultConfig.blockchainSettings.addressSchemeCharacter.toByte
  private val otherChainId = (chainId + 1).toByte

  private val invalidAssetId = IssuedAsset(ByteStr(("1" * 5).getBytes(StandardCharsets.UTF_8)))

  private val alice     = TxHelpers.signer(1) // signer(0) forges blocks and this affects the balance
  private val aliceAddr = alice.toAddress(chainId)

  private val bob               = TxHelpers.signer(2)
  private val bobAddr           = bob.toAddress(chainId)
  private val bobOtherChainAddr = bob.toAddress(otherChainId)

  private val aliceRegularAssetTx       = TxHelpers.issue(issuer = alice, amount = Long.MaxValue - 1)
  private val aliceRegularAssetId       = aliceRegularAssetTx.id()
  private val aliceNotReIssuableAssetTx = TxHelpers.issue(issuer = alice, amount = 100, reissuable = false)
  private val aliceLeasingTx            = TxHelpers.lease(sender = alice, recipient = bobAddr)
  private val aliceLeasingId            = aliceLeasingTx.id()
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

  private val defaultInitWavesBalances = Map(
    aliceAddr -> ENOUGH_AMT,
    bobAddr   -> ENOUGH_AMT
  )

  private val defaultScript = mkScriptWithOneAction("""IntegerEntry("i", 1)""")

  "After RideV6 activation" - {
    val cases = Seq(
      Case(
        "NODE-540 If an invoke exceeds the limit of writes",
        "Stored data count limit is exceeded",
        { targetComplexity =>
          val baseComplexity = 101 + 101 // 101 for list, 101 for IntegerEntry
          mkV6FooScript(
            s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
               | ${(1 to 101).map(i => s"""IntegerEntry("i$i", complexInt)""").mkString("[", ", ", "]")}
               | """.stripMargin
          )
        }
      ),
      Case(
        "NODE-540 If inner invokes exceed the limit of writes",
        "Stored data count limit is exceeded",
        { targetComplexity =>
          // 1 for strict, 75 for invoke, 1 for Address, 500 for bob.foo() body, 1 for tuple, 1 for list,
          // 1 for IntegerEntry, 1 for throw
          val baseComplexity = 1 + 75 + 1 + 500 + 1 + 1 + 1 + 1
          mkV6FooScript(
            s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
               | strict res = invoke(Address(base58'$bobAddr'), "bar", [], [])
               | ([IntegerEntry("i", complexInt)], res.exactAs[Int])
               | """.stripMargin
          )
        },
        knownTxs = Seq(
          aliceRegularAssetTx,
          TxHelpers.setScript(
            bob, {
              val baseComplexity = 1 + 100 + 100 // 1 for tuple, 100 for list, 100 for IntegerEntry
              mkV6Script(
                s""" @Callable(inv)
                   | func bar() = {
                   |   (
                   |     ${(1 to 100).map(i => s"""IntegerEntry("i$i", 1)""").mkString("[", ", ", "]")},
                   |     ${mkIntExprWithComplexity(500 - baseComplexity)}
                   |   )
                   | }
                   | """.stripMargin
              )
            }
          )
        )
      ),
      Case(
        "NODE-542 If an invoke exceeds the limit of writes size",
        "WriteSet size can't exceed 5120 bytes, actual: 5121 bytes",
        { targetComplexity =>
          val baseComplexity = 2 + 1 + 1 // 2 for list, 1 for BinaryEntry, 1 for IntegerEntry
          // See DataTxValidator.realUserPayloadSize: IntegerDataEntry - 8, "b" and "i" keys - 2
          val limitedBinaryEntrySize = ContractLimits.MaxWriteSetSizeInBytes - 8 - 2
          mkV6FooScript(
            s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
               | [
               |   BinaryEntry("b", base64'${Base64.encode(Array.fill[Byte](limitedBinaryEntrySize + 1)(0))}'),
               |   IntegerEntry("i", complexInt)
               | ]""".stripMargin
          )
        }
      ),
      Case(
        "NODE-544 If an invoke exceeds the limit of writes number through a WriteSet",
        "Stored data count limit is exceeded",
        { targetComplexity =>
          val baseComplexity = 101 + 2 * 101 + 1 // 101 for list, 2 * 101 for DataEntry, 1 for WriteSet
          mkV3FooScript(
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
          s"NODE-546 If an invoke writes an empty $entryType key to the state",
          "Empty keys aren't allowed in tx version >= 2",
          { targetComplexity =>
            val baseComplexity = 2 + 1 + 1 // 2 for list (two elements), 1 for IntegerEntry, 1 for test entry
            mkV6FooScript(
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
          s"NODE-548 If an invoke exceeds the limit of $actionType actions",
          "ScriptTransfer, Lease, LeaseCancel actions count limit is exceeded",
          { targetComplexity =>
            // 1 for Address, 1 for tuple, 101 for list, 101 for actions, 10 for getInteger, 2 for valueOrElse, 1 for "+"
            val baseComplexity = 1 + 1 + 101 + 101 + 10 + 2 + 1
            mkV6FooScript(
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
          "NODE-548 If an invoke exceeds the limit of mixed non-data actions",
          "ScriptTransfer, Lease, LeaseCancel actions count limit is exceeded",
          { targetComplexity =>
            val baseComplexity = 1 + 101 + 101 // 1 for Address, 101 for list, 101 for actions
            mkV6FooScript(
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
          "NODE-550 If an invoke sends a self-payment",
          "DApp self-payment is forbidden since V4",
          invokeTx = aliceInvokeTx // self-payment, because this is a self-invoke
            .copy(payments = Seq(InvokeScriptTransaction.Payment(1, Waves)))
            .signWith(alice.privateKey)
        ),
        Case(
          "NODE-552 If an invoke sends a ScriptTransfer for himself",
          "DApp self-transfer is forbidden since V4",
          mkScriptWithOneAction("ScriptTransfer(this, 1, unit)")
        ),
        Case(
          "NODE-554 If an invoke sends a ScriptTransfer with a negative amount",
          "Negative transfer amount",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 + 1 // 1 for Address, 1 for list, 1 for ScriptTransfer, 1 for "-complexInt"
            mkV6FooScript(
              s""" let to = Address(base58'$bobAddr')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | [ ScriptTransfer(to, -complexInt, unit) ]
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-556 If an invoke leads to a Waves overflow with a ScriptTransfer (initial balances)",
          "Waves balance sum overflow",
          { targetComplexity =>
            // 1 for Address, 1 for tuple, 1 for list, 1 for ScriptTransfer, 10 for wavesBalance, 1 for Address(alice),
            //   and 1 for "-"
            val baseComplexity = 1 + 1 + 1 + 1 + 10 + 1 + 1
            mkV6FooScript(
              s""" let to = Address(base58'$bobAddr')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | (
                 |   [ScriptTransfer(to, wavesBalance(Address(base58'$aliceAddr')).available - ${aliceInvokeTx.fee}, unit)],
                 |   complexInt
                 | )
                 | """.stripMargin
            )
          },
          initBalances = Map(
            aliceAddr -> Long.MaxValue,
            bobAddr   -> ENOUGH_AMT
          )
        ),
        Case(
          "NODE-556 If an invoke leads to a Waves overflow with a ScriptTransfer (multiple transfers)",
          "negative waves balance",
          { targetComplexity =>
            // 1 for Address, 1 for ScriptTransfer, 10 for wavesBalance, 1 for Address(alice), 1 for "-",
            // 1 for tuple, 2 for list
            val baseComplexity = 1 + 1 + 10 + 1 + 1 + 1 + 2
            mkV6FooScript(
              s""" let to = Address(base58'$bobAddr')
                 | let transfer = ScriptTransfer(to, wavesBalance(Address(base58'$aliceAddr')).available - ${aliceInvokeTx.fee}, unit)
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([transfer, transfer], complexInt)
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-556 If an invoke leads to an asset overflow with a ScriptTransfer",
          "ScriptTransfer overflow",
          { targetComplexity =>
            // 1 for Address, 1 for ScriptTransfer, 1 for tuple, 2 for list
            val baseComplexity = 1 + 1 + 1 + 2
            mkV6FooScript(
              s""" let to = Address(base58'$bobAddr')
                 | let transfer = ScriptTransfer(to, ${Long.MaxValue - 1}, base58'$aliceRegularAssetId')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([transfer, transfer], complexInt)
                 | """.stripMargin
            )
          },
          knownTxs = Seq(aliceRegularAssetTx)
        ),
        Case(
          "NODE-558 If an invoke issues a token with an invalid name",
          "Invalid asset name",
          mkScriptWithOneAction("""Issue("n", "Test token", 1, 2, true)""")
        ),
        Case(
          "NODE-560 If an invoke issues a token with an invalid description",
          "Invalid asset description",
          mkScriptWithOneAction(s"""Issue("Token", "${"a" * (IssueTransaction.MaxAssetDescriptionLength + 1)}", 1, 2, true)""")
        ),
        Case(
          "NODE-562 If an invoke sets a sponsorship for another account asset",
          "was not issued from address of current dApp",
          mkScriptWithOneAction(s"SponsorFee(base58'$bobAssetId', 1)"),
          knownTxs = Seq(bobAssetTx)
        ), {
          val aliceApprovingSmartAssetTx = TxHelpers.issue(
            issuer = alice,
            script = Some(
              // TODO mkAssetScript
              TestCompiler(V5).compileAsset(
                s""" {-# STDLIB_VERSION 5 #-}
                   | {-# CONTENT_TYPE EXPRESSION #-}
                   | {-# SCRIPT_TYPE ASSET #-}
                   | true
                   |""".stripMargin
              )
            )
          )
          Case(
            "NODE-564 If an invoke sets a sponsorship for a smart asset",
            "Sponsorship smart assets is disabled",
            mkScriptWithOneAction(s"SponsorFee(base58'${aliceApprovingSmartAssetTx.id()}', 1)"),
            knownTxs = Seq(aliceApprovingSmartAssetTx)
          )
        },
        Case(
          "NODE-566 If an invoke sends a ScriptTransfer with an invalid assetId",
          "invalid asset ID",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 // 1 for Address, 1 for list, 1 for ScriptTransfer
            mkV6FooScript(
              s""" let to = Address(base58'$bobAddr')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | [ ScriptTransfer(to, complexInt, base58'$invalidAssetId') ]
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-568 If an invoke sends a ScriptTransfer with an unknown assetId",
          s"Transfer error: asset '$bobAssetId' is not found on the blockchain",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 // 1 for Address, 1 for list, 1 for ScriptTransfer
            mkV6FooScript(
              s""" let to = Address(base58'$bobAddr')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | [ ScriptTransfer(to, complexInt, base58'$bobAssetId') ]
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-570 If an invoke tries to overflow the amount of assets through Reissue",
          "Asset total value overflow",
          mkScriptWithOneAction(s"Reissue(base58'$aliceRegularAssetId', ${Long.MaxValue - defaultInitWavesBalances(aliceAddr) + 1}, true)"),
          knownTxs = Seq(aliceRegularAssetTx)
        ),
        Case(
          "NODE-572 If an invoke reissues a not reissuable asset",
          "Asset is not reissuable",
          mkScriptWithOneAction(s"Reissue(base58'${aliceNotReIssuableAssetTx.id()}', 1, true)"),
          knownTxs = Seq(aliceNotReIssuableAssetTx)
        ),
        Case(
          "NODE-574 If an invoke reissues an asset issued by other address",
          "Asset was issued by other address",
          mkScriptWithOneAction(s"Reissue(base58'$bobAssetId', 1, true)"),
          knownTxs = Seq(bobAssetTx)
        )
      ) ++ Seq(
        "Burn"    -> s"Burn(base58'$bobAssetId', 1)",
        "Reissue" -> s"Reissue(base58'$bobAssetId', 1, true)"
      ).map { case (actionType, actionSrc) =>
        Case(
          s"NODE-576 If an invoke sends $actionType with an unknown assetId",
          "Referenced assetId not found",
          mkScriptWithOneAction(actionSrc)
        )
      } ++ Seq(
        Case(
          "NODE-576 If an invoke sends SponsorFee with an unknown assetId",
          "was not issued from address of current dApp",
          mkScriptWithOneAction(s"SponsorFee(base58'$bobAssetId', 1)")
        )
      ) ++ Seq(
        "ScriptTransfer" -> "ScriptTransfer(to, complexInt, unit)",
        "Lease"          -> "Lease(to, complexInt)"
      ).map { case (actionType, actionSrc) =>
        Case(
          s"NODE-578 If an invoke sends $actionType with an address of another network",
          "Address belongs to another network",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 // 1 for Address, 1 for list, 1 for action
            mkV6FooScript(
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
            mkV6FooScript(
              s""" let to = Address(base58'$bobOtherChainAddr')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | strict res = invoke(to, "bar", [complexInt], [])
                 | ([], res.exactAs[Int])
                 | """.stripMargin
            )
          }
        )
      ) ++ Seq(
        "ScriptTransfer" -> "ScriptTransfer(to, 1, unit)",
        "Lease"          -> "Lease(to, 1)"
      ).map { case (actionType, actionSrc) =>
        Case(
          s"NODE-580 If an invoke sends $actionType with an invalid address",
          "Wrong addressBytes length",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 + 1 // 1 for Address, 1 for tuple, 1 for list, 1 for action
            mkV6FooScript(
              s""" let to = Address(base58'${bobAddr.toString.take(5)}')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([$actionSrc], complexInt)
                 | """.stripMargin
            )
          }
        )
      } ++ Seq(
        ("negative", -1, "Negative lease amount = -1"),
        ("zero", 0, "NonPositiveAmount(0,waves)")
      ).map { case (tpe, leaseAmount, rejectError) =>
        Case(
          s"NODE-584 If an invoke leases $tpe amount",
          rejectError,
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 + 1 // 1 for Address, 1 for tuple, 1 for list, 1 for Lease
            mkV6FooScript(
              s""" let to = Address(base58'$bobAddr')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([Lease(to, $leaseAmount)], complexInt)
                 | """.stripMargin
            )
          }
        )
      } ++ Seq(
        Case(
          "NODE-586 If an invoke does a self-leasing",
          "Cannot lease to self",
          mkScriptWithOneAction("Lease(this, 1)")
        ),
        Case(
          "NODE-588 If an invoke does the same leasing multiple times",
          "is already in the state",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 2 + 1 + 1 // 1 for Address, 1 for tuple, 2 for list, 1+1 for Lease
            mkV6FooScript(
              s""" let to = Address(base58'$bobAddr')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([Lease(to, 1, 3), Lease(to, 1, 3)], complexInt) # Leasing id is a hash of its data and invoke tx id
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-590 If an invoke leases the nonexistent funds",
          "Cannot lease more than own",
          { targetComplexity =>
            // 1 for Address(bob), 10 for wavesBalance, 1 for Address(alice), 1 for tuple, 2 for list, 1+1 for Lease
            val baseComplexity = 1 + 10 + 1 + 1 + 2 + 1 + 1
            mkV6FooScript(
              s""" let to = Address(base58'$bobAddr')
                 | let available = wavesBalance(Address(base58'$aliceAddr')).available
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([Lease(to, available, 1), Lease(to, available, 2)], complexInt)
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-592 If an invoke cancels a leasing with a wrong lease id",
          s"Lease id=${bobLeasingId.toString.take(5)} has invalid length",
          mkScriptWithOneAction(s"LeaseCancel(base58'${bobLeasingId.toString.take(5)}')")
        ),
        Case(
          "NODE-594 If an invoke cancels an unknown leasing",
          s"Lease with id=$bobLeasingId not found",
          mkScriptWithOneAction(s"LeaseCancel(base58'$bobLeasingId')")
        ),
        Case(
          "NODE-596 If an invoke cancels the same leasing twice",
          "Duplicate LeaseCancel id(s)",
          { targetComplexity =>
            val baseComplexity = 1 + 2 + 1 // 1 for tuple, 2 for list, 1 for LeaseCancel
            mkV6FooScript(
              s""" let cancel = LeaseCancel(base58'$aliceLeasingId')
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([cancel, cancel], complexInt)
                 | """.stripMargin
            )
          },
          knownTxs = Seq(aliceLeasingTx)
        ),
        Case(
          "NODE-596 If an invoke cancels the already cancelled leasing",
          "Cannot cancel already cancelled lease",
          mkScriptWithOneAction(s"LeaseCancel(base58'$aliceLeasingId')"),
          knownTxs = Seq(aliceLeasingTx, TxHelpers.leaseCancel(aliceLeasingId, sender = alice))
        ),
        Case(
          "NODE-598 If an invoke cancels another account leasing",
          "LeaseTransaction was leased by other sender and time",
          mkScriptWithOneAction(s"LeaseCancel(base58'$bobLeasingId')"),
          knownTxs = Seq(bobLeasingTx)
        )
      ) ++ Seq(
        ("false", 0, "Transaction is not allowed by script of the asset"),
        ("""throw("test error")""", 1, "test error")
      ).map {
        case (assetScriptResult, assetScriptComplexity, expectingError) => {
          val aliceRejectingSmartAssetTx = TxHelpers.issue(
            issuer = alice,
            script = Some(
              TestCompiler(V5).compileAsset(
                s""" {-# STDLIB_VERSION 5 #-}
                   | {-# CONTENT_TYPE EXPRESSION #-}
                   | {-# SCRIPT_TYPE ASSET #-}
                   | $assetScriptResult
                   |""".stripMargin
              )
            )
          )
          Case(
            s"NODE-604 If an invoke failed by a asset script ($assetScriptResult)",
            expectingError,
            { targetComplexity =>
              // 1 for Address(bob), 1 for tuple, 1 for list, 1 for ScriptTransfer
              val baseComplexity = 1 + 1 + 1 + 1 + assetScriptComplexity
              mkV6FooScript(
                s""" let to = Address(base58'$bobAddr')
                   | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                   | ([ScriptTransfer(to, 1, base58'${aliceRejectingSmartAssetTx.id()}')], complexInt)
                   | """.stripMargin
              )
            },
            knownTxs = Seq(aliceRejectingSmartAssetTx)
          )
        }
      } ++ Seq(
        Case.withInnerPayment(
          "NODE-606 If an inner invoke contains a negative Waves payment",
          "with attached WAVES amount = -1",
          "AttachedPayment(unit, -1)"
        ),
        Case.withInnerPayment(
          "NODE-606 If an inner invoke contains a negative asset payment",
          s"with attached token $aliceRegularAssetId amount = -1",
          s"AttachedPayment(base58'$aliceRegularAssetId', -1)"
        ),
        Case(
          "NODE-608 If an invoke issues the same asset multiple times",
          "is already issued",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 2 // 1 for Issue, 1 for tuple, 2 for list
            mkV6FooScript(
              s""" let issue = Issue("bucks", "Test token", 1, 2, true)
                 | let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | ([issue, issue], complexInt)
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-610 If an invoke reissues an asset with a negative quantity",
          "Negative reissue quantity",
          mkScriptWithOneAction(s"Reissue(base58'$aliceRegularAssetId', -1, true)"),
          knownTxs = Seq(aliceRegularAssetTx)
        ),
        Case(
          "NODE-612 If an invoke burns an asset with a negative quantity",
          "Negative burn quantity",
          mkScriptWithOneAction(s"Burn(base58'$aliceRegularAssetId', -1)"),
          knownTxs = Seq(aliceRegularAssetTx)
        ),
        Case(
          "NODE-618 If an invoke sets a sponsorship with a negative quantity",
          "Negative sponsor amount",
          mkScriptWithOneAction(s"SponsorFee(base58'$aliceRegularAssetId', -1)"),
          knownTxs = Seq(aliceRegularAssetTx)
        ),
        Case(
          "NODE-620 If a negative balance happens during the invoke",
          "negative waves balance",
          { targetComplexity =>
            // 75 for invoke, 1 for Address, 1 for list, 500 for bob.foo() body
            val baseComplexity = 75 + 1 + 1 + 500
            mkV6FooScript(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | strict res = invoke(Address(base58'$bobAddr'), "bar", [ complexInt ], [])
                 | ([], res.exactAs[Int])
                 | """.stripMargin
            )
          },
          knownTxs = Seq(
            TxHelpers.setScript(
              bob, {
                // 1 for Address(alice), 1 for tuple, 1 for list, 1 for ScriptTransfer, 10 for wavesBalance,
                // 1 for Address(bob) and 1 for "+"
                val baseComplexity = 1 + 1 + 1 + 1 + 10 + 1 + 1
                mkV6Script(
                  s""" @Callable(inv)
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
        ),
        Case(
          "NODE-698 If an inner invoke to an account without a script",
          "No contract at address",
          { targetComplexity =>
            // 75 for invoke, 1 for Address, 1 for list
            val baseComplexity = 75 + 1 + 1
            mkV6FooScript(
              s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | strict res = invoke(Address(base58'$bobAddr'), "bar", [ complexInt ], [])
                 | ([], res.exactAs[Int])
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-760 If an inner invoke has a payment with an unknown assetId",
          s"Transfer error: asset '$bobAssetId' is not found on the blockchain",
          { targetComplexity =>
            // bar: 1 for Address, 1 for tuple, 1 for list, 1 for ScriptTransfer
            // foo: 75 for invoke
            val baseComplexity = 1 + 1 + 1 + 1 + 75
            mkV6Script(
              s""" @Callable(inv)
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
        Case.withInnerPayment(
          "NODE-761 If an inner payment has with an invalid assetId",
          s"invalid asset ID '$invalidAssetId'",
          s"AttachedPayment(base58'$invalidAssetId', 1)"
        )
      )

    cases.foreach { testCase =>
      testCase.title - {
        "<=1000 complexity - rejected" - Seq(110, ContractLimits.FailFreeInvokeComplexity).foreach { complexity =>
          s"complexity = $complexity" in test(complexity) { d =>
            d.createDiffE(testCase.invokeTx) should produce(testCase.rejectError)
          }
        }
        ">1000 complexity - failed" in {
          val complexity = ContractLimits.FailFreeInvokeComplexity + 1
          test(complexity) { d =>
            val diff              = d.createDiffE(testCase.invokeTx).value
            val (_, scriptResult) = diff.scriptResults.headOption.value
            scriptResult.error.value.text should include(testCase.rejectError)

            d.appendBlock(testCase.invokeTx)
            val invokeTxMeta = d.transactionsApi.transactionById(testCase.invokeTx.id()).value
            invokeTxMeta.spentComplexity shouldBe complexity
            invokeTxMeta.succeeded shouldBe false
          }
        }

        def test(complexity: Int)(f: Domain => Unit): Unit =
          withDomain(
            DomainPresets.RideV6,
            testCase.initBalances.map(Function.tupled(AddrWithBalance.apply)).toSeq
          ) { d =>
            if (testCase.hasDApp) {
              val setScriptTx = TxHelpers.setScript(alice, testCase.mkDApp(complexity), 1.waves)
              d.appendBlock((testCase.knownTxs :+ setScriptTx)*)
            }
            f(d)
          }
      }
    }
  }

  /** @param mkDApp
    *   Int is the targetComplexity of the script
    */
  private case class Case(
      title: String,
      rejectError: String,
      mkDApp: Int => Script = defaultScript,
      invokeTx: InvokeScriptTransaction = aliceInvokeTx,
      knownTxs: Seq[Transaction] = Seq.empty,
      initBalances: Map[Address, Long] = defaultInitWavesBalances,
      hasDApp: Boolean = true // This is better than Option[Int => Script] or Int => Option[Script], because doesn't affect 99% of tests
  )

  private object Case {
    def withInnerPayment(title: String, rejectError: String, payment: String): Case = Case(
      title,
      rejectError,
      { targetComplexity =>
        // 75 for invoke, 1 for Address, 1+1 for lists, 1 for payment
        val baseComplexity = 75 + 1 + 1 + 1 + 1
        mkV6Script(
          s""" @Callable(inv)
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
        aliceRegularAssetTx,
        TxHelpers.setScript(
          bob, {
            val baseComplexity = 1 // 1 for tuple
            mkV6Script(
              s""" @Callable(inv)
                 | func bar(n: Int) = {
                 |   ([], ${mkIntExprWithComplexity(500 - baseComplexity)})
                 | }
                 | """.stripMargin
            )
          }
        )
      )
    )
  }

  private def mkScriptWithOneAction(actionSrc: String): Int => Script = { targetComplexity =>
    val baseComplexity = 1 + 1 + 1 // 1 for tuple, 1 for list, 1 for action
    mkV6FooScript(
      s""" let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
         | ([$actionSrc], complexInt)
         | """.stripMargin
    )
  }

  private def mkV3FooScript(fooBody: String): Script = mkFooScript(V3, fooBody)
  private def mkV6FooScript(fooBody: String): Script = mkFooScript(V6, fooBody)
  private def mkFooScript(v: StdLibVersion, fooBody: String): Script =
    mkScript(
      v,
      s""" @Callable(inv)
         | func foo() = {
         |   $fooBody
         | }
         | """.stripMargin
    )

  private def mkV6Script(scriptBody: String): Script = mkScript(V6, scriptBody)
  private def mkScript(v: StdLibVersion, scriptBody: String): Script =
    TestCompiler(v).compileContract(
      s""" {-#STDLIB_VERSION ${v.id} #-}
         | {-#SCRIPT_TYPE ACCOUNT #-}
         | {-#CONTENT_TYPE DAPP #-}
         | $scriptBody
         | """.stripMargin
    )

  private def mkIntExprWithComplexity(targetComplexity: Int): String = s"1${" + 1" * targetComplexity}"
}
