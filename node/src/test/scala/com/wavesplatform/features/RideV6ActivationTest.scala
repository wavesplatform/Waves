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
          val baseComplexity = 1 + 101 + 101 // 1 for strict, 101 for list, 101 for IntegerEntry
          mkV6FooScript(
            s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
               | ${(1 to 101).map(i => s"""IntegerEntry("i$i", 1)""").mkString("[", ", ", "]")}
               | """.stripMargin
          )
        }
      ),
      Case(
        "NODE-540 If inner invokes exceed the limit of writes",
        "Stored data count limit is exceeded",
        { targetComplexity =>
          // 1+1 for strict, 75 for invoke, 1 for Address, 500 for bob.foo() body, 1 for list, 1 for IntegerEntry
          val baseComplexity = 1 + 1 + 75 + 1 + 500 + 1 + 1
          mkV6FooScript(
            s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
               | strict res = invoke(Address(base58'$bobAddr'), "bar", [], [])
               | [IntegerEntry("i", 1)]
               | """.stripMargin
          )
        },
        knownTxs = Seq(
          aliceRegularAssetTx,
          TxHelpers.setScript(
            bob, {
              val baseComplexity = 1 + 1 + 100 + 100 // 1 for strict, 1 for tuple, 100 for list, 100 for IntegerEntry
              mkV6Script(
                s""" @Callable(inv)
                   | func bar() = {
                   |   strict complexInt = ${mkIntExprWithComplexity(500 - baseComplexity)}
                   |   (
                   |     ${(1 to 100).map(i => s"""IntegerEntry("i$i", 1)""").mkString("[", ", ", "]")},
                   |     1
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
          val baseComplexity = 1 + 2 + 1 + 1 // 1 for strict, 2 for list, 1 for BinaryEntry, 1 for IntegerEntry
          // See DataTxValidator.realUserPayloadSize: IntegerDataEntry - 8, "b" and "i" keys - 2
          val limitedBinaryEntrySize = ContractLimits.MaxWriteSetSizeInBytes - 8 - 2
          mkV6FooScript(
            s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
               | [
               |   BinaryEntry("b", base64'${Base64.encode(Array.fill[Byte](limitedBinaryEntrySize + 1)(0))}'),
               |   IntegerEntry("i", 1)
               | ]""".stripMargin
          )
        }
      ),
      Case(
        "NODE-544 If an invoke exceeds the limit of writes number through a WriteSet",
        "Stored data count limit is exceeded",
        { targetComplexity =>
          val baseComplexity = 1 + 101 + 2 * 101 + 1 // 1 for strict, 101 for list, 2 * 101 for DataEntry, 1 for WriteSet
          mkV3FooScript(
            s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
               | ${(1 to 101).map(i => s"""DataEntry("i$i", 1)""").mkString("WriteSet([", ", ", "])")}
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
            val baseComplexity = 1 + 2 + 1 + 1 // 1 for strict, 2 for list (two elements), 1 for IntegerEntry, 1 for test entry
            mkV6FooScript(
              s"""
                 | strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | [ IntegerEntry("k", 1), $entry ]
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
          // "Actions count limit is exceeded",
          { targetComplexity =>
            // 1 for strict, 1 for Address, 1 for tuple, 101 for list, 101 for actions, 10 for getInteger, 2 for valueOrElse
            val baseComplexity = 1 + 1 + 1 + 101 + 101 + 10 + 2
            mkV6FooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let to = Address(base58'$bobAddr')
                 | (
                 |   [ ${(1 to 101).map(_ => v).mkString(", ")} ],
                 |   # otherwise a script with LeaseCancel will be with a different complexity
                 |   valueOrElse(getInteger(to, "force-use-of-to"), 0)
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
          // "Actions count limit is exceeded",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 101 + 101 // 1 for strict, 1 for Address, 101 for list, 101 for actions
            mkV6FooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let to = Address(base58'$bobAddr')
                 | [
                 |   ${(1 to 34).map(_ => s"""ScriptTransfer(to, 1, unit)""").mkString(", ")},
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
            val baseComplexity = 1 + 1 + 1 + 1 // 1 for strict, 1 for Address, 1 for list, 1 for ScriptTransfer
            mkV6FooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let to = Address(base58'$bobAddr')
                 | [ ScriptTransfer(to, -1, unit) ]
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-556 If an invoke leads to a Waves overflow with a ScriptTransfer (initial balances)",
          "Waves balance sum overflow",
          { targetComplexity =>
            // 1 for strict, 1 for Address, 1 for list, 1 for ScriptTransfer, 10 for wavesBalance,
            //   1 for Address(alice) and 1 for "-"
            val baseComplexity = 1 + 1 + 1 + 1 + 10 + 1 + 1
            mkV6FooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let to = Address(base58'$bobAddr')
                 | [ ScriptTransfer(to, wavesBalance(Address(base58'$aliceAddr')).available - ${aliceInvokeTx.fee}, unit) ]
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
            // 1 for strict, 1 for Address, 1 for ScriptTransfer, 10 for wavesBalance, 1 for Address(alice), 1 for "-",
            // 2 for list
            val baseComplexity = 1 + 1 + 1 + 10 + 1 + 1 + 2
            mkV6FooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let to = Address(base58'$bobAddr')
                 | let transfer = ScriptTransfer(to, wavesBalance(Address(base58'$aliceAddr')).available - ${aliceInvokeTx.fee}, unit)
                 | [transfer, transfer]
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-556 If an invoke leads to an asset overflow with a ScriptTransfer",
          "ScriptTransfer overflow",
          { targetComplexity =>
            // 1 for strict, 1 for Address, 1 for ScriptTransfer, 2 for list
            val baseComplexity = 1 + 1 + 1 + 2
            mkV6FooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let to = Address(base58'$bobAddr')
                 | let transfer = ScriptTransfer(to, ${Long.MaxValue - 1}, base58'$aliceRegularAssetId')
                 | [transfer, transfer]
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
            script = Some(mkAssetScript("true"))
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
            val baseComplexity = 1 + 1 + 1 + 1 // 1 for strict, 1 for Address, 1 for list, 1 for ScriptTransfer
            mkV6FooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let to = Address(base58'$bobAddr')
                 | [ ScriptTransfer(to, complexInt, base58'$invalidAssetId') ]
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-568 If an invoke sends a ScriptTransfer with an unknown assetId",
          s"Transfer error: asset '$bobAssetId' is not found on the blockchain",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 + 1 // 1 for strict, 1 for Address, 1 for list, 1 for ScriptTransfer
            mkV6FooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let to = Address(base58'$bobAddr')
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
            val baseComplexity = 1 + 1 + 1 + 1 // 1 for strict, 1 for Address, 1 for list, 1 for action
            mkV6FooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let to = Address(base58'$bobOtherChainAddr')
                 | [$actionSrc]
                 | """.stripMargin
            )
          }
        )
      } ++ Seq(
        Case(
          "NODE-578 If a transaction sends an invoke with address of another network",
          "Address belongs to another network",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 75 + 1 // 1 for strict, 1 for Address, 75 for invoke, 1 for list ([complexInt])
            mkV6FooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let to = Address(base58'$bobOtherChainAddr')
                 | strict res = invoke(to, "bar", [complexInt], [])
                 | []
                 | """.stripMargin
            )
          }
        )
      ) ++ {
        for {
          (actionType, actionSrc) <- Seq(
            "ScriptTransfer" -> "ScriptTransfer(to, 1, unit)",
            "Lease"          -> "Lease(to, 1)"
          )
          addrLen <- Seq(0, Address.AddressLength - 1, Address.AddressLength + 1)
        } yield Case(
          s"NODE-580 If an invoke sends $actionType with an invalid address (len=$addrLen)",
          "Wrong addressBytes length",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 + 1 // 1 for strict, 1 for Address, 1 for list, 1 for action
            mkV6FooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let to = Address(base58'${"1" * addrLen}')
                 | [$actionSrc]
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
            val baseComplexity = 1 + 1 + 1 + 1 // 1 for strict, 1 for Address, 1 for list, 1 for Lease
            mkV6FooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let to = Address(base58'$bobAddr')
                 | [Lease(to, $leaseAmount)]
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
            val baseComplexity = 1 + 1 + 2 + 1 + 1 // 1 for strict, 1 for Address, 2 for list, 1+1 for Lease
            mkV6FooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let to = Address(base58'$bobAddr')
                 | [Lease(to, 1, 3), Lease(to, 1, 3)] # Leasing id is a hash of its data and invoke tx id
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-590 If an invoke leases the nonexistent funds",
          "Cannot lease more than own",
          { targetComplexity =>
            // 1 for strict, 1 for Address(bob), 10 for wavesBalance, 1 for Address(alice), 2 for list, 1+1 for Lease
            val baseComplexity = 1 + 1 + 10 + 1 +  2 + 1 + 1
            mkV6FooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let to = Address(base58'$bobAddr')
                 | let available = wavesBalance(Address(base58'$aliceAddr')).available
                 | [Lease(to, available, 1), Lease(to, available, 2)]
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
            val baseComplexity = 1 + 2 + 1 // 1 for strict, 2 for list, 1 for LeaseCancel
            mkV6FooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let cancel = LeaseCancel(base58'$aliceLeasingId')
                 | [cancel, cancel]
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
        case (assetScriptBody, assetScriptComplexity, expectingError) => {
          val aliceRejectingSmartAssetTx = TxHelpers.issue(
            issuer = alice,
            script = Some(mkAssetScript(assetScriptBody))
          )
          Case(
            s"NODE-604 If an invoke failed by a asset script ($assetScriptBody)",
            expectingError,
            { targetComplexity =>
              // 1 for strict, 1 for Address(bob), 1 for list, 1 for ScriptTransfer
              val baseComplexity = 1 + 1 + 1 + 1 + assetScriptComplexity
              mkV6FooScript(
                s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                   | let to = Address(base58'$bobAddr')
                   | [ScriptTransfer(to, 1, base58'${aliceRejectingSmartAssetTx.id()}')]
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
            val baseComplexity = 1 + 1 + 2 // 1 for strict, 1 for Issue, 2 for list
            mkV6FooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let issue = Issue("bucks", "Test token", 1, 2, true)
                 | [issue, issue]
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
            // 1 for strict, 75 for invoke, 1 for Address, 1 for list, 500 for bob.foo() body
            val baseComplexity = 1 + 75 + 1 + 1 + 500
            mkV6FooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | strict res = invoke(Address(base58'$bobAddr'), "bar", [ complexInt ], [])
                 | []
                 | """.stripMargin
            )
          },
          knownTxs = Seq(
            TxHelpers.setScript(
              bob, {
                // 1 for Address(alice), 1 for tuple, 1 for list, 1 for ScriptTransfer, 10 for wavesBalance,
                //   1 for Address(bob) and 1 for "+"
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
            // 1 for strict, 75 for invoke, 1 for Address, 1 for list
            val baseComplexity = 1 + 75 + 1 + 1
            mkV6FooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | strict res = invoke(Address(base58'$bobAddr'), "bar", [1], [])
                 | []
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-760 If an inner invoke has a payment with an unknown assetId",
          s"Transfer error: asset '$bobAssetId' is not found on the blockchain",
          { targetComplexity =>
            // bar: 1 for strict, 1 for Address, 1 for tuple, 1 for list, 1 for ScriptTransfer
            // foo: 75 for invoke
            val baseComplexity = 1 + 1 + 1 + 1 + 1 + 75
            mkV6Script(
              s""" @Callable(inv)
                 | func bar() = {
                 |   strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 |   let to = Address(base58'$bobAddr')
                 |   ([ ScriptTransfer(to, 1, base58'$bobAssetId') ], 1)
                 | }
                 |
                 | @Callable(inv)
                 | func foo() = {
                 |   strict res = invoke(this, "bar", [], [])
                 |   []
                 | }
                 | """.stripMargin
            )
          }
        ),
        Case.withInnerPayment(
          "NODE-761 If an inner payment has with an invalid assetId",
          s"invalid asset ID '$invalidAssetId'",
          s"AttachedPayment(base58'$invalidAssetId', 1)"
        ),
        Case(
          "NODE-762 If an invoke failed by a throw in the script",
          "test error",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 // 1 for strict, 1 for compare, 1 for tuple / throw
            mkV6FooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let to = Address(base58'$bobAddr')
                 | if (complexInt > 0) then {
                 |   throw("test error")
                 | } else {
                 |   []
                 | }
                 | """.stripMargin
            )
          }
        )
      )

    cases.foreach { testCase =>
      testCase.title - {
        "<=1000 complexity - rejected" - Seq(500, ContractLimits.FailFreeInvokeComplexity).foreach { complexity =>
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
        // 1 for strict, 75 for invoke, 1 for Address, 1+1 for lists, 1 for payment
        val baseComplexity = 1 + 75 + 1 + 1 + 1 + 1
        mkV6Script(
          s""" @Callable(inv)
             | func foo() = {
             |   strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
             |   strict res = invoke(Address(base58'$bobAddr'), "bar", [1], [$payment])
             |   []
             | }
             | """.stripMargin
        )
      },
      knownTxs = Seq(
        aliceRegularAssetTx,
        TxHelpers.setScript(
          bob, {
            val baseComplexity = 1 // 1 for strict
            mkV6Script(
              s""" @Callable(inv)
                 | func bar(n: Int) = {
                 |   strict complexInt = ${mkIntExprWithComplexity(500 - baseComplexity)}
                 |   []
                 | }
                 | """.stripMargin
            )
          }
        )
      )
    )
  }

  private def mkScriptWithOneAction(actionSrc: String): Int => Script = { targetComplexity =>
    val baseComplexity = 1 + 1 + 1 // 1 for strict, 1 for list, 1 for action
    mkV6FooScript(
      s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
         | [$actionSrc]
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
  private def mkScript(v: StdLibVersion, scriptBody: String): Script = TestCompiler(v).compileContract(
    s""" {-#STDLIB_VERSION ${v.id} #-}
       | {-#SCRIPT_TYPE ACCOUNT #-}
       | {-#CONTENT_TYPE DAPP #-}
       | $scriptBody
       | """.stripMargin
  )

  private def mkAssetScript(scriptBody: String): Script = TestCompiler(V5).compileAsset(
    s""" {-# STDLIB_VERSION 5 #-}
       | {-# CONTENT_TYPE EXPRESSION #-}
       | {-# SCRIPT_TYPE ASSET #-}
       | $scriptBody
       | """.stripMargin
  )

  private def mkIntExprWithComplexity(targetComplexity: Int): String = s"1${" + 1" * targetComplexity}"
}
