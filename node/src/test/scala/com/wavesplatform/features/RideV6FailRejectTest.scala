package com.wavesplatform.features

import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.TxMeta.Status
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.utils.EthConverters.*
import com.wavesplatform.transaction.{EthTxGenerator, EthereumTransaction, Transaction, TxHelpers, TxVersion}
import org.scalatest.{EitherValues, OptionValues}

import java.nio.charset.StandardCharsets

class RideV6FailRejectTest extends FreeSpec with WithDomain with OptionValues with EitherValues {
  private val otherChainId = (AddressScheme.current.chainId + 1).toByte

  private val invalidAssetId = IssuedAsset(ByteStr(("1" * 5).getBytes(StandardCharsets.UTF_8)))

  private val alice     = TxHelpers.signer(1) // signer(0) forges blocks and this affects the balance
  private val aliceAddr = alice.toAddress

  private val bob               = TxHelpers.signer(2)
  private val bobAddr           = bob.toAddress
  private val bobOtherChainAddr = bob.toAddress(otherChainId)

  private val invoker     = TxHelpers.signer(3)
  private val invokerAddr = invoker.toAddress

  private val ethInvoker     = invoker.toEthKeyPair
  private val ethInvokerAddr = ethInvoker.toWavesAddress

  private val aliceRegularAssetTx       = TxHelpers.issue(issuer = alice, amount = Long.MaxValue - 1)
  private val aliceRegularAssetId       = aliceRegularAssetTx.id()
  private val aliceNotReIssuableAssetTx = TxHelpers.issue(issuer = alice, amount = 100, reissuable = false)
  private val aliceLeasingTx            = TxHelpers.lease(sender = alice, recipient = bobAddr)
  private val aliceLeasingId            = aliceLeasingTx.id()
  private val aliceInvokeTx = TxHelpers.invoke(
    dApp = aliceAddr,
    invoker = invoker,
    func = Some("foo"),
    fee = 3.waves
  )
  private val ethAliceInvokeTx = EthTxGenerator.generateEthInvoke(
    keyPair = ethInvoker,
    address = aliceAddr,
    funcName = "foo",
    args = Seq.empty,
    payments = Seq.empty,
    fee = 3.waves
  )

  private val bobAssetTx   = TxHelpers.issue(issuer = bob)
  private val bobAssetId   = bobAssetTx.id()
  private val bobLeasingTx = TxHelpers.lease(sender = bob, recipient = aliceAddr)
  private val bobLeasingId = bobLeasingTx.id()

  private val defaultInitWavesBalances = Map(
    aliceAddr      -> ENOUGH_AMT,
    bobAddr        -> ENOUGH_AMT,
    invokerAddr    -> ENOUGH_AMT,
    ethInvokerAddr -> ENOUGH_AMT
  )

  private val defaultScript = mkScriptWithOneAction("""IntegerEntry("i", 1)""")
  private val inV5V6        = Seq(V5, V6)
  private val inV4V6        = V4 +: inV5V6

  "After RideV6 activation" - {
    val cases = Seq(
      {
        val aliceSmartAssetTx = TxHelpers.issue(
          issuer = alice,
          script = Some(
            mkAssetScript(
              """
                | match tx {
                |   case _: ReissueTransaction => false
                |   case _ => true
                | }
                |""".stripMargin
            )
          )
        )
        Case(
          "NODE-124 Asset script can forbid reissue",
          "Transaction is not allowed by script of the asset",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 + 1 // 1 for strict, 1 for list, 1 for Reissue, 1 for asset script
            mkFooScript(
              s"""
                 |strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 |[Reissue(base58'${aliceSmartAssetTx.id()}', 10, true)]
                 |""".stripMargin
            )
          },
          knownTxs = Seq(aliceSmartAssetTx)
        )
      }, {
        val aliceSmartAssetTx = TxHelpers.issue(
          issuer = alice,
          script = Some(
            mkAssetScript(
              """
                | match tx {
                |   case _: BurnTransaction => false
                |   case _ => true
                | }
                |""".stripMargin
            )
          )
        )
        Case(
          "NODE-125 Asset script can forbid burn",
          "Transaction is not allowed by script of the asset",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 + 1 // 1 for strict, 1 for list, 1 for Burn, 1 for asset script
            mkFooScript(
              s"""
                 |strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 |[Burn(base58'${aliceSmartAssetTx.id()}', 10)]
                 |""".stripMargin
            )
          },
          knownTxs = Seq(aliceSmartAssetTx)
        )
      }, {
        val bobSmartAssetTx = TxHelpers.issue(
          issuer = bob,
          script = Some(
            mkAssetScript(
              s"""
                 |match tx {
                 |  case tr: InvokeScriptTransaction => throw()
                 |  case _ => true
                 |}
                 |""".stripMargin
            )
          )
        )
        Case(
          "NODE-522 DApp completes successfully, but asset script fails for payments",
          s"Transaction is not allowed by script of the asset ${bobSmartAssetTx.id()}",
          { targetComplexity =>
            val baseComplexity = 1 + 2 // 1 for strict, 2 for asset script
            mkFooScript(
              s"""
                 |strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 |[]
                 |""".stripMargin
            )
          },
          invokeTx = TxHelpers
            .invoke(dApp = aliceAddr, invoker = invoker, func = Some("foo"), fee = 3.waves, payments = Seq(Payment(1, bobSmartAssetTx.asset))),
          ethInvokeTx = Some(EthTxGenerator.generateEthInvoke(ethInvoker, aliceAddr, "foo", Seq.empty, Seq(Payment(1, bobSmartAssetTx.asset)))),
          knownTxs = Seq(
            bobSmartAssetTx,
            TxHelpers.transfer(bob, invokerAddr, 1, bobSmartAssetTx.asset),
            TxHelpers.transfer(bob, ethInvokerAddr, 1, bobSmartAssetTx.asset)
          )
        )
      },
      Case(
        "NODE-540 If an invoke exceeds the limit of writes",
        "Stored data count limit is exceeded",
        { targetComplexity =>
          val baseComplexity = 1 + 101 + 101 // 1 for strict, 101 for list, 101 for IntegerEntry
          mkFooScript(
            s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
               | [${(1 to 101).map(i => s"""IntegerEntry("i$i", 1)""").mkString(", ")}]
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
          mkFooScript(
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
              val baseComplexity = 1 + 100 + 100 // 1 for strict, 100 for list, 100 for IntegerEntry
              mkScript(
                s""" @Callable(inv)
                   | func bar() = {
                   |   strict complexInt = ${mkIntExprWithComplexity(500 - baseComplexity)}
                   |   [${(1 to 100).map(i => s"""IntegerEntry("i$i", 1)""").mkString(", ")}]
                   | }
                   | """.stripMargin
              )(V5)
            }
          )
        ),
        supportedVersions = inV5V6
      ),
      Case(
        "NODE-542 If an invoke exceeds the limit of writes size",
        "WriteSet size can't exceed 5120 bytes, actual: 5121 bytes",
        { targetComplexity =>
          val baseComplexity = 1 + 2 + 1 + 1 // 1 for strict, 2 for list, 1 for BinaryEntry, 1 for IntegerEntry
          // See DataTxValidator.realUserPayloadSize: IntegerDataEntry - 8, "b" and "i" keys - 2
          val limitedBinaryEntrySize = ContractLimits.MaxWriteSetSizeInBytes - 8 - 2
          mkFooScript(
            s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
               | [
               |   BinaryEntry("b", base64'${Base64.encode(Array.fill[Byte](limitedBinaryEntrySize + 1)(0))}'),
               |   IntegerEntry("i", 1)
               | ]""".stripMargin
          )
        }
      ),
      Case(
        "NODE-542 If an inner invoke exceeds the limit of writes size",
        "Storing data size should not exceed 15360, actual: 15361 bytes",
        { targetComplexity =>
          // qux: 4 = 2 for list, 1 for BinaryEntry, 1 for IntegerEntry
          // baz: 78 = 1 for strict, 75 for invoke, 1 for list, 1 for BinaryEntry
          // bar: 78 = 1 for strict, 75 for invoke, 1 for list, 1 for BinaryEntry
          // foo: 78 = 1 for strict, 75 for invoke, 1 for list, 1 for BinaryEntry
          val baseComplexity = 4 + 78 * 3
          // See DataTxValidator.realUserPayloadSize: 8 for IntegerDataEntry, 4 for 4 binary keys.
          // Also we have 1 integer key, that exceeds the limit.
          val oneEntrySize   = (ContractLimits.MaxTotalWriteSetSizeInBytes - 8 - 4) / 4 // % 4 == 0
          val binaryEntrySrc = s"""BinaryEntry("b", base64'${Base64.encode(Array.fill[Byte](oneEntrySize)(0))}')"""

          mkScript(
            s""" @Callable(inv)
               | func qux() = {
               |   let complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
               |   [$binaryEntrySrc, IntegerEntry("i", complexInt)]
               | }
               |
               | @Callable(inv)
               | func baz() = {
               |   strict res = invoke(this, "qux", [], [])
               |   [$binaryEntrySrc]
               | }
               |
               | @Callable(inv)
               | func bar() = {
               |   strict res = invoke(this, "baz", [], [])
               |   [$binaryEntrySrc]
               | }
               |
               | @Callable(inv)
               | func foo() = {
               |   strict res = invoke(this, "bar", [], [])
               |   [$binaryEntrySrc]
               | }
               | """.stripMargin
          )
        },
        supportedVersions = inV5V6
      ),
      Case(
        "NODE-544 If an invoke exceeds the limit of writes number through a WriteSet",
        "Stored data count limit is exceeded",
        { targetComplexity =>
          val baseComplexity = 1 + 101 + 2 * 101 + 1 // 1 for strict, 101 for list, 2 * 101 for DataEntry, 1 for WriteSet
          mkFooScript(
            s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
               | WriteSet([${(1 to 101).map(i => s"""DataEntry("i$i", 1)""").mkString(", ")}])
               | """.stripMargin
          )
        },
        supportedVersions = Seq(V3)
      )
    ) ++ {
      val dataEntries = Seq(
        "Binary"  -> s"""BinaryEntry("", base58'${Base58.encode("test".getBytes(StandardCharsets.UTF_8))}')""",
        "Boolean" -> """BooleanEntry("", false)""",
        "Delete"  -> """DeleteEntry("")""",
        "Integer" -> """IntegerEntry("", 0)""",
        "String"  -> """StringEntry("", "lie")"""
      )

      def mkDAppFunc(entry: String): Int => StdLibVersion => Script = { targetComplexity =>
        val baseComplexity = 1 + 2 + 1 + 1 // 1 for strict, 2 for list (two elements), 1 for IntegerEntry, 1 for test entry
        mkFooScript(
          s"""
             | strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
             | [IntegerEntry("k", 1), $entry]
             |""".stripMargin
        )
      }

      dataEntries.map { case (entryType, entry) =>
        Case(
          s"NODE-546 If an invoke writes an empty $entryType key to the state",
          "Data entry key should not be empty",
          mkDAppFunc(entry),
          invokeTx = aliceInvokeTx.copy(version = TxVersion.V1).signWith(invoker.privateKey)
        )
      } ++
        dataEntries.map { case (entryType, entry) =>
          Case(
            s"NODE-546 If an invoke writes an empty $entryType key to the state (invoke version >= 2)",
            "Empty keys aren't allowed in tx version >= 2",
            mkDAppFunc(entry),
            ethInvokeTx = None
          )
        }
    } ++ {
      def mkNode548OneActionTypeScript(actionSrc: String): Int => StdLibVersion => Script = { targetComplexity =>
        // 1+1 for strict, 1 for Address, 101 for list, 101 for actions
        val baseComplexity = 1 + 1 + 1 + 101 + 101
        mkFooScript(
          s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
             | strict to = Address(base58'$bobAddr')
             | [${(1 to 101).map(_ => actionSrc).mkString(", ")}]
             | """.stripMargin
        )
      }

      Seq(
        Case(
          "NODE-548 If an invoke exceeds the limit of ScriptTransfer actions",
          "Actions count limit is exceeded",
          mkNode548OneActionTypeScript("ScriptTransfer(to, 1, unit)"),
          supportedVersions = Seq(V4)
        )
      ) ++ Seq(
        "ScriptTransfer" -> "ScriptTransfer(to, 1, unit)",
        "Lease"          -> "Lease(to, 1)",
        "LeaseCancel"    -> s"LeaseCancel(base58'$bobLeasingId')"
      ).flatMap { case (actionType, actionSrc) =>
        Seq(
          Case(
            s"NODE-548 If an invoke exceeds the limit of $actionType actions",
            "Actions count limit is exceeded",
            mkNode548OneActionTypeScript(actionSrc),
            supportedVersions = Seq(V5)
          ),
          Case(
            s"NODE-548 If an invoke exceeds the limit of $actionType actions",
            "ScriptTransfer, Lease, LeaseCancel actions count limit is exceeded",
            mkNode548OneActionTypeScript(actionSrc),
            supportedVersions = Seq(V6)
          )
        )
      } ++ Seq(
        V5 -> "Actions count limit is exceeded",
        V6 -> "ScriptTransfer, Lease, LeaseCancel actions count limit is exceeded"
      ).map { case (v, rejectError) =>
        Case(
          "NODE-548 If an invoke exceeds the limit of mixed non-data actions",
          rejectError,
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 101 + 101 // 1 for strict, 1 for Address, 101 for list, 101 for actions
            mkFooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let to = Address(base58'$bobAddr')
                 | [
                 |   ${(1 to 34).map(_ => s"""ScriptTransfer(to, 1, unit)""").mkString(", ")},
                 |   ${(1 to 34).map(_ => s"""Lease(to, 1)""").mkString(", ")},
                 |   ${(1 to 33).map(_ => s"""LeaseCancel(base58'$bobLeasingId')""").mkString(", ")}
                 | ]
                 | """.stripMargin
            )
          },
          supportedVersions = Seq(v)
        )
      }
    } ++
      Seq(
        Case(
          "NODE-550 If an invoke sends a self-payment",
          "DApp self-payment is forbidden since V4",
          invokeTx = TxHelpers
            .invoke(
              dApp = aliceAddr,
              invoker = alice,
              func = Some("foo"),
              fee = 3.waves,
              payments = Seq(InvokeScriptTransaction.Payment(1, Waves))
            ),
          ethInvokeTx = None
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
            val baseComplexity = 1 + 1 + 1 + 1 // 1 for strict, 1 for list, 1 for ScriptTransfer, 1 for Address
            mkFooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | [ScriptTransfer(Address(base58'$bobAddr'), -1, unit)]
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
            mkFooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let to = Address(base58'$bobAddr')
                 | [ScriptTransfer(to, wavesBalance(Address(base58'$aliceAddr')).available - ${aliceInvokeTx.fee}, unit)]
                 | """.stripMargin
            )
          },
          initBalances = Map(
            aliceAddr      -> Long.MaxValue,
            bobAddr        -> ENOUGH_AMT,
            invokerAddr    -> ENOUGH_AMT,
            ethInvokerAddr -> ENOUGH_AMT
          )
        ),
        Case(
          "NODE-556 If an invoke leads to a Waves overflow with a ScriptTransfer (multiple transfers)",
          "negative waves balance",
          { targetComplexity =>
            // 1 for strict, 1 for Address, 1 for ScriptTransfer, 10 for wavesBalance, 1 for Address(alice), 1 for "-",
            // 2 for list
            val baseComplexity = 1 + 1 + 1 + 10 + 1 + 1 + 2
            mkFooScript(
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
            // 1 for strict, 1 for ScriptTransfer, 1 for Address, 2 for list
            val baseComplexity = 1 + 1 + 1 + 2
            mkFooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let transfer = ScriptTransfer(Address(base58'$bobAddr'), ${Long.MaxValue - 1}, base58'$aliceRegularAssetId')
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
            val baseComplexity = 1 + 1 + 1 + 1 // 1 for strict, 1 for list, 1 for ScriptTransfer, 1 for Address
            mkFooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | [ScriptTransfer(Address(base58'$bobAddr'), complexInt, base58'$invalidAssetId')]
                 | """.stripMargin
            )
          }
        ),
        Case(
          "NODE-568 If an invoke sends a ScriptTransfer with an unknown assetId",
          s"Transfer error: asset '$bobAssetId' is not found on the blockchain",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 + 1 // 1 for strict, 1 for list, 1 for ScriptTransfer, 1 for Address
            mkFooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | [ScriptTransfer(Address(base58'$bobAddr'), complexInt, base58'$bobAssetId')]
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
        ("ScriptTransfer", "ScriptTransfer(to, complexInt, unit)", inV4V6),
        ("Lease", "Lease(to, complexInt)", inV5V6)
      ).map { case (actionType, actionSrc, supportedVersions) =>
        Case(
          s"NODE-578 If an invoke sends $actionType with an address of another network",
          "Address belongs to another network",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 + 1 // 1 for strict, 1 for Address, 1 for list, 1 for action
            mkFooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let to = Address(base58'$bobOtherChainAddr')
                 | [$actionSrc]
                 | """.stripMargin
            )
          },
          supportedVersions = supportedVersions
        )
      } ++ Seq(
        Case(
          "NODE-578 If a transaction sends an invoke with address of another network",
          "Address belongs to another network",
          { targetComplexity =>
            val baseComplexity = 1 + 75 + 1 + 1 // 1 for strict, 75 for invoke, 1 for Address, 1 for list ([complexInt])
            mkFooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | strict res = invoke(Address(base58'$bobOtherChainAddr'), "bar", [complexInt], [])
                 | []
                 | """.stripMargin
            )
          },
          supportedVersions = inV5V6
        )
      ) ++ {
        for {
          (actionType, actionSrc, supportedVersions) <- Seq(
            ("ScriptTransfer", "ScriptTransfer(to, 1, unit)", inV4V6),
            ("Lease", "Lease(to, 1)", inV5V6)
          )
          addrLen <- Seq(0, Address.AddressLength - 1, Address.AddressLength + 1)
        } yield Case(
          s"NODE-580 If an invoke sends $actionType with an invalid address (len=$addrLen)",
          "Wrong addressBytes length",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 + 1 // 1 for strict, 1 for Address, 1 for list, 1 for action
            mkFooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let to = Address(base58'${"1" * addrLen}')
                 | [$actionSrc]
                 | """.stripMargin
            )
          },
          supportedVersions = supportedVersions
        )
      } ++ Seq(
        ("negative", -1, "Negative lease amount = -1"),
        ("zero", 0, "NonPositiveAmount(0,waves)")
      ).map { case (tpe, leaseAmount, rejectError) =>
        Case(
          s"NODE-584 If an invoke leases $tpe amount",
          rejectError,
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 + 1 // 1 for strict, 1 for list, 1 for Lease, 1 for Address
            mkFooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | [Lease(Address(base58'$bobAddr'), $leaseAmount)]
                 | """.stripMargin
            )
          },
          supportedVersions = inV5V6
        )
      } ++ Seq(
        Case(
          "NODE-586 If an invoke does a self-leasing",
          "Cannot lease to self",
          mkScriptWithOneAction("Lease(this, 1)"),
          supportedVersions = inV5V6
        ),
        Case(
          "NODE-588 If an invoke does the same leasing multiple times",
          "is already in the state",
          { targetComplexity =>
            val baseComplexity = 1 + 1 + 1 + 2 // 1 for strict, 1 for Lease, 1 for Address, 2 for list
            mkFooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let lease = Lease(Address(base58'$bobAddr'), 1, 3)
                 | [lease, lease]
                 | """.stripMargin
            )
          },
          supportedVersions = inV5V6
        ),
        Case(
          "NODE-590 If an invoke leases the nonexistent funds",
          "Cannot lease more than own",
          { targetComplexity =>
            // 1 for strict, 1 for Address(bob), 10 for wavesBalance, 1 for Address(alice), 2 for list, 1+1 for Lease
            val baseComplexity = 1 + 1 + 10 + 1 + 2 + 1 + 1
            mkFooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let to = Address(base58'$bobAddr')
                 | let available = wavesBalance(Address(base58'$aliceAddr')).available
                 | [Lease(to, available, 1), Lease(to, available, 2)]
                 | """.stripMargin
            )
          },
          supportedVersions = inV5V6
        ),
        Case(
          "NODE-592 If an invoke cancels a leasing with a wrong lease id",
          s"Lease id=${bobLeasingId.toString.take(5)} has invalid length",
          mkScriptWithOneAction(s"LeaseCancel(base58'${bobLeasingId.toString.take(5)}')"),
          supportedVersions = inV5V6
        ),
        Case(
          "NODE-594 If an invoke cancels an unknown leasing",
          s"Lease with id=$bobLeasingId not found",
          mkScriptWithOneAction(s"LeaseCancel(base58'$bobLeasingId')"),
          supportedVersions = inV5V6
        ),
        Case(
          "NODE-596 If an invoke cancels the same leasing twice",
          "Duplicate LeaseCancel id(s)",
          { targetComplexity =>
            val baseComplexity = 1 + 2 + 1 // 1 for strict, 2 for list, 1 for LeaseCancel
            mkFooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | let cancel = LeaseCancel(base58'$aliceLeasingId')
                 | [cancel, cancel]
                 | """.stripMargin
            )
          },
          knownTxs = Seq(aliceLeasingTx),
          supportedVersions = inV5V6
        ),
        Case(
          "NODE-596 If an invoke cancels the already cancelled leasing",
          "Cannot cancel already cancelled lease",
          mkScriptWithOneAction(s"LeaseCancel(base58'$aliceLeasingId')"),
          knownTxs = Seq(aliceLeasingTx, TxHelpers.leaseCancel(aliceLeasingId, sender = alice)),
          supportedVersions = inV5V6
        ),
        Case(
          "NODE-598 If an invoke cancels another account leasing",
          "LeaseTransaction was leased by other sender and time",
          mkScriptWithOneAction(s"LeaseCancel(base58'$bobLeasingId')"),
          knownTxs = Seq(bobLeasingTx),
          supportedVersions = inV5V6
        )
      ) ++ Seq(
        ("false", 0, "Transaction is not allowed by script of the asset"),
        ("""throw("test error")""", 1, "test error")
      ).map { case (assetScriptBody, assetScriptComplexity, expectingError) =>
        val aliceRejectingSmartAssetTx = TxHelpers.issue(
          issuer = alice,
          script = Some(mkAssetScript(assetScriptBody))
        )
        Case(
          s"NODE-604 If an invoke failed by a asset script ($assetScriptBody)",
          expectingError,
          { targetComplexity =>
            // 1 for strict, 1 for list, 1 for ScriptTransfer, 1 for Address
            val baseComplexity = 1 + 1 + 1 + 1 + assetScriptComplexity
            mkFooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | [ScriptTransfer(Address(base58'$bobAddr'), 1, base58'${aliceRejectingSmartAssetTx.id()}')]
                 | """.stripMargin
            )
          },
          knownTxs = Seq(aliceRejectingSmartAssetTx)
        )
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
            mkFooScript(
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
            // 1+1 for strict, 75 for invoke, 1 for Address, 1 for list, 500 for bob.foo() body
            val baseComplexity = 1 + 1 + 75 + 1 + 1 + 500
            mkFooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | strict res = invoke(Address(base58'$bobAddr'), "bar", [complexInt], [])
                 | []
                 | """.stripMargin
            )
          },
          knownTxs = Seq(
            TxHelpers.setScript(
              bob, {
                // 1 for strict, 1 for Address(alice), 1 for list, 1 for ScriptTransfer, 10 for wavesBalance,
                //   1 for Address(bob) and 1 for "+"
                val baseComplexity = 1 + 1 + 1 + 10 + 1 + 1
                mkScript(
                  s""" @Callable(inv)
                     | func bar(n: Int) = {
                     |   strict complexInt = ${mkIntExprWithComplexity(500 - baseComplexity)}
                     |   let to = Address(base58'$aliceAddr')
                     |   [ScriptTransfer(to, wavesBalance(Address(base58'$bobAddr')).available + 1, unit)]
                     | }
                     | """.stripMargin
                )(V5)
              }
            )
          ),
          supportedVersions = inV5V6
        ),
        Case(
          "NODE-698 If an inner invoke to an account without a script",
          "No contract at address",
          { targetComplexity =>
            // 1 for strict, 75 for invoke, 1 for Address, 1 for list
            val baseComplexity = 1 + 75 + 1 + 1
            mkFooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 | strict res = invoke(Address(base58'$bobAddr'), "bar", [1], [])
                 | []
                 | """.stripMargin
            )
          },
          supportedVersions = inV5V6
        ),
        Case(
          "NODE-760 If an inner invoke has a payment with an unknown assetId",
          s"Transfer error: asset '$bobAssetId' is not found on the blockchain",
          { targetComplexity =>
            // bar: 1 for strict, 1 for list, 1 for ScriptTransfer, 1 for Address
            // foo: 75 for invoke
            val baseComplexity = 1 + 1 + 1 + 1 + 75
            mkScript(
              s""" @Callable(inv)
                 | func bar() = {
                 |   strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
                 |   [ScriptTransfer(Address(base58'$bobAddr'), 1, base58'$bobAssetId')]
                 | }
                 |
                 | @Callable(inv)
                 | func foo() = {
                 |   strict res = invoke(this, "bar", [], [])
                 |   []
                 | }
                 | """.stripMargin
            )
          },
          supportedVersions = inV5V6
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
            mkFooScript(
              s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
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
        testCase.supportedVersions.foreach { v =>
          s"$v" - {
            "<=1000 complexity - rejected" in rejectTxTest(testCase.invokeTx)
            ">1000 complexity - failed" in failTxTest(testCase.invokeTx)

            testCase.ethInvokeTx.foreach { ethInvoke =>
              "<=1000 complexity - rejected (Ethereum)" in rejectTxTest(ethInvoke)
              ">1000 complexity - failed (Ethereum)" in failTxTest(ethInvoke)
            }

            def test(complexity: Int)(f: Domain => Unit): Unit =
              withDomain(
                DomainPresets.RideV6,
                testCase.initBalances.map(Function.tupled(AddrWithBalance.apply)).toSeq
              ) { d =>
                val setScriptTx = TxHelpers.setScript(alice, testCase.mkDApp(complexity)(v), 1.waves)
                d.appendBlock((testCase.knownTxs :+ setScriptTx)*)
                f(d)
              }

            def rejectTxTest(invoke: Transaction): Unit =
              test(ContractLimits.FailFreeInvokeComplexity - 300) { d =>
                d.createDiffE(invoke) should produce(testCase.rejectError)
              }

            def failTxTest(invoke: Transaction): Unit = {
              val complexity = ContractLimits.FailFreeInvokeComplexity + 1
              test(complexity) { d =>
                val diff              = d.createDiffE(invoke).value
                val (_, scriptResult) = diff.scriptResults.headOption.value
                scriptResult.error.value.text should include(testCase.rejectError)

                d.appendBlock(invoke)
                val invokeTxMeta = d.transactionsApi.transactionById(invoke.id()).value
                invokeTxMeta.spentComplexity shouldBe complexity
                invokeTxMeta.status == Status.Succeeded shouldBe false
              }
            }
          }
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
      mkDApp: Int => StdLibVersion => Script = defaultScript,
      invokeTx: InvokeScriptTransaction = aliceInvokeTx,
      ethInvokeTx: Option[EthereumTransaction] = Some(ethAliceInvokeTx),
      knownTxs: Seq[Transaction] = Seq.empty,
      initBalances: Map[Address, Long] = defaultInitWavesBalances,
      supportedVersions: Seq[StdLibVersion] = inV4V6
  )

  private object Case {
    def withInnerPayment(title: String, rejectError: String, payment: String): Case = Case(
      title,
      rejectError,
      { targetComplexity =>
        // 1 for strict, 75 for invoke, 1 for Address, 1+1 for lists, 1 for payment
        val baseComplexity = 1 + 75 + 1 + 1 + 1 + 1
        mkScript(
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
            mkScript(
              s""" @Callable(inv)
                 | func bar(n: Int) = {
                 |   strict complexInt = ${mkIntExprWithComplexity(500 - baseComplexity)}
                 |   []
                 | }
                 | """.stripMargin
            )(V5)
          }
        )
      ),
      supportedVersions = inV5V6
    )
  }

  private def mkScriptWithOneAction(actionSrc: String): Int => StdLibVersion => Script = { targetComplexity =>
    val baseComplexity = 1 + 1 + 1 // 1 for strict, 1 for list, 1 for action
    mkFooScript(
      s""" strict complexInt = ${mkIntExprWithComplexity(targetComplexity - baseComplexity)}
         | [$actionSrc]
         | """.stripMargin
    )
  }

  private def mkFooScript(fooBody: String): StdLibVersion => Script = mkScript(
    s""" @Callable(inv)
       | func foo() = {
       |   $fooBody
       | }
       | """.stripMargin
  )

  private def mkScript(scriptBody: String): StdLibVersion => Script = { v =>
    TestCompiler(v).compileContract(scriptBody)
  }

  private def mkAssetScript(scriptBody: String): Script = TestCompiler(V5).compileAsset(scriptBody)

  private def mkIntExprWithComplexity(targetComplexity: Int): String = s"1${" + 1" * targetComplexity}"
}
