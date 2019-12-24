package com.wavesplatform.events

import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff
import com.wavesplatform.state.diffs.{BlockDiffer, ENOUGH_AMT}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.{NoShrink, RequestGen}
import monix.execution.Scheduler.Implicits.global
import monix.reactive.subjects.ReplaySubject
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration._

class BlockchainUpdateTriggersImplSpec extends FreeSpec with Matchers with RequestGen with ScalaCheckPropertyChecks with NoShrink with WithDomain {

  private val sigGen: Gen[ByteStr]   = bytes64gen.map(ByteStr.apply)
  private val heightGen: Gen[Int]    = Gen.choose(1, 1000)
  private val assetAmtGen: Gen[Long] = Gen.oneOf(Gen.const[Long](1), Gen.choose[Long](2, ENOUGH_AMT))

  private def produceEvent(useTrigger: BlockchainUpdateTriggers => Unit): BlockchainUpdated = {
    val evts = ReplaySubject[BlockchainUpdated]()
    val t    = new BlockchainUpdateTriggersImpl(evts)
    useTrigger(t)
    evts.onComplete()
    evts.toListL.runSyncUnsafe(500.milliseconds).head
  }

  private def appendBlock(b: Block, bc: Blockchain): BlockAppended =
    produceEvent(_.onProcessBlock(b, detailedDiffFromBlock(b, bc), bc)) match {
      case ba: BlockAppended => ba
      case _                 => fail()
    }

  private def withBlockchainAndGenesis[A](test: (Blockchain, KeyPair) => A): A =
    withDomain() { d =>
      val masterAccount = accountGen.sample.get
      val genesisBlock = TestBlock
        .create(0, Seq(GenesisTransaction.create(masterAccount, ENOUGH_AMT, 0).explicitGet()))
      d.blockchainUpdater.processBlock(genesisBlock).explicitGet()
      test(d.blockchainUpdater, masterAccount)
    }

  private def detailedDiffFromBlock(b: Block, bc: Blockchain): DetailedDiff =
    BlockDiffer.fromBlock(bc, None, b, MiningConstraint.Unlimited, verify = false).explicitGet().detailedDiff

  private def detailedDiffFromMicroBlock(mb: MicroBlock, bc: Blockchain): DetailedDiff =
    BlockDiffer.fromMicroBlock(bc, Some(0), mb, 1, MiningConstraint.Unlimited, verify = false).explicitGet().detailedDiff

  private def areBlocksEqual(b1: Block, b2: Block): Boolean =
    b1.signature == b2.signature && b1.transactionData == b2.transactionData

  "rollbacks" - {
    "block rollback produces correct events in the observer" in forAll(sigGen, heightGen) { (sig, height) =>
      produceEvent(_.onRollback(sig, height)) match {
        case RollbackCompleted(toId, toHeight) =>
          toId shouldBe sig
          toHeight shouldBe height
        case _ => fail()
      }
    }

    "microblock rollback produces correct events in the observer" in forAll(sigGen, heightGen) { (sig, height) =>
      produceEvent(_.onMicroBlockRollback(sig, height)) match {
        case MicroBlockRollbackCompleted(toId, toHeight) =>
          toId shouldBe sig
          toHeight shouldBe height
        case _ => fail()
      }
    }
  }

  "appends" - {
    "empty block" in withBlockchainAndGenesis { (bc, _) =>
      val b                                          = TestBlock.create(1, Seq.empty)
      val BlockAppended(toId, toHeight, block, _, _) = appendBlock(b, bc)

      toId shouldBe b.signature
      toHeight shouldBe bc.height + 1
      areBlocksEqual(b, block) shouldBe true
    }

    "block with balance updates" in withBlockchainAndGenesis { (bc, master) =>
      forAll {
        for {
          recipient <- accountGen.map(_.publicKey.toAddress)
          miner     <- accountGen
          amt       <- Gen.choose(1, ENOUGH_AMT / 3)
          tx        <- transferGeneratorPV2(1, master, recipient, amt)
        } yield (recipient, miner, tx)
      } {
        case (recipient, miner, tx) =>
          val b                                                                 = TestBlock.create(miner, Seq(tx))
          val BlockAppended(_, _, _, blockStateUpdate, transactionStateUpdates) = appendBlock(b, bc)

          // miner reward
          blockStateUpdate.balances.head match {
            case (address, asset, newBalance) =>
              address shouldBe miner.publicKey.toAddress
              asset shouldBe Waves
              newBalance shouldBe tx.fee
          }

          // transferred Waves
          val masterUpd = transactionStateUpdates.head.balances.find(_._1 == master.publicKey.toAddress).get
          masterUpd._3 shouldBe (ENOUGH_AMT - tx.amount - tx.fee)

          val recipientUpd = transactionStateUpdates.head.balances.find(_._1 == recipient).get
          recipientUpd._3 shouldBe tx.amount
      }
    }

    "block with data entries" in withBlockchainAndGenesis { (bc, master) =>
      forAll(dataTransactionGen) { tx =>
        val b = TestBlock.create(master, Seq(tx))
        appendBlock(b, bc).transactionStateUpdates.head.dataEntries.map(_._2).sortBy(_.key) shouldBe tx.data.sortBy(_.key)
      }
    }

    "asset state updates" - {
      "issue" in withBlockchainAndGenesis { (bc, master) =>
        forAll(issueV2TransactionGen(Gen.const(master))) { tx =>
          val b = TestBlock.create(master, Seq(tx))
          appendBlock(b, bc).transactionStateUpdates.head.assets.head match {
            case Issue(asset, name, description, decimals, reissuable, volume, script, nft) =>
              asset.id shouldBe tx.id()
              name.left.get shouldBe tx.nameBytes
              description.left.get shouldBe tx.descriptionBytes
              decimals shouldBe tx.decimals
              reissuable shouldBe tx.reissuable
              volume shouldBe tx.quantity
              script shouldBe tx.script
              nft shouldBe (tx.quantity == 1 && decimals == 0 && !tx.reissuable)
            case _ => fail()
          }
        }
      }

      "reissue" in withBlockchainAndGenesis { (bc, master) =>
        forAll {
          for {
            issueAmt            <- assetAmtGen
            reissueAmt          <- assetAmtGen
            (issue, reissue, _) <- issueReissueBurnGeneratorP(issueAmt, reissueAmt, 1, master).suchThat(_._1.reissuable)
          } yield (issue, reissue)
        } {
          case (issue, reissue) =>
            val b              = TestBlock.create(master, Seq(issue, reissue))
            val reissueUpdates = appendBlock(b, bc).transactionStateUpdates.last.assets

            // update volume
            reissueUpdates.exists {
              case UpdateAssetVolume(_, newVolume) =>
                newVolume shouldBe (BigInt(issue.quantity) + BigInt(reissue.quantity))
                true
              case _ => false
            } shouldBe true

            // check forbid reissue
            if (!reissue.reissuable) {
              reissueUpdates.exists {
                case ForbidReissue(asset) =>
                  asset.id shouldBe issue.assetId
                  true
                case _ => false
              } shouldBe true
            }
        }
      }

      "burn" in withBlockchainAndGenesis { (bc, master) =>
        forAll {
          for {
            issueAmt         <- assetAmtGen
            burnAmt          <- Gen.choose(1, issueAmt)
            (issue, _, burn) <- issueReissueBurnGeneratorP(issueAmt, 1, burnAmt, master)
          } yield (issue, burn)
        } {
          case (issue, burn) =>
            val b = TestBlock.create(master, Seq(issue, burn))
            appendBlock(b, bc).transactionStateUpdates.last.assets.head match {
              case UpdateAssetVolume(_, newVolume) =>
                newVolume shouldBe (issue.quantity - burn.quantity)
              case _ => fail()
            }
        }
      }

      "set script" in withBlockchainAndGenesis { (bc, master) =>
        forAll(issueAndSetAssetScriptGen(master)) {
          case (issue, setAssetScript) =>
            val b = TestBlock.create(master, Seq(issue, setAssetScript))
            appendBlock(b, bc).transactionStateUpdates.last.assets.head match {
              case SetAssetScript(asset, script) =>
                asset.id shouldBe issue.id()
                script shouldBe setAssetScript.script
              case _ => fail()
            }
        }
      }

      "set sponsorship" in withBlockchainAndGenesis { (bc, master) =>
        forAll(sponsorFeeCancelSponsorFeeGen(master)) {
          case (issue, setSponsorship, _, cancelSponsorship) =>
            val txs                                                = Seq(issue, setSponsorship, cancelSponsorship)
            val b                                                  = TestBlock.create(master, txs)
            val BlockAppended(_, _, _, _, transactionStateUpdates) = appendBlock(b, bc)

            // set sponsoprship
            transactionStateUpdates(1).assets.head match {
              case SetSponsorship(asset, sponsorship) =>
                asset.id shouldBe issue.id()
                sponsorship shouldBe setSponsorship.minSponsoredAssetFee
              case _ => fail()
            }

            // cancel sponsorship
            transactionStateUpdates.last.assets.head match {
              case SetSponsorship(asset, sponsorship) =>
                asset.id shouldBe issue.id()
                sponsorship shouldBe cancelSponsorship.minSponsoredAssetFee
              case _ => fail()
            }
        }
      }

      // @todo invoke script with assets actions?
    }
  }
}
