package com.wavesplatform.events

import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff
import com.wavesplatform.state.diffs.{BlockDiffer, ENOUGH_AMT}
import com.wavesplatform.state.{Blockchain, Diff}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{BlockchainUpdater, GenesisTransaction, Transaction}
import com.wavesplatform.{BlockGen, TestHelpers, crypto}
import monix.execution.Scheduler.Implicits.global
import monix.reactive.subjects.ReplaySubject
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration._

class BlockchainUpdateTriggersImplSpec extends FreeSpec with Matchers with BlockGen with ScalaCheckPropertyChecks with WithBlockchain {
  override protected def settings: WavesSettings = TestHelpers.enableNG(super.settings)

  // add a genesis block to the blockchain
  private val master: KeyPair = accountGen.sample.get
  private val genesis         = TestBlock.create(0, Seq(GenesisTransaction.create(master, ENOUGH_AMT, 0).explicitGet()), master)
  override protected def initBlockchain(blockchainUpdater: Blockchain with BlockchainUpdater): Unit = {
    blockchainUpdater.processBlock(genesis).explicitGet()
    super.initBlockchain(blockchainUpdater)
  }

  private val sigGen: Gen[ByteStr]   = bytes64gen.map(ByteStr.apply)
  private val heightGen: Gen[Int]    = Gen.choose(1, 1000)
  private val assetAmtGen: Gen[Long] = Gen.oneOf(Gen.const[Long](1), Gen.choose[Long](2, ENOUGH_AMT))

  private def microBlockGen(txs: Seq[Transaction], signer: KeyPair): Gen[MicroBlock] =
    for {
      sig <- byteArrayGen(crypto.SignatureLength).map(ByteStr.apply)
      mb = MicroBlock.buildAndSign(3.toByte, signer, txs, genesis.signature, sig).explicitGet()
    } yield mb

  private def produceEvent(useTrigger: BlockchainUpdateTriggers => Unit): BlockchainUpdated = {
    val evts = ReplaySubject[BlockchainUpdated]()
    val t    = new BlockchainUpdateTriggersImpl(evts)
    useTrigger(t)
    evts.onComplete()
    evts.toListL.runSyncUnsafe(500.milliseconds).head
  }

  private def detailedDiffFromBlock(b: Block): DetailedDiff =
    BlockDiffer.fromBlock(blockchain, None, b, MiningConstraint.Unlimited, verify = false).explicitGet().detailedDiff

  private def appendBlock(b: Block): BlockAppended =
    produceEvent(_.onProcessBlock(b, detailedDiffFromBlock(b), blockchain)) match {
      case ba: BlockAppended => ba
      case _                 => fail()
    }

  private def appendMicroBlock(mb: MicroBlock): MicroBlockAppended = {
    val dd = BlockDiffer.fromMicroBlock(blockchain, Some(0), mb, 1, MiningConstraint.Unlimited, verify = false).explicitGet().detailedDiff
    produceEvent(_.onProcessMicroBlock(mb, dd, blockchain)) match {
      case mba: MicroBlockAppended => mba
      case _                       => fail()
    }
  }

  /**
    * Tests the assertion both for transactions added in a block and in a microblock
    */
  private def testTxsStateUpdates[A](txs: Seq[Transaction])(assertion: Seq[StateUpdate] => A): Unit = {
    val b = blockGen(txs, master).sample.get
    assertion(appendBlock(b).transactionStateUpdates)

    val mb = microBlockGen(txs, master).sample.get
    assertion(appendMicroBlock(mb).transactionStateUpdates)
  }

  "rollbacks correctly" - {
    "block" in forAll(sigGen, heightGen) { (sig, height) =>
      produceEvent(_.onRollback(sig, height)) match {
        case RollbackCompleted(toId, toHeight) =>
          toId shouldBe sig
          toHeight shouldBe height
        case _ => fail()
      }
    }

    "microblock" in forAll(sigGen, heightGen) { (sig, height) =>
      produceEvent(_.onMicroBlockRollback(sig, height)) match {
        case MicroBlockRollbackCompleted(toId, toHeight) =>
          toId shouldBe sig
          toHeight shouldBe height
        case _ => fail()
      }
    }
  }

  "appends correctly" - {
    "empty block" in forAll {
      for {
        b <- blockGen(Seq.empty, master)
        ba = appendBlock(b)
      } yield (b, ba)
    } {
      case (b, BlockAppended(toId, toHeight, block, _, _)) =>
        toId shouldBe b.signature
        toHeight shouldBe blockchain.height + 1

        block.signature shouldBe b.signature
        block.transactionData shouldBe b.transactionData
    }

    "microblock with one transaction" in forAll {
      for {
        tx <- dataTransactionGen(0)
        mb <- microBlockGen(Seq(tx), master)
        mba = appendMicroBlock(mb)
      } yield (mb, mba)
    } {
      case (mb, MicroBlockAppended(toId, toHeight, microBlock, _, _)) =>
        toId shouldBe mb.totalResBlockSig
        toHeight shouldBe blockchain.height

        microBlock.signature shouldBe mb.signature
        microBlock.transactionData shouldBe mb.transactionData
    }

    "including correct miner rewards for" - {
      "block" in forAll {
        for {
          miner <- accountGen
          tx    <- dataTransactionGen(0)
          mb    <- blockGen(Seq(tx), miner)
          ba = appendBlock(mb)
        } yield (tx, miner, ba.blockStateUpdate)
      } { case (tx, miner, su) => su.balances.find(_._1 == miner.publicKey.toAddress).get._3 shouldBe 0.4 * tx.fee }

      "microblock, giving reward to a key block miner" in forAll {
        for {
          tx <- dataTransactionGen(0)
          mb <- microBlockGen(Seq(tx), master)
          mba = appendMicroBlock(mb)
        } yield (tx, mba.microBlockStateUpdate)
      } {
        case (tx, su) =>
          su.balances.find(_._1 == master.publicKey.toAddress).get._3 shouldBe (0.4 * tx.fee + ENOUGH_AMT)
      }
    }

    "block/microblock with balance updates from transfer txs" in forAll {
      for {
        sender        <- accountGen
        recipient     <- accountGen
        master2sender <- transferGeneratorPV2(1, master, sender, ENOUGH_AMT / 3)
        fee           <- Gen.choose(1, master2sender.amount - 1)
        sender2recipient = TransferTransaction
          .selfSigned(2.toByte, sender, recipient, Waves, master2sender.amount - fee, Waves, fee, None, 2)
          .explicitGet()
      } yield (sender, recipient, master2sender, sender2recipient)
    } {
      case (sender, recipient, master2sender, sender2recipient) =>
        testTxsStateUpdates(Seq(master2sender, sender2recipient)) { transactionStateUpdates =>
          transactionStateUpdates.last.balances.find(_._1 == sender.publicKey.toAddress).get._3 shouldBe 0

          transactionStateUpdates.last.balances.find(_._1 == recipient.publicKey.toAddress).get._3 shouldBe
            sender2recipient.amount
        }
    }

    "block/microblock with a data transaction" in forAll(dataTransactionGen) { tx =>
      testTxsStateUpdates(Seq(tx)) {
        _.head.dataEntries.map(_._2).sortBy(_.key) shouldBe tx.data.sortBy(_.key)
      }
    }

    "blocks/microblocks with correct asset state updates by" - {
      "issue transaction" in forAll(issueV2TransactionGen(Gen.const(master))) { tx =>
        testTxsStateUpdates(Seq(tx)) {
          _.head.assets.head match {
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

      "reissue transaction" in forAll {
        for {
          issueAmt            <- assetAmtGen
          reissueAmt          <- assetAmtGen
          (issue, reissue, _) <- issueReissueBurnGeneratorP(issueAmt, reissueAmt, 1, master).suchThat(_._1.reissuable)
        } yield (issue, reissue)
      } {
        case (issue, reissue) =>
          testTxsStateUpdates(Seq(issue, reissue)) { transactionStateUpdates =>
            val reissueUpdates = transactionStateUpdates.last.assets

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

      "burn transaction" in forAll {
        for {
          issueAmt         <- assetAmtGen
          burnAmt          <- Gen.choose(1, issueAmt)
          (issue, _, burn) <- issueReissueBurnGeneratorP(issueAmt, 1, burnAmt, master)
        } yield (issue, burn)
      } {
        case (issue, burn) =>
          testTxsStateUpdates(Seq(issue, burn)) {
            _.last.assets.head match {
              case UpdateAssetVolume(_, newVolume) =>
                newVolume shouldBe (issue.quantity - burn.quantity)
              case _ => fail()
            }
          }
      }

      "set asset script transaction" in forAll(issueAndSetAssetScriptGen(master)) {
        case (issue, setAssetScript) =>
          testTxsStateUpdates(Seq(issue, setAssetScript)) {
            _.last.assets.head match {
              case SetAssetScript(asset, script) =>
                asset.id shouldBe issue.id()
                script shouldBe setAssetScript.script
              case _ => fail()
            }
          }
      }

      "sponsor fee transaction " in forAll(sponsorFeeCancelSponsorFeeGen(master)) {
        case (issue, setSponsorship, _, cancelSponsorship) =>
          testTxsStateUpdates(Seq(issue, setSponsorship, cancelSponsorship)) { transactionStateUpdates =>
            // set sponsoprship
            transactionStateUpdates(1).assets.head match {
              case StartSponsorship(asset, sponsorship) =>
                asset.id shouldBe issue.id()
                sponsorship shouldBe setSponsorship.minSponsoredAssetFee.get
              case _ => fail()
            }

            // cancel sponsorship
            transactionStateUpdates.last.assets.head match {
              case CancelSponsorship(asset) =>
                asset.id shouldBe issue.id()
              case _ => fail()
            }
          }
      }

      "invokeScript transaction (diff emulated by issue, reussue and burn txs)" in forAll {
        for {
          issueAmt            <- assetAmtGen
          reissueAmt          <- assetAmtGen
          (issue, reissue, _) <- issueReissueBurnGeneratorP(issueAmt, reissueAmt, 1, master).suchThat(_._1.reissuable)
          invoke              <- invokeScriptGen(Gen.const(Seq.empty))
        } yield (issue, reissue, invoke)
      } {
        case (issue, reissue, invoke) =>
          // create a block with issue and reissue txs, getting their diffs
          val assetsDummyBlock     = TestBlock.create(master, Seq(issue, reissue))
          val assetsDummyBlockDiff = detailedDiffFromBlock(assetsDummyBlock)

          val invokeBlock = TestBlock.create(master, Seq(invoke))
          // merge issue/reissue diffs as if they were produced by a single invoke
          val invokeTxDiff = assetsDummyBlockDiff.transactionDiffs
            .foldLeft(Diff.empty)(Diff.diffMonoid.combine)
            .copy(transactions = Map((invoke.id(), (invoke, Set(master)))))
          val invokeBlockDetailedDiff = assetsDummyBlockDiff.copy(transactionDiffs = Seq(invokeTxDiff))

          produceEvent(_.onProcessBlock(invokeBlock, invokeBlockDetailedDiff, blockchain)) match {
            case ba: BlockAppended =>
              val i = ba.transactionStateUpdates.head.assets.collectFirst { case u: Issue => u }.get
              i.asset.id shouldBe issue.id()

              val uav = ba.transactionStateUpdates.head.assets.collectFirst { case u: UpdateAssetVolume => u }
              uav.map(_.newVolume) shouldBe Some((BigInt(issue.quantity) + BigInt(reissue.quantity)))

              // if asset reussue has been forbidden, should get a corresponding event
              val fr: Option[ForbidReissue] = ba.transactionStateUpdates.head.assets.collectFirst { case u: ForbidReissue => u }
              fr.isDefined shouldBe !reissue.reissuable
            case _ => fail()
          }
      }
    }
  }
}
