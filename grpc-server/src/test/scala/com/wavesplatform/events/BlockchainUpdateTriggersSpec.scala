//package com.wavesplatform.events
//
//import com.wavesplatform.account.KeyPair
//import com.wavesplatform.block.MicroBlock
//import com.wavesplatform.common.state.ByteStr
//import com.wavesplatform.common.utils.EitherExt2
//import com.wavesplatform.features.EstimatorProvider
//import com.wavesplatform.history.Domain.BlockchainUpdaterExt
//import com.wavesplatform.lagonaki.mocks.TestBlock
//import com.wavesplatform.lang.script.Script
//import com.wavesplatform.protobuf.utils.PBImplicitConversions.PBByteStringOps
//import com.wavesplatform.settings.{Constants, WavesSettings}
//import com.wavesplatform.state.diffs.ENOUGH_AMT
//import com.wavesplatform.state.{Blockchain, Diff, NewTransactionInfo}
//import com.wavesplatform.transaction.Asset.Waves
//import com.wavesplatform.transaction.assets.IssueTransaction
//import com.wavesplatform.transaction.transfer.TransferTransaction
//import com.wavesplatform.transaction.{BlockchainUpdater, DataTransaction, GenesisTransaction, Transaction}
//import com.wavesplatform.{BlockGen, TestHelpers, crypto, state}
//import org.scalacheck.Gen
//import org.scalatest.{FreeSpec, Matchers}
//import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
//
//class BlockchainUpdateTriggersSpec extends FreeSpec with Matchers with BlockGen with ScalaCheckPropertyChecks with EventsHelpers {
//  private val WAVES_AMOUNT = Constants.UnitsInWave * Constants.TotalWaves
//
//  override protected def settings: WavesSettings = TestHelpers.enableNG(super.settings)
//
//  // add a genesis block to the blockchain
//  private val master: KeyPair     = accountGen.sample.get
//  private val rich: KeyPair       = accountGen.sample.get
//  private val initialAmount: Long = WAVES_AMOUNT / 2
//  private val genesis = TestBlock.create(
//    0,
//    Seq(
//      GenesisTransaction.create(master.toAddress, initialAmount, 0).explicitGet(),
//      GenesisTransaction.create(rich.toAddress, initialAmount, 0).explicitGet()
//    ),
//    master
//  )
//  override protected def initBlockchain(blockchainUpdater: Blockchain with BlockchainUpdater): Unit = {
//    blockchainUpdater.processBlock(genesis).explicitGet()
//    super.initBlockchain(blockchainUpdater)
//  }
//
//  private val sigGen: Gen[ByteStr]   = bytes64gen.map(ByteStr.apply)
//  private val heightGen: Gen[Int]    = Gen.choose(1, 1000)
//  private val assetAmtGen: Gen[Long] = Gen.oneOf(Gen.const[Long](1), Gen.choose[Long](2, ENOUGH_AMT))
//
//  private def microBlockGen(txs: Seq[Transaction], signer: KeyPair): Gen[MicroBlock] =
//    for {
//      sig <- byteArrayGen(crypto.SignatureLength).map(ByteStr.apply)
//      mb = MicroBlock.buildAndSign(3.toByte, signer, txs, genesis.signature, sig).explicitGet()
//    } yield mb
//
//  /**
//    * Tests the assertion both for transactions added in a block and in a microblock
//    */
//  private def testTxsStateUpdates[A](txs: Seq[Transaction])(assertion: Seq[StateUpdate] => A): Unit = {
//    val b = blockGen(txs, master).sample.get
//    assertion(appendBlock(b).transactionStateUpdates)
//
//    val mb = microBlockGen(txs, master).sample.get
//    assertion(appendMicroBlock(mb).transactionStateUpdates)
//  }
//
//  private def isNFT(tx: IssueTransaction): Boolean = tx.quantity == 1 && tx.decimals == 0 && !tx.reissuable
//
//  "updated Waves amount is calculated correctly for miner reward" in forAll {
//    for {
//      b      <- blockGen(Seq.empty, master)
//      reward <- Gen.option(Gen.choose(1L, 1000000L))
//      ba = appendBlock(b, reward)
//    } yield (reward, ba)
//  } {
//    case (reward, BlockAppended(_, _, _, updatedWavesAmount, _, _)) =>
//      updatedWavesAmount shouldBe WAVES_AMOUNT + reward.getOrElse(0L)
//  }
//
//  "rollbacks correctly" - {
//    "block" in forAll(sigGen, heightGen) { (sig, height) =>
//      produceEvent(_.onRollback(sig, height)) match {
//        case RollbackCompleted(toId, toHeight) =>
//          toId shouldBe sig
//          toHeight shouldBe height
//        case _ => fail()
//      }
//    }
//
//    "microblock" in forAll(sigGen, heightGen) { (sig, height) =>
//      produceEvent(_.onMicroBlockRollback(sig, height)) match {
//        case MicroBlockRollbackCompleted(toId, toHeight) =>
//          toId shouldBe sig
//          toHeight shouldBe height
//        case _ => fail()
//      }
//    }
//  }
//
//  "appends correctly" - {
//    "empty block" in forAll {
//      for {
//        b <- blockGen(Seq.empty, master)
//        ba = appendBlock(b)
//      } yield (b, ba)
//    } {
//      case (b, BlockAppended(toId, toHeight, block, _, _, _)) =>
//        toId shouldBe b.signature
//        toHeight shouldBe blockchain.height + 1
//
//        block.signature shouldBe b.signature
//        block.transactionData shouldBe b.transactionData
//    }
//
//    "microblock with one transaction" in forAll {
//      for {
//        tx <- dataTransactionGen(0, sender = Some(rich))
//        mb <- microBlockGen(Seq(tx), master)
//        mba = appendMicroBlock(mb)
//      } yield (mb, mba)
//    } {
//      case (mb, MicroBlockAppended(toId, toHeight, microBlock, _, _)) =>
//        toId shouldBe mb.totalResBlockSig
//        toHeight shouldBe blockchain.height
//
//        microBlock.signature shouldBe mb.signature
//        microBlock.transactionData shouldBe mb.transactionData
//    }
//
//    "including correct miner rewards for" - {
//      "block" in forAll {
//        for {
//          miner <- accountGen
//          tx    <- dataTransactionGen(0, sender = Some(rich))
//          mb    <- blockGen(Seq(tx), miner)
//          ba = appendBlock(mb)
//        } yield (tx, miner, ba.blockStateUpdate)
//      } { case (tx, miner, su) => su.balances.find(_._1 == miner.publicKey.toAddress).get._3 shouldBe 0.4 * tx.fee }
//
//      "microblock, giving reward to a key block miner" in forAll {
//        for {
//          tx <- dataTransactionGen(0, sender = Some(rich))
//          mb <- microBlockGen(Seq(tx), master)
//          mba = appendMicroBlock(mb)
//        } yield (tx, mba.microBlockStateUpdate)
//      } {
//        case (tx, su) =>
//          su.balances.find(_._1 == master.publicKey.toAddress).get._3 shouldBe (0.4 * tx.fee + initialAmount)
//      }
//    }
//
//    "block/microblock with balance updates from transfer txs" in forAll {
//      for {
//        sender        <- accountGen
//        recipient     <- accountGen
//        amount        <- Gen.choose(1L, initialAmount - Constants.UnitsInWave)
//        master2sender <- transferGeneratorPV2(1, master, sender.toAddress, amount)
//        fee           <- Gen.choose(1, master2sender.amount - 1)
//        sender2recipient = TransferTransaction.selfSigned(2.toByte, sender, recipient.toAddress, Waves, master2sender.amount - fee, Waves, fee, ByteStr.empty, 2)
//          .explicitGet()
//      } yield (sender, recipient, master2sender, sender2recipient)
//    } {
//      case (sender, recipient, master2sender, sender2recipient) =>
//        testTxsStateUpdates(Seq(master2sender, sender2recipient)) { transactionStateUpdates =>
//          transactionStateUpdates.last.balances.find(_._1 == sender.publicKey.toAddress).get._3 shouldBe 0
//
//          transactionStateUpdates.last.balances.find(_._1 == recipient.publicKey.toAddress).get._3 shouldBe
//            sender2recipient.amount
//        }
//    }
//
//    "block/microblock with a data transaction" in forAll(dataTransactionGen(DataTransaction.MaxEntryCount, sender = Some(rich))) { tx =>
//      testTxsStateUpdates(Seq(tx)) {
//        _.head.dataEntries.map(_._2).sortBy(_.key) shouldBe tx.data.sortBy(_.key)
//      }
//    }
//
//    "blocks/microblocks with correct asset state updates by" - {
//      "issue transaction" in forAll(issueV2TransactionGen(Gen.const(master))) { tx =>
//        testTxsStateUpdates(Seq(tx)) { upds =>
//          val AssetStateUpdate(asset, decimals, name, description, reissuable, volume, script, sponsorship, nft, assetExistedBefore) =
//            upds.head.assets.head
//          asset.id shouldBe tx.id()
//          name shouldBe tx.name.byteStr
//          description shouldBe tx.description.byteStr
//          decimals shouldBe tx.decimals
//          reissuable shouldBe tx.reissuable
//          volume.toLong shouldBe tx.quantity
//          script.map(_.script) shouldBe tx.script
//          nft shouldBe isNFT(tx)
//          sponsorship shouldBe None
//          assetExistedBefore shouldBe false
//        }
//      }
//
//      "reissue transaction" in forAll {
//        for {
//          issueAmt            <- assetAmtGen
//          reissueAmt          <- assetAmtGen
//          (issue, reissue, _) <- issueReissueBurnGeneratorP(issueAmt, reissueAmt, 1, master).suchThat(_._1.reissuable)
//        } yield (issue, reissue)
//      } {
//        case (issue, reissue) =>
//          testTxsStateUpdates(Seq(issue, reissue)) { upds =>
//            val issueUpd   = upds.head.assets.head
//            val reissueUpd = upds.last.assets.head
//
//            reissueUpd shouldBe issueUpd.copy(
//              volume = BigInt(issue.quantity) + BigInt(reissue.quantity),
//              reissuable = reissue.reissuable,
//              assetExistedBefore = !issueUpd.assetExistedBefore
//            )
//          }
//      }
//
//      "burn transaction" in forAll {
//        for {
//          issueAmt         <- assetAmtGen
//          burnAmt          <- Gen.choose(1, issueAmt)
//          (issue, _, burn) <- issueReissueBurnGeneratorP(issueAmt, 1, burnAmt, master)
//        } yield (issue, burn)
//      } {
//        case (issue, burn) =>
//          testTxsStateUpdates(Seq(issue, burn)) { upds =>
//            val issueUpd = upds.head.assets.head
//            val burnUpd  = upds.last.assets.head
//
//            burnUpd shouldBe issueUpd.copy(
//              volume = BigInt(issue.quantity) - BigInt(burn.quantity),
//              assetExistedBefore = !issueUpd.assetExistedBefore
//            )
//          }
//      }
//
//      "set asset script transaction" in forAll(issueAndSetAssetScriptGen(master)) {
//        case (issue, setAssetScript) =>
//          testTxsStateUpdates(Seq(issue, setAssetScript)) { upds =>
//            val issueUpd  = upds.head.assets.head
//            val scriptUpd = upds.last.assets.head
//
//            scriptUpd shouldBe issueUpd.copy(
//              script = setAssetScript.script.map(
//                s =>
//                  state.AssetScriptInfo(
//                    s,
//                    Script.estimate(s, EstimatorProvider.EstimatorBlockchainExt(blockchain).estimator, useContractVerifierLimit = false).explicitGet()
//                  )
//              ),
//              assetExistedBefore = !issueUpd.assetExistedBefore
//            )
//          }
//      }
//
//      "sponsor fee transaction " in forAll(sponsorFeeCancelSponsorFeeGen(master)) {
//        case (issue, startSponsorship, _, cancelSponsorship) =>
//          testTxsStateUpdates(Seq(issue, startSponsorship, cancelSponsorship)) { upds =>
//            val issueUpd             = upds.head.assets.head
//            val startSponsorshipUpd  = upds(1).assets.head
//            val cancelSponsorshipUpd = upds(2).assets.head
//
//            startSponsorshipUpd shouldBe issueUpd.copy(
//              sponsorship = startSponsorship.minSponsoredAssetFee,
//              assetExistedBefore = !issueUpd.assetExistedBefore
//            )
//
//            cancelSponsorshipUpd shouldBe startSponsorshipUpd.copy(
//              sponsorship = cancelSponsorship.minSponsoredAssetFee
//            )
//          }
//      }
//
//      "invokeScript transaction (diff emulated by issue, reussue and burn txs)" in forAll {
//        for {
//          issueAmt            <- assetAmtGen
//          reissueAmt          <- assetAmtGen
//          (issue, reissue, _) <- issueReissueBurnGeneratorP(issueAmt, reissueAmt, 1, master).suchThat(_._1.reissuable)
//          invoke              <- invokeScriptGen(Gen.const(Seq.empty))
//        } yield (issue, reissue, invoke)
//      } {
//        case (issue, reissue, invoke) =>
//          // create a block with issue and reissue txs, getting their diffs
//          val assetsDummyBlock     = TestBlock.create(master, Seq(issue, reissue))
//          val assetsDummyBlockDiff = detailedDiffFromBlock(assetsDummyBlock)
//
//          val invokeBlock = TestBlock.create(master, Seq(invoke))
//          // merge issue/reissue diffs as if they were produced by a single invoke
//          val invokeTxDiff = assetsDummyBlockDiff.transactionDiffs
//            .foldLeft(Diff.empty)(Diff.diffMonoid.combine)
//            .copy(transactions = Map(invoke.id() -> NewTransactionInfo(invoke, Set(master.toAddress), true)))
//          val invokeBlockDetailedDiff = assetsDummyBlockDiff.copy(transactionDiffs = Seq(invokeTxDiff))
//
//          produceEvent(_.onProcessBlock(invokeBlock, invokeBlockDetailedDiff, None, blockchain)) match {
//            case ba: BlockAppended =>
//              val AssetStateUpdate(asset, decimals, name, description, reissuable, volume, script, sponsorship, nft, assetExistedBefore) =
//                ba.transactionStateUpdates.head.assets.head
//
//              asset.id shouldBe issue.assetId
//              decimals shouldBe issue.decimals
//              name shouldBe issue.name.byteStr
//              description shouldBe issue.description.byteStr
//              reissuable shouldBe reissue.reissuable
//              volume shouldBe (BigInt(issue.quantity) + BigInt(reissue.quantity))
//              script shouldBe issue.script
//              sponsorship shouldBe None
//              nft shouldBe isNFT(issue)
//              assetExistedBefore shouldBe false
//            case _ => fail()
//          }
//      }
//    }
//  }
//}
