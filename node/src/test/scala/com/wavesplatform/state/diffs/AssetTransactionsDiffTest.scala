package com.wavesplatform.state.diffs

import cats._
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.{BlockchainFeatures, EstimatorProvider}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.{Expression, StdLibVersion, V1, V4}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.smart.smartEnabledFS
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}
import com.wavesplatform.{BlocksTransactionsHelpers, NoShrink, TransactionGen}
import fastparse.Parsed
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class AssetTransactionsDiffTest
    extends PropSpec
    with PropertyChecks
    with Matchers
    with TransactionGen
    with NoShrink
    with BlocksTransactionsHelpers
    with WithDomain {

  def issueReissueBurnTxs(isReissuable: Boolean): Gen[((GenesisTransaction, IssueTransaction), (ReissueTransaction, BurnTransaction))] =
    for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      ia                     <- positiveLongGen
      ra                     <- positiveLongGen
      ba                     <- positiveLongGen.suchThat(x => x < ia + ra)
      (issue, reissue, burn) <- issueReissueBurnGeneratorP(ia, ra, ba, master) suchThat (_._1.reissuable == isReissuable)
    } yield ((genesis, issue), (reissue, burn))

  property("Issue+Reissue+Burn do not break waves invariant and updates state") {
    forAll(issueReissueBurnTxs(isReissuable = true)) {
      case ((gen, issue), (reissue, burn)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(gen, issue))), TestBlock.create(Seq(reissue, burn))) {
          case (blockDiff, newState) =>
            val totalPortfolioDiff = Monoid.combineAll(blockDiff.portfolios.values)

            totalPortfolioDiff.balance shouldBe 0
            totalPortfolioDiff.effectiveBalance shouldBe 0
            totalPortfolioDiff.assets shouldBe Map(reissue.asset -> (reissue.quantity - burn.quantity))

            val totalAssetVolume = issue.quantity + reissue.quantity - burn.quantity
            newState.balance(issue.sender.toAddress, reissue.asset) shouldEqual totalAssetVolume
        }
    }
  }

  property("Cannot reissue/burn non-existing alias") {
    val setup: Gen[(GenesisTransaction, ReissueTransaction, BurnTransaction)] = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      reissue <- reissueGen
      burn    <- burnGen
    } yield (genesis, reissue, burn)

    forAll(setup) {
      case (gen, reissue, burn) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen))), TestBlock.create(Seq(reissue))) { blockDiffEi =>
          blockDiffEi should produce("Referenced assetId not found")
        }
        assertDiffEi(Seq(TestBlock.create(Seq(gen))), TestBlock.create(Seq(burn))) { blockDiffEi =>
          blockDiffEi should produce("Referenced assetId not found")
        }
    }
  }

  property("Cannot reissue/burn non-owned alias") {
    val setup = for {
      ((gen, issue), (_, _)) <- issueReissueBurnTxs(isReissuable = true)
      other                  <- accountGen.suchThat(_.toAddress != issue.sender.toAddress)
      quantity               <- positiveLongGen
      reissuable2            <- Arbitrary.arbitrary[Boolean]
      fee                    <- Gen.choose(1L, 2000000L)
      timestamp              <- timestampGen
      reissue = ReissueTransaction.selfSigned(1.toByte, other, issue.asset, quantity, reissuable2, fee, timestamp).explicitGet()
      burn    = BurnTransaction.selfSigned(1.toByte, other, issue.asset, quantity, fee, timestamp).explicitGet()
    } yield ((gen, issue), reissue, burn)

    forAll(setup) {
      case ((gen, issue), reissue, burn) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen, issue))), TestBlock.create(Seq(reissue))) { blockDiffEi =>
          blockDiffEi should produce("Asset was issued by other address")
        }
        assertDiffEi(Seq(TestBlock.create(Seq(gen, issue))), TestBlock.create(Seq(burn))) { blockDiffEi =>
          blockDiffEi should produce("Asset was issued by other address")
        }
    }
  }

  property("Can burn non-owned alias if feature 'BurnAnyTokens' activated") {
    val setup = for {
      issuer    <- accountGen
      burner    <- accountGen.suchThat(_ != issuer)
      timestamp <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(issuer.toAddress, ENOUGH_AMT, timestamp).explicitGet()
      (issue, _, _) <- issueReissueBurnGeneratorP(ENOUGH_AMT, issuer)
      assetTransfer <- transferGeneratorP(issuer, burner.toAddress, issue.asset, Waves)
      wavesTransfer <- wavesTransferGeneratorP(issuer, burner.toAddress)
      burn = BurnTransaction
        .selfSigned(1.toByte, burner, issue.asset, assetTransfer.amount, wavesTransfer.amount, timestamp)
        .explicitGet()
    } yield (genesis, issue, assetTransfer, wavesTransfer, burn)

    val fs =
      TestFunctionalitySettings.Enabled
        .copy(
          preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0, BlockchainFeatures.BurnAnyTokens.id -> 0)
        )

    forAll(setup) {
      case (genesis, issue, assetTransfer, wavesTransfer, burn) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, issue, assetTransfer, wavesTransfer))), TestBlock.create(Seq(burn)), fs) {
          case (_, newState) =>
            newState.balance(burn.sender.toAddress, burn.asset) shouldEqual 0
        }
    }
  }

  property("Can not reissue > long.max") {
    val setup = for {
      issuer    <- accountGen
      timestamp <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(issuer.toAddress, ENOUGH_AMT, timestamp).explicitGet()
      assetName   <- genBoundedBytes(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
      description <- genBoundedBytes(0, IssueTransaction.MaxAssetDescriptionLength)
      quantity    <- Gen.choose(Long.MaxValue / 200, Long.MaxValue / 100)
      fee         <- Gen.choose(MinIssueFee, 2 * MinIssueFee)
      decimals    <- Gen.choose(1: Byte, 8: Byte)
      issue       <- createLegacyIssue(issuer, assetName, description, quantity, decimals, reissuable = true, fee, timestamp)
      assetId = issue.asset
      reissue = ReissueTransaction.selfSigned(1.toByte, issuer, assetId, Long.MaxValue, reissuable = true, 1, timestamp).explicitGet()
    } yield (issuer, assetId, genesis, issue, reissue)

    val fs =
      TestFunctionalitySettings.Enabled
        .copy(
          preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0, BlockchainFeatures.DataTransaction.id -> 0)
        )

    forAll(setup) {
      case (_, _, genesis, issue, reissue) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, issue))), TestBlock.create(Seq(reissue)), fs) { ei =>
          ei should produce("Asset total value overflow")
        }
    }
  }

  property("Can request reissue > long.max before BurnAnyTokens activated") {
    val setup = for {
      issuer    <- accountGen
      timestamp <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(issuer.toAddress, ENOUGH_AMT, timestamp).explicitGet()
      assetName   <- genBoundedBytes(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
      description <- genBoundedBytes(0, IssueTransaction.MaxAssetDescriptionLength)
      quantity    <- Gen.choose(Long.MaxValue / 200, Long.MaxValue / 100)
      fee         <- Gen.choose(MinIssueFee, 2 * MinIssueFee)
      decimals    <- Gen.choose(1: Byte, 8: Byte)
      issue       <- createLegacyIssue(issuer, assetName, description, quantity, decimals, reissuable = true, fee, timestamp)
      assetId = issue.asset
      reissue = ReissueTransaction.selfSigned(1.toByte, issuer, assetId, Long.MaxValue, reissuable = true, 1, timestamp).explicitGet()
    } yield (issuer, assetId, genesis, issue, reissue)

    val fs =
      TestFunctionalitySettings.Enabled

    forAll(setup) {
      case (_, _, genesis, issue, reissue) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, issue))), TestBlock.create(Seq(reissue)), fs) { ei =>
          ei should produce("negative asset balance")
        }
    }
  }

  property("Can not total issue > long.max") {
    val setup = for {
      issuer    <- accountGen
      holder    <- accountGen.suchThat(_ != issuer)
      timestamp <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(issuer.toAddress, ENOUGH_AMT, timestamp).explicitGet()
      assetName   <- genBoundedBytes(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
      description <- genBoundedBytes(0, IssueTransaction.MaxAssetDescriptionLength)
      quantity    <- Gen.choose(Long.MaxValue / 200, Long.MaxValue / 100)
      fee         <- Gen.choose(MinIssueFee, 2 * MinIssueFee)
      decimals    <- Gen.choose(1: Byte, 8: Byte)
      issue       <- createLegacyIssue(issuer, assetName, description, quantity, decimals, reissuable = true, fee, timestamp)
      assetId = issue.asset
      attachment <- genBoundedBytes(0, TransferTransaction.MaxAttachmentSize)
      transfer = TransferTransaction.selfSigned(1.toByte, issuer, holder.toAddress, assetId, quantity - 1, Waves, fee, ByteStr(attachment), timestamp)
        .explicitGet()
      reissue = ReissueTransaction
        .selfSigned(1.toByte, issuer, assetId, (Long.MaxValue - quantity) + 1, reissuable = true, 1, timestamp)
        .explicitGet()
    } yield (issuer, assetId, genesis, issue, reissue, transfer)

    val fs =
      TestFunctionalitySettings.Enabled
        .copy(
          preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0, BlockchainFeatures.DataTransaction.id -> 0)
        )

    forAll(setup) {
      case (_, _, genesis, issue, reissue, transfer) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, issue, transfer))), TestBlock.create(Seq(reissue)), fs) { ei =>
          ei should produce("Asset total value overflow")
        }
    }
  }

  property("Cannot reissue non-reissuable alias") {
    forAll(issueReissueBurnTxs(isReissuable = false)) {
      case ((gen, issue), (reissue, _)) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen, issue))), TestBlock.create(Seq(reissue))) { blockDiffEi =>
          blockDiffEi should produce("Asset is not reissuable")
        }
    }
  }

  private def createScript(code: String, version: StdLibVersion) = {
    val Parsed.Success(expr, _) = Parser.parseExpr(code).get
    ExprScript(version, ExpressionCompiler(compilerContext(version, Expression, isAssetScript = false), expr).explicitGet()._1).explicitGet()
  }

  def genesisIssueTransferReissue(
      code: String,
      version: StdLibVersion = V1
  ): Gen[(Seq[GenesisTransaction], IssueTransaction, TransferTransaction, ReissueTransaction, ReissueTransaction)] =
    for {
      timestamp          <- timestampGen
      initialWavesAmount <- Gen.choose(Long.MaxValue / 1000, Long.MaxValue / 100)
      accountA           <- accountGen
      accountB           <- accountGen
      smallFee           <- Gen.choose(1L, 10L)
      genesisTx1 = GenesisTransaction.create(accountA.toAddress, initialWavesAmount, timestamp).explicitGet()
      genesisTx2 = GenesisTransaction.create(accountB.toAddress, initialWavesAmount, timestamp).explicitGet()
      reissuable = true
      (_, assetName, description, quantity, decimals, _, _, _) <- issueParamGen
      issue = IssueTransaction(
        TxVersion.V2,
        accountA.publicKey,
        assetName,
        description,
        quantity,
        decimals,
        reissuable,
        Some(createScript(code, version)),
        smallFee,
        timestamp + 1
      ).signWith(accountA.privateKey)
      assetId = IssuedAsset(issue.id())
      transfer = TransferTransaction
        .selfSigned(TxVersion.V1, accountA, accountB.toAddress, assetId, issue.quantity, Waves, smallFee, ByteStr.empty,  timestamp + 2)
        .explicitGet()
      reissue        = ReissueTransaction.selfSigned(TxVersion.V1, accountA, assetId, quantity, reissuable, smallFee, timestamp + 3).explicitGet()
      illegalReissue = ReissueTransaction.selfSigned(TxVersion.V1, accountB, assetId, quantity, reissuable, smallFee, timestamp + 3).explicitGet()
    } yield (Seq(genesisTx1, genesisTx2), issue, transfer, reissue, illegalReissue)

  property("Can issue smart asset with script") {
    forAll(for {
      acc        <- accountGen
      genesis    <- genesisGeneratorP(acc.toAddress)
      smartIssue <- issueV2TransactionGen(acc)
    } yield (genesis, smartIssue)) {
      case (gen, issue) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(gen))), TestBlock.create(Seq(issue)), smartEnabledFS) {
          case (blockDiff, newState) =>
            newState.assetDescription(IssuedAsset(issue.id())) shouldBe Some(
              AssetDescription(
                issue.assetId,
                issue.sender,
                issue.name,
                issue.description,
                issue.decimals,
                issue.reissuable,
                BigInt(issue.quantity),
                Height @@ 2,
                issue.script.map(
                  s =>
                    AssetScriptInfo(
                      s,
                      Script
                        .estimate(s, EstimatorProvider.EstimatorBlockchainExt(newState).estimator, useContractVerifierLimit = false)
                        .explicitGet()
                    )
                ),
                0L,
                issue.decimals == 0 && issue.quantity == 1 && !issue.reissuable
              )
            )
            blockDiff.transactions.contains(issue.id()) shouldBe true
            newState.transactionInfo(issue.id()).isDefined shouldBe true
            newState.transactionInfo(issue.id()).isDefined shouldEqual true
        }
    }
  }

  property("Can transfer when script evaluates to TRUE") {
    forAll(genesisIssueTransferReissue("true")) {
      case (gen, issue, transfer, _, _) =>
        assertDiffAndState(Seq(TestBlock.create(gen)), TestBlock.create(Seq(issue, transfer)), smartEnabledFS) {
          case (blockDiff, newState) =>
            val totalPortfolioDiff = Monoid.combineAll(blockDiff.portfolios.values)
            totalPortfolioDiff.assets(IssuedAsset(issue.id())) shouldEqual issue.quantity
            newState.balance(newState.resolveAlias(transfer.recipient).explicitGet(), IssuedAsset(issue.id())) shouldEqual transfer.amount
        }
    }
  }

  property("Cannot transfer when script evaluates to FALSE") {
    forAll(genesisIssueTransferReissue("false")) {
      case (gen, issue, transfer, _, _) =>
        assertDiffEi(Seq(TestBlock.create(gen)), TestBlock.create(Seq(issue, transfer)), smartEnabledFS)(
          ei => ei should produce("TransactionNotAllowedByScript")
        )
    }
  }

  property("Cannot reissue when script evaluates to FALSE") {
    forAll(genesisIssueTransferReissue("false")) {
      case (gen, issue, _, reissue, _) =>
        assertDiffEi(Seq(TestBlock.create(gen)), TestBlock.create(Seq(issue, reissue)), smartEnabledFS)(
          ei => ei should produce("TransactionNotAllowedByScript")
        )
    }
  }

  property("Only issuer can reissue") {
    forAll(genesisIssueTransferReissue("true")) {
      case (gen, issue, _, _, illegalReissue) =>
        assertDiffEi(Seq(TestBlock.create(gen)), TestBlock.create(Seq(issue, illegalReissue)), smartEnabledFS) { ei =>
          ei should produce("Asset was issued by other address")
        }
    }
  }

  val assetInfoUpdateEnabled: FunctionalitySettings = TestFunctionalitySettings.Enabled
    .copy(
      preActivatedFeatures = TestFunctionalitySettings.Enabled.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0) + (BlockchainFeatures.NG.id -> 0),
      minAssetInfoUpdateInterval = 100
    )

  property("Can't update before activation") {
    forAll(genesisIssueUpdate) {
      case (gen, issue, update) =>
        assertDiffEi(Seq(TestBlock.create(gen)), TestBlock.create(Seq(issue, update))) { ei =>
          ei should produce("Ride V4, VRF, Protobuf, Failed transactions feature has not been activated yet")
        }
    }
  }

  property(s"Can't update right before ${assetInfoUpdateEnabled.minAssetInfoUpdateInterval} blocks") {
    forAll(genesisIssueUpdate, Gen.chooseNum(0, assetInfoUpdateEnabled.minAssetInfoUpdateInterval - 2)) {
      case ((gen, issue, update), blocksCount) =>
        val blocks = Seq.fill(blocksCount)(TestBlock.create(Seq.empty, Block.ProtoBlockVersion))

        assertDiffEi(TestBlock.create(gen :+ issue) +: blocks, TestBlock.create(Seq(update), Block.ProtoBlockVersion), assetInfoUpdateEnabled) { ei =>
          ei should produce(
            s"Can't update info of asset with id=${issue.id()} " +
              s"before ${assetInfoUpdateEnabled.minAssetInfoUpdateInterval + 1} block, " +
              s"current height=${blocks.size + 2}, minUpdateInfoInterval=${assetInfoUpdateEnabled.minAssetInfoUpdateInterval}"
          )
        }
    }
  }

  property(s"Can update after ${assetInfoUpdateEnabled.minAssetInfoUpdateInterval} blocks") {
    forAll(genesisIssueUpdate) {
      case (gen, issue, update) =>
        val blocks =
          TestBlock.create(gen :+ issue) +: Seq.fill(assetInfoUpdateEnabled.minAssetInfoUpdateInterval)(
            TestBlock.create(Seq.empty, Block.ProtoBlockVersion)
          )

        assertDiffEi(blocks, TestBlock.create(Seq(update), Block.ProtoBlockVersion), assetInfoUpdateEnabled) { ei =>
          val info = ei
            .explicitGet()
            .updatedAssets(update.assetId)
            .left
            .get

          info.name.toStringUtf8 shouldEqual update.name
          info.description.toStringUtf8 shouldEqual update.description
        }
    }
  }

  property(s"Can update with CompositeBlockchain") {
    forAll(genesisIssueUpdateWithSecondAsset) {
      case (gen, Seq(issue, issue1), signer, update1) =>
        withDomain(domainSettingsWithFS(assetInfoUpdateEnabled.copy(minAssetInfoUpdateInterval = 0))) { d =>
          val blockchain   = d.blockchainUpdater
          val genesisBlock = TestBlock.create(gen :+ issue :+ issue1)
          d.appendBlock(genesisBlock)

          val (keyBlock, Seq(microBlock)) =
            UnsafeBlocks.unsafeChainBaseAndMicro(
              genesisBlock.id(),
              Nil,
              Seq(Seq(update1)),
              signer,
              Block.ProtoBlockVersion,
              genesisBlock.header.timestamp + 100
            )
          d.appendBlock(keyBlock)
          val microBlockId = d.appendMicroBlock(microBlock)

          { // Check liquid block
            val desc = blockchain.assetDescription(issue.asset).get
            desc.name shouldBe issue.name
            desc.description shouldBe issue.description

            val desc1 = blockchain.assetDescription(issue1.asset).get
            desc1.name.toStringUtf8 shouldBe update1.name
            desc1.description.toStringUtf8 shouldBe update1.description

            desc.lastUpdatedAt shouldBe 1
            desc1.lastUpdatedAt shouldBe blockchain.height
          }

          val (keyBlock1, _) =
            UnsafeBlocks.unsafeChainBaseAndMicro(microBlockId, Nil, Nil, signer, Block.ProtoBlockVersion, keyBlock.header.timestamp + 100)
          d.appendBlock(keyBlock1)

          { // Check after new key block
            val desc = blockchain.assetDescription(issue.asset).get
            desc.name shouldBe issue.name
            desc.description shouldBe issue.description

            val desc1 = blockchain.assetDescription(issue1.asset).get
            desc1.name.toStringUtf8 shouldBe update1.name
            desc1.description.toStringUtf8 shouldBe update1.description

            desc.lastUpdatedAt shouldBe 1
            desc1.lastUpdatedAt shouldBe (blockchain.height - 1)
          }
        }
    }
  }

  property("Asset V4 complexity limit is 4000") {
    val exprV4WithComplexityBetween3000And4000 =
      """
        | {-#STDLIB_VERSION 4 #-}
        | {-#SCRIPT_TYPE ASSET #-}
        | {-#CONTENT_TYPE EXPRESSION #-}
        |
        | groth16Verify_15inputs(base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK')
      """.stripMargin

    val exprV4WithComplexityAbove4000 =
      """
        | {-#STDLIB_VERSION 4 #-}
        | {-#SCRIPT_TYPE ASSET #-}
        | {-#CONTENT_TYPE EXPRESSION #-}
        |
        | groth16Verify_15inputs(base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') &&
        | groth16Verify_15inputs(base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK')
      """.stripMargin

    val rideV4Activated = TestFunctionalitySettings.Enabled.copy(
      preActivatedFeatures = Map(
        BlockchainFeatures.Ride4DApps.id -> 0,
        BlockchainFeatures.BlockV5.id    -> 0
      )
    )

    forAll(genesisIssueTransferReissue(exprV4WithComplexityBetween3000And4000, V4)) {
      case (gen, issue, _, _, _) =>
        assertDiffAndState(Seq(TestBlock.create(gen)), TestBlock.create(Seq(issue)), rideV4Activated) {
          case (blockDiff, _) =>
            val totalPortfolioDiff = Monoid.combineAll(blockDiff.portfolios.values)
            totalPortfolioDiff.assets(IssuedAsset(issue.id())) shouldEqual issue.quantity
        }
    }

    forAll(genesisIssueTransferReissue(exprV4WithComplexityAbove4000, V4)) {
      case (gen, issue, _, _, _) =>
        assertDiffEi(Seq(TestBlock.create(gen)), TestBlock.create(Seq(issue)), rideV4Activated) {
          _ should produce("Script is too complex: 5207 > 4000")
        }
    }
  }

  private val genesisIssueUpdate =
    for {
      timestamp          <- timestampGen
      initialWavesAmount <- Gen.choose(Long.MaxValue / 1000, Long.MaxValue / 100)
      accountA           <- accountGen
      accountB           <- accountGen
      smallFee           <- Gen.choose(1L, 10L)
      genesisTx1 = GenesisTransaction.create(accountA.toAddress, initialWavesAmount, timestamp).explicitGet()
      genesisTx2 = GenesisTransaction.create(accountB.toAddress, initialWavesAmount, timestamp).explicitGet()
      (_, assetName, description, quantity, decimals, _, _, _) <- issueParamGen
      updName                                                  <- genBoundedString(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
      updDescription                                           <- genBoundedString(0, IssueTransaction.MaxAssetDescriptionLength)
      issue = IssueTransaction(TxVersion.V2, accountA.publicKey, assetName, description, quantity, decimals, false, None, smallFee, timestamp + 1)
        .signWith(accountA.privateKey)
      assetId = IssuedAsset(issue.id())
      update = UpdateAssetInfoTransaction
        .selfSigned(
          TxVersion.V1,
          accountA,
          assetId.id,
          updName,
          updDescription,
          timestamp,
          smallFee,
          Waves
        )
        .explicitGet()

    } yield (Seq(genesisTx1, genesisTx2), issue, update)

  private val genesisIssueUpdateWithSecondAsset = for {
    (gen, issue, _) <- genesisIssueUpdate
    accountC        <- accountGen
    genesisTx3 = GenesisTransaction.create(accountC.toAddress, Long.MaxValue / 100, gen.head.timestamp).explicitGet()
    issue1 = IssueTransaction
      .selfSigned(
        TxVersion.V2,
        accountC,
        issue.name.toStringUtf8,
        issue.description.toStringUtf8,
        issue.quantity,
        issue.decimals,
        false,
        None,
        10,
        issue.timestamp
      )
      .explicitGet()
    update1 = UpdateAssetInfoTransaction
      .selfSigned(
        TxVersion.V1,
        accountC,
        issue1.assetId,
        "Invalid",
        "Invalid",
        issue1.timestamp + 1,
        10,
        Waves
      )
      .explicitGet()
  } yield (gen :+ genesisTx3, Seq(issue, issue1), accountC, update1)
}
