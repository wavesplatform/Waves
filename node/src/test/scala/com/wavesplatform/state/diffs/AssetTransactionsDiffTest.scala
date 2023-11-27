package com.wavesplatform.state.diffs

import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.BlockV5
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.utils.*
import com.wavesplatform.lang.v1.ContractLimits.MaxExprSizeInBytes
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, EXPR}
import com.wavesplatform.lang.v1.compiler.{ExpressionCompiler, TestCompiler}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.parser.Parser.LibrariesOffset.NoLibraries
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.state.diffs.smart.smartEnabledFS
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxHelpers.defaultAddress
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.*
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.transaction.{GenesisTransaction, Transaction, TxHelpers, TxVersion}
import com.wavesplatform.{BlocksTransactionsHelpers, TestValues}
import fastparse.Parsed
import monix.eval.Coeval

class AssetTransactionsDiffTest extends PropSpec with BlocksTransactionsHelpers with WithDomain {

  def issueReissueBurnTxs(isReissuable: Boolean): ((GenesisTransaction, IssueTransaction), (ReissueTransaction, BurnTransaction)) = {
    val master = TxHelpers.signer(1)

    val genesis = TxHelpers.genesis(master.toAddress)

    val issue   = TxHelpers.issue(master, 100, reissuable = isReissuable, version = TxVersion.V1)
    val asset   = IssuedAsset(issue.id())
    val reissue = TxHelpers.reissue(asset, master, 50, version = TxVersion.V1, fee = 1.waves)
    val burn    = TxHelpers.burn(asset, 10, master, version = TxVersion.V1)

    ((genesis, issue), (reissue, burn))
  }

  property("Issue+Reissue+Burn do not break waves invariant and updates state") {
    val ((gen, issue), (reissue, burn)) = issueReissueBurnTxs(isReissuable = true)
    withDomain(RideV3) { d =>
      d.appendBlock(gen)
      d.appendBlock(issue)
      d.appendBlock(reissue, burn)
      val assetQuantityDiff = reissue.quantity.value - burn.quantity.value
      d.liquidSnapshot.balances.toSeq
        .map {
          case ((`defaultAddress`, Waves), amount) =>
            val carryFee = (-issue.fee.value + reissue.fee.value + burn.fee.value) / 5 * 3
            Waves -> (amount - d.rocksDBWriter.balance(defaultAddress, Waves) + carryFee)
          case ((address, asset), amount) =>
            asset -> (amount - d.rocksDBWriter.balance(address, asset))
        }
        .groupMap(_._1)(_._2)
        .foreach {
          case (asset, Seq(balanceDiff)) if asset == reissue.asset => balanceDiff shouldBe assetQuantityDiff
          case (_, balanceDiff)                                    => balanceDiff.sum shouldBe 0
        }
      val resultQuantity = issue.quantity.value + assetQuantityDiff
      d.liquidSnapshot.assetVolumes.view.mapValues(_.volume).toMap shouldBe Map(reissue.asset -> resultQuantity)
      d.balance(issue.sender.toAddress, reissue.asset) shouldEqual resultQuantity
    }
  }

  property("Cannot reissue/burn non-existing alias") {
    val ((gen, _), (reissue, burn)) = issueReissueBurnTxs(true)
    assertDiffEi(Seq(TestBlock.create(Seq(gen))), TestBlock.create(Seq(reissue))) { blockDiffEi =>
      blockDiffEi should produce("Referenced assetId not found")
    }
    assertDiffEi(Seq(TestBlock.create(Seq(gen))), TestBlock.create(Seq(burn))) { blockDiffEi =>
      blockDiffEi should produce("Referenced assetId not found")
    }
  }

  property("Cannot reissue/burn non-owned alias") {
    val setup = {
      val issuer    = TxHelpers.signer(1)
      val nonIssuer = TxHelpers.signer(2)

      val genesis = TxHelpers.genesis(issuer.toAddress)
      val issue   = TxHelpers.issue(issuer, 100, version = TxVersion.V1)
      val asset   = IssuedAsset(issue.id())
      val reissue = TxHelpers.reissue(asset, nonIssuer, 50, version = TxVersion.V1)
      val burn    = TxHelpers.burn(asset, 10, nonIssuer, version = TxVersion.V1)

      ((genesis, issue), reissue, burn)
    }

    val ((gen, issue), reissue, burn) = setup
    assertDiffEi(Seq(TestBlock.create(Seq(gen, issue))), TestBlock.create(Seq(reissue))) { blockDiffEi =>
      blockDiffEi should produce("Asset was issued by other address")
    }
    assertDiffEi(Seq(TestBlock.create(Seq(gen, issue))), TestBlock.create(Seq(burn))) { blockDiffEi =>
      blockDiffEi should produce("Asset was issued by other address")
    }
  }

  property("Can burn non-owned alias if feature 'BurnAnyTokens' activated") {
    val setup = {
      val issuer = TxHelpers.signer(1)
      val burner = TxHelpers.signer(2)

      val genesis       = TxHelpers.genesis(issuer.toAddress)
      val issue         = TxHelpers.issue(issuer, ENOUGH_AMT, version = TxVersion.V1)
      val asset         = IssuedAsset(issue.id())
      val assetTransfer = TxHelpers.transfer(issuer, burner.toAddress, 1, asset, version = TxVersion.V1)
      val wavesTransfer = TxHelpers.transfer(issuer, burner.toAddress, version = TxVersion.V1)
      val burn          = TxHelpers.burn(asset, assetTransfer.amount.value, burner, fee = wavesTransfer.amount.value, version = TxVersion.V1)

      (genesis, issue, assetTransfer, wavesTransfer, burn)
    }

    val fs =
      TestFunctionalitySettings.Enabled
        .copy(preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0, BlockchainFeatures.BurnAnyTokens.id -> 0))

    val (genesis, issue, assetTransfer, wavesTransfer, burn) = setup
    assertDiffAndState(Seq(TestBlock.create(Seq(genesis, issue, assetTransfer, wavesTransfer))), TestBlock.create(Seq(burn)), fs) {
      case (_, newState) =>
        newState.balance(burn.sender.toAddress, burn.asset) shouldEqual 0
    }
  }

  property("Can not reissue > long.max") {
    val setup = {
      val issuer = TxHelpers.signer(1)

      val genesis = TxHelpers.genesis(issuer.toAddress)
      val issue   = TxHelpers.issue(issuer, version = TxVersion.V1)
      val asset   = IssuedAsset(issue.id())
      val reissue = TxHelpers.reissue(asset, issuer, Long.MaxValue, version = TxVersion.V1)

      (issuer, asset, genesis, issue, reissue)
    }

    val fs =
      TestFunctionalitySettings.Enabled
        .copy(preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0, BlockchainFeatures.DataTransaction.id -> 0))

    val (_, _, genesis, issue, reissue) = setup
    assertDiffEi(Seq(TestBlock.create(Seq(genesis, issue))), TestBlock.create(Seq(reissue)), fs) { ei =>
      ei should produce("Asset total value overflow")
    }
  }

  property("Can request reissue > long.max before BurnAnyTokens activated") {
    val setup = {
      val issuer = TxHelpers.signer(1)

      val genesis = TxHelpers.genesis(issuer.toAddress)
      val issue   = TxHelpers.issue(issuer, version = TxVersion.V1)
      val asset   = IssuedAsset(issue.id())
      val reissue = TxHelpers.reissue(asset, issuer, Long.MaxValue, version = TxVersion.V1)

      (issuer, asset, genesis, issue, reissue)
    }

    val fs =
      TestFunctionalitySettings.Enabled

    val (_, _, genesis, issue, reissue) = setup
    assertDiffEi(Seq(TestBlock.create(Seq(genesis, issue))), TestBlock.create(Seq(reissue)), fs) { ei =>
      ei should produce("Asset balance sum overflow")
    }
  }

  property("Can not total issue > long.max") {
    val setup = {
      val issuer = TxHelpers.signer(1)
      val holder = TxHelpers.signer(2)

      val genesis = TxHelpers.genesis(issuer.toAddress)
      val amount  = 100
      val issue   = TxHelpers.issue(issuer, amount, version = TxVersion.V1)
      val asset   = issue.asset
      val transfer = TxHelpers.transfer(
        issuer,
        holder.toAddress,
        amount - 1,
        asset,
        attachment = ByteStr.fill(TransferTransaction.MaxAttachmentSize)(1),
        version = TxVersion.V1
      )
      val reissue = TxHelpers.reissue(asset, issuer, (Long.MaxValue - amount) + 1, version = TxVersion.V1)

      (issuer, asset, genesis, issue, reissue, transfer)
    }

    val fs =
      TestFunctionalitySettings.Enabled
        .copy(preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0, BlockchainFeatures.DataTransaction.id -> 0))

    val (_, _, genesis, issue, reissue, transfer) = setup
    assertDiffEi(Seq(TestBlock.create(Seq(genesis, issue, transfer))), TestBlock.create(Seq(reissue)), fs) { ei =>
      ei should produce("Asset total value overflow")
    }
  }

  property("Cannot reissue non-reissuable alias") {
    val ((gen, issue), (reissue, _)) = issueReissueBurnTxs(isReissuable = false)
    assertDiffEi(Seq(TestBlock.create(Seq(gen, issue))), TestBlock.create(Seq(reissue))) { blockDiffEi =>
      blockDiffEi should produce("Asset is not reissuable")
    }
  }

  private def createScript(code: String, version: StdLibVersion) = {
    val Parsed.Success(expr, _) = Parser.parseExpr(code).get
    ExprScript(version, ExpressionCompiler(compilerContext(version, Expression, isAssetScript = false), expr).explicitGet()._1).explicitGet()
  }

  def genesisIssueTransferReissue(
      code: String,
      version: StdLibVersion = V1
  ): (Seq[GenesisTransaction], IssueTransaction, TransferTransaction, ReissueTransaction, ReissueTransaction) = {
    val accountA = TxHelpers.signer(1)
    val accountB = TxHelpers.signer(2)

    val genesis        = Seq(accountA, accountB).map(acc => TxHelpers.genesis(acc.toAddress, Long.MaxValue / 100))
    val issue          = TxHelpers.issue(accountA, 100, script = Some(createScript(code, version)))
    val asset          = issue.asset
    val transfer       = TxHelpers.transfer(accountA, accountB.toAddress, issue.quantity.value, asset, version = TxVersion.V1)
    val reissue        = TxHelpers.reissue(asset, accountA, issue.quantity.value, version = TxVersion.V1)
    val illegalReissue = TxHelpers.reissue(asset, accountB, issue.quantity.value, version = TxVersion.V1)

    (genesis, issue, transfer, reissue, illegalReissue)
  }

  property("Can issue smart asset with script") {
    val acc     = TxHelpers.signer(1)
    val genesis = TxHelpers.genesis(acc.toAddress)
    val issue   = TxHelpers.issue(acc, 100, script = Some(ExprScript(CONST_BOOLEAN(true)).explicitGet()))

    assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(issue)), RideV6.blockchainSettings.functionalitySettings) {
      case (blockDiff, newState) =>
        newState.assetDescription(IssuedAsset(issue.id())) shouldBe Some(
          AssetDescription(
            issue.assetId,
            issue.sender,
            issue.name,
            issue.description,
            issue.decimals.value,
            issue.reissuable,
            BigInt(issue.quantity.value),
            Height @@ 2,
            issue.script.map(AssetScriptInfo(_, 0)),
            0L,
            issue.decimals.value == 0 && issue.quantity.value == 1 && !issue.reissuable,
            1,
            Height @@ 2
          )
        )
        blockDiff.transactions.get(issue.id()) shouldBe defined
        newState.transactionInfo(issue.id()).isDefined shouldBe true
    }
  }

  property("Can transfer when script evaluates to TRUE") {
    val (gen, issue, transfer, _, _) = genesisIssueTransferReissue("true")
    assertDiffAndState(Seq(TestBlock.create(gen)), TestBlock.create(Seq(issue, transfer)), smartEnabledFS) { case (blockDiff, newState) =>
      val asset = IssuedAsset(issue.id())
      blockDiff.balances.collect { case ((_, `asset`), quantity) => quantity }.sum shouldEqual issue.quantity.value
      newState.balance(newState.resolveAlias(transfer.recipient).explicitGet(), IssuedAsset(issue.id())) shouldEqual transfer.amount.value
    }
  }

  property("Cannot transfer when script evaluates to FALSE") {
    val (gen, issue, transfer, _, _) = genesisIssueTransferReissue("false")
    assertDiffEi(Seq(TestBlock.create(gen)), TestBlock.create(Seq(issue, transfer)), smartEnabledFS)(ei =>
      ei should produce("TransactionNotAllowedByScript")
    )
  }

  property("Cannot reissue when script evaluates to FALSE") {
    val (gen, issue, _, reissue, _) = genesisIssueTransferReissue("false")
    assertDiffEi(Seq(TestBlock.create(gen)), TestBlock.create(Seq(issue, reissue)), smartEnabledFS)(ei =>
      ei should produce("TransactionNotAllowedByScript")
    )
  }

  property("Only issuer can reissue") {
    val (gen, issue, _, _, illegalReissue) = genesisIssueTransferReissue("true")
    assertDiffEi(Seq(TestBlock.create(gen)), TestBlock.create(Seq(issue, illegalReissue)), smartEnabledFS) { ei =>
      ei should produce("Asset was issued by other address")
    }
  }

  val assetInfoUpdateEnabled: FunctionalitySettings = TestFunctionalitySettings.Enabled
    .copy(
      preActivatedFeatures =
        TestFunctionalitySettings.Enabled.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0) + (BlockchainFeatures.NG.id -> 0),
      minAssetInfoUpdateInterval = 100
    )

  property("Can't update before activation") {
    val (gen, issue, update) = genesisIssueUpdate
    assertDiffEi(Seq(TestBlock.create(gen)), TestBlock.create(Seq(issue, update))) { ei =>
      ei should produce("Ride V4, VRF, Protobuf, Failed transactions feature has not been activated yet")
    }
  }

  property(s"Can't update right before ${assetInfoUpdateEnabled.minAssetInfoUpdateInterval} blocks") {
    val blocksCount          = assetInfoUpdateEnabled.minAssetInfoUpdateInterval - 2
    val (gen, issue, update) = genesisIssueUpdate
    val blocks               = Seq.fill(blocksCount)(TestBlock.create(Seq.empty, Block.ProtoBlockVersion))

    assertDiffEi(TestBlock.create(gen :+ issue) +: blocks, TestBlock.create(Seq(update), Block.ProtoBlockVersion), assetInfoUpdateEnabled) { ei =>
      ei should produce(
        s"Can't update info of asset with id=${issue.id()} " +
          s"before ${assetInfoUpdateEnabled.minAssetInfoUpdateInterval + 1} block, " +
          s"current height=${blocks.size + 2}, minUpdateInfoInterval=${assetInfoUpdateEnabled.minAssetInfoUpdateInterval}"
      )
    }
  }

  property(s"Can update after ${assetInfoUpdateEnabled.minAssetInfoUpdateInterval} blocks") {
    val (gen, issue, update) = genesisIssueUpdate
    val blocks =
      TestBlock.create(gen :+ issue) +: Seq.fill(assetInfoUpdateEnabled.minAssetInfoUpdateInterval)(
        TestBlock.create(Seq.empty, Block.ProtoBlockVersion)
      )

    assertDiffEi(blocks, TestBlock.create(Seq(update), Block.ProtoBlockVersion), assetInfoUpdateEnabled) { ei =>
      val info = ei.explicitGet().assetNamesAndDescriptions(update.assetId)
      info.name.toStringUtf8 shouldEqual update.name
      info.description.toStringUtf8 shouldEqual update.description
    }
  }

  property(s"Can update with CompositeBlockchain") {
    val (gen, issues, _, update1) = genesisIssueUpdateWithSecondAsset
    withDomain(domainSettingsWithFS(assetInfoUpdateEnabled.copy(minAssetInfoUpdateInterval = 0))) { d =>
      val blockchain   = d.blockchainUpdater
      val genesisBlock = TestBlock.create(gen ++ issues).block
      d.appendBlock(genesisBlock)

      d.appendBlock()
      d.appendMicroBlock(update1)

      val issue  = issues(0)
      val issue1 = issues(1)

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

      d.appendKeyBlock()

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

    val rideV4Activated = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures =
      Map(
        BlockchainFeatures.Ride4DApps.id -> 0,
        BlockchainFeatures.BlockV5.id    -> 0
      )
    )

    val (genesis1, issue1, _, _, _) = genesisIssueTransferReissue(exprV4WithComplexityBetween3000And4000, V4)
    assertDiffAndState(Seq(TestBlock.create(genesis1)), TestBlock.create(Seq(issue1)), rideV4Activated) { case (blockDiff, _) =>
      val asset = IssuedAsset(issue1.id())
      blockDiff.balances.collect { case ((_, `asset`), quantity) => quantity }.sum shouldEqual issue1.quantity.value
    }

    val (genesis2, issue2, _, _, _) = genesisIssueTransferReissue(exprV4WithComplexityAbove4000, V4)
    assertDiffEi(Seq(TestBlock.create(genesis2)), TestBlock.create(Seq(issue2)), rideV4Activated) {
      _ should produce("Script is too complex: 5207 > 4000")
    }
  }

  private def genesisIssueUpdate = {
    val accountA = TxHelpers.signer(1)
    val accountB = TxHelpers.signer(2)

    val genesis     = Seq(accountA, accountB).map(acc => TxHelpers.genesis(acc.toAddress, Long.MaxValue / 100))
    val issue       = TxHelpers.issue(accountA, 100, reissuable = false)
    val asset       = issue.asset
    val updateAsset = TxHelpers.updateAssetInfo(asset.id, sender = accountA)

    (genesis, issue, updateAsset)
  }

  private def genesisIssueUpdateWithSecondAsset = {
    val (genesis1, issue1, _) = genesisIssueUpdate

    val accountC = TxHelpers.signer(1)

    val genesis2 = TxHelpers.genesis(accountC.toAddress, Long.MaxValue / 100)
    val issue2   = TxHelpers.issue(accountC, issue1.quantity.value, reissuable = false)
    val update2  = TxHelpers.updateAssetInfo(issue2.asset.id, "Invalid", "Invalid", accountC)

    (genesis1 :+ genesis2, Seq(issue1, issue2), accountC, update2)
  }

  property("estimation overflow") {
    val testScript = TestCompiler(V3).compileExpression {
      val n = 65
      s"""
         | func f0() = true
         | ${(0 until n).map(i => s"func f${i + 1}() = if (f$i()) then f$i() else f$i()").mkString("\n")}
         | f$n()
       """.stripMargin
    }

    def t       = System.currentTimeMillis()
    val sender  = accountGen.sample.get
    val genesis = GenesisTransaction.create(sender.toAddress, ENOUGH_AMT, t).explicitGet()

    def issue(script: Script) =
      IssueTransaction.selfSigned(2.toByte, sender, "name", "", ENOUGH_AMT, 0, true, Some(script), 100000000, t).explicitGet()
    def setAssetScript(asset: IssuedAsset) =
      SetAssetScriptTransaction.selfSigned(2.toByte, sender, asset, Some(testScript), 100000000, t).explicitGet()

    def settings(checkNegative: Boolean = false, checkSumOverflow: Boolean = false): FunctionalitySettings = {
      TestFunctionalitySettings
        .withFeatures(BlockV5)
        .copy(estimationOverflowFixHeight = if (checkNegative) 0 else 999, estimatorSumOverflowFixHeight = if (checkSumOverflow) 0 else 999)
    }

    def assert(preparingTxs: Seq[Transaction], scriptedTx: () => Transaction) = {
      withDomain(domainSettingsWithFS(settings())) { db =>
        db.appendBlock(preparingTxs*)
        val tx = scriptedTx()
        db.appendBlock(tx)
        db.liquidSnapshot.errorMessage(tx.id()) shouldBe None
      }

      withDomain(domainSettingsWithFS(settings(checkNegative = true))) { db =>
        db.appendBlock(preparingTxs*)
        (the[Exception] thrownBy db.appendBlock(scriptedTx())).getMessage should include("Unexpected negative complexity")
      }

      withDomain(domainSettingsWithFS(settings(checkSumOverflow = true))) { db =>
        db.appendBlock(preparingTxs*)
        (the[Exception] thrownBy db.appendBlock(scriptedTx())).getMessage should include("Illegal script")
      }
    }

    val emptyIssue = issue(TestCompiler(V3).compileExpression("true"))
    assert(Seq(genesis, emptyIssue), () => setAssetScript(IssuedAsset(emptyIssue.id())))
    assert(Seq(genesis), () => issue(testScript))
  }

  property("synchronous calls are not allowed in asset script") {
    val issuer   = accountGen.sample.get
    val ts: Long = System.currentTimeMillis()

    def issue(script: Option[Script]): IssueTransaction =
      IssueTransaction
        .selfSigned(
          version = TxVersion.V2,
          sender = issuer,
          name = "test",
          description = "desc",
          quantity = 1,
          decimals = 0,
          reissuable = true,
          script = script,
          fee = 1.waves,
          timestamp = ts
        )
        .explicitGet()

    withDomain(DomainPresets.RideV5) { d =>
      val genesis = GenesisTransaction.create(issuer.toAddress, ENOUGH_AMT, ts).explicitGet()

      val successfulIssue = issue(Some(TestCompiler(V5).compileAsset("true")))
      val setAssetScriptWithInvoke = TxHelpers
        .setAssetScript(issuer, successfulIssue.asset, getScriptWithSyncCall("invoke"), 1.waves, version = TxVersion.V2)
      val setAssetScriptWithReentrantInvoke = TxHelpers
        .setAssetScript(issuer, successfulIssue.asset, getScriptWithSyncCall("reentrantInvoke"), 1.waves, version = TxVersion.V2)

      d.appendBlock(genesis)
      d.appendBlockE(issue(Some(getScriptWithSyncCall("invoke")))) should produce("function 'Native(1020)' not found")
      d.appendBlockE(issue(Some(getScriptWithSyncCall("reentrantInvoke")))) should produce("function 'Native(1021)' not found")
      d.appendBlock(successfulIssue)
      d.appendBlockE(setAssetScriptWithInvoke) should produce("function 'Native(1020)' not found")
      d.appendBlockE(setAssetScriptWithReentrantInvoke) should produce("function 'Native(1021)' not found")
    }
  }

  property("only Waves can be used to pay fees for UpdateAssetInfoTransaction after RideV6 activation") {
    val sponsoredIssuer = TxHelpers.signer(1)
    val updatedIssuer   = TxHelpers.signer(2)

    withDomain(
      DomainPresets.RideV5
        .setFeaturesHeight((BlockchainFeatures.RideV6, 5))
        .configure(_.copy(minAssetInfoUpdateInterval = 0)),
      AddrWithBalance.enoughBalances(sponsoredIssuer, updatedIssuer)
    ) { d =>
      val sponsorIssue = TxHelpers.issue(sponsoredIssuer)
      val sponsor      = TxHelpers.sponsor(sponsorIssue.asset, sender = sponsoredIssuer)
      val transfer     = TxHelpers.transfer(sponsoredIssuer, updatedIssuer.toAddress, sponsorIssue.quantity.value / 2, sponsorIssue.asset)

      val updatedIssue = TxHelpers.issue(updatedIssuer)
      val updateAssetInfo = () =>
        TxHelpers.updateAssetInfo(
          assetId = updatedIssue.assetId,
          sender = updatedIssuer,
          fee = Sponsorship.fromWaves(TestValues.fee, sponsor.minSponsoredAssetFee.get.value),
          feeAsset = sponsorIssue.asset
        )

      d.appendBlock(sponsorIssue, updatedIssue, sponsor, transfer)
      d.appendAndAssertSucceed(updateAssetInfo())
      d.appendBlock()
      d.appendBlockE(updateAssetInfo()) should produceRejectOrFailedDiff("only Waves can be used to pay fees for UpdateAssetInfoTransaction")
      d.appendAndAssertSucceed(TxHelpers.updateAssetInfo(updatedIssue.assetId, sender = updatedIssuer))
    }
  }

  property(
    s"Asset script size should be less than $MaxExprSizeInBytes after ${BlockchainFeatures.BlockRewardDistribution} activation"
  ) {
    def scriptWithSize(size: Int): Script = new ExprScript {
      val stdLibVersion: StdLibVersion     = V6
      val isFreeCall: Boolean              = false
      val expr: EXPR                       = TxHelpers.exprScript(V6)("true").expr
      val bytes: Coeval[ByteStr]           = Coeval(ByteStr(new Array[Byte](size)))
      val containsBlockV2: Coeval[Boolean] = Coeval(false)
      val containsArray: Boolean           = false
    }

    val issuer = TxHelpers.signer(1)

    withDomain(
      ConsensusImprovements.setFeaturesHeight(BlockchainFeatures.BlockRewardDistribution -> 5),
      AddrWithBalance.enoughBalances(issuer)
    ) { d =>
      val issue = TxHelpers.issue(issuer, name = "asset1", script = Some(TestCompiler(V6).compileAsset("true")))

      val issueWithBigScript: String => IssueTransaction =
        name => TxHelpers.issue(issuer, name = name, script = Some(scriptWithSize(MaxExprSizeInBytes + 1)))
      val updateWithBigScript = () => TxHelpers.setAssetScript(issuer, issue.asset, scriptWithSize(MaxExprSizeInBytes + 1))

      d.appendBlock(issue)
      d.appendAndAssertSucceed(issueWithBigScript("asset2"))
      d.appendAndAssertSucceed(updateWithBigScript())

      d.blockchain.isFeatureActivated(BlockchainFeatures.BlockRewardDistribution, d.blockchain.height + 1) shouldBe true

      val errorMsg = s"Script is too large: ${MaxExprSizeInBytes + 1} bytes > $MaxExprSizeInBytes bytes"

      // activation height
      val invalidIssue = issueWithBigScript("asset3")
      d.appendAndCatchError(invalidIssue) shouldBe TransactionValidationError(
        GenericError(errorMsg),
        invalidIssue
      )
      val invalidSetAssetScript = updateWithBigScript()
      d.appendAndCatchError(invalidSetAssetScript) shouldBe TransactionValidationError(
        GenericError(errorMsg),
        invalidSetAssetScript
      )
      d.appendAndAssertSucceed(
        TxHelpers.issue(issuer, name = "asset3", script = Some(scriptWithSize(MaxExprSizeInBytes))),
        TxHelpers.setAssetScript(issuer, issue.asset, scriptWithSize(MaxExprSizeInBytes))
      )
    }
  }

  private def getScriptWithSyncCall(syncCall: String): ExprScript = {
    val expr =
      s"""
         |strict a = $syncCall(Address(base58'123'), "test", [], [])
         |true
         |""".stripMargin

    ExpressionCompiler
      .compileBoolean(expr, NoLibraries, compilerContext(DirectiveSet(V5, Call, Expression).explicitGet()), V5)
      .flatMap(ExprScript(V5, _))
      .explicitGet()
  }
}
