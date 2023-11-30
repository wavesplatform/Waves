package com.wavesplatform.api.grpc.test

import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.api.grpc.{BlockRangeRequest, BlockRequest, BlockWithHeight, BlocksApiGrpcImpl}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.{Domain, defaultSigner}
import com.wavesplatform.protobuf.*
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.state.{BlockRewardCalculator, Blockchain}
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order, OrderType}
import com.wavesplatform.transaction.{TxHelpers, TxVersion}
import com.wavesplatform.utils.DiffMatchers
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{Assertion, BeforeAndAfterAll}
import com.wavesplatform.utils.byteStrOrdering
import scala.concurrent.Await
import scala.concurrent.duration.{DurationInt, FiniteDuration}

class BlocksApiGrpcSpec extends FreeSpec with BeforeAndAfterAll with DiffMatchers with WithDomain with GrpcApiHelpers {

  val sender: KeyPair         = TxHelpers.signer(1)
  val recipient: KeyPair      = TxHelpers.signer(2)
  val timeout: FiniteDuration = 1.minute

  "GetBlock should work" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
    val grpcApi = getGrpcApi(d)

    val block = d.appendBlock(TxHelpers.transfer(sender, recipient.toAddress, 1))

    d.liquidAndSolidAssert { () =>
      val vrf = getBlockVrfPB(d, block)
      vrf.isEmpty shouldBe false
      val expectedResult = BlockWithHeight.of(
        Some(PBBlocks.protobuf(block)),
        2,
        vrf,
        Seq(RewardShare(ByteString.copyFrom(block.sender.toAddress.bytes), d.blockchain.settings.rewardsSettings.initial))
      )

      val resultById = Await.result(
        grpcApi.getBlock(BlockRequest.of(BlockRequest.Request.BlockId(block.id().toByteString), includeTransactions = true)),
        timeout
      )

      resultById shouldBe expectedResult

      val resultByHeight = Await.result(
        grpcApi.getBlock(BlockRequest.of(BlockRequest.Request.Height(2), includeTransactions = true)),
        timeout
      )

      resultByHeight shouldBe expectedResult
    }
  }

  "GetBlockRange should work" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
    val grpcApi = getGrpcApi(d)

    val blocks = (1 to 10).map { _ =>
      d.appendBlock(TxHelpers.transfer(sender, recipient.toAddress, 1))
    }.toList

    d.liquidAndSolidAssert { () =>
      val (observer, result) = createObserver[BlockWithHeight]
      grpcApi.getBlockRange(
        BlockRangeRequest.of(2, 11, BlockRangeRequest.Filter.Empty, includeTransactions = true),
        observer
      )
      result.runSyncUnsafe() shouldBe blocks.zipWithIndex.map { case (block, idx) =>
        val vrf = getBlockVrfPB(d, block)
        vrf.isEmpty shouldBe false
        BlockWithHeight.of(
          Some(PBBlocks.protobuf(block)),
          idx + 2,
          vrf,
          Seq(RewardShare(ByteString.copyFrom(block.sender.toAddress.bytes), d.blockchain.settings.rewardsSettings.initial))
        )
      }
    }
  }

  "NODE-972. GetBlock and GetBlockRange should return correct data for orders with attachment" in {
    def checkOrderAttachment(block: BlockWithHeight, expectedAttachment: ByteStr): Assertion = {
      PBTransactions
        .vanilla(block.block.get.transactions.head, unsafe = true)
        .explicitGet()
        .asInstanceOf[ExchangeTransaction]
        .order1
        .attachment shouldBe Some(expectedAttachment)
    }

    val sender = TxHelpers.signer(1)
    val issuer = TxHelpers.signer(2)
    withDomain(DomainPresets.TransactionStateSnapshot, balances = AddrWithBalance.enoughBalances(sender, issuer)) { d =>
      val grpcApi = getGrpcApi(d)

      val attachment = ByteStr.fill(32)(1)
      val issue      = TxHelpers.issue(issuer)
      val exchange =
        TxHelpers.exchangeFromOrders(
          TxHelpers.order(OrderType.BUY, Waves, issue.asset, version = Order.V4, attachment = Some(attachment)),
          TxHelpers.order(OrderType.SELL, Waves, issue.asset, version = Order.V4, sender = issuer),
          version = TxVersion.V3
        )

      d.appendBlock(issue)
      val exchangeBlock = d.appendBlock(exchange)

      d.liquidAndSolidAssert { () =>
        val resultById = Await.result(
          grpcApi.getBlock(BlockRequest.of(BlockRequest.Request.BlockId(exchangeBlock.id().toByteString), includeTransactions = true)),
          1.minute
        )

        checkOrderAttachment(resultById, attachment)

        val resultByHeight = Await.result(
          grpcApi.getBlock(BlockRequest.of(BlockRequest.Request.Height(3), includeTransactions = true)),
          1.minute
        )

        checkOrderAttachment(resultByHeight, attachment)

        val (observer, resultRange) = createObserver[BlockWithHeight]
        grpcApi.getBlockRange(
          BlockRangeRequest.of(3, 3, BlockRangeRequest.Filter.Empty, includeTransactions = true),
          observer
        )

        checkOrderAttachment(resultRange.runSyncUnsafe().head, attachment)
      }
    }
  }

  "NODE-844. GetBlock should return correct rewardShares" in {
    blockRewardSharesTestCase { case (daoAddress, xtnBuybackAddress, d, grpcApi) =>
      val miner                       = d.appendBlock().sender.toAddress
      val blockBeforeBlockRewardDistr = d.appendBlock()
      val heightToBlock = (3 to 5).map { h =>
        h -> d.appendBlock().id()
      }.toMap
      d.appendBlock()

      // reward distribution features not activated
      checkBlockRewards(
        blockBeforeBlockRewardDistr.id(),
        2,
        Seq(RewardShare(ByteString.copyFrom(miner.bytes), d.blockchain.settings.rewardsSettings.initial))
      )(grpcApi)

      // BlockRewardDistribution activated
      val configAddrReward3 = d.blockchain.settings.rewardsSettings.initial / 3
      val minerReward3      = d.blockchain.settings.rewardsSettings.initial - 2 * configAddrReward3

      checkBlockRewards(
        heightToBlock(3),
        3,
        Seq(
          RewardShare(ByteString.copyFrom(miner.bytes), minerReward3),
          RewardShare(ByteString.copyFrom(daoAddress.bytes), configAddrReward3),
          RewardShare(ByteString.copyFrom(xtnBuybackAddress.bytes), configAddrReward3)
        ).sortBy(_.address.toByteStr)
      )(grpcApi)

      // CappedReward activated
      val configAddrReward4 = BlockRewardCalculator.MaxAddressReward
      val minerReward4      = d.blockchain.settings.rewardsSettings.initial - 2 * configAddrReward4

      checkBlockRewards(
        heightToBlock(4),
        4,
        Seq(
          RewardShare(ByteString.copyFrom(miner.bytes), minerReward4),
          RewardShare(ByteString.copyFrom(daoAddress.bytes), configAddrReward4),
          RewardShare(ByteString.copyFrom(xtnBuybackAddress.bytes), configAddrReward4)
        ).sortBy(_.address.toByteStr)
      )(grpcApi)

      // CeaseXTNBuyback activated with expired XTN buyback reward period
      val configAddrReward5 = BlockRewardCalculator.MaxAddressReward
      val minerReward5      = d.blockchain.settings.rewardsSettings.initial - configAddrReward5

      checkBlockRewards(
        heightToBlock(5),
        5,
        Seq(
          RewardShare(ByteString.copyFrom(miner.bytes), minerReward5),
          RewardShare(ByteString.copyFrom(daoAddress.bytes), configAddrReward5)
        ).sortBy(_.address.toByteStr)
      )(grpcApi)
    }
  }

  "NODE-845. GetBlockRange should return correct rewardShares" in {
    blockRewardSharesTestCase { case (daoAddress, xtnBuybackAddress, d, grpcApi) =>
      val miner = d.appendBlock().sender.toAddress
      d.appendBlock()

      (3 to 5).foreach(_ => d.appendBlock())
      d.appendBlock()

      val (observer, result) = createObserver[BlockWithHeight]
      grpcApi.getBlockRange(
        BlockRangeRequest.of(2, 5, BlockRangeRequest.Filter.Empty, includeTransactions = true),
        observer
      )
      val blocks = result.runSyncUnsafe()

      // reward distribution features not activated
      blocks.head.rewardShares shouldBe Seq(RewardShare(ByteString.copyFrom(miner.bytes), d.blockchain.settings.rewardsSettings.initial))

      // BlockRewardDistribution activated
      val configAddrReward3 = d.blockchain.settings.rewardsSettings.initial / 3
      val minerReward3      = d.blockchain.settings.rewardsSettings.initial - 2 * configAddrReward3

      blocks(1).rewardShares shouldBe Seq(
        RewardShare(ByteString.copyFrom(miner.bytes), minerReward3),
        RewardShare(ByteString.copyFrom(daoAddress.bytes), configAddrReward3),
        RewardShare(ByteString.copyFrom(xtnBuybackAddress.bytes), configAddrReward3)
      ).sortBy(_.address.toByteStr)

      // CappedReward activated
      val configAddrReward4 = BlockRewardCalculator.MaxAddressReward
      val minerReward4      = d.blockchain.settings.rewardsSettings.initial - 2 * configAddrReward4

      blocks(2).rewardShares shouldBe Seq(
        RewardShare(ByteString.copyFrom(miner.bytes), minerReward4),
        RewardShare(ByteString.copyFrom(daoAddress.bytes), configAddrReward4),
        RewardShare(ByteString.copyFrom(xtnBuybackAddress.bytes), configAddrReward4)
      ).sortBy(_.address.toByteStr)

      // CeaseXTNBuyback activated with expired XTN buyback reward period
      val configAddrReward5 = BlockRewardCalculator.MaxAddressReward
      val minerReward5      = d.blockchain.settings.rewardsSettings.initial - configAddrReward5

      blocks(3).rewardShares shouldBe Seq(
        RewardShare(ByteString.copyFrom(miner.bytes), minerReward5),
        RewardShare(ByteString.copyFrom(daoAddress.bytes), configAddrReward5)
      ).sortBy(_.address.toByteStr)
    }
  }

  "NODE-922. GetBlock should return correct data for challenging block" in {
    val sender = TxHelpers.signer(1)
    withDomain(DomainPresets.TransactionStateSnapshot, balances = AddrWithBalance.enoughBalances(sender, defaultSigner)) { d =>
      val grpcApi          = getGrpcApi(d)
      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, 1000.waves)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val invalidStateHash = ByteStr.fill(DigestLength)(1)
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq(TxHelpers.transfer(sender)),
        strictTime = true,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = d.createChallengingBlock(challengingMiner, originalBlock)
      val blockHeight      = 1002

      d.appendBlockE(challengingBlock) should beRight

      d.liquidAndSolidAssert { () =>
        val vrf = getBlockVrfPB(d, challengingBlock)
        vrf.isEmpty shouldBe false
        val expectedResult = BlockWithHeight.of(
          Some(PBBlocks.protobuf(challengingBlock)),
          blockHeight,
          vrf,
          getExpectedRewardShares(blockHeight, challengingMiner.toAddress, d.blockchain)
        )

        val resultById = Await.result(
          grpcApi.getBlock(BlockRequest.of(BlockRequest.Request.BlockId(challengingBlock.id().toByteString), includeTransactions = true)),
          timeout
        )

        resultById shouldBe expectedResult

        val resultByHeight = Await.result(
          grpcApi.getBlock(BlockRequest.of(BlockRequest.Request.Height(blockHeight), includeTransactions = true)),
          timeout
        )

        resultByHeight shouldBe expectedResult
      }
    }
  }

  "NODE-922. GetBlockRange should return correct data for challenging block" in {
    val sender = TxHelpers.signer(1)
    withDomain(DomainPresets.TransactionStateSnapshot, balances = AddrWithBalance.enoughBalances(sender, defaultSigner)) { d =>
      val grpcApi          = getGrpcApi(d)
      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, 1000.waves)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val invalidStateHash = ByteStr.fill(DigestLength)(1)
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq(TxHelpers.transfer(sender)),
        strictTime = true,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = d.createChallengingBlock(challengingMiner, originalBlock)
      val blockHeight      = 1002

      d.appendBlockE(challengingBlock) should beRight

      d.liquidAndSolidAssert { () =>
        val (observer, result) = createObserver[BlockWithHeight]
        grpcApi.getBlockRange(
          BlockRangeRequest.of(blockHeight, blockHeight, BlockRangeRequest.Filter.Empty, includeTransactions = true),
          observer
        )

        val vrf = getBlockVrfPB(d, challengingBlock)
        vrf.isEmpty shouldBe false

        result.runSyncUnsafe() shouldBe Seq(
          BlockWithHeight.of(
            Some(PBBlocks.protobuf(challengingBlock)),
            blockHeight,
            vrf,
            getExpectedRewardShares(blockHeight, challengingMiner.toAddress, d.blockchain)
          )
        )
      }
    }
  }

  private def getBlockVrfPB(d: Domain, block: Block): ByteString =
    d.blocksApi.block(block.id()).flatMap(_._1.vrf).map(_.toByteString).getOrElse(ByteString.EMPTY)

  private def getGrpcApi(d: Domain) =
    new BlocksApiGrpcImpl(d.blocksApi)

  private def checkBlockRewards(blockId: ByteStr, height: Int, expected: Seq[RewardShare])(api: BlocksApiGrpcImpl): Assertion = {
    Await
      .result(
        api.getBlock(BlockRequest.of(BlockRequest.Request.BlockId(blockId.toByteString), includeTransactions = false)),
        timeout
      )
      .rewardShares shouldBe expected

    Await
      .result(
        api.getBlock(BlockRequest.of(BlockRequest.Request.Height(height), includeTransactions = false)),
        timeout
      )
      .rewardShares shouldBe expected
  }

  private def blockRewardSharesTestCase(checks: (Address, Address, Domain, BlocksApiGrpcImpl) => Unit): Unit = {
    val daoAddress        = TxHelpers.address(3)
    val xtnBuybackAddress = TxHelpers.address(4)

    val settings = DomainPresets.ConsensusImprovements
    val settingsWithFeatures = settings
      .copy(blockchainSettings =
        settings.blockchainSettings.copy(
          functionalitySettings = settings.blockchainSettings.functionalitySettings
            .copy(daoAddress = Some(daoAddress.toString), xtnBuybackAddress = Some(xtnBuybackAddress.toString), xtnBuybackRewardPeriod = 1),
          rewardsSettings = settings.blockchainSettings.rewardsSettings.copy(initial = BlockRewardCalculator.FullRewardInit + 1.waves)
        )
      )
      .setFeaturesHeight(
        BlockchainFeatures.BlockRewardDistribution -> 3,
        BlockchainFeatures.CappedReward            -> 4,
        BlockchainFeatures.CeaseXtnBuyback         -> 5
      )

    withDomain(settingsWithFeatures) { d =>
      val grpcApi = getGrpcApi(d)

      checks(daoAddress, xtnBuybackAddress, d, grpcApi)
    }
  }

  private def getExpectedRewardShares(height: Int, miner: Address, blockchain: Blockchain): Seq[RewardShare] = {
    val expectedRewardShares = BlockRewardCalculator.getSortedBlockRewardShares(height, miner, blockchain)
    expectedRewardShares.map { case (addr, reward) =>
      RewardShare(ByteString.copyFrom(addr.bytes), reward)
    }
  }
}
