package com.wavesplatform.api.grpc.test

import com.google.protobuf.ByteString
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.grpc.{BlockRangeRequest, BlockRequest, BlockWithHeight, BlocksApiGrpcImpl}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.{Domain, defaultSigner}
import com.wavesplatform.protobuf.*
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.DiffMatchers
import monix.execution.Scheduler.Implicits.global
import org.scalatest.BeforeAndAfterAll

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class BlocksApiGrpcSpec extends FreeSpec with BeforeAndAfterAll with DiffMatchers with WithDomain with GrpcApiHelpers {

  val sender: KeyPair    = TxHelpers.signer(1)
  val recipient: KeyPair = TxHelpers.signer(2)

  "GetBlock should work" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
    val grpcApi = getGrpcApi(d)

    val block = d.appendBlock(TxHelpers.transfer(sender, recipient.toAddress, 1))

    d.liquidAndSolidAssert { () =>
      val vrf = getBlockVrfPB(d, block)
      vrf.isEmpty shouldBe false
      val expectedResult = BlockWithHeight.of(Some(PBBlocks.protobuf(block)), 2, vrf)

      val resultById = Await.result(
        grpcApi.getBlock(BlockRequest.of(BlockRequest.Request.BlockId(block.id().toByteString), includeTransactions = true)),
        Duration.Inf
      )

      resultById shouldBe expectedResult

      val resultByHeight = Await.result(
        grpcApi.getBlock(BlockRequest.of(BlockRequest.Request.Height(2), includeTransactions = true)),
        Duration.Inf
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
        BlockWithHeight.of(Some(PBBlocks.protobuf(block)), idx + 2, vrf)
      }
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
        val expectedResult = BlockWithHeight.of(Some(PBBlocks.protobuf(challengingBlock)), blockHeight, vrf)

        val resultById = Await.result(
          grpcApi.getBlock(BlockRequest.of(BlockRequest.Request.BlockId(challengingBlock.id().toByteString), includeTransactions = true)),
          Duration.Inf
        )

        resultById shouldBe expectedResult

        val resultByHeight = Await.result(
          grpcApi.getBlock(BlockRequest.of(BlockRequest.Request.Height(blockHeight), includeTransactions = true)),
          Duration.Inf
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

        result.runSyncUnsafe() shouldBe Seq(BlockWithHeight.of(Some(PBBlocks.protobuf(challengingBlock)), blockHeight, vrf))
      }
    }
  }

  private def getBlockVrfPB(d: Domain, block: Block): ByteString =
    d.blocksApi.block(block.id()).flatMap(_._1.vrf).map(_.toByteString).getOrElse(ByteString.EMPTY)

  private def getGrpcApi(d: Domain) =
    new BlocksApiGrpcImpl(d.blocksApi)
}
