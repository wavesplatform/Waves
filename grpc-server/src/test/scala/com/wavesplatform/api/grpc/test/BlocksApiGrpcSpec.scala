package com.wavesplatform.api.grpc.test

import com.google.protobuf.ByteString
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.grpc.{BlockRangeRequest, BlockRequest, BlockWithHeight, BlocksApiGrpcImpl}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.protobuf.*
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order, OrderType}
import com.wavesplatform.transaction.{TxHelpers, TxVersion}
import com.wavesplatform.utils.DiffMatchers
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{Assertion, BeforeAndAfterAll}

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}

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

  private def getBlockVrfPB(d: Domain, block: Block): ByteString =
    d.blocksApi.block(block.id()).flatMap(_._1.vrf).map(_.toByteString).getOrElse(ByteString.EMPTY)

  private def getGrpcApi(d: Domain) =
    new BlocksApiGrpcImpl(d.blocksApi)
}
