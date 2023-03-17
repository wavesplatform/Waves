package com.wavesplatform.api.grpc.test

import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.grpc.{BlockRangeRequest, BlockWithHeight, BlocksApiGrpcImpl}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.DiffMatchers
import monix.execution.Scheduler.Implicits.global
import org.scalatest.BeforeAndAfterAll

class BlocksApiGrpcSpec extends FreeSpec with BeforeAndAfterAll with DiffMatchers with WithDomain with GrpcApiHelpers {

  val sender: KeyPair    = TxHelpers.signer(1)
  val recipient: KeyPair = TxHelpers.signer(2)

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
        BlockWithHeight.of(Some(PBBlocks.protobuf(block)), idx + 2)
      }
    }
  }

  private def getGrpcApi(d: Domain) =
    new BlocksApiGrpcImpl(d.blocksApi)
}
