package com.wavesplatform.it.grpc

import com.google.common.primitives.Ints
import com.typesafe.config.Config
import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.api.grpc.{BlockRangeRequest, BlockRequest, BlocksApiGrpc}
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync.grpc.GrpcBaseTransactionSuite
import com.wavesplatform.protobuf.block._
import com.wavesplatform.protobuf.transaction.PBRecipients

class BlocksApiSuite extends GrpcBaseTransactionSuite {
  private val BlockV4Height = 3
  private val BlockV5Height = 5
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.raw(s"""waves {
                             |  miner { 
                             |    quorum = 0
                             |    max-transactions-in-micro-block = 1
                             |  }
                             |  blockchain.custom.functionality.pre-activated-features {
                             |    14 = $BlockV4Height
                             |    15 = $BlockV5Height
                             |  }
                             |}""".stripMargin))
      .withDefault(1)
      .buildNonConflicting()

  private lazy val blocksApi = BlocksApiGrpc.blockingStub(sender.grpcChannel)

  private def validateHeaders(range: Range)(assertion: PBBlockHeader => Unit): Unit = {
    val headersByHeight = range.map(height => blocksApi.getBlock(BlockRequest(request = BlockRequest.Request.Height(height))))
    val headersRange    = blocksApi.getBlockRange(BlockRangeRequest(range.min, range.max))

    headersByHeight zip headersRange foreach { case (h1, h2) =>
      h1.getBlock.header shouldEqual h2.getBlock.header
      h1.getBlock.signature shouldEqual h2.getBlock.signature
      h1.getBlock.transactions should be(empty)
      h1.height shouldEqual h2.height
    }

    (headersByHeight ++ headersRange).foreach(bwh => assertion(bwh.block.get.header.get))
  }

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    1 to 100 foreach { i =>
      sender.broadcastTransfer(
        sender.keyPair,
        PBRecipients.create(PublicKey(Ints.toByteArray(i) ++ new Array[Byte](28)).toAddress),
        100000000L,
        100000L
      )
    }
  }

  test("Validate Block v3 header fields") {
    sender.waitForHeight(BlockV4Height)
    validateHeaders(2 to BlockV4Height) { header =>
      header.chainId shouldEqual AddressScheme.current.chainId
      header.version shouldEqual 3
    }
  }

  test("Validate Block v4 header fields") {
    sender.waitForHeight(BlockV5Height)
    validateHeaders(BlockV4Height + 1 until BlockV5Height) { header =>
      header.chainId shouldEqual AddressScheme.current.chainId
      header.version shouldEqual 4
      header.rewardVote shouldEqual -1
    }
  }

  test("Validate Block v5 header fields") {
    sender.waitForHeight(BlockV5Height + 2)
    validateHeaders(BlockV5Height until BlockV5Height + 2) { header =>
      header.chainId shouldEqual AddressScheme.current.chainId
      header.version shouldEqual 5
      header.rewardVote shouldEqual -1
    }
  }
}
