package com.wavesplatform.api.grpc.test

import com.google.protobuf.ByteString
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.grpc.{AssetInfoResponse, AssetsApiGrpcImpl, NFTRequest, NFTResponse}
import com.wavesplatform.block.Block.ProtoBlockVersion
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.DiffMatchers
import monix.execution.Scheduler.Implicits.global
import org.scalatest.BeforeAndAfterAll

class AssetsApiGrpcSpec extends FreeSpec with BeforeAndAfterAll with DiffMatchers with WithDomain with GrpcApiHelpers {

  val sender: KeyPair = TxHelpers.signer(1)

  "GetNFTList should work" in withDomain(RideV6.addFeatures(BlockchainFeatures.ReduceNFTFee), AddrWithBalance.enoughBalances(sender)) { d =>
    val grpcApi = getGrpcApi(d)

    val nftIssues = (1 to 10).map(idx => TxHelpers.issue(sender, 1, name = s"nft$idx", reissuable = false))

    d.appendBlock(nftIssues*)

    d.liquidAndSolidAssert { () =>
      val (observer, result) = createObserver[NFTResponse]
      grpcApi.getNFTList(
        NFTRequest.of(ByteString.copyFrom(sender.toAddress.bytes), 10, ByteString.EMPTY),
        observer
      )
      result.runSyncUnsafe() shouldBe nftIssues.zipWithIndex.map { case (nftTx, i) =>
        NFTResponse.of(
          ByteString.copyFrom(nftTx.asset.id.arr),
          Some(
            AssetInfoResponse.of(
              ByteString.copyFrom(sender.publicKey.arr),
              nftTx.name.toStringUtf8,
              nftTx.description.toStringUtf8,
              nftTx.decimals.value,
              nftTx.reissuable,
              nftTx.quantity.value,
              None,
              0,
              None,
              0,
              sequenceInBlock = i + 1,
              issueHeight = 2
            )
          )
        )
      }
    }
  }

  "NODE-999. GetNftList limit should work properly" in withDomain(
    RideV6.addFeatures(BlockchainFeatures.ReduceNFTFee),
    AddrWithBalance.enoughBalances(sender)
  ) { d =>
    val nftIssues = (1 to 5).map(idx => TxHelpers.issue(sender, 1, name = s"nft$idx", reissuable = false))
    val limit     = 2
    val afterId   = 1 // second element

    d.appendBlock()
    val mb1 = d.appendMicroBlock(nftIssues.take(afterId + 1)*)
    d.appendMicroBlock(nftIssues.drop(afterId + 1)*)

    // full liquid
    d.rocksDBWriter.containsTransaction(nftIssues(afterId)) shouldBe false
    d.rocksDBWriter.containsTransaction(nftIssues(afterId + 1)) shouldBe false
    check()

    // liquid afterId
    d.appendBlock(d.createBlock(ProtoBlockVersion, nftIssues.drop(afterId + 1), Some(mb1)))
    d.rocksDBWriter.containsTransaction(nftIssues(afterId)) shouldBe true
    d.rocksDBWriter.containsTransaction(nftIssues(afterId + 1)) shouldBe false
    check()

    // full solid
    d.appendBlock()
    d.rocksDBWriter.containsTransaction(nftIssues(afterId)) shouldBe true
    d.rocksDBWriter.containsTransaction(nftIssues(afterId + 1)) shouldBe true
    check()

    def check() = {
      val (observer, result) = createObserver[NFTResponse]
      val request = NFTRequest.of(
        ByteString.copyFrom(sender.toAddress.bytes),
        limit,
        afterAssetId = ByteString.copyFrom(nftIssues(afterId).asset.id.arr)
      )
      getGrpcApi(d).getNFTList(request, observer)
      val response = result.runSyncUnsafe()
      response.size shouldBe limit
      response.map(_.assetInfo.get.name) shouldBe nftIssues.slice(afterId + 1, afterId + limit + 1).map(_.name.toStringUtf8)
    }
  }

  private def getGrpcApi(d: Domain) =
    new AssetsApiGrpcImpl(d.assetsApi, d.accountsApi)
}
