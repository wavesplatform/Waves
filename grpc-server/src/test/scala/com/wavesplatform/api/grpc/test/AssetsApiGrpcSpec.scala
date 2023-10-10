package com.wavesplatform.api.grpc.test

import com.google.protobuf.ByteString
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.grpc.{AssetInfoResponse, AssetRequest, AssetsApiGrpcImpl, NFTRequest, NFTResponse}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.utils.DiffMatchers
import monix.execution.Scheduler.Implicits.global
import org.scalatest.BeforeAndAfterAll
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime

import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration

class AssetsApiGrpcSpec extends FreeSpec with BeforeAndAfterAll with DiffMatchers with WithDomain with GrpcApiHelpers {

  val sender: KeyPair = TxHelpers.signer(1)
  val timeout: FiniteDuration = 2.minutes

  "GetInfo" - {
    "NODE-992. GetInfo for existing asset_id should return right result" in withDomain(RideV6.addFeatures(BlockchainFeatures.ReduceNFTFee), AddrWithBalance.enoughBalances(sender)) { d =>
      val grpcApi = getGrpcApi(d)
      val assetIssue = TxHelpers.issue(sender, 1, name = "asset", description = "some asset")

      d.appendBlock(assetIssue)
      d.liquidAndSolidAssert { () =>

        val result = Await.result(grpcApi.getInfo(
          AssetRequest(ByteString.copyFrom(assetIssue.id.value().arr))),
          timeout
        )

        result shouldBe AssetInfoResponse.of(
          ByteString.copyFrom(sender.publicKey.arr),
          assetIssue.name.toStringUtf8,
          assetIssue.description.toStringUtf8,
          assetIssue.decimals.value,
          assetIssue.reissuable,
          assetIssue.quantity.value,
          None,
          0,
          None,
          0,
          sequenceInBlock = 1,
          issueHeight = 2

        )

      }

    }
  }

  "GetNFTList" - {
    "NODE-940. GetNFTList should work" in withDomain(RideV6.addFeatures(BlockchainFeatures.ReduceNFTFee), AddrWithBalance.enoughBalances(sender)) { d =>
      val grpcApi = getGrpcApi(d)

      val nftIssues = (1 to 10).map(idx => TxHelpers.issue(sender, 1, name = s"nft$idx", reissuable = false))

      d.appendBlock(nftIssues *)

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
//todo after
    "NODE-999. GetNftList limit and after should work" in withDomain(RideV6.addFeatures(BlockchainFeatures.ReduceNFTFee), AddrWithBalance.enoughBalances(sender)) { d =>
      val grpcApi = getGrpcApi(d)

      val nftIssues = (1 to 10).map(idx => TxHelpers.issue(sender, 1, name = s"nft$idx", reissuable = false))

      d.appendBlock(nftIssues *)

      d.liquidAndSolidAssert { () =>
        val (observer, result) = createObserver[NFTResponse]
        grpcApi.getNFTList(
          NFTRequest.of(ByteString.copyFrom(sender.toAddress.bytes), 5, ByteString.EMPTY),
          observer
        )
        result.runSyncUnsafe() shouldBe nftIssues.take(5).zipWithIndex.map { case (nftTx, i) =>

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

    "NODE-1000. GetNftList should return empty response on address without NFT" in withDomain(RideV6.addFeatures(BlockchainFeatures.ReduceNFTFee), AddrWithBalance.enoughBalances(sender)) { d =>
      val grpcApi = getGrpcApi(d)

      val issueAsset = TxHelpers.issue(sender, 1, name = s"nonnft", reissuable = true)

      d.appendBlock(issueAsset)

      d.liquidAndSolidAssert { () =>
        val (observer, result) = createObserver[NFTResponse]
        grpcApi.getNFTList(
          NFTRequest.of(ByteString.copyFrom(sender.toAddress.bytes), 5, ByteString.EMPTY),
          observer
        )
        result.runSyncUnsafe() shouldBe List.empty
      }
    }

    "NODE-1004. GetNftList limit 0 should return empty list" in withDomain(RideV6.addFeatures(BlockchainFeatures.ReduceNFTFee), AddrWithBalance.enoughBalances(sender)) { d =>
      val grpcApi = getGrpcApi(d)

      val nftIssues = (1 to 10).map(idx => TxHelpers.issue(sender, 1, name = s"nft$idx", reissuable = false))

      d.appendBlock(nftIssues *)

      d.liquidAndSolidAssert { () =>
        val (observer, result) = createObserver[NFTResponse]
        grpcApi.getNFTList(
          NFTRequest.of(ByteString.copyFrom(sender.toAddress.bytes), 0, ByteString.EMPTY),
          observer
        )
        result.runSyncUnsafe() shouldBe List.empty
      }
    }

    //todo 10000 nft
    "NODE-1005. GetNftList max limit should return equal number of assets" in withDomain(RideV6.addFeatures(BlockchainFeatures.ReduceNFTFee), AddrWithBalance.enoughBalances(sender)) { d =>
      val grpcApi = getGrpcApi(d)

      val nftIssues: Seq[IssueTransaction] = (1 to 10).flatMap { bId =>
        val nftIssue = (1 to 1000).map(idx => TxHelpers.issue(sender, 1, name = s"nft${(bId - 1) * 1000 + idx}", reissuable = false))

        d.appendBlock(nftIssue *)
        nftIssue
      }

      d.liquidAndSolidAssert { () =>
        val (observer, result) = createObserver[NFTResponse]
        grpcApi.getNFTList(
          NFTRequest.of(ByteString.copyFrom(sender.toAddress.bytes), 9999, ByteString.EMPTY),
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
  }

  private def getGrpcApi(d: Domain) =
    new AssetsApiGrpcImpl(d.assetsApi, d.accountsApi)
}
