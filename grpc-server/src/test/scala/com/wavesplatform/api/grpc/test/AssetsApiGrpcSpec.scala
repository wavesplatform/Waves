package com.wavesplatform.api.grpc.test

import com.google.protobuf.ByteString
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.grpc.{AssetInfoResponse, AssetsApiGrpcImpl, NFTRequest, NFTResponse}
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
      result.runSyncUnsafe() shouldBe nftIssues.map { nftTx =>
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
              0
            )
          )
        )
      }
    }
  }

  private def getGrpcApi(d: Domain) =
    new AssetsApiGrpcImpl(d.assetsApi, d.accountsApi)
}
