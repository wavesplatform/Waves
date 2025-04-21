package com.wavesplatform.api.grpc.test

import com.wavesplatform.api.grpc.TransactionsApiGrpcImpl
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.test.{FlatSpec, NumericExt}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.*
import com.wavesplatform.transaction.utils.EthConverters.*
import com.wavesplatform.transaction.{EthTxGenerator, TxExchangeAmount, TxHelpers, TxMatcherFee, TxOrderPrice, TxVersion}
import com.wavesplatform.utils.Schedulers
import monix.execution.ExecutionModel.SynchronousExecution
import monix.execution.Scheduler
import org.scalatest.ParallelTestExecution
import org.web3j.crypto.Bip32ECKeyPair

import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.Try

class GRPCBroadcastSpec extends FlatSpec with WithDomain with ParallelTestExecution with GrpcApiHelpers {
  import GRPCBroadcastSpec.{ethBuyOrderSigned, ethSellOrderSigned}

  private given scheduler: Scheduler = Schedulers.singleThread("grpc", executionModel = SynchronousExecution)

  "GRPC broadcast" should "accept Exchange with ETH orders" in {
    val assetIssuer      = TxHelpers.defaultSigner
    val buyerEthAccount  = TxHelpers.signer(1).toEthKeyPair
    val sellerEthAccount = TxHelpers.signer(2).toEthKeyPair

    val balances = Seq(
      AddrWithBalance(buyerEthAccount.toWavesAddress, 1000.waves),
      AddrWithBalance(sellerEthAccount.toWavesAddress, 1000.waves),
      AddrWithBalance(TxHelpers.matcher.toAddress, 1000.waves)
    )

    withDomain(DomainPresets.RideV6, balances) { d =>
      val grpcApi = getGrpcApi(d)

      // Issue an asset
      val issueTx   = TxHelpers.issue(assetIssuer, Long.MaxValue)
      val testAsset = issueTx.asset
      d.appendBlock(issueTx)

      // Transfer asset to seller
      d.appendBlock(TxHelpers.transfer(assetIssuer, sellerEthAccount.toWavesAddress, 1000L, testAsset))

      val ethBuyOrder  = ethBuyOrderSigned(testAsset, buyerEthAccount, TxHelpers.timestamp)
      val ethSellOrder = ethSellOrderSigned(testAsset, sellerEthAccount, TxHelpers.timestamp)

      val transaction = TxHelpers.exchange(ethBuyOrder, ethSellOrder, TxHelpers.matcher, price = 100, version = TxVersion.V3)
      Await.result(grpcApi.broadcast(PBTransactions.protobuf(transaction)), 10.seconds)
    }
  }

  it should "reject eth transactions" in {
    withDomain(DomainPresets.RideV6) { d =>
      val grpcApi = getGrpcApi(d)

      val transaction = EthTxGenerator.generateEthTransfer(TxHelpers.defaultEthSigner, TxHelpers.secondAddress, 10, Waves)

      Try(Await.result(grpcApi.broadcast(PBTransactions.protobuf(transaction)), 10.seconds)).toEither should matchPattern {
        case Left(err) if err.toString.contains("ETH transactions should not be broadcasted over gRPC") =>
      }
    }
  }

  private def getGrpcApi(d: Domain) =
    new TransactionsApiGrpcImpl(d.blockchain, d.transactionsApi)
}

object GRPCBroadcastSpec {
  private val emptySignature = OrderAuthentication.Eip712Signature(ByteStr(new Array[Byte](64)))

  def ethBuyOrderSigned(testAsset: IssuedAsset, buyerEthAccount: Bip32ECKeyPair, timestamp: Long): Order = {
    val ethBuyOrderTemplate: Order = Order(
      Order.V4,
      emptySignature,
      TxHelpers.matcher.publicKey,
      AssetPair(testAsset, Waves),
      OrderType.BUY,
      TxExchangeAmount.unsafeFrom(1),
      TxOrderPrice.unsafeFrom(100L),
      timestamp,
      timestamp + 10000,
      TxMatcherFee.unsafeFrom(100000),
      Waves
    )

    ethBuyOrderTemplate.copy(
      orderAuthentication = OrderAuthentication.Eip712Signature(ByteStr(EthOrders.signOrder(ethBuyOrderTemplate, buyerEthAccount)))
    )
  }

  def ethSellOrderSigned(testAsset: IssuedAsset, sellerEthAccount: Bip32ECKeyPair, timestamp: Long): Order = {
    val ethSellOrderTemplate: Order = Order(
      Order.V4,
      emptySignature,
      TxHelpers.matcher.publicKey,
      AssetPair(testAsset, Waves),
      OrderType.SELL,
      TxExchangeAmount.unsafeFrom(1),
      TxOrderPrice.unsafeFrom(100L),
      timestamp,
      timestamp + 10000,
      TxMatcherFee.unsafeFrom(100000),
      Waves
    )

    ethSellOrderTemplate.copy(
      orderAuthentication = OrderAuthentication.Eip712Signature(ByteStr(EthOrders.signOrder(ethSellOrderTemplate, sellerEthAccount)))
    )
  }
}
