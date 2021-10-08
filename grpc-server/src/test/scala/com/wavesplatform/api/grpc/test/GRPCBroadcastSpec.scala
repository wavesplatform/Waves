package com.wavesplatform.api.grpc.test

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.test.{FlatSpec, TestTime}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.BlockchainStubHelpers
import com.wavesplatform.api.common.{CommonTransactionsApi, TransactionMeta}
import com.wavesplatform.api.grpc.TransactionsApiGrpcImpl
import com.wavesplatform.block.Block
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.state.{Blockchain, Height}
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, Transaction, TxHelpers, TxVersion}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.TransactionType.TransactionType
import com.wavesplatform.transaction.utils.EthTxGenerator
import com.wavesplatform.utils.{DiffMatchers, EthHelpers, EthSetChainId}
import io.grpc.StatusException
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.BeforeAndAfterAll
import com.wavesplatform.transaction.utils.EthConverters._

class GRPCBroadcastSpec
    extends FlatSpec
    with BeforeAndAfterAll
    with PathMockFactory
    with BlockchainStubHelpers
    with EthHelpers
    with EthSetChainId
    with DiffMatchers {
  // Fake NTP time
  val FakeTime: TestTime = TestTime(100)

  "GRPC broadcast" should "accept Exchange with ETH orders" in {
    val ethBuyOrder = Order(
      Order.V4,
      TestEthPublicKey,
      TxHelpers.matcher.publicKey,
      AssetPair(IssuedAsset(ByteStr(EthStubBytes32)), Waves),
      OrderType.BUY,
      1,
      100L,
      1,
      123,
      100000,
      Waves,
      eip712Signature = EthSignature(
        "0xe5ff562bfb0296e95b631365599c87f1c5002597bf56a131f289765275d2580f5344c62999404c37cd858ea037328ac91eca16ad1ce69c345ebb52fde70b66251c"
      )
    )

    val ethSellOrder = Order(
      Order.V4,
      TestEthPublicKey,
      TxHelpers.matcher.publicKey,
      AssetPair(IssuedAsset(ByteStr(EthStubBytes32)), Waves),
      OrderType.SELL,
      1,
      100L,
      1,
      123,
      100000,
      Waves,
      eip712Signature = EthSignature(
        "0xc8ba2bdafd27742546b3be34883efc51d6cdffbb235798d7b51876c6854791f019b0522d7a39b6f2087cba46ae86919b71a2d9d7920dfc8e00246d8f02a258f21b"
      )
    )

    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.creditBalance(TxHelpers.matcher.toAddress, *)
      sh.creditBalance(TestEthPublicKey.toAddress, *)
      sh.issueAsset(ByteStr(EthStubBytes32))
    }

    val transaction = TxHelpers.exchange(ethBuyOrder, ethSellOrder, TxVersion.V3, 100)
    FakeTime.setTime(transaction.timestamp)
    blockchain.assertBroadcast(transaction)
  }

  it should "reject eth transactions" in {
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.creditBalance(TxHelpers.defaultEthAddress, Waves)
      sh.activateFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.SynchronousCalls)
    }

    val transaction = EthTxGenerator.generateEthTransfer(TxHelpers.defaultEthSigner, TxHelpers.secondAddress, 10, Waves)
    FakeTime.setTime(transaction.timestamp)
    intercept[Exception](blockchain.assertBroadcast(transaction)).toString should include("ETH transactions should not be broadcasted over gRPC")
  }

  //noinspection NotImplementedCode
  implicit class BlockchainBroadcastExt(blockchain: Blockchain) {
    def grpcTxApi: TransactionsApiGrpcImpl =
      new TransactionsApiGrpcImpl(blockchain, new CommonTransactionsApi {
        def aliasesOfAddress(address: Address): Observable[(Height, CreateAliasTransaction)] = ???
        def transactionById(txId: ByteStr): Option[TransactionMeta]                          = ???
        def unconfirmedTransactions: Seq[Transaction]                                        = ???
        def unconfirmedTransactionById(txId: ByteStr): Option[Transaction]                   = ???
        def calculateFee(tx: Transaction): Either[ValidationError, (Asset, Long, Long)]      = ???
        def transactionsByAddress(
            subject: Address,
            sender: Option[Address],
            transactionTypes: Set[TransactionType],
            fromId: Option[ByteStr]
        ): Observable[TransactionMeta]                                                     = ???
        def transactionProofs(transactionIds: List[ByteStr]): List[Block.TransactionProof] = ???
        def broadcastTransaction(tx: Transaction): Future[TracedResult[ValidationError, Boolean]] = {
          val differ = blockchain.stub.transactionDiffer(FakeTime)
          Future.successful(differ(tx).map(_ => true))
        }
      })(Scheduler.global)

    @throws[StatusException]("on failed broadcast")
    def assertBroadcast(tx: Transaction): Unit = {
      Await.result(grpcTxApi.broadcast(PBTransactions.protobuf(tx)), Duration.Inf)
    }
  }
}
