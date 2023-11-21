package com.wavesplatform.api.grpc.test

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.test.{FlatSpec, SharedDomain, TestTime}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.api.common.{CommonTransactionsApi, TransactionMeta}
import com.wavesplatform.api.grpc.TransactionsApiGrpcImpl
import com.wavesplatform.block.Block
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.state.{Blockchain, Height}
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, EthTxGenerator, Transaction, TxHelpers, TxVersion}
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.TransactionType.TransactionType
import com.wavesplatform.utils.{DiffMatchers, EthHelpers}
import io.grpc.StatusException
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalatest.BeforeAndAfterAll

class GRPCBroadcastSpec extends FlatSpec with BeforeAndAfterAll with EthHelpers with DiffMatchers with SharedDomain {
  // Fake NTP time
  val FakeTime: TestTime = TestTime(100)

  "GRPC broadcast" should "accept Exchange with ETH orders" in {
    import com.wavesplatform.transaction.assets.exchange.EthOrderSpec.{ethBuyOrder, ethSellOrder}

    val transaction = TxHelpers.exchange(ethBuyOrder, ethSellOrder, price = 100, version = TxVersion.V3, timestamp = 100)
    FakeTime.setTime(transaction.timestamp)
    domain.blockchain.assertBroadcast(transaction)
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
}
