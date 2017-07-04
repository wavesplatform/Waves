package com.wavesplatform.it.transactions

import com.wavesplatform.it.network.client.RawBytes
import com.wavesplatform.it.util.TransactionGenerator
import com.wavesplatform.it.{IntegrationSuiteWithThreeAddresses, Node}
import scorex.account.PrivateKeyAccount
import scorex.crypto.encode.Base58
import scorex.transaction.TransactionParser.{TransactionType => TT}
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.{CreateAliasTransaction, PaymentTransaction, Transaction}
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class TransactionsGeneratorTest(override val allNodes: Seq[Node]) extends IntegrationSuiteWithThreeAddresses {
  test("generated transactions should lead to the right state") {
    val n = 100

    val txs = TransactionGenerator.gen(Map(
      TT.IssueTransaction -> 0.1f,
      TT.CreateAliasTransaction -> 0.1f,
      TT.LeaseTransaction -> 0.1f,
      TT.TransferTransaction -> 0.3f,
      TT.ReissueTransaction -> 0.05f,
      TT.BurnTransaction -> 0.075f,
      TT.LeaseCancelTransaction -> 0.075f,
      TT.ExchangeTransaction -> 0.1f,
      TT.PaymentTransaction -> 0.1f
    ), allNodes.map(n => PrivateKeyAccount(Base58.decode(n.privateKey).get)), n)

    val node = allNodes.head

    def sendTransactionsPerBlock(transactions: Seq[Transaction]): Future[Unit] = Future {
      def splitTransactionsIntoValidBlocks(txs: Seq[Transaction]): Seq[Seq[Transaction]] = {
        txs.sortBy {
          case _: IssueTransaction => 1
          case _: CreateAliasTransaction => 1
          case _: LeaseTransaction => 2
          case _: TransferTransaction => 2
          case _: PaymentTransaction => 2
          case _: ReissueTransaction => 3
          case _: BurnTransaction => 3
          case _: LeaseCancelTransaction => 4
          case _: ExchangeTransaction => 4
        }.grouped(100).toSeq
      }

      splitTransactionsIntoValidBlocks(transactions).foreach(txsPerBlock => {
        val sendF = for {
          _ <- node.sendByNetwork(txsPerBlock.map(tx => RawBytes(25.toByte, tx.bytes)): _*)
          _ <- node.waitForNextBlock
        } yield succeed
        Await.result(sendF, 30 seconds)
      })
    }

    Await.result(sendTransactionsPerBlock(txs), 2.minutes)
    println("!!!")
  }
}
