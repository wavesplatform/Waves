package com.wavesplatform.it.transactions

import com.wavesplatform.it.util.TransactionGenerator
import com.wavesplatform.it.{IntegrationSuiteWithThreeAddresses, Node}
import com.wavesplatform.network.RawBytes
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

object TransactionsGeneratorTest {
  def splitTransactionsIntoValidBlocks(txs: Seq[Transaction]): Seq[Seq[Transaction]] = {
    val sortedTxs = txs.sortBy {
      case _: IssueTransaction => 1
      case _: CreateAliasTransaction => 1
      case _: LeaseTransaction => 1
      case _: TransferTransaction => 2
      case _: PaymentTransaction => 2
      case r: ReissueTransaction if r.reissuable => 3
      case _: BurnTransaction => 3
      case r: ReissueTransaction if !r.reissuable => 4
      case _: LeaseCancelTransaction => 4
      case _: ExchangeTransaction => 4
    }.toList

    def importantTransactionType(tt: TT.Value): Boolean = {
      tt == TT.IssueTransaction ||
        tt == TT.CreateAliasTransaction ||
        tt == TT.LeaseTransaction
    }

    val lastImportantTransactionIndex = sortedTxs.lastIndexWhere(t => importantTransactionType(t.transactionType))

    val (mostImportant, lessImportant) = sortedTxs.splitAt(lastImportantTransactionIndex)
    mostImportant.grouped(100).toSeq ++ lessImportant.grouped(100).toSeq
  }
}

class TransactionsGeneratorTest(override val allNodes: Seq[Node]) extends IntegrationSuiteWithThreeAddresses {

  import TransactionsGeneratorTest._

  test("generated transactions should lead to the right state") {
    val n = 200

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
    ), allNodes.map(n => PrivateKeyAccount(Base58.decode(n.accountSeed).get)), n)

    val node = allNodes.head

    def sendTransactionsPerBlock(transactions: Seq[Transaction]): Future[Unit] = Future {
      splitTransactionsIntoValidBlocks(transactions).foreach(txsPerBlock => {
        val sendF = for {
          _ <- node.sendByNetwork(txsPerBlock.map(tx => {
            println((tx.id, tx.json.toString(), Base58.encode(tx.bytes)))
            RawBytes(25.toByte, tx.bytes)
          }): _*)
          h <- node.height
          _ <- node.waitForHeight(h + 1)
        } yield
          succeed
        Await.result(sendF, 5 minutes)
      })
    }

    Await.result(sendTransactionsPerBlock(txs), 5.minutes)
    println("!!!")
  }
}
