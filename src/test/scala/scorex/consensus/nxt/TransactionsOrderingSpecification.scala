package scorex.consensus.nxt

import org.scalatest.{Assertions, Matchers, PropSpec}
import scorex.account.{Account, PrivateKeyAccount}
import scorex.consensus.TransactionsOrdering
import scorex.crypto.encode.Base58
import scorex.transaction.PaymentTransaction
import scorex.transaction.assets.TransferTransaction

import scala.util.Random

class TransactionsOrderingSpecification extends PropSpec with Assertions with Matchers {

  property("TransactionsOrdering.InBlock should sort correctly") {
    val txsDifferentById = (0 to 3).map(i =>
      TransferTransaction.create(None, PrivateKeyAccount(Array.fill(32)(0)), Account.fromBase58String("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").right.get, 100000, 5, None, 125L, Array(i.toByte)).right.get).sortBy(t => Base58.encode(t.id))

    val correctSeq = txsDifferentById ++ Seq(
      TransferTransaction.create(None, PrivateKeyAccount(Array.fill(32)(0)), Account.fromBase58String("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").right.get, 100000, 1, None, 125L, Array.empty).right.get,
      TransferTransaction.create(None, PrivateKeyAccount(Array.fill(32)(0)), Account.fromBase58String("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").right.get, 100000, 2, None, 124L, Array.empty).right.get,
      TransferTransaction.create(None, PrivateKeyAccount(Array.fill(32)(0)), Account.fromBase58String("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").right.get, 100000, 1, None, 124L, Array.empty).right.get,
      TransferTransaction.create(None, PrivateKeyAccount(Array.fill(32)(0)), Account.fromBase58String("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").right.get, 100000, 2, Some(Array.empty), 124L, Array.empty).right.get,
      TransferTransaction.create(None, PrivateKeyAccount(Array.fill(32)(0)), Account.fromBase58String("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").right.get, 100000, 1, Some(Array.empty), 124L, Array.empty).right.get)

    val sorted = Random.shuffle(correctSeq).sorted(TransactionsOrdering.InBlock)

    sorted shouldBe correctSeq
  }

  property("TransactionsOrdering.InUTXPool should sort correctly") {
    val txsDifferentById = (0 to 3).map(i =>
      TransferTransaction.create(None, PrivateKeyAccount(Array.fill(32)(0)),Account.fromBase58String("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").right.get, 100000, 5, None, 125L, Array(i.toByte)).right.get).sortBy(t => Base58.encode(t.id))

    val correctSeq = txsDifferentById ++ Seq(
      TransferTransaction.create(None, PrivateKeyAccount(Array.fill(32)(0)), Account.fromBase58String("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").right.get, 100000, 1, None, 124L, Array.empty).right.get,
      TransferTransaction.create(None, PrivateKeyAccount(Array.fill(32)(0)), Account.fromBase58String("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").right.get, 100000, 1, None, 123L, Array.empty).right.get,
      TransferTransaction.create(None, PrivateKeyAccount(Array.fill(32)(0)), Account.fromBase58String("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").right.get, 100000, 2, None, 123L, Array.empty).right.get,
      TransferTransaction.create(None, PrivateKeyAccount(Array.fill(32)(0)), Account.fromBase58String("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").right.get, 100000, 1, Some(Array.empty), 124L, Array.empty).right.get,
      TransferTransaction.create(None, PrivateKeyAccount(Array.fill(32)(0)), Account.fromBase58String("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").right.get, 100000, 2, Some(Array.empty), 124L, Array.empty).right.get)

    val sorted = Random.shuffle(correctSeq).sorted(TransactionsOrdering.InUTXPool)

    sorted shouldBe correctSeq
  }

  property("TransactionsOrdering.InBlock should sort txs by decreasing block timestamp") {
    val correctSeq = Seq(
      PaymentTransaction.create(PrivateKeyAccount(Array.fill(32)(0)), Account.fromBase58String("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").right.get, 100000, 1, 124L).right.get,
      PaymentTransaction.create(PrivateKeyAccount(Array.fill(32)(0)), Account.fromBase58String("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").right.get, 100000, 1, 123L).right.get)

    Random.shuffle(correctSeq).sorted(TransactionsOrdering.InBlock) shouldBe correctSeq
  }

  property("TransactionsOrdering.InUTXPool should sort txs by ascending block timestamp") {
    val correctSeq = Seq(
      PaymentTransaction.create(PrivateKeyAccount(Array.fill(32)(0)), Account.fromBase58String("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").right.get, 100000, 1, 123L).right.get,
      PaymentTransaction.create(PrivateKeyAccount(Array.fill(32)(0)), Account.fromBase58String("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").right.get, 100000, 1, 124L).right.get)
    Random.shuffle(correctSeq).sorted(TransactionsOrdering.InUTXPool) shouldBe correctSeq
  }
}
