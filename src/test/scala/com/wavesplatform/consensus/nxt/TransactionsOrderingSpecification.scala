package com.wavesplatform.consensus.nxt

import com.wavesplatform.state.{ByteStr, EitherExt2}
import org.scalatest.{Assertions, Matchers, PropSpec}
import com.wavesplatform.account.{Address, PrivateKeyAccount}
import com.wavesplatform.consensus.TransactionsOrdering
import com.wavesplatform.transaction.transfer._

import scala.util.Random

class TransactionsOrderingSpecification extends PropSpec with Assertions with Matchers {

  property("TransactionsOrdering.InBlock should sort correctly") {
    val correctSeq = Seq(
      TransferTransactionV1
        .selfSigned(None,
                    PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
                    100000,
                    1,
                    None,
                    125L,
                    Array.empty)
        .right
        .get,
      TransferTransactionV1
        .selfSigned(None,
                    PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
                    100000,
                    2,
                    None,
                    124L,
                    Array.empty)
        .right
        .get,
      TransferTransactionV1
        .selfSigned(None,
                    PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
                    100000,
                    1,
                    None,
                    124L,
                    Array.empty)
        .right
        .get,
      TransferTransactionV1
        .selfSigned(
          None,
          PrivateKeyAccount(Array.fill(32)(0)),
          Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
          100000,
          2,
          Some(ByteStr.empty),
          124L,
          Array.empty
        )
        .right
        .get,
      TransferTransactionV1
        .selfSigned(
          None,
          PrivateKeyAccount(Array.fill(32)(0)),
          Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
          100000,
          1,
          Some(ByteStr.empty),
          124L,
          Array.empty
        )
        .right
        .get
    )

    val sorted = Random.shuffle(correctSeq).sorted(TransactionsOrdering.InBlock)

    sorted shouldBe correctSeq
  }

  property("TransactionsOrdering.InUTXPool should sort correctly") {
    val correctSeq = Seq(
      TransferTransactionV1
        .selfSigned(None,
                    PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
                    100000,
                    1,
                    None,
                    124L,
                    Array.empty)
        .right
        .get,
      TransferTransactionV1
        .selfSigned(None,
                    PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
                    100000,
                    1,
                    None,
                    123L,
                    Array.empty)
        .right
        .get,
      TransferTransactionV1
        .selfSigned(None,
                    PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
                    100000,
                    2,
                    None,
                    123L,
                    Array.empty)
        .right
        .get,
      TransferTransactionV1
        .selfSigned(
          None,
          PrivateKeyAccount(Array.fill(32)(0)),
          Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
          100000,
          1,
          Some(ByteStr.empty),
          124L,
          Array.empty
        )
        .right
        .get,
      TransferTransactionV1
        .selfSigned(
          None,
          PrivateKeyAccount(Array.fill(32)(0)),
          Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
          100000,
          2,
          Some(ByteStr.empty),
          124L,
          Array.empty
        )
        .right
        .get
    )

    val sorted = Random.shuffle(correctSeq).sorted(TransactionsOrdering.InUTXPool)

    sorted shouldBe correctSeq
  }

  property("TransactionsOrdering.InBlock should sort txs by decreasing block timestamp") {
    val correctSeq = Seq(
      TransferTransactionV1
        .selfSigned(None,
                    PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
                    100000,
                    124L,
                    None,
                    1,
                    Array())
        .right
        .get,
      TransferTransactionV1
        .selfSigned(None,
                    PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
                    100000,
                    123L,
                    None,
                    1,
                    Array())
        .right
        .get
    )

    Random.shuffle(correctSeq).sorted(TransactionsOrdering.InBlock) shouldBe correctSeq
  }

  property("TransactionsOrdering.InUTXPool should sort txs by ascending block timestamp") {
    val correctSeq = Seq(
      TransferTransactionV1
        .selfSigned(None,
                    PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
                    100000,
                    123L,
                    None,
                    1,
                    Array())
        .right
        .get,
      TransferTransactionV1
        .selfSigned(None,
                    PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
                    100000,
                    124L,
                    None,
                    1,
                    Array())
        .right
        .get
    )
    Random.shuffle(correctSeq).sorted(TransactionsOrdering.InUTXPool) shouldBe correctSeq
  }
}
