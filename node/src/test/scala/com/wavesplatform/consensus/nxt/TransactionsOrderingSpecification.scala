package com.wavesplatform.consensus.nxt

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.TransactionsOrdering
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer._

import scala.util.Random

class TransactionsOrderingSpecification extends PropSpec {

  private val kp: KeyPair = KeyPair(ByteStr(new Array[Byte](32)))
  property("TransactionsOrdering.InBlock should sort correctly") {
    val correctSeq = Seq(
      TransferTransaction
        .selfSigned(
          1.toByte,
          kp,
          Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
          Waves,
          100000,
          Waves,
          125L,
          ByteStr.empty,
          1
        )
        .explicitGet(),
      TransferTransaction
        .selfSigned(
          1.toByte,
          kp,
          Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
          Waves,
          100000,
          Waves,
          124L,
          ByteStr.empty,
          2
        )
        .explicitGet(),
      TransferTransaction
        .selfSigned(
          1.toByte,
          kp,
          Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
          Waves,
          100000,
          Waves,
          124L,
          ByteStr.empty,
          1
        )
        .explicitGet(),
      TransferTransaction
        .selfSigned(
          1.toByte,
          kp,
          Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
          Waves,
          100000,
          Asset.fromCompatId(Some(ByteStr.empty)),
          124L,
          ByteStr.empty,
          2
        )
        .explicitGet(),
      TransferTransaction
        .selfSigned(
          1.toByte,
          kp,
          Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
          Waves,
          100000,
          Asset.fromCompatId(Some(ByteStr.empty)),
          124L,
          ByteStr.empty,
          1
        )
        .explicitGet()
    )

    val sorted = Random.shuffle(correctSeq).sorted(TransactionsOrdering.InBlock)

    sorted shouldBe correctSeq
  }

  property("TransactionsOrdering.InUTXPool should sort correctly") {
    val correctSeq = Seq(
      TransferTransaction
        .selfSigned(
          1.toByte,
          kp,
          Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
          Waves,
          100000,
          Waves,
          124L,
          ByteStr.empty,
          1
        )
        .explicitGet(),
      TransferTransaction
        .selfSigned(
          1.toByte,
          kp,
          Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
          Waves,
          100000,
          Waves,
          123L,
          ByteStr.empty,
          1
        )
        .explicitGet(),
      TransferTransaction
        .selfSigned(
          1.toByte,
          kp,
          Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
          Waves,
          100000,
          Waves,
          123L,
          ByteStr.empty,
          2
        )
        .explicitGet(),
      TransferTransaction
        .selfSigned(
          1.toByte,
          kp,
          Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
          Waves,
          100000,
          Asset.fromCompatId(Some(ByteStr.empty)),
          124L,
          ByteStr.empty,
          1
        )
        .explicitGet(),
      TransferTransaction
        .selfSigned(
          1.toByte,
          kp,
          Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
          Waves,
          100000,
          Asset.fromCompatId(Some(ByteStr.empty)),
          124L,
          ByteStr.empty,
          2
        )
        .explicitGet()
    )

    val sorted = Random.shuffle(correctSeq).sorted(TransactionsOrdering.InUTXPool(Set.empty))

    sorted shouldBe correctSeq
  }

  property("TransactionsOrdering.InBlock should sort txs by decreasing block timestamp") {
    val correctSeq = Seq(
      TransferTransaction
        .selfSigned(
          1.toByte,
          kp,
          Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
          Waves,
          100000,
          Waves,
          1,
          ByteStr.empty,
          124L
        )
        .explicitGet(),
      TransferTransaction
        .selfSigned(
          1.toByte,
          kp,
          Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
          Waves,
          100000,
          Waves,
          1,
          ByteStr.empty,
          123L
        )
        .explicitGet()
    )

    Random.shuffle(correctSeq).sorted(TransactionsOrdering.InBlock) shouldBe correctSeq
  }

  property("TransactionsOrdering.InUTXPool should sort txs by ascending block timestamp taking into consideration whitelisted senders") {
    val whitelisted = KeyPair(Array.fill(32)(1: Byte))
    val correctSeq = Seq(
      TransferTransaction
        .selfSigned(
          1.toByte,
          whitelisted,
          Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
          Waves,
          100000,
          Waves,
          2,
          ByteStr.empty,
          123L
        )
        .explicitGet(),
      TransferTransaction
        .selfSigned(
          1.toByte,
          KeyPair(Array.fill(32)(0: Byte)),
          Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(),
          Waves,
          100000,
          Waves,
          2,
          ByteStr.empty,
          124L
        )
        .explicitGet()
    )
    Random.shuffle(correctSeq).sorted(TransactionsOrdering.InUTXPool(Set(whitelisted.toAddress.toString))) shouldBe correctSeq
  }
}
