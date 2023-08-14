package com.wavesplatform

import com.wavesplatform.account.{AddressOrAlias, KeyPair}
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.history.DefaultBaseTarget
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.state.StringDataEntry
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.utils.Signed
import com.wavesplatform.transaction.{DataTransaction, Transaction, TxVersion}
import org.scalacheck.Gen

trait BlocksTransactionsHelpers { self: TransactionGen =>
  object QuickTX {
    val FeeAmount = 400000

    def transfer(
        from: KeyPair,
        to: AddressOrAlias = accountGen.sample.get.toAddress,
        amount: Long = smallFeeGen.sample.get,
        timestamp: Gen[Long] = timestampGen
    ): Gen[Transaction] =
      for {
        timestamp <- timestamp
      } yield TransferTransaction.selfSigned(1.toByte, from, to, Waves, amount, Waves, FeeAmount, ByteStr.empty, timestamp).explicitGet()

    def transferV2(
        from: KeyPair,
        to: AddressOrAlias = accountGen.sample.get.toAddress,
        amount: Long = smallFeeGen.sample.get,
        timestamp: Gen[Long] = timestampGen
    ): Gen[Transaction] =
      for {
        timestamp <- timestamp
      } yield TransferTransaction.selfSigned(2.toByte, from, to, Waves, amount, Waves, FeeAmount, ByteStr.empty, timestamp).explicitGet()

    def transferAsset(
        asset: IssuedAsset,
        from: KeyPair,
        to: AddressOrAlias = accountGen.sample.get.toAddress,
        amount: Long = smallFeeGen.sample.get,
        timestamp: Gen[Long] = timestampGen
    ): Gen[Transaction] =
      for {
        timestamp <- timestamp
      } yield TransferTransaction.selfSigned(1.toByte, from, to, asset, amount, Waves, FeeAmount, ByteStr.empty, timestamp).explicitGet()

    def lease(
        from: KeyPair,
        to: AddressOrAlias = accountGen.sample.get.toAddress,
        amount: Long = smallFeeGen.sample.get,
        timestamp: Gen[Long] = timestampGen
    ): Gen[LeaseTransaction] =
      for {
        timestamp <- timestamp
      } yield LeaseTransaction.selfSigned(1.toByte, from, to, amount, FeeAmount, timestamp).explicitGet()

    def leaseCancel(from: KeyPair, leaseId: ByteStr, timestamp: Gen[Long] = timestampGen): Gen[LeaseCancelTransaction] =
      for {
        timestamp <- timestamp
      } yield LeaseCancelTransaction.selfSigned(1.toByte, from, leaseId, FeeAmount, timestamp).explicitGet()

    def data(from: KeyPair, dataKey: String, timestamp: Gen[Long] = timestampGen): Gen[DataTransaction] =
      for {
        timestamp <- timestamp
      } yield DataTransaction.selfSigned(1.toByte, from, List(StringDataEntry(dataKey, Gen.numStr.sample.get)), FeeAmount, timestamp).explicitGet()

    def nftIssue(from: KeyPair, timestamp: Gen[Long] = timestampGen): Gen[IssueTransaction] =
      for {
        timestamp <- timestamp
      } yield IssueTransaction
        .selfSigned(
          TxVersion.V1,
          from,
          "test",
          "",
          1,
          0,
          reissuable = false,
          script = None,
          100000000L,
          timestamp
        )
        .explicitGet()

    def setScript(from: KeyPair, script: Script, timestamp: Gen[Long] = timestampGen): Gen[SetScriptTransaction] =
      for {
        timestamp <- timestamp
      } yield SetScriptTransaction.selfSigned(1.toByte, from, Some(script), FeeAmount, timestamp).explicitGet()

    def invokeScript(
        from: KeyPair,
        dapp: AddressOrAlias,
        call: FUNCTION_CALL,
        payments: Seq[InvokeScriptTransaction.Payment] = Nil,
        timestamp: Gen[Long] = timestampGen
    ): Gen[InvokeScriptTransaction] =
      for {
        timestamp <- timestamp
      } yield Signed.invokeScript(1.toByte, from, dapp, Some(call), payments, FeeAmount * 2, Waves, timestamp)
  }

  object UnsafeBlocks {
    def unsafeChainBaseAndMicro(
        totalRefTo: ByteStr,
        base: Seq[Transaction],
        micros: Seq[Seq[Transaction]],
        signer: KeyPair,
        version: Byte,
        timestamp: Long
    ): (Block, Seq[MicroBlock]) = {
      val block = unsafeBlock(totalRefTo, base, signer, version, timestamp)
      val microBlocks = micros
        .foldLeft((block, Seq.empty[MicroBlock])) { case ((lastTotal, allMicros), txs) =>
          val (newTotal, micro) = unsafeMicro(totalRefTo, lastTotal, txs, signer, version, timestamp)
          (newTotal, allMicros :+ micro)
        }
        ._2
      (block, microBlocks)
    }

    def unsafeMicro(
        totalRefTo: ByteStr,
        prevTotal: Block,
        txs: Seq[Transaction],
        signer: KeyPair,
        version: TxVersion,
        ts: Long
    ): (Block, MicroBlock) = {
      val newTotalBlock = unsafeBlock(totalRefTo, prevTotal.transactionData ++ txs, signer, version, ts)
      (newTotalBlock, MicroBlock.buildAndSign(version, signer, txs, prevTotal.id(), newTotalBlock.signature, None).explicitGet())
    }

    def unsafeBlock(
        reference: ByteStr,
        txs: Seq[Transaction],
        signer: KeyPair,
        version: Byte,
        timestamp: Long,
        bTarget: Long = DefaultBaseTarget
    ): Block = {
      val unsigned: Block = Block.create(
        version = version,
        timestamp = timestamp,
        reference = reference,
        baseTarget = bTarget,
        generationSignature = com.wavesplatform.history.generationSignature,
        generator = signer.publicKey,
        featureVotes = Seq.empty,
        rewardVote = -1L,
        transactionData = txs,
        stateHash = None,
        challengedHeader = None
      )
      val toSign =
        if (version < Block.ProtoBlockVersion) unsigned.bytes()
        else PBBlocks.protobuf(unsigned).header.get.toByteArray
      unsigned.copy(signature = crypto.sign(signer.privateKey, toSign))
    }
  }
}
