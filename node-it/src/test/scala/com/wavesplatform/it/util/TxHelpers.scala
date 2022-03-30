package com.wavesplatform.it.util

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.google.protobuf.ByteString
import com.wavesplatform.account.{AddressScheme, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.crypto
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.{MassTransferTransactionData, PBTransaction}
import com.wavesplatform.protobuf.utils.PBImplicitConversions._
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.serialization.Deser

import com.wavesplatform.transaction.{Asset, TxVersion}
import com.wavesplatform.transaction.transfer.MassTransferTransaction

object TxHelpers {
  def massTransferBodyBytes(sender: KeyPair,
                            assetId: Option[String],
                            transfers: Seq[MassTransferTransactionData.Transfer],
                            attachment: ByteString,
                            fee: Long,
                            timestamp: Long,
                            version: Int = 1): ByteStr = {
    val bodyBytes = version match {
      case TxVersion.V1 =>
        val transferBytes = transfers.map { t =>
          Bytes.concat(t.getRecipient.toAddressOrAlias(AddressScheme.current.chainId).explicitGet().bytes, Longs.toByteArray(t.amount))
        }

        Bytes.concat(
          Array(MassTransferTransaction.typeId, version.toByte),
          sender.publicKey.arr,
          Asset.fromString(assetId).byteRepr,
          Shorts.toByteArray(transfers.size.toShort),
          Bytes.concat(transferBytes: _*),
          Longs.toByteArray(timestamp),
          Longs.toByteArray(fee),
          Deser.serializeArrayWithLength(attachment.toByteArray)
        )

      case _ =>
        val unsigned = PBTransaction(
          AddressScheme.current.chainId,
          ByteString.copyFrom(sender.publicKey.arr),
          Some(Amount.of(ByteString.EMPTY, fee)),
          timestamp,
          version,
          PBTransaction.Data.MassTransfer(
            MassTransferTransactionData.of(
              if (assetId.isDefined) ByteString.copyFrom(Base58.decode(assetId.get)) else ByteString.EMPTY,
              transfers,
              attachment
            )
          )
        )

        PBUtils.encodeDeterministic(unsigned)
    }

    crypto.sign(sender.privateKey, bodyBytes)
  }

}
