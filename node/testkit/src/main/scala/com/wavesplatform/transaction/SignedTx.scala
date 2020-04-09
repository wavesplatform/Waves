package com.wavesplatform.transaction

import com.wavesplatform.account._
import com.wavesplatform.transaction.transfer.{Attachment, TransferTransaction}

object SignedTx {
  def transfer(
      version: TxVersion,
      sender: PublicKey,
      recipient: AddressOrAlias,
      asset: Asset,
      amount: TxAmount,
      feeAsset: Asset,
      fee: TxAmount,
      attachment: Option[Attachment],
      timestamp: TxTimestamp,
      signer: PrivateKey
  ): TransferTransaction =
    TransferTransaction(version, sender, recipient, asset, amount, feeAsset, fee, attachment, timestamp, Proofs.empty, AddressScheme.current.chainId)
      .signWith(signer)

  def transfer(
      version: TxVersion,
      sender: KeyPair,
      recipient: AddressOrAlias,
      asset: Asset,
      amount: TxAmount,
      feeAsset: Asset,
      fee: TxAmount,
      attachment: Option[Attachment],
      timestamp: TxTimestamp
  ): TransferTransaction =
    transfer(version, sender.publicKey, recipient, asset, amount, feeAsset, fee, attachment, timestamp, sender.privateKey)
}
