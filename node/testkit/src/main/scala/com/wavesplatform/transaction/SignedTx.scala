package com.wavesplatform.transaction

import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.transaction.transfer.TransferTransaction

object SignedTx {
  def payment(sender: KeyPair, recipient: Address, amount: Long, fee: Long, timestamp: Long): PaymentTransaction = {
    val pt = PaymentTransaction(sender.publicKey, recipient, amount, fee, timestamp, ByteStr.empty, recipient.chainId)
    pt.copy(signature = crypto.sign(sender.privateKey, pt.bodyBytes()))
  }

  def transfer(
      version: TxVersion,
      sender: PublicKey,
      recipient: AddressOrAlias,
      asset: Asset,
      amount: TxAmount,
      feeAsset: Asset,
      fee: TxAmount,
      attachment: ByteStr,
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
      attachment: ByteStr,
      timestamp: TxTimestamp
  ): TransferTransaction =
    transfer(version, sender.publicKey, recipient, asset, amount, feeAsset, fee, attachment, timestamp, sender.privateKey)
}
