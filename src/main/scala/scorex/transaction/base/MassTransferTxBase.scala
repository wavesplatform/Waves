package scorex.transaction.base

import scorex.account.PublicKeyAccount
import scorex.transaction.AssetId
import scorex.transaction.assets.MassTransferTransaction.ParsedTransfer

trait MassTransferTxBase extends TxBase {
  def assetId: Option[AssetId]
  def sender: PublicKeyAccount
  def transfers: List[ParsedTransfer]
}
