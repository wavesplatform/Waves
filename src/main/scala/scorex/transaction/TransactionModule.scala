package scorex.transaction

import scorex.network.ConnectedPeer

trait TransactionModule {
  def onNewOffchainTransaction[T <: Transaction](transaction: T, exceptOf: Option[ConnectedPeer] = None): Either[ValidationError, T]
}

