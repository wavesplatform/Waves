package com.wavesplatform.state.extensions

import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.state.{Blockchain, BlockchainUpdaterImpl}

trait BlockchainExtensions extends AddressTransactions.Prov[Blockchain] with Distributions.Prov[Blockchain] {
  override def addressTransactions(value: Blockchain): AddressTransactions = value match {
    case ldb: LevelDBWriter        => LevelDBWriter.addressTransactions(ldb)
    case bu: BlockchainUpdaterImpl => BlockchainUpdaterImpl.addressTransactions(bu)
    case c: CompositeBlockchain    => CompositeBlockchain.addressTransactions(c)
    case _                         => AddressTransactions.Empty
  }

  override def distributions(value: Blockchain): Distributions = value match {
    case ldb: LevelDBWriter        => LevelDBWriter.distributions(ldb)
    case bu: BlockchainUpdaterImpl => BlockchainUpdaterImpl.distributions(bu)
    case c: CompositeBlockchain    => CompositeBlockchain.distributions(c)
    case _                         => Distributions.Empty
  }
}
