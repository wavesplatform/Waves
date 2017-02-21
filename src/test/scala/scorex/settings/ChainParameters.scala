package scorex.settings

import scorex.account.{Account, AddressScheme}
import scorex.transaction.{GenesisTransaction, Transaction}

object TestChainParameters {
  trait GenesisData { this: ChainParameters =>
    override def genesisTxs: Seq[Transaction] = {
      val ipoMembers = List(
        "3N3rfWUDPkFsf2GEZBCLw491A79G46djvQk",
        "3N3keodUiS8WLEw9W4BKDNxgNdUpwSnpb3K",
        "3N6dsnfD88j5yKgpnEavaaJDzAVSRBRVbMY"
      )

      val timestamp = 0L

      val txs = ipoMembers.map { addr =>
        val recipient = new Account(addr)
        GenesisTransaction.create(recipient, initialBalance / ipoMembers.length, timestamp)
      }.map(_.right.get)

      txs
    }
  }

  val Disabled = new ChainParameters with GenesisData {
    override def allowTemporaryNegativeUntil: Long = Long.MaxValue
    override def requireSortedTransactionsAfter: Long = Long.MaxValue
    override def allowInvalidPaymentTransactionsByTimestamp: Long = Long.MaxValue
    override def generatingBalanceDepthFrom50To1000AfterHeight: Long = Long.MaxValue
    override def minimalGeneratingBalanceAfterTimestamp: Long = Long.MaxValue
    override def allowTransactionsFromFutureUntil: Long = Long.MaxValue
    override def allowUnissuedAssetsUntil: Long = Long.MaxValue
    override def allowBurnTransactionAfterTimestamp: Long = Long.MaxValue
    override def requirePaymentUniqueId: Long = Long.MaxValue
    override def allowLeaseTransactionAfterTimestamp: Long = Long.MaxValue

    override def initialBalance: Long = 100000000000000L

    override def genesisTimestamp: Long = ???

    override def addressScheme: AddressScheme = ???

    override def allowExchangeTransactionAfterTimestamp: Long = Long.MaxValue
  }

  val Enabled = new ChainParameters with GenesisData {
    override def allowTemporaryNegativeUntil: Long = 0
    override def requireSortedTransactionsAfter: Long = 0
    override def allowInvalidPaymentTransactionsByTimestamp: Long = 0
    override def generatingBalanceDepthFrom50To1000AfterHeight: Long = 0
    override def minimalGeneratingBalanceAfterTimestamp: Long = 0
    override def allowTransactionsFromFutureUntil: Long = 0
    override def allowUnissuedAssetsUntil: Long = 0
    override def allowBurnTransactionAfterTimestamp: Long = 0
    override def requirePaymentUniqueId: Long = 0
    override def allowLeaseTransactionAfterTimestamp: Long = 0

    override def initialBalance: Long = ???

    override def genesisTimestamp: Long = ???

    override def addressScheme: AddressScheme = ???

    override def allowExchangeTransactionAfterTimestamp: Long = 0L
  }
}
