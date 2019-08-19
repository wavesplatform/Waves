package com.wavesplatform.state

package object extensions {
  final type ApiExtensions = AddressTransactions with Distributions
  object ApiExtensions {
    val empty: ApiExtensions = ApiExtensionsImpl.fromAddressTransactionsAndDistributions(AddressTransactions.empty, Distributions.empty)
  }
}
