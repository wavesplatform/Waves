package com.wavesplatform.state

package object extensions {
  final type ApiExtensions = AddressTransactions with Distributions with AccountAggregations
  object ApiExtensions {
    val empty: ApiExtensions = ApiExtensionsImpl.compose(AddressTransactions.empty, Distributions.empty, AccountAggregations.empty)
  }
}
