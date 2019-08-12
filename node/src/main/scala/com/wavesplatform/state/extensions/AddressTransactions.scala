package com.wavesplatform.state.extensions

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.Height
import com.wavesplatform.transaction._
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.concurrent.duration.Duration

trait AddressTransactions {
  def addressTransactionsObservable(address: Address,
                                    types: Set[TransactionParser],
                                    fromId: Option[ByteStr] = None): Observable[(Height, Transaction)]
}

object AddressTransactions {
  val empty: AddressTransactions = (_: Address, _: Set[TransactionParser], _: Option[BlockId]) => Observable.empty

  implicit class AddressTransactionsExt(private val provider: AddressTransactions) extends AnyVal {
    def aliasesOfAddress(address: Address)(implicit sc: Scheduler): Seq[Alias] = {
      provider
        .addressTransactionsObservable(address, Set(CreateAliasTransactionV1, CreateAliasTransactionV2), None)
        .collect {
          case (_, a: CreateAliasTransaction) => a.alias
        }
        .toListL
        .runSyncUnsafe(Duration.Inf)
    }
  }

}
