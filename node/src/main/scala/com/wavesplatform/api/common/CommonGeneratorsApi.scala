package com.wavesplatform.api.common

import com.google.common.primitives.Longs
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonGeneratorsApi.GeneratorEntry
import com.wavesplatform.database.{AddressId, DBExt, Keys, RDB}
import com.wavesplatform.state.{Blockchain, Height, TransactionId}

trait CommonGeneratorsApi {
  def generators(at: Height): Seq[GeneratorEntry]
}

object CommonGeneratorsApi {
  def apply(rdb: RDB, blockchain: Blockchain): CommonGeneratorsApi = new CommonGeneratorsApi {
    override def generators(at: Height): Seq[GeneratorEntry] = {
      val period = blockchain.generationPeriodOf(at)
      val (committed, addresses, rawBalances) = rdb.db.readOnly { ro =>
        val committed   = ro.get(Keys.committedGenerators(period, at))
        val rawBalances = ro.get(Keys.generatorBalances(at, rdb.apiHandle))

        val addressKeys = rawBalances.keys.map(Keys.idToAddress).toIndexedSeq
        val addresses   = ro.multiGet(addressKeys, Longs.BYTES)

        (committed, addresses, rawBalances)
      }

      val committedMap = committed.map((addressId, _, txnId) => addressId -> txnId).toMap

      addresses
        .zip(rawBalances)
        .collect { case (Some(address), (addressId, balance)) =>
          val txnId = committedMap.getOrElse(
            addressId,
            throw new RuntimeException(s"Can't find committed generator transaction for addressId: $addressId")
          )
          GeneratorEntry(address, balance, txnId)
        }
    }
  }

  case class GeneratorEntry(address: Address, balance: Long, commitTxnId: TransactionId)
}
