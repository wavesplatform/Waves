package com.wavesplatform.api.common

import com.google.common.primitives.Ints
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonGeneratorsApi.GeneratorEntry
import com.wavesplatform.crypto.bls.BlsPublicKey
import com.wavesplatform.database.{AddressId, DBExt, Keys, RDB}
import com.wavesplatform.state.{Blockchain, Height, NG, TransactionId}
import com.wavesplatform.utils.ScorexLogging

import scala.collection.mutable

trait CommonGeneratorsApi {
  def generators(at: Height): Seq[GeneratorEntry]
}

object CommonGeneratorsApi {
  def apply(rdb: RDB, blockchain: Blockchain & NG): CommonGeneratorsApi = new CommonGeneratorsApi with ScorexLogging {
    private val maxGenerators = blockchain.settings.functionalitySettings.maxGenerators

    override def generators(at: Height): Seq[GeneratorEntry] = {
      val period = blockchain.generationPeriodOf(at)

      val (addressIds, addresses, blsPks, txIds, balances) = rdb.db.readOnly { ro =>
        // TODO: Use Blockchain for this? NG.committed?
        //  Technically this works, because generators committed on a previous period
        val key       = Keys.committedGenerators(period, at)
        val keyPrefix = key.keyBytes.dropRight(Ints.BYTES) // Drop height

        val addressIds = new mutable.ArrayBuffer[AddressId](maxGenerators)
        val blsPks     = new mutable.ArrayBuffer[BlsPublicKey](maxGenerators)
        val txnIds     = new mutable.ArrayBuffer[TransactionId](maxGenerators)
        ro.iterateOver(keyPrefix) { dbEntry =>
          key
            .parse(dbEntry.getValue)
            .getOrElse(Seq.empty)
            .foreach { (addressId, blsPk, txnId) =>
              addressIds.append(addressId)
              blsPks.append(blsPk)
              txnIds.append(txnId)
            }
        }

        val addresses = ro.multiGet(addressIds.map(Keys.idToAddress), Address.AddressLength)
        val balances =
          if (at == blockchain.height) blockchain.currentGeneratorBalances()
          else if (at == blockchain.height - 1) blockchain.parentGeneratorBalances()
          else ro.get(Keys.generatorBalances(at, rdb.apiHandle)).getOrElse(Seq.empty)

        (addressIds, addresses, blsPks, txnIds, balances)
      }

      addressIds
        .lazyZip(addresses)
        .lazyZip(balances)
        .lazyZip(blsPks)
        .lazyZip(txIds)
        .collect { case ((_, Some(address), balance, _), txnId) => // TODO: address=None ?
          GeneratorEntry(address, balance, txnId)
        }
        .toSeq
    }
  }

  case class GeneratorEntry(address: Address, balance: Long, commitTxnId: TransactionId)
}
