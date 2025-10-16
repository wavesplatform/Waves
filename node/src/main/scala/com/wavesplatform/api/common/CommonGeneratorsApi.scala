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
    private val approxGenerators = blockchain.settings.functionalitySettings.maxEndorsements // Rough buffer size

    override def generators(at: Height): Seq[GeneratorEntry] = {
      val period = blockchain.generationPeriodOf(at)

      val (addressIds, addresses, blsPks, txIds, balances) = rdb.db.readOnly { ro =>
        // TODO: Use Blockchain for this? NG.committed?
        //  Technically this works, because generators committed on a previous period
        val generatorsKey       = Keys.committedGenerators(period, at)
        val generatorsKeyPrefix = generatorsKey.keyBytes.dropRight(Ints.BYTES) // Drop height

        val addressIds = new mutable.ArrayBuffer[AddressId](approxGenerators)
        val blsPks     = new mutable.ArrayBuffer[BlsPublicKey](approxGenerators)
        val txnIds     = new mutable.ArrayBuffer[TransactionId](approxGenerators)
        ro.iterateOver(generatorsKeyPrefix) { dbEntry =>
          generatorsKey
            .parse(dbEntry.getValue)
            .getOrElse(Seq.empty)
            .foreach { (addressId, blsPk) =>
              addressIds.append(addressId)
              blsPks.append(blsPk)
            }
        }

        val txnsKey       = Keys.commitmentTransactions(period, at)
        val txnsKeyPrefix = txnsKey.keyBytes.dropRight(Ints.BYTES) // Drop height
        ro.iterateOver(txnsKeyPrefix) { dbEntry =>
          txnIds.appendAll(
            txnsKey
              .parse(dbEntry.getValue)
              .getOrElse(Seq.empty)
          )
        }

        val addresses = ro.multiGet(addressIds.map(Keys.idToAddress), Address.AddressLength)
        val balances =
          if (at == blockchain.height) blockchain.currentGeneratorBalances().map { case (_, b) => b }
          else ro.get(Keys.generatorBalances(at, rdb.apiHandle)).getOrElse(Seq.empty)

        (addressIds, addresses, blsPks, txnIds, balances)
      }

      if (
        addressIds.size == addresses.size &&
        addresses.size == balances.size &&
        balances.size == blsPks.size &&
        blsPks.size == txIds.size
      )
        addressIds
          .lazyZip(addresses)
          .lazyZip(balances)
          .lazyZip(blsPks)
          .lazyZip(txIds)
          .collect { case ((_, Some(address), balance, _), txnId) => // TODO: address=None ?
            GeneratorEntry(address, balance, txnId)
          }
          .toSeq
      else {
        log.warn(s"Different size: addressIds=${addressIds.size}, addresses=${addresses.size}, balances=${balances.size}, blsPks=${blsPks.size}")
        Seq.empty
      }
    }
  }

  case class GeneratorEntry(address: Address, balance: Long, commitTxnId: TransactionId)
}
