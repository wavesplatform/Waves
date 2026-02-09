package com.wavesplatform.api.common

import com.google.common.primitives.Ints
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonGeneratorsApi.GeneratorEntry
import com.wavesplatform.crypto.bls.BlsPublicKey
import com.wavesplatform.database.{AddressId, DBExt, Keys, RDB}
import com.wavesplatform.state.{Blockchain, ConflictGenerators, GeneratorIndex, Height, NG, TransactionId}
import com.wavesplatform.utils.ScorexLogging

import scala.collection.mutable

trait CommonGeneratorsApi {
  def generators(at: Height): Seq[GeneratorEntry]
}

object CommonGeneratorsApi {
  def apply(rdb: RDB, blockchain: Blockchain & NG): CommonGeneratorsApi = new CommonGeneratorsApi with ScorexLogging {
    private val approxGenerators = blockchain.settings.functionalitySettings.maxValidEndorsers // Rough buffer size

    /** @note Doesn't work correctly for future heights
      */
    override def generators(at: Height): Seq[GeneratorEntry] = blockchain.generationPeriodOf(at).fold(Nil) { period =>
      val (addressIds, addresses, blsPks, txIds, balances, conflict) = rdb.db.readOnly { ro =>
        // This works even with NG, because generators committed on a previous period
        val committedKey       = Keys.committedGenerators(period, at)
        val committedKeyPrefix = committedKey.keyBytes.dropRight(Ints.BYTES) // Drop height

        val addressIds = new mutable.ArrayBuffer[AddressId](approxGenerators)
        val blsPks     = new mutable.ArrayBuffer[BlsPublicKey](approxGenerators)
        val txnIds     = new mutable.ArrayBuffer[TransactionId](approxGenerators)
        ro.iterateOver(committedKeyPrefix) { dbEntry =>
          committedKey
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
          txnIds.appendAll(txnsKey.parse(dbEntry.getValue))
        }

        val addresses = ro.multiGet(addressIds.map(Keys.idToAddress), Address.AddressLength)
        val balances =
          if (at.toInt == blockchain.height) blockchain.currentGeneratorSet.fold(Map.empty)(_.map(x => x.index -> x.balance).toMap)
          else {
            // TODO: fill with None if disabled
            val fromRdb = ro.get(Keys.generatorBalances(at, rdb.apiHandle)).getOrElse(Seq.empty)
            fromRdb.toMap
          }

        val conflictKey       = Keys.conflictGenerators(period, at)
        val conflictKeyPrefix = conflictKey.keyBytes.dropRight(Ints.BYTES) // Drop height

        val conflict = {
          if (at == Height(blockchain.height)) blockchain.conflictGenerators(period)
          else {
            var conflict = ConflictGenerators.empty
            ro.iterateOverWithSeek(conflictKeyPrefix, conflictKeyPrefix) { dbEntry =>
              val hBytes = dbEntry.getKey.takeRight(Ints.BYTES) // Take height
              val h      = Height(Ints.fromByteArray(hBytes))
              if (h > at) false
              else {
                val idxs = conflictKey.parse(dbEntry.getValue)
                conflict = conflict.appendAll(h, idxs*)
                true
              }
            }

            conflict
          }
        }

        (addressIds, addresses, blsPks, txnIds, balances, conflict)
      }

      if (
        addressIds.size == addresses.size &&
        addresses.size == blsPks.size &&
        blsPks.size == txIds.size
      ) {
        addressIds
          .lazyZip(addresses)
          .lazyZip(txIds)
          .lazyZip(Iterator.from(0).take(addressIds.size).map(GeneratorIndex(_)).to(Iterable))
          .collect { case (_, Some(address), txnId, idx) => // TODO: address=None ?
            GeneratorEntry(address, balances.getOrElse(idx, 0L), txnId, conflict.heightOf(idx))
          }
          .toSeq
      } else {
        log.warn(s"Different size: addressIds=${addressIds.size}, addresses=${addresses.size}, balances=${balances.size}, blsPks=${blsPks.size}")
        Seq.empty
      }
    }
  }

  case class GeneratorEntry(address: Address, balance: Long, commitTxnId: TransactionId, conflictHeight: Option[Height])
}
