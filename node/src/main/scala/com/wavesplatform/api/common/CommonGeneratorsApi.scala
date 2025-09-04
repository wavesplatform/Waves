package com.wavesplatform.api.common

import cats.syntax.either.*
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
          if (at == blockchain.height) blockchain.currentGeneratorBalances().asRight
          else if (at == blockchain.height - 1) blockchain.parentGeneratorBalances().asRight
          else ro.get(Keys.generatorBalances(at, rdb.apiHandle)).getOrElse(Map.empty).asLeft

        (addressIds, addresses, blsPks, txnIds, balances)
      }

      addressIds
        .lazyZip(addresses)
        .lazyZip(blsPks)
        .lazyZip(txIds)
        .collect { case (addressId, Some(address), blsPk, txnId) =>
          val mayBeBalance = balances match {
            case Left(balances)  => balances.get(addressId)
            case Right(balances) => balances.get(blsPk)
          }
          val balance = mayBeBalance.getOrElse {
            log.warn(s"Can't find balance for addressId=$addressId, address=$address (commitment tx id=$txnId)")
            0L
          }
          GeneratorEntry(address, balance, txnId)
        }
        .toSeq
    }
  }

  case class GeneratorEntry(address: Address, balance: Long, commitTxnId: TransactionId)
}
