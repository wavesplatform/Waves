package com.wavesplatform.matcher.db

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.DBExt
import com.wavesplatform.matcher.MatcherKeys
import com.wavesplatform.transaction.Asset.IssuedAsset
import org.iq80.leveldb.DB

import scala.collection.mutable.ListBuffer

trait RateDB {

  def upsertRate(asset: IssuedAsset, value: Double): Unit

  def getAllRates: Map[IssuedAsset, Double]

  def deleteRate(asset: IssuedAsset): Unit
}

object RateDB {

  def apply(db: DB): RateDB = new RateDB {

    def upsertRate(asset: IssuedAsset, value: Double): Unit = db.readWrite { _.put(MatcherKeys.rate(asset), value) }

    def getAllRates: Map[IssuedAsset, Double] = {

      val ratesListBuffer = ListBuffer[(IssuedAsset, Double)]()

      db.iterateOver(MatcherKeys.ratePrefix) { dbEntry =>
        val asset = IssuedAsset(ByteStr(dbEntry.getKey.drop(2)))
        val value = MatcherKeys.rate(asset).parse(dbEntry.getValue)
        ratesListBuffer.append(asset -> value)
      }

      ratesListBuffer.toMap
    }

    def deleteRate(asset: IssuedAsset): Unit = db.readWrite { _.delete(MatcherKeys.rate(asset)) }
  }
}
