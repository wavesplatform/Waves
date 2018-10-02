package com.wavesplatform.matcher

import com.wavesplatform.account.Address
import com.wavesplatform.database.{Key, RW, ReadOnlyDB}
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.assets.exchange.Order.Id
import com.wavesplatform.utils.Base58

class FinalizedOrdersCommonIndex(address: Address, elementsLimit: Int) extends FinalizedOrdersIndex(elementsLimit) {
  override protected def itemKey(idx: Index): Key[Option[Id]] = MatcherKeys.finalizedCommon(address, idx)
  override protected def newestKey: Key[Option[Index]]        = MatcherKeys.finalizedCommonSeqNr(address)
}

class FinalizedOrdersPairIndex(address: Address, pair: AssetPair, elementsLimit: Int) extends FinalizedOrdersIndex(elementsLimit) {
  override protected def itemKey(idx: Index): Key[Option[Id]] = MatcherKeys.finalizedPair(address, pair, idx)
  override protected def newestKey: Key[Option[Index]]        = MatcherKeys.finalizedPairSeqNr(address, pair)
}

abstract class FinalizedOrdersIndex(elementsLimit: Int) {
  type Index = Int

  def add(rw: RW, xs: Id*): Unit = if (xs.nonEmpty) {
    println(s"${getClass.getSimpleName}: add: ${xs.mkString(", ")}")

    val newestIdx = rw.get(newestKey).getOrElse(0)
    xs.zipWithIndex.foreach {
      case (id, offset) =>
        val idx = newestIdx + 1 + offset
        println(s"rw.put(itemKey($idx), $id)")
        rw.put(itemKey(idx), Some(id))
    }

    val size             = xs.size
    val updatedNewestIdx = newestIdx + size

    // WRONG!!!
    println(
      s"(updatedNewestIdx to 1).slice(elementsLimit, elementsLimit + size): ${(updatedNewestIdx to 1).slice(elementsLimit, elementsLimit + size).mkString(", ")}")
    (updatedNewestIdx to 1).slice(elementsLimit, elementsLimit + size).foreach { idx =>
      rw.delete(itemKey(idx))
      println(s"rw.delete(itemKey($idx))")
    }

    rw.put(newestKey, Some(updatedNewestIdx))
    println(s"rw.put(newestKey, Some(${newestIdx + size}))")
  }

  def iterator(ro: ReadOnlyDB): Iterator[Id] = {
    val newestIdx = ro.get(newestKey)
    println(s"${getClass.getSimpleName}: iterator: newestIdx: $newestIdx")
    newestIdx.fold(Iterator.empty: Iterator[Id]) { newestIdx =>
      val xs = (newestIdx to 1 by -1).take(elementsLimit)
      val r  = xs.flatMap(idx => ro.get(itemKey(idx)))
      print(s"xs: ${xs.mkString(", ")}, ")
      println(s"r (size=${r.size}): ${r.flatMap(x => Option(x.arr).map(Base58.encode)).mkString(", ")}")
      r.iterator
    }
  }

  protected def itemKey(idx: Index): Key[Option[Id]]
  protected def newestKey: Key[Option[Index]]
}
