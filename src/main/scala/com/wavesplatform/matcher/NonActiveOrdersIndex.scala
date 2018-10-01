package com.wavesplatform.matcher

import com.wavesplatform.account.Address
import com.wavesplatform.database.{RW, ReadOnlyDB}
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.assets.exchange.Order.Id

class NonActiveOrdersCommonIndex(address: Address, elementsLimit: Int) extends NonActiveOrdersIndex(elementsLimit) {

  protected def get(ro: ReadOnlyDB, idx: Index): Id   = ???
  protected def set(rw: RW, idx: Index, id: Id): Unit = ???
  protected def delete(rw: RW, idx: Index): Unit      = ???

  protected def getNewestIdx(ro: ReadOnlyDB): Option[Index] = ???
  protected def setNewestIdx(rw: RW, idx: Index): Unit      = ???

}

class NonActiveOrdersPairIndex(address: Address, pair: AssetPair, elementsLimit: Int) extends NonActiveOrdersIndex(elementsLimit) {

  protected def get(ro: ReadOnlyDB, idx: Index): Id   = ???
  protected def set(rw: RW, idx: Index, id: Id): Unit = ???
  protected def delete(rw: RW, idx: Index): Unit      = ???

  protected def getNewestIdx(ro: ReadOnlyDB): Option[Index] = ???
  protected def setNewestIdx(rw: RW, idx: Index): Unit      = ???

}

abstract class NonActiveOrdersIndex(elementsLimit: Int) {

  type Index = Int

  def add(rw: RW, id: Id): Unit = {
    val ro               = rw.ro
    val newestIdx        = getNewestIdx(ro)
    val updatedNewestIdx = newestIdx.getOrElse(0) + 1

    set(rw, updatedNewestIdx, id)
    if (updatedNewestIdx > elementsLimit) delete(rw, updatedNewestIdx - elementsLimit)

    setNewestIdx(rw, updatedNewestIdx)
  }

  protected def get(ro: ReadOnlyDB, idx: Index): Id
  protected def set(rw: RW, idx: Index, id: Id): Unit
  protected def delete(rw: RW, idx: Index): Unit

  protected def getNewestIdx(ro: ReadOnlyDB): Option[Index]
  protected def setNewestIdx(rw: RW, idx: Index): Unit

  def iterator(ro: ReadOnlyDB): Iterator[Id] = getNewestIdx(ro).fold(Iterator.empty: Iterator[Id]) { newestIdx =>
    (newestIdx to 1 by -1).iterator.take(elementsLimit).map(get(ro, _))
  }

}
