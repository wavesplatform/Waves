package com.wavesplatform.api.common.lease

import com.google.common.collect.AbstractIterator
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database
import com.wavesplatform.database.{AddressId, DBResource, Keys}
import com.wavesplatform.state.LeaseDetails

import scala.collection.mutable

private class LeaseByAddressIterator(resource: DBResource, addressId: AddressId) extends AbstractIterator[Seq[(ByteStr, LeaseDetails)]] {
  private val seqNr = resource.get(Keys.addressLeaseSeqNr(addressId))
  resource.withSafePrefixIterator(_.seekForPrev(Keys.addressLeaseSeq(addressId, seqNr).keyBytes))()

  final override def computeNext(): Seq[(ByteStr, LeaseDetails)] =
    resource.withSafePrefixIterator { iterator =>
      val buffer = mutable.Map[ByteStr, LeaseDetails]()
      while (iterator.isValid) {
        for {
          id      <- database.readLeaseIdSeq(iterator.value())
          details <- database.loadLease(resource, id) if details.isActive
        } buffer.update(id, details)
        iterator.prev()
      }
      if (buffer.nonEmpty)
        buffer.toSeq
      else
        endOfData()
    }(
      endOfData()
    )
}
