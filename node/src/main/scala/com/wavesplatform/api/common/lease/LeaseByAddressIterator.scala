package com.wavesplatform.api.common.lease

import com.google.common.collect.AbstractIterator
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database
import com.wavesplatform.database.{AddressId, DBResource, Keys, readLeaseIdSeq}
import com.wavesplatform.state.reader.LeaseDetails

import scala.collection.mutable

private class LeaseByAddressIterator(db: DBResource, addressId: AddressId) extends AbstractIterator[Seq[(ByteStr, LeaseDetails)]] {
  private val seqNr = db.get(Keys.addressLeaseSeqNr(addressId))
  db.withSafePrefixIterator(_.seekForPrev(Keys.addressLeaseSeq(addressId, seqNr).keyBytes))()

  final override def computeNext(): Seq[(ByteStr, LeaseDetails)] =
    db.withSafePrefixIterator { dbIterator =>
      val buffer = mutable.Map[ByteStr, LeaseDetails]()
      while (dbIterator.isValid) {
        for {
          id      <- readLeaseIdSeq(dbIterator.value())
          details <- loadLease(id)
        } buffer.updateWith(id)(_.fold(Option(details))(oldDetails => if (oldDetails.isActive) Some(details) else None))
        dbIterator.prev()
      }
      if (buffer.nonEmpty)
        buffer.toSeq
      else
        endOfData()
    }(
      endOfData()
    )

  private def loadLease(id: ByteStr): Option[LeaseDetails] =
    database
      .fromHistory(db, Keys.leaseDetailsHistory(id), Keys.leaseDetails(id))
      .flatten
      .flatMap(_.toOption)
}
