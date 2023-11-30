package com.wavesplatform.api.common.lease

import com.wavesplatform.account.Address
import com.wavesplatform.api.common.LeaseInfo
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{AddressId, DBExt, DBResource, Keys, RDB}
import com.wavesplatform.state.{LeaseDetails, StateSnapshot}
import monix.eval.Task
import monix.reactive.Observable

import scala.jdk.CollectionConverters.IteratorHasAsScala

object AddressLeaseInfo {
  def activeLeases(
      rdb: RDB,
      snapshot: StateSnapshot,
      subject: Address
  ): Observable[LeaseInfo] = {
    val snapshotLeases = leasesFromSnapshot(snapshot, subject)
    val dbLeases       = leasesFromDb(rdb, subject)
    Observable.fromIterable(snapshotLeases) ++ dbLeases.filterNot(info => snapshot.cancelledLeases.contains(info.id))
  }

  private def leasesFromSnapshot(snapshot: StateSnapshot, subject: Address): Seq[LeaseInfo] =
    snapshot.newLeases.collect {
      case (id, leaseStatic)
          if !snapshot.cancelledLeases.contains(id) &&
            (subject == leaseStatic.sender.toAddress || subject == leaseStatic.recipientAddress) =>
        LeaseInfo(
          id,
          leaseStatic.sourceId,
          leaseStatic.sender.toAddress,
          leaseStatic.recipientAddress,
          leaseStatic.amount.value,
          leaseStatic.height,
          LeaseInfo.Status.Active
        )
    }.toSeq

  private def leasesFromDb(rdb: RDB, subject: Address): Observable[LeaseInfo] =
    for {
      dbResource <- rdb.db.resourceObservable
      (leaseId, details) <- dbResource
        .get(Keys.addressId(subject))
        .map(fromLeaseDbIterator(dbResource, _))
        .getOrElse(Observable.empty)
    } yield LeaseInfo.fromLeaseDetails(leaseId, details)

  private def fromLeaseDbIterator(dbResource: DBResource, addressId: AddressId): Observable[(ByteStr, LeaseDetails)] =
    Observable
      .fromIterator(Task(new LeaseByAddressIterator(dbResource, addressId).asScala))
      .concatMapIterable(identity)
}
