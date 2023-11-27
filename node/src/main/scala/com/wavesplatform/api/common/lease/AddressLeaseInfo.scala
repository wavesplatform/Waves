package com.wavesplatform.api.common.lease

import com.wavesplatform.account.Address
import com.wavesplatform.api.common.LeaseInfo
import com.wavesplatform.api.common.LeaseInfo.Status.Active
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{AddressId, DBExt, DBResource, Keys, RDB}
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{Blockchain, StateSnapshot}
import monix.eval.Task
import monix.reactive.Observable

import scala.jdk.CollectionConverters.IteratorHasAsScala

object AddressLeaseInfo {
  def activeLeases(
      rdb: RDB,
      snapshot: StateSnapshot,
      blockchain: Blockchain,
      subject: Address
  ): Observable[LeaseInfo] = {
    val snapshotLeases = leasesFromSnapshot(snapshot, blockchain, subject)
    val dbLeases       = leasesFromDb(rdb, blockchain, subject)
    (Observable.fromIterable(snapshotLeases) ++ dbLeases.filterNot(info => snapshotLeases.exists(_.id == info.id)))
      .filter(_.status == Active)
  }

  private def leasesFromSnapshot(snapshot: StateSnapshot, blockchain: Blockchain, subject: Address): Seq[LeaseInfo] =
    snapshot.leaseStates.collect {
      case (id, leaseSnapshot)
          if subject == leaseSnapshot.sender.toAddress || blockchain.resolveAlias(leaseSnapshot.recipient).exists(subject == _) =>
        LeaseInfo.fromLeaseDetails(id, leaseSnapshot.toDetails(blockchain, None, blockchain.leaseDetails(id)), blockchain)
    }.toSeq

  private def leasesFromDb(rdb: RDB, blockchain: Blockchain, subject: Address): Observable[LeaseInfo] =
    for {
      dbResource <- rdb.db.resourceObservable
      (leaseId, details) <- dbResource
        .get(Keys.addressId(subject))
        .map(fromLeaseDbIterator(dbResource, _))
        .getOrElse(Observable.empty)
    } yield LeaseInfo.fromLeaseDetails(leaseId, details, blockchain)

  private def fromLeaseDbIterator(dbResource: DBResource, addressId: AddressId): Observable[(ByteStr, LeaseDetails)] =
    Observable
      .fromIterator(Task(new LeaseByAddressIterator(dbResource, addressId).asScala))
      .concatMapIterable(identity)
}
