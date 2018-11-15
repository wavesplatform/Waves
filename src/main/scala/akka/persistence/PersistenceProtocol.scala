package akka.persistence

object PersistenceProtocol {
  def mkSaveSnapshot[T](metadata: SnapshotMetadata, snapshot: T)                   = SnapshotProtocol.SaveSnapshot(metadata, snapshot)
  def mkDeleteSnapshot(persistenceId: String, criteria: SnapshotSelectionCriteria) = SnapshotProtocol.DeleteSnapshots(persistenceId, criteria)
}
