package com.wavesplatform.matcher

import akka.actor.ActorSystem
import akka.persistence.inmemory.extension.{InMemorySnapshotStorage, StorageExtension}
import akka.persistence.serialization.Snapshot
import akka.serialization.SerializationExtension
import akka.testkit.TestProbe

object SnapshotUtils {
  def provideSnapshot(actorName: String, snapshot: Snapshot)(implicit system: ActorSystem): Unit = {
    val snapshotBytes = SerializationExtension(system).serialize(snapshot).get
    val p             = TestProbe()
    p.send(
      StorageExtension(system).snapshotStorage,
      InMemorySnapshotStorage.Save(actorName, 0, 0, snapshotBytes)
    )
    p.expectMsg(akka.actor.Status.Success(""))
  }
}
