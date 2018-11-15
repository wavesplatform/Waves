package com.wavesplatform.matcher.market

import akka.actor.ActorSystem
import akka.persistence.inmemory.extension.{InMemoryJournalStorage, InMemorySnapshotStorage, StorageExtension}
import akka.testkit.{TestKitBase, TestProbe}
import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.loadConfig
import com.wavesplatform.utils.ScorexLogging
import org.scalatest._

abstract class MatcherSpec(actorSystemName: String)
    extends TestKitBase
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with ScorexLogging {

  implicit override lazy val system: ActorSystem = ActorSystem(
    actorSystemName,
    loadConfig(ConfigFactory.empty())
  )

  override protected def beforeEach(): Unit = {
    val p = TestProbe()
    p.send(StorageExtension(system).journalStorage, InMemoryJournalStorage.ClearJournal)
    p.expectMsg(akka.actor.Status.Success(""))
    p.send(StorageExtension(system).snapshotStorage, InMemorySnapshotStorage.ClearSnapshots)
    p.expectMsg(akka.actor.Status.Success(""))
    super.beforeEach()
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    shutdown(system)
  }
}
