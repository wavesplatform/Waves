package com.wavesplatform.matcher

import java.io._
import java.net.{URLDecoder, URLEncoder}
import java.nio.ByteBuffer

import akka.persistence._
import akka.persistence.serialization._
import akka.persistence.snapshot.SnapshotStore
import akka.serialization.SerializationExtension
import com.google.common.base.Charsets.UTF_8
import com.google.common.primitives.{Bytes, Ints}
import com.typesafe.config.Config
import com.wavesplatform.database._
import com.wavesplatform.db.openDB
import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.ReadOptions

import scala.concurrent.Future
import scala.util._
import scala.util.control.NonFatal

class MatcherSnapshotStore(config: Config) extends SnapshotStore {
  import MatcherSnapshotStore._

  private val streamDispatcher = context.system.dispatchers.lookup(config.getString("stream-dispatcher"))

  private val serializationExtension = SerializationExtension(context.system)

  private val writableDB = openDB(config.getString("leveldb-dir"))

  private def readOnly[A](f: ReadOnlyDB => A): A = {
    val s = writableDB.getSnapshot
    try f(new ReadOnlyDB(writableDB, new ReadOptions().snapshot(s)))
    finally s.close()
  }

  private def readWrite[A](f: RW => A): A = {
    val rw = new RW(writableDB)
    try f(rw)
    finally rw.close()
  }

  private var pendingActions = Map.empty[String, Future[_]]

  private def nextAction[A](persistenceId: String, f: => A): Future[A] = {
    val newAction = pendingActions
      .getOrElse(persistenceId, Future.unit)
      .transform { _ =>
        Try(f)
      }(streamDispatcher)
    pendingActions += persistenceId -> newAction
    newAction
  }

  override def loadAsync(persistenceId: String, criteria: SnapshotSelectionCriteria): Future[Option[SelectedSnapshot]] =
    nextAction(
      persistenceId,
      readOnly { db =>
        db.get(kSMHistory(persistenceId))
          .find(seqNr => db.get(kSM(persistenceId, seqNr)).matches(criteria))
          .map { seqNr =>
            val sm = db.get(kSM(persistenceId, seqNr))
            SelectedSnapshot(SnapshotMetadata(persistenceId, sm.seqNr, sm.ts), deserialize(db.get(kSnapshot(persistenceId, seqNr))).data)
          }
      }
    )

  override def saveAsync(metadata: SnapshotMetadata, snapshot: Any): Future[Unit] =
    nextAction(
      metadata.persistenceId,
      save(metadata, snapshot)
    )

  override def deleteAsync(metadata: SnapshotMetadata): Future[Unit] =
    nextAction(
      metadata.persistenceId,
      readWrite { db =>
        val historyKey = kSMHistory(metadata.persistenceId)
        val history    = db.get(historyKey)
        history
          .find(seqNr => db.get(kSM(metadata.persistenceId, seqNr)).matches(metadata))
          .foreach { seqNr =>
            db.put(historyKey, history.filterNot(_ == seqNr))
            db.delete(kSM(metadata.persistenceId, seqNr))
            db.delete(kSnapshot(metadata.persistenceId, seqNr))
          }
      }
    )

  override def deleteAsync(persistenceId: String, criteria: SnapshotSelectionCriteria): Future[Unit] =
    nextAction(
      persistenceId,
      readWrite { db =>
        val history            = db.get(kSMHistory(persistenceId))
        val (toDelete, toKeep) = history.map(seqNr => seqNr -> db.get(kSM(persistenceId, seqNr))).partition { case (_, sm) => sm.matches(criteria) }
        db.put(kSMHistory(persistenceId), toKeep.map(_._1).sorted.reverse)
        for ((seqNr, _) <- toDelete) {
          db.delete(kSM(persistenceId, seqNr))
          db.delete(kSnapshot(persistenceId, seqNr))
        }
      }
    )

  override def receivePluginInternal: Receive = {
    case SaveSnapshotSuccess(metadata) ⇒
    case _: SaveSnapshotFailure        ⇒ // ignore
    case _: DeleteSnapshotsSuccess     ⇒ // ignore
    case _: DeleteSnapshotsFailure     ⇒ // ignore
  }

  protected def save(metadata: SnapshotMetadata, snapshot: Any): Unit =
    readWrite { rw =>
      val historyKey      = kSMHistory(metadata.persistenceId)
      val previousHistory = rw.get(historyKey)
      val nextId          = previousHistory.headOption.getOrElse(0) + 1
      val nextHistory     = nextId +: previousHistory
      rw.put(historyKey, nextHistory)
      rw.put(kSM(metadata.persistenceId, nextId), SM(metadata.sequenceNr, metadata.timestamp))
      rw.put(kSnapshot(metadata.persistenceId, nextId), serialize(snapshot))
    }

  protected def deserialize(input: Array[Byte]): Snapshot =
    serializationExtension.deserialize(input, classOf[Snapshot]).get

  private def serialize(s: Any) = serializationExtension.serialize(Snapshot(s)).get

  override def preStart() {
    migrate()
    super.preStart()
  }

  private def migrate(): Unit = for (snapshotDir <- Option(new File(config.getString("dir"))) if snapshotDir.isDirectory) {
    log.info(s"Migrating snapshots from $snapshotDir")

    def snapshotFileForWrite(metadata: SnapshotMetadata, extension: String = ""): File =
      new File(snapshotDir,
               s"snapshot-${URLEncoder.encode(metadata.persistenceId, UTF_8.name())}-${metadata.sequenceNr}-${metadata.timestamp}$extension")

    val allSnapshots = Option(snapshotDir.listFiles()).fold(Seq.empty[SnapshotMetadata]) { fs =>
      fs.map(f => extractMetadata(f.getName)).collect {
        case Some((pid, snr, tms)) => SnapshotMetadata(URLDecoder.decode(pid, UTF_8.name()), snr, tms)
      }
    }

    if (allSnapshots.nonEmpty) {
      log.info(s"Collected ${allSnapshots.size} snapshot(s)")
      for (sm <- allSnapshots) {
        val snapshotFile        = snapshotFileForWrite(sm)
        val snapshotInputStream = new BufferedInputStream(new FileInputStream(snapshotFile))
        try {
          save(sm, deserialize(streamToBytes(snapshotInputStream)).data)
          snapshotInputStream.close()
          snapshotFile.delete()
        } catch {
          case NonFatal(e) => log.error(s"Error migrating snapshot $sm", e)
        }
      }
    } else {
      log.info("No snapshots found")
    }

    log.info("Migration completed")
  }
}

object MatcherSnapshotStore extends ScorexLogging {
  case class SM(seqNr: Long, ts: Long) {
    def matches(criteria: SnapshotSelectionCriteria): Boolean =
      criteria.minSequenceNr <= seqNr && seqNr <= criteria.maxSequenceNr &&
        criteria.minTimestamp <= ts && ts <= criteria.maxTimestamp

    def matches(metadata: SnapshotMetadata): Boolean =
      seqNr == metadata.sequenceNr && (metadata.timestamp == 0 || ts == metadata.timestamp)
  }

  private val persistenceIdStartIdx = 9 // Persistence ID starts after the "snapshot-" substring
  private def extractMetadata(filename: String): Option[(String, Long, Long)] = {
    val sequenceNumberEndIdx = filename.lastIndexOf('-')
    val persistenceIdEndIdx  = filename.lastIndexOf('-', sequenceNumberEndIdx - 1)
    val timestampString      = filename.substring(sequenceNumberEndIdx + 1)
    if (persistenceIdStartIdx >= persistenceIdEndIdx || timestampString.exists(!_.isDigit)) None
    else {
      val persistenceId  = filename.substring(persistenceIdStartIdx, persistenceIdEndIdx)
      val sequenceNumber = filename.substring(persistenceIdEndIdx + 1, sequenceNumberEndIdx).toLong
      val timestamp      = filename.substring(sequenceNumberEndIdx + 1).toLong
      Some((persistenceId, sequenceNumber, timestamp))
    }
  }

  private def readSnapshotMetadata(b: Array[Byte]) = {
    val bb = ByteBuffer.wrap(b)
    SM(bb.getLong, bb.getLong)
  }

  private def writeSnapshotMetadata(sm: SM) =
    ByteBuffer.allocate(16).putLong(sm.seqNr).putLong(sm.ts).array()

  private def kSMHistory(persistenceId: String) = Key[Seq[Int]](Bytes.concat(Array(1: Byte), persistenceId.getBytes(UTF_8)), readIntSeq, writeIntSeq)
  private def kSM(persistenceId: String, seqNr: Int) =
    Key[SM](Bytes.concat(Array(2: Byte), persistenceId.getBytes(UTF_8), Ints.toByteArray(seqNr)), readSnapshotMetadata, writeSnapshotMetadata)
  private def kSnapshot(persistenceId: String, seqNr: Int) =
    Key[Array[Byte]](Bytes.concat(Array(3: Byte), persistenceId.getBytes(UTF_8), Ints.toByteArray(seqNr)), identity, identity)
}
