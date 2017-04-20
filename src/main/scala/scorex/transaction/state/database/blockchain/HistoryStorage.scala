package scorex.transaction.state.database.blockchain

import org.h2.mvstore.{MVMap, MVStore}
import scorex.block.Block.BlockId
import scorex.network.Checkpoint
import scorex.utils.LogMVMapBuilder

trait HistoryStorage {
  val blockBodyByHeight: java.util.Map[Int, Array[Byte]]
  val blockIdByHeight: java.util.Map[Int, BlockId]
  val heightByBlockId: java.util.Map[BlockId, Int]
  val scoreByHeight: java.util.Map[Int, BigInt]
  val checkpoint: java.util.Map[Int, Checkpoint]

  def commit(): Unit
}

class MVStoreHistoryStorage(db: MVStore) extends HistoryStorage {
  val blockBodyByHeight: MVMap[Int, Array[Byte]] = db.openMap("blocks", new LogMVMapBuilder[Int, Array[Byte]])
  val blockIdByHeight: MVMap[Int, BlockId] = db.openMap("signatures", new LogMVMapBuilder[Int, BlockId])
  val heightByBlockId: MVMap[BlockId, Int] = db.openMap("signaturesReverse", new LogMVMapBuilder[BlockId, Int])
  val checkpoint: MVMap[Int, Checkpoint] = db.openMap("checkpoint", new LogMVMapBuilder[Int, Checkpoint])
  val scoreByHeight: MVMap[Int, BigInt] = db.openMap("score", new LogMVMapBuilder[Int, BigInt])

  override def commit(): Unit = db.commit()
}