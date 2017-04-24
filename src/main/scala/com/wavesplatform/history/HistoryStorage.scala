package com.wavesplatform.history

import org.h2.mvstore.{MVMap, MVStore}
import scorex.block.Block.BlockId
import scorex.utils.LogMVMapBuilder

trait HistoryStorage {
  def blockBodyByHeight: java.util.Map[Int, Array[Byte]]
  def blockIdByHeight: java.util.Map[Int, BlockId]
  def heightByBlockId: java.util.Map[BlockId, Int]
  def scoreByHeight: java.util.Map[Int, BigInt]

  def commit(): Unit
}

class MVStoreHistoryStorage(db: MVStore) extends HistoryStorage {
  val blockBodyByHeight: MVMap[Int, Array[Byte]] = db.openMap("blocks", new LogMVMapBuilder[Int, Array[Byte]])
  val blockIdByHeight: MVMap[Int, BlockId] = db.openMap("signatures", new LogMVMapBuilder[Int, BlockId])
  val heightByBlockId: MVMap[BlockId, Int] = db.openMap("signaturesReverse", new LogMVMapBuilder[BlockId, Int])
  val scoreByHeight: MVMap[Int, BigInt] = db.openMap("score", new LogMVMapBuilder[Int, BigInt])

  override def commit(): Unit = db.commit()
}
