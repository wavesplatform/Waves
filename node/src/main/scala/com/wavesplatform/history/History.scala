package com.wavesplatform.history

import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database
import com.wavesplatform.database.DBExt
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.state.{Blockchain, Height}
import org.iq80.leveldb.DB

trait History {
  def loadBlockBytes(id: ByteStr): Option[(Byte, Array[Byte])]
  def loadMicroBlock(id: ByteStr): Option[MicroBlock]
  def blockIdsAfter(candidates: Seq[ByteStr], count: Int): Seq[ByteStr]
}

object History {
  private def versionedBytes(block: Block): (Byte, Array[Byte]) =
    block.header.version ->
      (if (block.header.version < Block.ProtoBlockVersion) block.bytes() else PBBlocks.protobuf(block).toByteArray)

  def apply(blockchain: Blockchain, liquidBlock: ByteStr => Option[Block], microBlock: ByteStr => Option[MicroBlock], db: DB): History = new History {
    override def loadBlockBytes(id: ByteStr): Option[(Byte, Array[Byte])] =
      liquidBlock(id)
        .orElse(blockchain.heightOf(id).flatMap { h =>
          db.readOnly { ro =>
            database.loadBlock(Height(h), ro)
          }
        })
        .map(versionedBytes)

    override def loadMicroBlock(id: ByteStr): Option[MicroBlock] = microBlock(id)

    override def blockIdsAfter(candidates: Seq[ByteStr], count: Int): Seq[ByteStr] =
      candidates.view.flatMap(blockchain.heightOf).headOption.fold[Seq[ByteStr]](Seq.empty) { firstCommonHeight =>
        (firstCommonHeight to firstCommonHeight + count).flatMap(blockchain.blockId)
      }
  }
}
