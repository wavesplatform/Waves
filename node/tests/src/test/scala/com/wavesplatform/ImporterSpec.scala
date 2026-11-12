package com.wavesplatform

import com.wavesplatform.Importer.ImportOptions
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.database.loadBlock
import com.wavesplatform.db.WithDomain
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.state.{BlockEndorser, Height}
import com.wavesplatform.test.*
import com.wavesplatform.utils.{Schedulers, Time}

import java.io.{BufferedInputStream, ByteArrayInputStream, ByteArrayOutputStream}

class ImporterSpec extends FreeSpec with WithDomain with MixedChain {
  private case class Exported(genesis: Block, blocks: Array[Byte], blockIds: Seq[String], time: Time)

  // Domains of one suite share a database, so the source chain is exported before the target domain is opened.
  private lazy val exported: Exported = {
    var result: Exported = null
    withMixedChain { d =>
      val height = d.rocksDBWriter.height
      val out    = new ByteArrayOutputStream()
      Exporter.exportBlocks(d.rdb, height, legacy = true, out, None)
      result = Exported(
        loadBlock(Height(1), d.rdb).get,
        out.toByteArray,
        (1 to height).map(h => d.blockchain.blockHeader(h).get.id().toString),
        d.testTime
      )
    }
    result
  }

  // Imports into a fresh domain that holds only the source's genesis block, and returns the ids of the target's blocks.
  private def importExported(options: ImportOptions): Seq[String] =
    withDomain(mixedChainSettings) { target =>
      val genesis = exported.genesis
      target.blockchain.processBlock(genesis, genesis.header.generationSignature, snapshot = None, generatorSet = Seq.empty).explicitGet()
      val appender = Schedulers.singleThread("test-appender")
      try {
        Importer.startImport(
          new BufferedInputStream(new ByteArrayInputStream(exported.blocks)),
          None,
          target.blockchain,
          // The fixture chain skips block validation (Domain.appendBlock), so the import does too.
          BlockAppender(target.blockchain, exported.time, target.utxPool, target.posSelector, BlockEndorser.Disabled, appender, verify = false),
          options,
          skipBlocks = false,
          appender
        )
        (1 to target.blockchain.height).map(h => target.blockchain.blockHeader(h).get.id().toString)
      } finally appender.shutdown()
    }

  "imports an exported chain block for block" in {
    importExported(ImportOptions()) shouldBe exported.blockIds
  }
}
