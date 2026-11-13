package com.wavesplatform

import com.google.common.primitives.Ints
import com.wavesplatform.Importer.ImportOptions
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.database.loadBlock
import com.wavesplatform.db.WithDomain
import com.wavesplatform.history.Domain
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.state.{BlockEndorser, Height}
import com.wavesplatform.test.*
import com.wavesplatform.utils.{Schedulers, Time}

import java.io.{BufferedInputStream, ByteArrayInputStream, ByteArrayOutputStream}

class ImporterSpec extends FreeSpec with WithDomain with MixedChain {
  private case class Exported(genesis: Block, blocks: Array[Byte], blockIds: Seq[String], time: Time)

  // Domains of one suite share a database, so the source chain is exported before the target domain is opened.
  private lazy val exported: Exported =
    withMixedChain { d =>
      val height = d.rocksDBWriter.height
      val out    = new ByteArrayOutputStream()
      Exporter.exportBlocks(d.rdb, height, legacy = true, out, None)
      Exported(
        loadBlock(Height(1), d.rdb).get,
        out.toByteArray,
        (1 to height).map(h => d.blockchain.blockHeader(h).get.id().toString),
        d.testTime
      )
    }

  // A fresh domain holding only the source's genesis block. Reads `exported` first: domains of one suite share a database.
  private def withTarget[A](f: Domain => A): A = {
    val genesis = exported.genesis
    withDomain(mixedChainSettings) { target =>
      target.blockchain.processBlock(genesis, genesis.header.generationSignature, snapshot = None, generatorSet = Seq.empty).explicitGet()
      f(target)
    }
  }

  private def importInto(target: Domain, options: ImportOptions, skipBlocks: Boolean = false, blocks: Array[Byte] = exported.blocks): Unit = {
    val appender = Schedulers.singleThread("test-appender")
    try
      Importer.startImport(
        new BufferedInputStream(new ByteArrayInputStream(blocks)),
        None,
        target.blockchain,
        // The fixture chain skips block validation (Domain.appendBlock), so the import does too.
        BlockAppender(target.blockchain, exported.time, target.utxPool, target.posSelector, BlockEndorser.Disabled, appender, verify = false),
        options,
        skipBlocks,
        appender
      )
    finally appender.shutdown()
  }

  private def blockIds(target: Domain): Seq[String] = (1 to target.blockchain.height).map(h => target.blockchain.blockHeader(h).get.id().toString)

  // Imports into a fresh target and returns the ids of its blocks.
  private def importExported(options: ImportOptions, blocks: Array[Byte] = exported.blocks): Seq[String] =
    withTarget { target =>
      importInto(target, options, blocks = blocks)
      blockIds(target)
    }

  "imports an exported chain block for block" in {
    importExported(ImportOptions()) shouldBe exported.blockIds
  }

  "stops at the requested height" in {
    importExported(ImportOptions(importHeight = 4)) shouldBe exported.blockIds.take(4)
  }

  "fails on a record that does not parse instead of ending the import as if the input had ended" in {
    val garbage = Array.fill[Byte](16)(-1)
    val blocks  = exported.blocks ++ Ints.toByteArray(garbage.length) ++ garbage
    an[Exception] should be thrownBy importExported(ImportOptions(), blocks)
  }

  "continues an earlier import when told to skip the blocks the target already has" in {
    withTarget { target =>
      importInto(target, ImportOptions(importHeight = 4))
      importInto(target, ImportOptions(), skipBlocks = true)
      blockIds(target) shouldBe exported.blockIds
    }
  }
}
