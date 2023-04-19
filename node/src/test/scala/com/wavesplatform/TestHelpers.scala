package com.wavesplatform

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import com.wavesplatform.account.Address
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.{FunctionalitySettings, GenesisSettings, GenesisTransactionSettings, WavesSettings}

import scala.concurrent.duration.*

object TestHelpers {
  def genesisSettings(balances: Map[Address, Long], blockTimestamp: Long = System.currentTimeMillis()): GenesisSettings = {
    val totalAmount = balances.values.sum
    val transactions = balances.map { case (account, amount) =>
      GenesisTransactionSettings(account.toString, amount)
    }.toSeq

    GenesisSettings(blockTimestamp, blockTimestamp, totalAmount, None, transactions, 1000, 60.seconds)
  }

  def enableNG(settings: FunctionalitySettings): FunctionalitySettings =
    settings.copy(blockVersion3AfterHeight = 0, preActivatedFeatures = settings.preActivatedFeatures ++ Map(BlockchainFeatures.NG.id -> 0))

  def enableNG(settings: WavesSettings): WavesSettings =
    settings.copy(
      blockchainSettings = settings.blockchainSettings.copy(functionalitySettings = enableNG(settings.blockchainSettings.functionalitySettings))
    )

  def deleteRecursively(path: Path): Unit = Files.walkFileTree(
    path,
    new SimpleFileVisitor[Path] {
      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
        Option(exc).fold {
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }(throw _)
      }

      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }
    }
  )
}
