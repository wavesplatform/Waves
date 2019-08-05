package com.wavesplatform

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import com.wavesplatform.settings.{GenesisSettings, GenesisTransactionSettings}
import com.wavesplatform.account.Address

import scala.concurrent.duration._

object TestHelpers {
  def genesisSettings(balances: Map[Address, Long], blockTimestamp: Long = System.currentTimeMillis()): GenesisSettings = {
    val totalAmount = balances.values.sum
    val transactions = balances.map {
      case (account, amount) =>
        GenesisTransactionSettings(account.stringRepr, amount)
    }.toSeq

    GenesisSettings(blockTimestamp, blockTimestamp, totalAmount, None, transactions, 1000, 60.seconds)
  }

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
