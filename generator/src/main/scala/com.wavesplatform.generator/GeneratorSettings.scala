package com.wavesplatform.generator

import java.net.InetSocketAddress

import cats.Show
import cats.implicits.showInterpolator
import scorex.account.PrivateKeyAccount
import scorex.crypto.encode.Base58

import scala.concurrent.duration.FiniteDuration

case class GeneratorSettings(chainId: String,
                             accounts: Seq[String],
                             sendTo: Seq[InetSocketAddress],
                             iterations: Int,
                             delay: FiniteDuration,
                             mode: Mode.Value,
                             narrow: NarrowTransactionGenerator.Settings,
                             wide: WideTransactionGenerator.Settings,
                             dynWide: DynamicWideTransactionGenerator.Settings) {
  val addressScheme: Char = chainId.head
  val privateKeyAccounts: Seq[PrivateKeyAccount] = accounts.map(s => PrivateKeyAccount(Base58.decode(s).get))
}

object GeneratorSettings {
  implicit val toPrintable: Show[GeneratorSettings] = { x =>
    import x._

    val modeSettings: String = (mode match {
      case Mode.NARROW => show"$narrow"
      case Mode.WIDE => show"$wide"
      case Mode.DYN_WIDE => show"$dynWide"
    }).toString

    s"""network byte: $chainId
       |rich accounts:
       |  ${accounts.mkString("\n  ")}
       |recipient nodes:
       |  ${sendTo.mkString("\n  ")}
       |number of iterations: $iterations
       |delay between iterations: $delay
       |mode: $mode
       |$mode settings:
       |  ${modeSettings.split('\n').mkString("\n  ")}""".stripMargin
  }
}
