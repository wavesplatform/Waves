package com.wavesplatform.crypto

import com.amazon.corretto.crypto.provider.AmazonCorrettoCryptoProvider
import com.typesafe.scalalogging.StrictLogging
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.conscrypt.Conscrypt

import java.security.{Security, Signature}
import scala.util.control.NonFatal

object Provider extends StrictLogging {
  val name: String =
    try {
      AmazonCorrettoCryptoProvider.install()
      Signature.getInstance("SHA256WithECDSA", AmazonCorrettoCryptoProvider.PROVIDER_NAME)
      logger.info("Using Amazon Corretto provider")
      "AmazonCorrettoCryptoProvider"
    } catch {
      case NonFatal(_) =>
        try {
          val conscrypt = Conscrypt.newProvider
          Security.addProvider(conscrypt)
          Signature.getInstance("SHA256WithECDSA", conscrypt.getName)
          logger.info("Using Conscrypt provider")
          conscrypt.getName
        } catch {
          case _: Throwable =>
            if (Security.getProvider("BC") == null) Security.addProvider(new BouncyCastleProvider)
            logger.info("Using Bouncy Castle provider")
            "BC"
        }
    }
}
