package com.wavesplatform.crypto

import com.typesafe.scalalogging.StrictLogging
import org.bouncycastle.asn1.{ASN1EncodableVector, ASN1Integer, DERSequence}
import org.web3j.utils.Numeric.toBytesPadded

import java.io.ByteArrayInputStream
import java.math.BigInteger
import java.security.cert.*
import java.security.interfaces.ECPublicKey
import java.security.spec.{ECGenParameterSpec, ECParameterSpec, ECPoint, ECPublicKeySpec}
import java.security.{AlgorithmParameters, KeyFactory, PublicKey, Signature}
import java.time.Instant
import java.util.{Collections, Date}
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.SeqHasAsJava
import scala.util.control.NonFatal

object P256Curve extends StrictLogging {
  private val signaturePool         = ThreadLocal.withInitial(() => Signature.getInstance("SHA256withECDSA", Provider.name))
  private val certPathValidatorPool = ThreadLocal.withInitial(() => CertPathValidator.getInstance("PKIX"))
  private val certificateFactory    = CertificateFactory.getInstance("X.509")
  private val keyFactory            = KeyFactory.getInstance("EC")
  private val ecSpec = {
    val params = AlgorithmParameters.getInstance("EC")
    params.init(new ECGenParameterSpec("secp256r1"))
    params.getParameterSpec(classOf[ECParameterSpec])
  }

  private def validateCertChain(root: X509Certificate, certPath: CertPath, crls: Seq[X509CRL], verificationDate: Date): Unit = {
    val params = new PKIXParameters(Collections.singleton(new TrustAnchor(root, null)))
    params.setDate(verificationDate)
    params.addCertStore(CertStore.getInstance("Collection", new CollectionCertStoreParameters(crls.asJava)))
    params.setRevocationEnabled(true)
    params.setSigProvider(Provider.name)
    certPathValidatorPool.get().validate(certPath, params)
  }

  def validateCertChain(certificateChain: Seq[Array[Byte]], crls: Seq[Array[Byte]], timestamp: Long): Either[String, Array[Byte]] = try {
    val certs = certificateChain.map(bs => certificateFactory.generateCertificate(new ByteArrayInputStream(bs)).asInstanceOf[X509Certificate])

    validateCertChain(
      certs.last,
      certificateFactory.generateCertPath(certs.init.asJava),
      crls.map(bs => certificateFactory.generateCRL(new ByteArrayInputStream(bs)).asInstanceOf[X509CRL]),
      Date.from(Instant.ofEpochMilli(timestamp))
    )

    Right(publicKeyToBytes(certs.head.getPublicKey))
  } catch {
    case NonFatal(e) =>
      logger.trace("Error validating certificate chain", e)
      Left(collectMessages("", e))
  }

  private def publicKeyFromBytes(bs: Array[Byte]): PublicKey =
    keyFactory.generatePublic(
      new ECPublicKeySpec(
        new ECPoint(
          new BigInteger(1, bs, 0, 32),
          new BigInteger(1, bs, 32, 32)
        ),
        ecSpec
      )
    )

  private def rawToDer(raw64: Array[Byte]): Array[Byte] = {
    require(raw64.length == 64, "Signature must be exactly 64 bytes")

    val v = new ASN1EncodableVector()
    v.add(new ASN1Integer(new BigInteger(1, raw64, 0, 32)))
    v.add(new ASN1Integer(new BigInteger(1, raw64, 32, 32)))

    new DERSequence(v).getEncoded
  }

  private def publicKeyToBytes(key: PublicKey): Array[Byte] = {
    val point = key.asInstanceOf[ECPublicKey].getW
    toBytesPadded(point.getAffineX, 32) ++ toBytesPadded(point.getAffineY, 32)
  }

  def verify(message: Array[Byte], signature: Array[Byte], publicKey: Array[Byte]): Either[String, Boolean] = try {
    val ecdsa = signaturePool.get()
    ecdsa.initVerify(publicKeyFromBytes(publicKey))
    ecdsa.update(message)
    Right(ecdsa.verify(rawToDer(signature)))
  } catch {
    case NonFatal(e) =>
      logger.trace("Error verifying P-256 signature", e)
      Left(collectMessages("", e))
  }

  @tailrec
  private final def collectMessages(prev: String, t: Throwable): String =
    if (t.getCause == null) (if (prev.isEmpty) "" else prev + ": ") + t.getMessage
    else collectMessages((if (prev.isBlank) "" else prev + ": ") + t.getMessage, t.getCause)

}
