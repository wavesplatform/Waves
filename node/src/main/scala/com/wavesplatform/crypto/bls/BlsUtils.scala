package com.wavesplatform.crypto.bls

import cats.syntax.either.*
import supranational.blst
// Jacobian for calculations, affine for storing
import supranational.blst.{
  BLST_ERROR,
  P1 as BlstPublicKeyJacobian,
  P1_Affine as BlstPublicKeyAffine,
  P2 as BlstSignatureJacobian,
  P2_Affine as BlstSignatureAffine
}

import java.nio.charset.StandardCharsets
import scala.util.control.NonFatal

private[bls] object BlsUtils {
  val BlsDomainSeparationTag = "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_"           // We have a non-standard PoP
  private val BlsKeyGenSalt  = "BLS-SIG-KEYGEN-SALT-".getBytes(StandardCharsets.UTF_8) // From v4

  private val P1CompressedSizeInBytes = 48
  private val P2CompressedSizeInBytes = 96

  val PublicKeySizeInBytes = P1CompressedSizeInBytes
  val SignatureSizeInBytes = P2CompressedSizeInBytes

  /** @param seed Should be more or equal to 32 bytes, otherwise returns a zero secret key
    */
  def mkSecretKey(seed: Array[Byte]): blst.SecretKey = {
    val sk = new blst.SecretKey()
    sk.keygen_v5(seed, BlsKeyGenSalt)
    sk
  }

  def mkPublicKey(sk: blst.SecretKey): Array[Byte] = new BlstPublicKeyJacobian(sk).compress()

  def signBasic(sk: blst.SecretKey, message: Array[Byte]): Array[Byte] =
    new BlstSignatureJacobian()
      .hash_to(message, BlsDomainSeparationTag)
      .sign_with(sk)
      .compress()

  /** @param blsSigBytes Validated internally https://github.com/supranational/blst#signature-verification
    * @param blsPkBytes Expected to be validated
    */
  def verifyBasic(blsSigBytes: Array[Byte], message: Array[Byte], blsPkBytes: Array[Byte]): Either[String, Unit] =
    verify(blsSigBytes, message, new BlstPublicKeyAffine(blsPkBytes))

  /** @param sigs Validated internally
    * @return Not validated, but must be in the group
    */
  def aggSig(sigs: Iterable[Array[Byte]]): Either[String, Array[Byte]] = for {
    _ <- Either.raiseWhen(sigs.isEmpty)("Empty BLS signature list")
    aggSig <- Either
      .catchNonFatal {
        sigs.map(new BlstSignatureJacobian(_)).reduce(_.add(_))
      }
      .leftMap(e => s"Error aggregating BLS signatures: ${e.getMessage}")
  } yield new BlstSignatureAffine(aggSig).compress()

  /** @param aggSigBytes Validated internally
    * @param blsPks Expected to have validated public keys
    * @see https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-05#name-fastaggregateverify
    */
  def verifyAgg(aggSigBytes: Array[Byte], message: Array[Byte], blsPks: Iterable[Array[Byte]]): Either[String, Unit] = for {
    _ <- Either.raiseWhen(blsPks.isEmpty)("Empty BLS public key list")
    aggPk <- Either
      .catchNonFatal {
        blsPks.map(new BlstPublicKeyJacobian(_)).reduce(_.add(_))
      }
      .leftMap(e => s"Error aggregating BLS public keys: ${e.getMessage}")
    res <- verify(aggSigBytes, message, new BlstPublicKeyAffine(aggPk))
  } yield res

  private def verify(blsSigBytes: Array[Byte], message: Array[Byte], blsPkBytes: BlstPublicKeyAffine): Either[String, Unit] = try {
    val ctx       = new blst.Pairing(true, BlsDomainSeparationTag)
    val aggResult = ctx.aggregate(blsPkBytes, new BlstSignatureAffine(blsSigBytes), message)
    if (aggResult != BLST_ERROR.BLST_SUCCESS) s"Can't aggregate during verification of BLS signature: $aggResult".asLeft
    else {
      ctx.commit()
      if (ctx.finalverify()) Either.unit
      else "Wrong BLS signature".asLeft
    }
  } catch {
    case NonFatal(e) => s"Error verifying BLS signature: ${e.getMessage}".asLeft
  }

  def validatePublicKey(bytes: Array[Byte]): Either[String, Unit] = for {
    pk <- Either.catchNonFatal(new BlstPublicKeyAffine(bytes)).leftMap(e => s"Error in creating BLS public key: ${e.getMessage}")
    _  <- Either.raiseUnless(pk.in_group())("Wrong BLS public key: not in a group")
    _  <- Either.raiseWhen(pk.is_inf())("Wrong BLS public key: point at infinity")
  } yield ()

  def sanityCheckPublicKey(bytes: Array[Byte]): Either[String, Unit] =
    Either.raiseUnless(bytes.length == PublicKeySizeInBytes) {
      s"Unexpected BLS public key length: ${bytes.length}, expected $PublicKeySizeInBytes"
    }

  // Not validating like public key, because it is validated internally
  def sanityCheckSignature(bytes: Array[Byte]): Either[String, Unit] =
    Either.raiseUnless(bytes.length == SignatureSizeInBytes) {
      s"Unexpected BLS signature length: ${bytes.length}, expected $SignatureSizeInBytes"
    }
}
