package com.wavesplatform.crypto.bls

import supranational.blst
import supranational.blst.BLST_ERROR

import java.nio.charset.StandardCharsets
import scala.util.control.NonFatal

object BlsUtils {
  val BlsDomainSeparationTag = "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_"           // We have a non-standard PoP
  private val BlsKeyGenSalt  = "BLS-SIG-KEYGEN-SALT-".getBytes(StandardCharsets.UTF_8) // From v4

  def mkBlsSecretKey(arr: Array[Byte]): blst.SecretKey = {
    val sk = new blst.SecretKey()
    sk.keygen_v5(arr, BlsKeyGenSalt)
    sk
  }

  def mkBlsPublicKey(sk: blst.SecretKey): Array[Byte] = new blst.P1(sk).compress()

  def signBasic(sk: blst.SecretKey, message: Array[Byte]): Array[Byte] =
    new blst.P2()
      .hash_to(message, BlsDomainSeparationTag)
      .sign_with(sk)
      .compress()

  def verifyBasic(blsSigBytes: Array[Byte], message: Array[Byte], blsPkBytes: Array[Byte]): Boolean = try {
    val sig = new blst.P2_Affine(blsSigBytes)
    val pk  = new blst.P1_Affine(blsPkBytes)
    if (!pk.in_group()) throw new java.lang.RuntimeException("Not in group")

    val ctx = new blst.Pairing(true, BlsDomainSeparationTag)
    ctx.aggregate(pk, sig, message)
    ctx.commit()
    ctx.finalverify()
  } catch {
    case NonFatal(_) => false
  }

  def aggSign(baseSig: Array[Byte], appendSig: Array[Byte]): Array[Byte] =
    new blst.P2(baseSig).add(new blst.P2(appendSig)).compress()

  /** @see
    *   https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-05#name-fastaggregateverify
    */
  def verifyAgg(aggSig: Array[Byte], message: Array[Byte], blsPks: Iterable[Array[Byte]]): Either[String, Boolean] = try {
    val aggPk     = blsPks.map(new blst.P1(_)).reduceLeft(_.add(_))
    val ctx       = new blst.Pairing(true, BlsDomainSeparationTag)
    val aggResult = ctx.aggregate(new blst.P1_Affine(aggPk), new blst.P2_Affine(aggSig), message)
    if (aggResult != BLST_ERROR.BLST_SUCCESS) Left(s"Can't aggregate during verification of BLS signature: $aggResult")
    else {
      ctx.commit()
      Right(ctx.finalverify())
    }
  } catch {
    case NonFatal(e) => Left("Error verifying aggregated BLS signature: " + e.getMessage)
  }
}
