package com.wavesplatform.utils

import java.io.{File, PrintWriter}

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec
import play.api.libs.json.{Json, Reads, Writes}
import scorex.crypto.encode.{Base64 => ScorexBase64}

import scala.io.Source
import scala.util.control.NonFatal

object JsonFileStorage {
  private[this] val Encoding          = "UTF-8"
  private[this] val KeySalt           = "0495c728-1614-41f6-8ac3-966c22b4a62d"
  private[this] val AES               = "AES"
  private[this] val Algorithm         = AES + "/ECB/PKCS5Padding"
  private[this] val HashingAlgorithm  = "PBKDF2WithHmacSHA512"
  private[this] val HashingIterations = 999999
  private[this] val KeySizeBits       = 128

  def prepareKey(key: String): SecretKeySpec = {
    import java.security.NoSuchAlgorithmException
    import java.security.spec.InvalidKeySpecException

    import javax.crypto.SecretKeyFactory
    import javax.crypto.spec.PBEKeySpec

    def hashPassword(password: Array[Char], salt: Array[Byte], iterations: Int, keyLength: Int): Array[Byte] =
      try {
        val keyFactory = SecretKeyFactory.getInstance(HashingAlgorithm)
        val keySpec    = new PBEKeySpec(password, salt, iterations, keyLength)
        val key        = keyFactory.generateSecret(keySpec)
        key.getEncoded
      } catch {
        case e @ (_: NoSuchAlgorithmException | _: InvalidKeySpecException) =>
          throw new RuntimeException("Password hashing error", e)
      }

    new SecretKeySpec(hashPassword(key.toCharArray, KeySalt.getBytes(Encoding), HashingIterations, KeySizeBits), AES)
  }

  def save[T](value: T, path: String, key: Option[SecretKeySpec])(implicit w: Writes[T]): Unit = {
    val folder = new File(path).getParentFile
    if (!folder.exists()) folder.mkdirs()

    val file = new PrintWriter(path)
    try {
      val json = Json.toJson(value).toString()
      val data = key.fold(json)(k => encrypt(k, json))
      file.write(data)
    } finally file.close()
  }

  def save[T](value: T, path: String)(implicit w: Writes[T]): Unit =
    save(value, path, None)

  def load[T](path: String, key: Option[SecretKeySpec] = None)(implicit r: Reads[T]): T = {
    val file = Source.fromFile(path)
    try {
      val dataStr = file.mkString
      Json.parse(key.fold(dataStr)(k => decrypt(k, dataStr))).as[T]
    } finally file.close()
  }

  def load[T](path: String)(implicit r: Reads[T]): T =
    load(path, Option.empty[SecretKeySpec])(r)

  private[this] def encrypt(key: SecretKeySpec, value: String): String = {
    try {
      val cipher: Cipher = Cipher.getInstance(Algorithm)
      cipher.init(Cipher.ENCRYPT_MODE, key)
      ScorexBase64.encode(cipher.doFinal(value.getBytes(Encoding)))
    } catch {
      case NonFatal(e) =>
        throw new RuntimeException("File storage encrypt error", e)
    }
  }

  private[this] def decrypt(key: SecretKeySpec, encryptedValue: String): String = {
    try {
      val cipher: Cipher = Cipher.getInstance(Algorithm)
      cipher.init(Cipher.DECRYPT_MODE, key)
      new String(cipher.doFinal(ScorexBase64.decode(encryptedValue)))
    } catch {
      case NonFatal(e) =>
        throw new RuntimeException("File storage decrypt error", e)
    }
  }
}
