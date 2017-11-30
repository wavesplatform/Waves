package com.wavesplatform.utils

import java.io.PrintWriter
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

import play.api.libs.json.{Json, Reads, Writes}
import scorex.crypto.encode.Base64

import scala.io.{BufferedSource, Source}

object JsonFileStorage {
  private val encoding = "UTF-8"
  private val keySalt = "0495c728-1614-41f6-8ac3-966c22b4a62d"
  private val aes = "AES"
  private val algorithm = aes + "/ECB/PKCS5Padding"
  private val hashing = "PBKDF2WithHmacSHA512"

  import java.security.NoSuchAlgorithmException
  import java.security.spec.InvalidKeySpecException
  import javax.crypto.SecretKeyFactory
  import javax.crypto.spec.PBEKeySpec

  private def hashPassword(password: Array[Char], salt: Array[Byte], iterations: Int, keyLength: Int): Array[Byte] = try {
    val skf = SecretKeyFactory.getInstance(hashing)
    val spec = new PBEKeySpec(password, salt, iterations, keyLength)
    val key = skf.generateSecret(spec)
    val res = key.getEncoded
    res
  } catch {
    case e@(_: NoSuchAlgorithmException | _: InvalidKeySpecException) =>
      throw new RuntimeException(e)
  }

  private def keyToSpec(key: String) =
    new SecretKeySpec(hashPassword(key.toCharArray, keySalt.getBytes(encoding), 999999, 128), aes)

  private def encrypt(key: String, value: String): String = {
    val cipher: Cipher = Cipher.getInstance(algorithm)
    cipher.init(Cipher.ENCRYPT_MODE, keyToSpec(key))
    Base64.encode(cipher.doFinal(value.getBytes(encoding)))
  }

  private def decrypt(key: String, encryptedValue: String): String = {
    val cipher: Cipher = Cipher.getInstance(algorithm)
    cipher.init(Cipher.DECRYPT_MODE, keyToSpec(key))
    new String(cipher.doFinal(Base64.decode(encryptedValue)))
  }

  def save[T](value: T, path: String, key: Option[String] = None)(implicit w: Writes[T]): Unit = {
    var file: Option[PrintWriter] = None
    try {
      file = Option(new PrintWriter(path))
      file.foreach {
        val json = Json.toJson(value).toString()
        val data = key.fold(json)(k => encrypt(k, json))
        _.write(data)
      }
    }
    finally {
      file.foreach(_.close())
    }
  }

  def load[T](path: String, key: Option[String] = None)(implicit r: Reads[T]): T = {
    var file: Option[BufferedSource] = None
    try {
      file = Option(Source.fromFile(path))
      val data = file.get.mkString
      Json.parse(key.fold(data)(k => decrypt(k, data))).as[T]
    }
    finally {
      file.foreach(_.close())
    }
  }
}
