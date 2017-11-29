package com.wavesplatform.utils

import java.io.PrintWriter
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

import play.api.libs.json.{Json, Reads, Writes}
import scorex.crypto.encode.Base64
import scorex.crypto.hash.Sha256

import scala.io.{BufferedSource, Source}

object JsonFileStorage {
  private val encoding = "UTF-8"
  private val keySalt = "0495c728-1614-41f6-8ac3-966c22b4a62d"
  private val aes = "AES"
  private val algorithm = aes + "/ECB/PKCS5Padding"

  private def keyToSpec(key: String): SecretKeySpec = {
    var keyBytes: Array[Byte] = (keySalt + key).getBytes(encoding)
    keyBytes = Sha256.hash(keyBytes)
    new SecretKeySpec(keyBytes.drop(keyBytes.length - 16), aes)
  }

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
