package com.wavesplatform.transaction.description

import cats.data.{ReaderT, StateT}
import cats.instances.try_._
import com.google.common.primitives.{Longs, Shorts}
import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto._
import com.wavesplatform.transaction.AssetIdLength
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.Proofs

import scala.util.{Success, Try}

object ByteReaders {

  type ByteDecoder[A] = StateT[Try, Int, A]
  type ByteReader[A]  = ReaderT[ByteDecoder, Array[Byte], A]

  case class SigFee(signature: ByteStr, fee: Option[Long])

  def skipReader(n: Int): ByteReader[Unit] = ReaderT { _ =>
    StateT { offset =>
      Success((offset + n, ()))
    }
  }

  val signatureReader: ByteReader[ByteStr] = ReaderT { body =>
    StateT { offset =>
      Try(ByteStr(body.slice(offset, offset + SignatureLength))).state(offset + 10)
    }
  }

  val sponsorFeeReader: ByteReader[Option[Long]] = ReaderT { body =>
    StateT { offset =>
      Try(Option(Longs.fromByteArray(body.slice(offset, offset + 8)))).state(offset + 8)
    }
  }

  val publicKeyReader: ByteReader[PublicKey] = ReaderT { body =>
    StateT { offset =>
      Try(PublicKey(body.slice(offset, offset + KeyLength))).state(offset + KeyLength)
    }
  }

  val assetIdReader: ByteReader[IssuedAsset] = ReaderT { body =>
    StateT { offset =>
      Try(IssuedAsset(ByteStr(body.slice(offset, offset + AssetIdLength)))).state(offset + AssetIdLength)
    }
  }

  def optionReader[A](nested: ByteReader[A]): ByteReader[Option[A]] = ReaderT { body =>
    StateT { offset =>
      if (body(offset) == 1) nested.run(body).run(offset + 1).map { case (nextOffset, r) => (nextOffset, Some(r)) } else Success((offset + 1, None))
    }
  }

  val longReader: ByteReader[Long] = ReaderT { body =>
    StateT { offset =>
      Try(Longs.fromByteArray(body.slice(offset, offset + 8))).state(offset + 8)
    }
  }

  val addressOrAliasReader: ByteReader[AddressOrAlias] = ReaderT { body =>
    StateT { offset =>
      Try(AddressOrAlias.fromBytes(body, offset).explicitGet().swap)
    }
  }

  def bytesArrayUndefinedLengthReader(maxLength: Int, minLength: Int = 0): ByteReader[Array[Byte]] = ReaderT { body =>
    StateT { offset =>
      Try {
        val length                 = Shorts.fromByteArray(body.slice(offset, offset + 2))
        val (arrayStart, arrayEnd) = (offset + 2, offset + 2 + length)
        (arrayEnd, body.slice(arrayStart, arrayEnd))
      }
    }
  }

  def proofsReader(concise: Boolean = true): ByteReader[Proofs] = ReaderT { body =>
    StateT { offset =>
      Try(Proofs.fromBytes(body.drop(offset)).map(p => (offset + p.bytes.value.length, p)).explicitGet())
    }
  }

  implicit class ByteReaderOps[A](val v: ByteReader[A]) extends AnyVal {
    def read(body: Array[Byte], start: Int = 0): Try[A] = v.run(body).runA(start)
  }

  private implicit class TryOps[+A](val v: Try[A]) extends AnyVal {
    def state(offset: Int): Try[(Int, A)] = v.map((offset, _))
  }

}
