package com.wavesplatform.state

import com.wavesplatform.account.Alias
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.{DBExt, KeyTags}
import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

class DBStateBenchmark {
  import DBStateBenchmark._
  @Benchmark
  def foo(bh: Blackhole, st: St): Unit = {
    val aliasNr = Random.nextInt(st.allAliases.size)
    bh.consume(st.levelDBWriter.resolveAlias(st.allAliases(aliasNr)))
  }
}

object DBStateBenchmark {
  class St extends DBState {
    lazy val allAliases: Vector[Alias] = {
      val builder = Vector.newBuilder[Alias]
      db.iterateOver(KeyTags.AddressIdOfAlias) { e =>
        builder += Alias.fromBytes(e.getKey.drop(2)).explicitGet()
      }
      builder.result()
    }
  }
}
