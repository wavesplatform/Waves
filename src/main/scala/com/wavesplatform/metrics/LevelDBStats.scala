package com.wavesplatform.metrics

import com.wavesplatform.database.Key
import kamon.Kamon
import kamon.metric.TimerMetric
import supertagged._

object LevelDBStats {
  object DbTimer extends TaggedType[TimerMetric]

  type DbTimer = DbTimer.Type

  implicit class DbTimerExt(val t: DbTimer) extends AnyVal {
    def measureForKey[A](key: Key[_])(f: => A): A = measureForKey(key.name)(f)
    def measureForKey[A](str: String)(f: => A): A = t.refine("key-type", str).measure(f)
  }

  val read: DbTimer   = DbTimer(Kamon.timer("db.read"))
  val write: DbTimer  = DbTimer(Kamon.timer("db.write"))
  val delete: DbTimer = DbTimer(Kamon.timer("db.delete"))
}
