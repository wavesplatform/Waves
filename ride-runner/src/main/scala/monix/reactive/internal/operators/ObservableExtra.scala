package monix.reactive.internal.operators

import monix.reactive.Observable

trait ObservableExtraSyntax {
  @`inline` implicit final def ObservableExtraSyntaxRange[T](self: Observable[T]): ObservableExtraOps[T] = new ObservableExtraOps(self)
}

final class ObservableExtraOps[T](private val self: Observable[T]) extends AnyVal {
  def onErrorRestartWith(p: PartialFunction[Throwable, T]): Observable[T] = new OnErrorRetryWithObservable(self, p)
}
