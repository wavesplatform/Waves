package monix.reactive.operators

import monix.reactive.Observable

trait ObservableExtraSyntax {
  @`inline` implicit final def observableExtraSyntaxRange[T](self: Observable[T]): ObservableExtraOps[T] = new ObservableExtraOps(self)
}

final class ObservableExtraOps[T](private val self: Observable[T]) extends AnyVal {
  def onErrorRestartWith(p: PartialFunction[Throwable, T]): Observable[T] = new OnErrorRetryWithObservable(self, p)
}
