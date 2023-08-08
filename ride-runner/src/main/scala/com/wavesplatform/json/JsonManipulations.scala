package com.wavesplatform.json

import play.api.libs.json.{JsObject, JsValue, Json}

import scala.annotation.tailrec

object JsonManipulations {

  /** @param path
    *   dot-separated path, e.g.: foo.bar.baz
    * @return
    *   Removes all subtrees except a subtree with specified path
    */
  def prune(js: JsValue, path: String): JsValue = prune(js, path.split('.').toList)

  /** @return
    *   Removes all subtrees except a subtree with specified path
    */
  def prune(js: JsValue, path: List[String]): JsValue =
    pick(js, path).fold[JsValue](JsObject.empty)(mkTrunk(path, _))

  /** @param path
    *   dot-separated path, e.g.: foo.bar.baz
    * @return
    *   A subtree by this path
    */
  def pick(js: JsValue, path: String): Option[JsValue] = pick(js, splitPath(path))

  /** @return
    *   A subtree by this path
    */
  @tailrec def pick(js: JsValue, path: List[String]): Option[JsValue] = path match {
    case Nil => Some(js)
    case segment :: restSegments =>
      (js \ segment).asOpt[JsValue] match {
        case None          => None
        case Some(subTree) => pick(subTree, restSegments)
      }
  }

  /** @param path
    *   dot-separated path, e.g.: foo.bar.baz
    * @return
    *   A nested object with specified path and the deepest nested leaf
    */
  def mkTrunk(path: String, leaf: JsValue): JsValue = mkTrunk(splitPath(path), leaf)

  /** @return
    *   A nested object with specified path and the deepest nested leaf
    */
  def mkTrunk(path: List[String], leaf: JsValue): JsValue = {
    @tailrec def loop(reversedPath: List[String], acc: JsValue): JsValue = reversedPath match {
      case innerSegment :: outerSegments => loop(outerSegments, Json.obj(innerSegment -> acc))
      case Nil                           => acc
    }

    if (path.isEmpty) leaf
    else loop(path.reverse, leaf)
  }

  private def splitPath(path: String): List[String] = {
    val trimmed = path.trim
    if (trimmed.isEmpty) Nil else trimmed.split('.').toList
  }
}
