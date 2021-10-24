package mf.dabi.de.algorithm.space

import mf.dabi.de.algorithm.space.SearchSpace.Point

case class Result(point: Point, value: Double) {
  override def toString: String =
    s"Point: ${point.data.mkString("(", ",", ")")}, fValue: $value"
}
