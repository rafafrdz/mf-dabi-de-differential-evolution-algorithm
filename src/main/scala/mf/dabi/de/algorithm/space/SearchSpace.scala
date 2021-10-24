package mf.dabi.de.algorithm.space

import breeze.linalg.DenseVector
import mf.dabi.de.algorithm.space.SearchSpace._

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParMap
import scala.collection.parallel.mutable.ParArray
import scala.util.Random

trait SearchSpace {
  val dim: Int
  lazy val bound: Bound[Double] = List.fill(dim)((0, 1))

  /** Get a random Particle bounded by space's bound */
  def rndm: Point = rndmP(bound: _*)

  /** Get a random point bounded by `bound` */
  def rndmBounded(bound: Bound[Double]): Point = rndmP(bound: _*)

  /** Get a random point set of size `size` and (optional) point set bound to inicialized */
  def points(size: Int): PointSet = ParArray.fill(size)(rndm)

  def points(size: Int, bound: Bound[Double]): PointSet = ParArray.fill(size)(rndmBounded(bound))

}

object SearchSpace {
  type Point = DenseVector[Double]
  type PointSet = ParArray[Point]

  type Bound[@specialized(Int, Boolean) T] = List[Limit[T]]
  type Limit[@specialized(Int, Boolean) T] = Tuple2[T, T]


  def build(n: Int, limit: Bound[Double]): SearchSpace = new SearchSpace {
    override val dim: Int = n
    override lazy val bound: Bound[Double] = limit
  }

  def build(n: Int): SearchSpace = new SearchSpace {
    override val dim: Int = n
  }

  def rndmP(bound: Limit[Double]*): Point =
    DenseVector(bound.map { case (l0, lf) => Random.between(l0, lf) }: _*)

  /** Get a random number between `a` and `b` */
  def rndm(a: Double, b: Double): Double = Random.between(a, b)

  def rndm(a: Int, b: Int): Int = Random.between(a, b)

  /** Get a random number within [0,1] */
  def rndm01: Double = Random.between(0.0, 1.0)

  def maxPoint(set: PointSet, f: Point => Double): Point = {
    val mapPoint: ParMap[Double, Point] = set.map(p => f(p) -> p).toMap
    mapPoint(mapPoint.keys.max)
  }

  def minPoint(set: PointSet, f: Point => Double): Point = {
    val mapPoint: ParMap[Double, Point] = set.map(p => f(p) -> p).toMap
    mapPoint(mapPoint.keys.min)
  }

  def rndmSize[T](set: ParArray[T]): Int = {
    val size: Int = set.size
    Random.between(0, size)
  }

  @tailrec
  def choice(set: PointSet, condition: Point => Boolean): Point = {
    val rIdx: Int = rndmSize[Point](set)
    val rnPoint: Point = set(rIdx)
    if (condition(rnPoint)) rnPoint else choice(set, condition)
  }

  val distinctPoint: Point => Point => Boolean =
    (a: Point) => (b: Point) => a != b

}
