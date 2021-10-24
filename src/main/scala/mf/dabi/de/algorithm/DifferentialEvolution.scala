package mf.dabi.de.algorithm

import breeze.linalg.DenseVector
import mf.dabi.de.algorithm.space.SearchSpace._
import mf.dabi.de.algorithm.space.{Result, SearchSpace}

import scala.annotation.tailrec
import scala.language.implicitConversions

object DifferentialEvolution {

  /**
   * Diferential Evolution algorithm method
   *
   * @param sp         Search Space
   * @param sizePoints Population size
   * @param generation Number of evaluations
   * @param fctor      Scaling factor
   * @param CR         Crossover Rate
   * @param ftnssFunc  Function to optimized
   * @return Result (point and its value applying f)
   */
  def algorithm(sp: SearchSpace, sizePoints: Int)(generation: Int, fctor: Double, CR: Double, ftnssFunc: Point => Double): Result = {
    @tailrec
    def aux(init: PointSet, eval: Int): PointSet = {
      if (eval == generation + 1) init
      else {
        val xset: PointSet = initial(init, ftnssFunc)
        val vset: PointSet = mutationStep(xset, fctor)
        val uset: PointSet = crossoverStep(xset, vset, CR)
        val xxset: PointSet = fitnessStep(xset, uset, ftnssFunc)
        aux(xxset, eval + 1)
      }
    }

    val pntss: PointSet = aux(sp.points(sizePoints), 0)
    getResult(pntss, ftnssFunc)
  }

  private val initFunc: Point => Point => Point => Point =
    (max: Point) => (min: Point) => (x: Point) => {
      val dim: Int = x.length
      val data: Seq[Double] = (0 until dim).map(j => min(j) + rndm01 * (max(j) - min(j)))
      DenseVector(data: _*)
    }

  def initial(set: PointSet, f: Point => Double): PointSet = {
    val (xmax, xmin) = (maxPoint(set, f), minPoint(set, f))
    set.map(initFunc(xmax)(xmin))
  }

  private val mutation: (PointSet, Double) => (Point => Point) = (set: PointSet, F: Double) =>
    (xi: Point) => {
      val (xr1: Point, xr2: Point, xr3: Point) =
        (choice(set, distinctPoint(xi)), choice(set, distinctPoint(xi)), choice(set, distinctPoint(xi)))
      xr1 + F * (xr2 - xr3)
    }

  def mutationStep(set: PointSet, F: Double): PointSet = set.map(mutation(set, F))


  private val crossover: Double => (Point, Point) => Point =
    (CR: Double) => (x: Point, v: Point) => {
      val dRnd: Int = rndm(0, x.length)
      val data: Array[Double] = (0 until x.length).map(d => if (d == dRnd || rndm01 <= CR) v(d) else x(d)).toArray
      DenseVector(data)
    }

  def crossoverStep(xset: PointSet, vset: PointSet, CR: Double): PointSet = {
    xset.zip(vset).map { case (x, v) => crossover(CR)(x, v) }
  }

  private val fitness: (Point => Double) => (Point, Point) => Point =
    (f: Point => Double) => (x: Point, u: Point) => if (f(u) <= f(x)) u else x

  def fitnessStep(xset: PointSet, uset: PointSet, f: Point => Double): PointSet =
    xset.zip(uset).map { case (x, u) => fitness(f)(x, u) }

  def getResult(result: PointSet, ftnssFunc: Point => Double): Result = {
    val mappntss = result.map(p => ftnssFunc(p) -> p).toMap
    val min = mappntss.keys.min
    Result(mappntss(min), min)
  }

  /** Implicit to transform Result datatype to point datatype */
  implicit def resultToPoint(result: Result): Point = result.point
}
