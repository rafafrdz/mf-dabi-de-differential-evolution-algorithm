package mf.dabi.de.algorithm

import mf.dabi.de.algorithm.functions.AlghFunctions._
import mf.dabi.de.algorithm.space.SearchSpace._
import mf.dabi.de.algorithm.space.{Result, SearchSpace}

object DEFunctions {

  def main(args: Array[String]): Unit = {
    println(DESphere)
  }

  /** Parameters algorithm system */
  val dim: Int = 2
  val sizePoints: Int = 1000
  val generation: Int = 1000

  /** With Dim = 2 => Minimum is in (0,0) */
  lazy val limitsSphere: Bound[Double] = List.fill(dim)((-100, 100))
  lazy val spaceSphere: SearchSpace = SearchSpace.build(dim, limitsSphere)

  def DESphere: Result = DifferentialEvolution.algorithm(spaceSphere, sizePoints)(generation, 0.5, 1, sphere)

  /** With Dim = 2 => Minimum is in (1,1) */
  lazy val limitsRosenbrock: Bound[Double] = List.fill(dim)((-30, 30))
  lazy val spaceRosenbrock: SearchSpace = SearchSpace.build(dim, limitsRosenbrock)

  def DERosenbrock: Result = DifferentialEvolution.algorithm(spaceRosenbrock, sizePoints)(generation, 0.5, 1, rosenbrock)

  /** With Dim = 2 => Minimum is in (0,0) */
  lazy val limitsRastrigin: Bound[Double] = List.fill(dim)((-5.12, 5.12))
  lazy val spaceRastrigin: SearchSpace = SearchSpace.build(dim, limitsRastrigin)

  def DERastrigin: Result = DifferentialEvolution.algorithm(spaceRastrigin, sizePoints)(generation, 0.5, 1, rastrigin)

}
