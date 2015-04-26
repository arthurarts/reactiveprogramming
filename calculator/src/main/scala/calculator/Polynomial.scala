package calculator

import scala.math._

object Polynomial {

  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = Signal[Double] {

    b()*b() - (4 * a() * c()) // Δ = b² - 4ac
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal[Set[Double]] {

    // (-b + √Δ) / (2a)  (-b - √Δ) / (2a)
    val root1: Double = (-b() + sqrt(computeDelta(a, b, c)())) / (2 * a())
    val root2: Double = (-b() - sqrt(computeDelta(a, b, c)())) / (2 * a())

    if (computeDelta(a, b, c)() < 0) Set()
    else Set(root1, root2)
  }

}
