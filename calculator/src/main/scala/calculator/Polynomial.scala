package calculator

import scala.math._

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = Signal[Double] {

    pow(b(), 2) - 4 * a() * c() // Δ = b² - 4ac
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {

    // (-b + √Δ) / (2a)  (-b - √Δ) / (2a)
    val root1: Double = (-b() + sqrt(delta())) / 2 * a()
    val root2: Double = (-b() - sqrt(delta())) / 2 * a()

    if (delta() < 0) Set(0)
    if (delta() == 0) Set(root1)
    else Set(root1, root2)

  }
}
