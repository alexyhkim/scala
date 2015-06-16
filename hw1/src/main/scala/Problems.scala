import org.apache.commons.math3.stat.inference.KolmogorovSmirnovTest
import org.apache.commons.math3.distribution.NormalDistribution
import cern.jet.random.Uniform
import cern.jet.random.engine.MersenneTwister
import scala.util.Random
import scala.math
import org.apache.commons.math3.stat.descriptive.moment.Variance


object Problems {

  def genUniformPoints():Array[Double] = {
    var results = new Array[Double](1000)

    var current = 0.0
    var i = 0
    for (i <- 0 to 999) {
      results(i) = math.sqrt(current)
      current += (1.0 / 999.0)
    }
    return results
  }

  def variance(dataSet:Array[Double]):Double = {
    var i = 0
    var result = 0.0
    var mean = 0.0
    for (i <- 0 to dataSet.length-1) {
      mean += dataSet(i) / dataSet.length.toDouble
    }

    i = 0
    for (i <- 0 to dataSet.length-1) {
      result += math.pow((dataSet(i) - mean), 2) / (dataSet.length.toDouble - 1.0)
    }
    return result
    
  }

  def scalaUtilGaussian(mean:Double, stdev:Double, number:Int):Array[Double] = {
    var results = new Array[Double](number)
    var gen:Random = new Random();
    var i = 0
    for (i <- 1 to number) {
      var sample:Double = gen.nextGaussian() * stdev + mean
      results(i-1) = sample
    }
    return results

  }

  def boxMullerGaussian(mersenneSeed:Int, number:Int):Array[Double] = {
    var results = new Array[Double](number * 2)
    var twister:MersenneTwister = new MersenneTwister(mersenneSeed)
    var uGen:Uniform = new Uniform(-1.0, 1.0, twister);
    var count = 0
    //uniform samples
    var u1:Double = 0.0
    var u2:Double = 0.0

    var s:Double = 1.0

    //Gaussian samples
    var g1:Double = 0.0
    var g2:Double = 0.0

    var i = 0
    for (i <- 1 to number) {
      while (s >= 1.0) {
        u1 = uGen.nextDouble()
        u2 = uGen.nextDouble()
        s = u1 * u1 + u2 * u2
      }

      s = math.sqrt( (-2.0 * math.log(s)) / s)
      g1 = u1 * s
      g2 = u2 * s

      results(2*i - 2) = g1
      results(2*i - 1)  = g2
      s = 1.0
    }

    return results
  }

  def inverseTransform(mersenneSeed:Int, number:Int):Array[Double] = {
    var results = new Array[Double](number)

    var twister:MersenneTwister = new MersenneTwister(mersenneSeed)
    var uGen:Uniform = new Uniform(0.0, 1.0, twister);
    var normal = new NormalDistribution()

    var i = 0
    for (i <- 1 to number) {
      var uSample:Double = uGen.nextDouble()
      var gSample:Double = normal.inverseCumulativeProbability(uSample)
      results(i - 1) = gSample
    }
    return results

  }

  def acceptReject(mersenneSeed:Int, number:Int):Array[Double] = {
    // use exponential distribution with lambda = 1/math.sqrt(2 * Pi) as enveloping function
    var results = new Array[Double](number)
    var twister:MersenneTwister = new MersenneTwister(mersenneSeed)
    var uGen:Uniform = new Uniform(0.0, 1.0, twister);

    var sample:Double = 1.0
    var threshold:Double = 0.0
    var candidate:Double = 0.0
    var i = 0
    for (i <- 1 to number) {
      while (sample > threshold) {
        // transform from uniform to exponential using inverse transform method
        var u1:Double = uGen.nextDouble()
        candidate = -math.log(1.0 - u1)

        // calculate threshold ratio
        threshold = math.exp(-math.pow(candidate - 1.0, 2.0)/2.0)
        sample = uGen.nextDouble()
      }

      // sample uniform to determine sign
      var u2:Double = uGen.nextDouble()
      if (u2 <= 0.5) {
        candidate = -candidate
      }
      // accept sample
      results(i - 1) = candidate

      sample = 1.0
      threshold = 0.0
    }

    return results
  }






  def main(args:Array[String]) {

    // compute variance of points uniformly distributed along unit interval
    println("variance function: " + variance(genUniformPoints()))
    
    // Only generating 500 samples because too many samples causes KS test to return NaN.
    var normal = new NormalDistribution(0.0, 1.0)
    var utilSample = scalaUtilGaussian(0, 1, 500)
    var boxMullerSample = boxMullerGaussian(0, 250)
    var inverseTransformSample = inverseTransform(0, 500)
    var rejectionSample = acceptReject(0, 500)
    var ksTest = new KolmogorovSmirnovTest()

    println("p-values for KS test:")

    println("Scala util sample: " + ksTest.kolmogorovSmirnovTest(normal, utilSample))
    println("Box Muller sample: " + ksTest.kolmogorovSmirnovTest(normal, boxMullerSample))
    println("Inverse transform sample: " + ksTest.kolmogorovSmirnovTest(normal, inverseTransformSample))
    println("Accept reject sample: " + ksTest.kolmogorovSmirnovTest(normal, rejectionSample))


  }
}