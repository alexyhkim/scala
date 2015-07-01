import scala.math
import scala.collection.mutable.HashMap

object Classifier{

  class DecisionNode(index:Int) {
    var attrIndex = index
    var children = HashMap[String, DecisionNode]()
    var isLeaf = false
    var leafValue = ""
  }


  def constructTree(lines:Array[Array[String]], chosenAttrs:HashMap[Int, Int], parentNode:DecisionNode) = {

    val line = lines(0)
    var i = 0
    var maxIg = 0.0
    var maxAttrI = 0
    val targetEntropyVal = targetEntropy(lines)
    if (targetEntropyVal <= 0.0) {
      var leafNode = new DecisionNode(line.length - 1)
      leafNode.isLeaf = true
      leafNode.leafValue = line(line.length - 1)
    }

    for (i <- 0 to line.length - 2) {
      val ig = targetEntropyVal - splitEntropy(lines, i)
      if (ig >= maxIg) {
        maxIg = ig
        maxAttrI = i
      }
    }





  }

  def calcEntropy(countMap:HashMap[String, Int]):Double = {
    var entropy = 0.0
    var total = 0
    for ((k,count) <- countMap) {
      total += count
    }

    for ((k,count) <- countMap) {
      val prob = count.toDouble / total.toDouble
      entropy += -prob * (math.log(prob) / math.log(2))
    }
    return entropy
  }

  def countsMap(lines:Array[Array[String]], index:Int):HashMap[String,Int] = {
    var counts = new HashMap[String, Int]()

    var i = 0
    for (i <- 0 to lines.length - 1) {
      val line = lines(i)
      val value = line(index)
      if (counts.contains(value)) {
        counts(value) += 1
      } else {
        counts(value) = 1
      }
    }

    return counts
  }

  def targetEntropy(lines:Array[Array[String]]):Double = {

    val line = lines(0)
    val counts = countsMap(lines, line.length - 1)

    return calcEntropy(counts)
  }



  def splitEntropy(lines:Array[Array[String]], attrIndex:Int):Double = {

    var entropy = 0.0

    val attrCounts = countsMap(lines, attrIndex)
    var counts = new HashMap[String, HashMap[String,Int]]()
    var i = 0
    for (i <- 0 to lines.length - 1) {
      val line = lines(i)
      val targetVal = line(line.length - 1)
      val attrVal = line(attrIndex)

      if (!counts.contains(attrVal)) {
        var targetMap = new HashMap[String, Int]()
        counts(attrVal) = targetMap
      }

      if (counts(attrVal).contains(targetVal)) {
        counts(attrVal)(targetVal) += 1
      } else {
        counts(attrVal)(targetVal) = 1
      }

    }

    var total = 0
    for ((attrVal, count) <- attrCounts) {
      total += count
    }

    for ((attrVal, count) <- attrCounts) {
      val prob = count.toDouble / total.toDouble
      entropy += prob * calcEntropy(counts(attrVal))
    }

    return entropy
  }


  def main(args:Array[String]) {
    val lineString = io.Source.fromFile("src/main/resources/car.data.txt").mkString
    val lines = lineString.split("\n")
    
    val lineArray = new Array[Array[String]](lines.length)

    var i = 0
    for (i <- 0 to lines.length-1) {
      lineArray(i) = lines(i).split(",")
    }

    println(splitEntropy(lineArray, 0))
    //constructTree(lineArray)


    
  }
}