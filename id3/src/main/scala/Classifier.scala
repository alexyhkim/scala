import scala.math
import scala.collection.mutable.HashMap
import scala.util.Random

object Classifier{

  class DecisionNode(index:Int) {
    var attrIndex = index
    var children = HashMap[String, DecisionNode]()
    var isLeaf = false
    var leafValue = ""

    // debugging purposes
    // override def toString():String = {

    //   var initial = ""
    //   if (isLeaf) {
    //     initial += leafValue + "\n"
    //   } else {
    //     initial += attrIndex + "\n"

    //     for ((k, v) <- children) {
    //       initial += k + ", "
    //     }

    //     for ((k, v) <- children) {

    //       initial += "\n" + k
    //       initial += v.toString()
    //     }
    //   }

    //   return initial
    // }


  }

  def classify(rootNode:DecisionNode, line:Array[String]):String = {
    var currentNode = rootNode

    while (!currentNode.isLeaf) {
      val lineAttr = line(currentNode.attrIndex)
      currentNode = currentNode.children(lineAttr)
    }
    return currentNode.leafValue
  }

  def partitionLines(lines:Array[Array[String]], partitionIndex:Int):HashMap[String, Array[Array[String]]] = {
    var partitions = HashMap[String, Array[Array[String]]]()

    val counts = countsMap(lines, partitionIndex)

    for ((attrVal, count) <- counts) {
      partitions(attrVal) = lines.filter(line => line(partitionIndex) == attrVal)
    }

    return partitions

  }

  def constructTree(lines:Array[Array[String]], chosenAttrs:HashMap[Int, Int]):DecisionNode = {
    
    val line = lines(0)
    val targetEntropyVal = targetEntropy(lines)

    //base case
    if (targetEntropyVal <= 0.0) {
      val leafNode = new DecisionNode(line.length - 1)
      leafNode.isLeaf = true
      leafNode.leafValue = line(line.length - 1)
      return leafNode
    } else {


      var i = 0
      var maxIg = 0.0
      var maxAttrI = -1

      for (i <- 0 to line.length - 2) {
        if (!chosenAttrs.contains(i)) {
          val ig = targetEntropyVal - splitEntropy(lines, i)
          if (ig >= maxIg) {
            maxIg = ig
            maxAttrI = i
          }
        }
      }

      if (maxAttrI == -1) {

        // base case, split on every attribute but target takes different values
        val targetCounts = countsMap(lines, line.length - 1)
        val mostFrequent = targetCounts.maxBy(_._2)._1

        val leafNode = new DecisionNode(line.length - 1)
        leafNode.isLeaf = true
        leafNode.leafValue = mostFrequent
        return leafNode
      } else {

        chosenAttrs(maxAttrI) = 1
        val chosen = new DecisionNode(maxAttrI)
        val partitions = partitionLines(lines, maxAttrI)
        for ((attrVal, partition) <- partitions) {
          chosen.children(attrVal) = constructTree(partition, chosenAttrs)
        }
        return chosen

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
    // val trainingLines = io.Source.fromFile("src/main/resources/poker-hand-training-true.data.txt").mkString.split("\n")
    // val trainingSet = new Array[Array[String]](trainingLines.length)
    // var i = 0
    // for (i <- 0 to trainingLines.length-1) {
    //   trainingSet(i) = trainingLines(i).split(",")
    // }

    // val testingLines = io.Source.fromFile("src/main/resources/poker-hand-testing.data.txt").mkString.split("\n")
    // var correct = 0
    // var total = testingLines.length

    // var chosenAttrs = new HashMap[Int, Int]()
    // var dTree = constructTree(trainingSet, chosenAttrs)
    // var j = 0
    // for (j <- 0 to testingLines.length - 1) {
    //   val testingLineArray = testingLines(j).split(",")
    //   val actual = testingLineArray(testingLineArray.length - 1)
    //   val classified = classify(dTree, testingLineArray)

    //   if (actual == classified) {
    //     correct += 1
    //   }
    // }

    // println(correct)

    // randomly choose lines to test
    val shuffledLines = (Random.shuffle(io.Source.fromFile("src/main/resources/car.data.txt").mkString.split("\n").toSeq)).toArray

    val trainingLines = shuffledLines  
    val trainingSet = new Array[Array[String]](trainingLines.length)
    var i = 0
    for (i <- 0 to trainingLines.length-1) {
      trainingSet(i) = trainingLines(i).split(",")
    }

    // test on 200 random lines
    val testingLines = shuffledLines.splitAt(200)._1
    var correct = 0
    var total = testingLines.length

    var chosenAttrs = new HashMap[Int, Int]()
    var dTree = constructTree(trainingSet, chosenAttrs)
    var j = 0
    for (j <- 0 to testingLines.length - 1) {
      val testingLineArray = testingLines(j).split(",")
      val actual = testingLineArray(testingLineArray.length - 1)
      val classified = classify(dTree, testingLineArray)

      if (actual == classified) {
        correct += 1
      }
    }

    println("accuracy: " + correct.toDouble / total.toDouble)





    
  }
}