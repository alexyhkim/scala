package remote

import akka.actor.{Actor, ActorRef, Props, ActorSystem, ExtendedActorSystem}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.math

object MyActorApp extends App  {
	val system = ActorSystem("MyActorSystem")
	val myAddress = system.asInstanceOf[ExtendedActorSystem].provider.getDefaultAddress.toString
	println("My Akka Address is:" + myAddress)

	// make time period 3 weeks (Jul 10 call)
	val deltaT = 18.0 / 252.0
	// specify parameters
	val riskFreeRate = 0.0027


	val stockMap:Map[String, Map[String, Double]] = Map(
		"JPM" -> Map(
			"initial" -> 68.37,
			"volatility" -> 0.127952,
			"divYield" -> 0.0257
		),
		"KO" -> Map(
			"initial" -> 40.08,
			"volatility" -> 0.065745,
			"divYield" -> 0.0329
		),
		"AAPL" -> Map(
			"initial" -> 127.6,
			"volatility" -> 0.145049,
			"divYield" -> 0.0163
		),
		"MMM" -> Map(
			"initial" -> 156.49,
			"volatility" -> 0.11587,
			"divYield" -> 0.0262
		),
		"AXP" -> Map(
			"initial" -> 80.39,
			"volatility" -> 0.121522,
			"divYield" -> 0.0144
		),
		"BA" -> Map(
			"initial" -> 142.36,
			"volatility" -> 0.1387,
			"divYield" -> 0.0282
		),
		"CAT" -> Map(
			"initial" -> 86.51,
			"volatility" -> 0.154314,
			"divYield" -> 0.0356
		)
		// ),
		// "CVX" -> Map(
		// 	"initial" -> 99.95,
		// 	"volatility" -> 0.119051,
		// 	"divYield" -> 0.0428
		// ),
		// "CSCO" -> Map(
		// 	"initial" -> 28.78,
		// 	"volatility" -> 0.12598,
		// 	"divYield" -> 0.0292
		// ),
		// "DIS" -> Map(
		// 	"initial" -> 111.05,
		// 	"volatility" -> 0.09465,
		// 	"divYield" -> 0.0104
		// ),
		// "DD" -> Map(
		// 	"initial" -> 69.19,
		// 	"volatility" -> 0.133633,
		// 	"divYield" -> 0.0283
		// ),
		// "XOM" -> Map(
		// 	"initial" -> 84.53,
		// 	"volatility" -> 0.097007,
		// 	"divYield" -> 0.0346
		// ),
		// "GE" -> Map(
		// 	"initial" -> 27.19,
		// 	"volatility" -> 0.095077,
		// 	"divYield" -> 0.0338
		// ),
		// "GS" -> Map(
		// 	"initial" -> 212.85,
		// 	"volatility" -> 0.126293,
		// 	"divYield" -> 0.0122
		// )
	)

	var finished = 0

	val stockPricerMap = stockMap.map{stock =>
		(stock._1, system.actorOf(Props(new StockPricer(stock._1, stock._2("initial"), stock._2("volatility"), stock._2("divYield"), riskFreeRate, deltaT)), name = stock._1 + "Pricer"))
	}.toMap

	stockPricerMap.foreach{stockTuple =>
		stockTuple._2 ! "start"
	}


	// var newPricer = system.actorOf(Props(new StockPricer(initial, volatility, divYield, riskFreeRate, deltaT)), name = "Pricer")

	//newPricer ! "start"

	class StockPricer(stockName:String, initial:Double, volatility:Double, divYield:Double, riskFreeRate:Double, deltaT:Double) extends Actor {

		val baseStrike = initial.toInt.toDouble
		var count = 0
		var optionPrices = new Array[Tuple2[Double,Double]](20) 
		def receive = {
			case startMsg:String =>

				(-9 to 10).map{i =>
					val newStrike = baseStrike + i * 0.5
					val newOptionPricer = system.actorOf(Props(new OptionPricer(newStrike, 100)), name = stockName + "Option" + newStrike)
					newOptionPricer ! "start"
				}
			case finishedOption:Tuple2[Double, Double] =>
				val inputStrike = finishedOption._1
				val finishPrice = finishedOption._2
				val index = ((inputStrike - baseStrike) / 0.5).toInt + 9
				optionPrices(index) = finishedOption
				count += 1
				if (count == 20) {
					println(stockName + " call options: \n" + optionPrices.mkString("\n"))

					finished += 1
					if (finished >= stockMap.size) {
						system.shutdown
					}
				}
		}

		class OptionPricer(strikePrice:Double, depth:Int) extends Actor {

			val dT = deltaT / depth.toDouble
			// calculate constants
			val upFactor = math.exp(volatility * math.sqrt(dT))
			val downFactor = 1.0 / upFactor
			val pUp = (math.exp(dT * (riskFreeRate - divYield)) - downFactor) / (upFactor - downFactor)
			val pDown = 1.0 - pUp

			val nodeMap:Map[Int, Map[Int, ActorRef]] =
			(0 to depth).map{i => 
				(i, (0 to i).map{j => 
					(j, system.actorOf(Props(new OptionNode(i, j)), name = stockName + strikePrice + "Node" + i + "," + j))}.toMap
				)
			}.toMap

			def receive = {
				case startMsg:String =>
					(0 to depth).foreach{i =>
						nodeMap(depth)(i) ! "start"
					}
			}

			class OptionNode(level:Int, height:Int) extends Actor {

				var receivedFirstPrice:Boolean = false
				var firstPrice:Double = 0.0

				def receive = {

					case price:Double =>
						if (receivedFirstPrice) {
							var exercisePrice = getExercisePrice(level, height)
							var upPrice = math.max(price, firstPrice)
							var downPrice = math.min(price, firstPrice)

							var optionPrice = math.exp(-riskFreeRate * dT) * (pUp * upPrice + pDown * downPrice)

							optionPrice = math.max(optionPrice, exercisePrice)

							if (level == 0) {

								var roundedPrice = math.rint(optionPrice * 100.0) / 100.0
								stockPricerMap(stockName) ! (strikePrice, roundedPrice)
							} else {
								if (nodeMap(level - 1).contains(height - 1)) {
									nodeMap(level - 1)(height - 1) ! optionPrice
									//printf("%s says: %s\n", this.self.path, optionPrice)
								}

								if (nodeMap(level - 1).contains(height)) {
									nodeMap(level - 1)(height) ! optionPrice
									//printf("%s says: %s\n", this.self.path, optionPrice)
								}
							}
						} else {
							firstPrice = price
							receivedFirstPrice = true
						}
					//printf("%s says: %s\n", this.self.path, price)

					case startMsg:String =>
						var exercisePrice:Double = getExercisePrice(level, height)
						if (nodeMap(level - 1).contains(height - 1)) {
							nodeMap(level - 1)(height - 1) ! exercisePrice
						}

						if (nodeMap(level - 1).contains(height)) {
							nodeMap(level - 1)(height) ! exercisePrice
						}

				}

				def getExercisePrice(level:Int, height:Int):Double = {
					var downMoves:Int = level - height
					var upMoves:Int = height
					return math.max(initial * math.pow(upFactor, upMoves - downMoves) - strikePrice, 0.0)
				}

			}

		}
	}
}

