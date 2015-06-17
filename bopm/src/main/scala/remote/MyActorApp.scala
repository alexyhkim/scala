package remote

import akka.actor.{Actor, ActorRef, Props, ActorSystem, ExtendedActorSystem}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.math

object MyActorApp extends App  {
	val system = ActorSystem("MyActorSystem")
	val myAddress = system.asInstanceOf[ExtendedActorSystem].provider.getDefaultAddress.toString
	println("My Akka Address is:" + myAddress)

	// calculate deltaT, using 4 weeks so 20 trading days
	val deltaT = 3.0 / 252.0
	// specify parameters
	val riskFreeRate = 0.0027
	val volatility = 0.127952
	val strikePrice = 65.0

	val divYield = 0.0257

	val initial = 68.37

	var newPricer = system.actorOf(Props(new StockPricer(initial, volatility, divYield, riskFreeRate, deltaT)), name = "Pricer")

	newPricer ! "start"

	class StockPricer(initial:Double, volatility:Double, divYield:Double, riskFreeRate:Double, deltaT:Double) extends Actor {

		val baseStrike = initial.toInt.toDouble
		var count = 0
		var optionPrices = new Array[Tuple2[Double,Double]](20) 
		def receive = {
			case startMsg:String =>

				(-9 to 10).map{i =>
					val newStrike = baseStrike + i * 0.5
					val newOptionPricer = system.actorOf(Props(new OptionPricer(newStrike, 100)), name = "Option" + newStrike)
					newOptionPricer ! "start"
				}
			case finishedOption:Tuple2[Double, Double] =>
				val inputStrike = finishedOption._1
				val finishPrice = finishedOption._2
				val index = ((inputStrike - baseStrike) / 0.5).toInt + 9
				optionPrices(index) = finishedOption
				count += 1
				if (count == 20) {
					println(optionPrices.mkString("\n"))
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
					(j, system.actorOf(Props(new OptionNode(i, j)), name = strikePrice + "Node" + i + "," + j))}.toMap
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
								newPricer ! (strikePrice, optionPrice)
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

