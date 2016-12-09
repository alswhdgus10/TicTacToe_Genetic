

import scala.util.Random

/**
  * Created by wschoi on 2016-10-12.
  */

object  Game_Rule {
  def apply(): Game_Rule = {

    val turn = Random.nextInt(8)
    val my_turn_size = if(turn > 0) turn/2 + Random.nextInt(1) else turn/2
    val your_turn_size = turn - my_turn_size
    val me = List.fill(my_turn_size)(Random.nextInt(8))
    val you = List.fill(your_turn_size)(Random.nextInt(8))
    val target = Random.nextInt(8)

    Game_Rule(turn, my_turn_size, your_turn_size, me, you, target )
  }
}
case class Game_Rule(turn: Int, my_turn_size:Int, your_turn_size: Int, me:List[Int], you: List[Int], target:Int )  {

  override def equals(other: scala.Any): Boolean =  elemEqual(other.asInstanceOf[Game_Rule])

  def elemEqual(other: Game_Rule): Boolean = {
    this.eq(other) ||
    this.productIterator.toList.zip(other.productIterator.toList).forall(  p => p._1 == p._2 )
  }
}




