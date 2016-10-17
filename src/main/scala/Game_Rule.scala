import scala.util.Random

/**
  * Created by wschoi on 2016-10-12.
  */
class Game_Rule() {

  val turn = Random.nextInt(8)
  val my_turn_size = if(turn > 0) turn/2 + Random.nextInt(1) else turn/2
  val your_turn_size = turn - my_turn_size
  val me = List.fill(my_turn_size)(Random.nextInt(8))
  val you = List.fill(your_turn_size)(Random.nextInt(8))
  val target = Random.nextInt(8)
}


class RandomRule() extends  Game_Rule {

}

