import scala.util.Random

/**
  * Created by wschoi on 2016-10-11.
  */
trait Player
{
  //Default O
  val game_Rule: List[Game_Rule]

  def move(board: GameBoard): Int = {
    val target = game_Rule.filter(rule => rule.me.toSet == board.positions_A.toSet && rule.you.toSet == board.positions_B.toSet).map(matched_rule => matched_rule.target)
    if(target isEmpty)  -1 else  target.head
  }

}

class RandomPlayer (num_of_rule: Int) extends  Player{

  val game_Rule = List.fill(num_of_rule)(new Game_Rule())

}


 class combinedPlayer(_1: Player, _2: Player, num_of_rule: Int) extends Player {

   override val game_Rule: List[Game_Rule] =   Random.shuffle((_1.game_Rule ++ _2.game_Rule ++ List.fill(num_of_rule/5)(new Game_Rule()))).take(num_of_rule).distinct
 }