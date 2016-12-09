import scala.util.Random

/**
  * Created by wschoi on 2016-10-11.
  */
abstract class Player
{
  //Default O
  val game_Rule: List[Game_Rule]
  val num_of_mutation: Int = 2
  def move(board: GameBoard): Int = {
    val target = game_Rule.filter(rule => rule.me.toSet == board.positions_A.toSet && rule.you.toSet == board.positions_B.toSet).map(matched_rule => matched_rule.target)
    if(target isEmpty)  -1 else  target.head
  }

  def equal_rule(other: Player): Boolean = {
    this.eq(other) ||
    (this.game_Rule.size == other.game_Rule.size
      && this.game_Rule.zip(other.game_Rule).forall( x => x._1 equals x._2))
  }

  override def equals(obj: scala.Any): Boolean = equal_rule(obj.asInstanceOf[Player])

}

case class RandomPlayer (num_of_rule: Int) extends  Player{

  val game_Rule = List.fill(num_of_rule)(Game_Rule())

}
case class RandomPlayer_withPool (num_of_rule: Int, rule_pool: List[Game_Rule]) extends  Player{

  val game_Rule = Random.shuffle(rule_pool ++ List.fill(num_of_mutation)(Game_Rule())).take(num_of_rule)

}


 case class combinedPlayer(_1: Player, _2: Player, num_of_rule: Int) extends Player {



   override val game_Rule: List[Game_Rule] =   {

     val given = Random.shuffle((_1.game_Rule ++ _2.game_Rule)).take(num_of_rule).distinct
     val with_mutation = Random.shuffle (given ++ List.fill(num_of_mutation)(Game_Rule())).take(num_of_rule).distinct

     if(with_mutation.size == num_of_rule) with_mutation else with_mutation ++ List.fill( num_of_rule - with_mutation.size )(Game_Rule())
   }
 }