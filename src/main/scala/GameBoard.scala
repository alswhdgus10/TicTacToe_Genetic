import scala.collection.mutable
import scala.util.Random

/**
  * Created by wschoi on 2016-10-12.
  */
class GameBoard(player_A: Player,player_B: Player )
{

  val positions_A = new mutable.MutableList[Int]
  val positions_B = new mutable.MutableList[Int]

  def move (player: Player, new_pos: Int): Boolean = {

    require(player == player_A || player== player_B, "illegal player")
    require( math.abs(positions_A.size - positions_B.size) <= 1, "illegal game" )


    if(new_pos<0 || new_pos >8) return false
    if(over) return true

    val positions = player match {
      case this.player_A => positions_A
      case this.player_B => positions_B
    }

    if(positions_A.contains(new_pos) || positions_B.contains( new_pos) ){

      val next_alternative = move_anlternative()
      if(next_alternative== -1) return false
      else return move(player, next_alternative)

    }

    positions += new_pos
    positions sortBy (x => x)

    return true

  }

  def move_next(player: Player): Int = {
    val res = player.move(this)

    if(res == -1) move_anlternative
    else res
  }

  def move_anlternative(): Int = {
    val list = (0 to 8).toSet.diff(positions_A.toSet).diff(positions_B.toSet).toList.sortBy(_=>math.random)
    if(list.isEmpty) -1
    else list.head
  }

  def fight_and_get_winner():Player = {

    while(!over()){

      if(move(player_A, move_next(player_A)))
        move(player_B, move_next(player_B))

      if(wonBy(player_A)) return player_A
      if(wonBy(player_B)) return player_B

    }

    return null

  }


  val winSets =
    List(
      List(0, 1, 2),
      List(3, 4, 5),
      List(6, 7, 8),
      List(0, 3, 6),
      List(1, 4, 7),
      List(2, 5, 8),
      List(0, 4, 8),
      List(2, 4, 6))

  def full(): Boolean =
  {
    return (positions_A.size + positions_B.size) == 9
  }

  def won(): Boolean =
  {
    return wonBy(player_A) || wonBy(player_B)
  }

  def wonBy(player: Player): Boolean =  {
    player match {
      case player_A => winSets exists (matched_winSet => matched_winSet.forall( elem => positions_A contains elem ) )
      case player_B => winSets exists (matched_winSet => matched_winSet.forall( elem => positions_B contains elem ) )
      case _ => false
    }

  }

  def over(): Boolean =
  {
    return full() || won()
  }


  override def toString: String =
  {
    return positions_A.toString() +  "\n" + positions_B.toString()
  }


}