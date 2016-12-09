import scala.collection.mutable
import scala.util.Random

/**
  * Created by wschoi on 2016-10-12.
  */
class GameBoard(val player_A: Player,val player_B: Player )
{

  val positions_A = new mutable.MutableList[Int]
  val positions_B = new mutable.MutableList[Int]

  def init () = {
    positions_A.clear()
    positions_B.clear()
  }
  def move (player: Player, new_pos: Int): Boolean = {

    require(player == player_A || player== player_B, "illegal player")
    require( math.abs(positions_A.size - positions_B.size) <= 1, "illegal game" )


    if(new_pos<0 || new_pos >8) return false
    if(over) return true

    val positions = if (player == player_A) positions_A else positions_B

    if(positions_A.contains(new_pos) || positions_B.contains( new_pos) ){

      val next_alternative = move_anlternative()
      if(next_alternative== -1) return false
      else return move(player, next_alternative)

    }

    positions += new_pos
    positions sortBy ( x => x )

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

  def get_Stronger(): Player = {

    val epoch = 5
    var A_win = 0
    var B_win = 0

    for(iter <- 1 to epoch) {

      val a_first = game_a_first()

      if(a_first == Some(player_A)) A_win = A_win +1
      else if (a_first == Some(player_B)) B_win = B_win +1

      val b_first = game_b_first()

      if(b_first == Some(player_A)) A_win = A_win +1
      else if (b_first == Some(player_B)) B_win = B_win +1

    }

    if (A_win > B_win) player_A else player_B

  }

  def game_a_first():Option[Player]= {

    init()

    while(!over()){

      if(move(player_A, move_next(player_A)))
        move(player_B, move_next(player_B))

      if(wonBy(player_A)) return Some(player_A)
      if(wonBy(player_B)) return Some(player_B)

    }

    return None

  }
  def game_b_first():Option[Player]= {

    init()

    while(!over()){

      if(move(player_B, move_next(player_B)))
        move(player_A, move_next(player_A))

      if(wonBy(player_B)) return Some(player_B)
      if(wonBy(player_A)) return Some(player_A)

    }

    return None

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
    if(player == player_A ) winSets exists (matched_winSet => matched_winSet.forall( elem => positions_A contains elem ) )
    else winSets exists (matched_winSet => matched_winSet.forall( elem => positions_B contains elem ) )

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