/**
  * Created by wschoi on 2016-10-11.
  */
object Trainer {

  def train (max_Epoch: Int, num_of_player: Int, list: List[Player]) : Double = train(0, max_Epoch, num_of_player, list)
  def train (epoch: Int, max_Epoch: Int, num_of_player: Int, list: List[Player]) : Double = {
    0
  }

  def main(args: Array[String]) {

    val a = 0
    val size  = 100
    val max_Epoch = 100

    var gen:List[Player] = List.fill(100)(new RandomPlayer(1))

    for ( i <- 1 to 10){

      val gen_pair =gen.flatMap(x => gen.map(y => (x,y))).filterNot( pair => pair._1 == pair._2)
      val take_winners = gen_pair.map( pair  => new GameBoard(pair._1, pair._2).fight_and_get_winner()).filter( p => p != null)
      val best_players = take_winners.map(player => (player, 1)).groupBy(x => x._1).map(pair => (pair._1, pair._2.size)).toList.sortBy(pair =>  - pair._2 ).take(15)

      val best_genpair = best_players.map(pair => pair._1).flatMap( x=> best_players.map(pair => pair._1).map(y => (x,y))).filterNot( pair => pair._1 == pair._2)
      val next_gen = best_genpair.map(pair => new combinedPlayer(pair._1,pair._2, 1))
      gen = next_gen.toList

      print(i+"th trial: " + best_players.map(x=> x._2) + " \n")

    }

    val random_gen:List[Player] = List.fill(30)(new RandomPlayer(1))
    val pair =gen.flatMap(x => random_gen.map(y => (x,y))).filterNot( pair => pair._1 == pair._2)
    val take_winners = pair.map( pair  => (pair._1, new GameBoard(pair._1, pair._2).fight_and_get_winner()))
    val take_winners_who_is_in_gen = pair.map( pair  => (pair._1, new GameBoard(pair._1, pair._2).fight_and_get_winner())).filter( pair => pair._1 == pair._2)
    val take_winners_who_is_notin_gen = pair.map( pair  => (pair._1, new GameBoard(pair._1, pair._2).fight_and_get_winner())).filter( pair => pair._1 != pair._2)

    val intersection = gen.flatMap(x => x.game_Rule).map(player => (player, 1)).groupBy(x => x._1).map(pair => (pair._1, pair._2.size)).toList.sortBy(pair =>  - pair._2 ).take(15)


  }

}

