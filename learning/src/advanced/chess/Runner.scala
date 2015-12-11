package advanced.chess

object Runner extends App{  
  new Queens().queens(8).foreach(println)
}