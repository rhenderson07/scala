package collections

object collections {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val numbers = List(1, 2, 3, 4)                  //> numbers  : List[Int] = List(1, 2, 3, 4)

  val range = List.range(0, 5)                    //> range  : List[Int] = List(0, 1, 2, 3, 4)

  val mySet = Set(1, 1, 2)                        //> mySet  : scala.collection.immutable.Set[Int] = Set(1, 2)

  val hostPort = ("localhost", 80)                //> hostPort  : (String, Int) = (localhost,80)
  hostPort._1                                     //> res0: String = localhost
  hostPort._2                                     //> res1: Int = 80
  val securePort = "ssh" -> 22                    //> securePort  : (String, Int) = (ssh,22)

  Map(1 -> "one", 2 -> "two")                     //> res2: scala.collection.immutable.Map[Int,String] = Map(1 -> one, 2 -> two)
  Map((1, "one"), (2, "two"))                     //> res3: scala.collection.immutable.Map[Int,String] = Map(1 -> one, 2 -> two)

  numbers.map((i: Int) => i * 2)                  //> res4: List[Int] = List(2, 4, 6, 8)
  numbers.map(_ * 2)                              //> res5: List[Int] = List(2, 4, 6, 8)

  val doubles = numbers.map((i: Int) => i * 2)    //> doubles  : List[Int] = List(2, 4, 6, 8)
  val triples = numbers.map(_ * 3)                //> triples  : List[Int] = List(3, 6, 9, 12)

  val myMap = Map(1 -> "one", 2 -> "two")         //> myMap  : scala.collection.immutable.Map[Int,String] = Map(1 -> one, 2 -> two
                                                  //| )

  val evens = numbers.filter(_ % 2 == 0)          //> evens  : List[Int] = List(2, 4)

  numbers.reduce((a, b) => a + b)                 //> res6: Int = 10
  numbers.reduce(_ + _)                           //> res7: Int = 10

}