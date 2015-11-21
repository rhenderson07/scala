package example

object ExampleMain extends App {
  def xs = List(1, 2, 3, 6, 7, 12, 34, 1004)

  printf("Sum = %d\n", Lists.sum(xs))
  printf("Max = %d\n", Lists.max(xs))
}