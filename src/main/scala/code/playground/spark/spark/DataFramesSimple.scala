package code.playground.spark.spark

import org.apache.spark.sql.SparkSession

object DataFramesSimple extends App {

  val BASE_DATA_PATH = "D:\\src\\scala\\mediation\\src\\main\\resources\\data\\"


  System.setProperty("HADOOP_HOME", "D:\\programs\\hadoop-3.2.1")

  val sparkSession = SparkSession.builder()
    .appName("simple app")
    .config("spark.master", "local")
    .getOrCreate()


  val firstDf = sparkSession.read
    .format("json")
    .option("inferSchema", "true")
    .option("multiline", "true")
    .load(BASE_DATA_PATH + "cars.json")


  firstDf.show()
  

}
