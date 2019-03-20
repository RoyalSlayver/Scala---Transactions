import scala.io.Source

case class Transaccion(Fecha: String,Producto:String,Venta:Int,TipoPago: String,Pais:String)

object Solucion{

  def main(args: Array[String]):Unit = {

    class miSolucion(fileName: String){

      val midatos = readfile().toList

      def readfile(): Iterator[Transaccion] = {
        for {line <- Source.fromFile(fileName).getLines.drop(1)
             item = line.split(",")
        }
          yield Transaccion(item(0),item(1),item(2).toInt,item(3),item(4))
      }
    }
    val a = new miSolucion( fileName = "C:\\Users\\Ivan_\\Downloads\\Transacciones.txt" )

    val datos = a.midatos

    //Número total de transacciones en el fichero.
    println("total: "+ datos.size)

    //Imprimir los datos de la Venta número 100.
    println("Transaccion nº100: "+ datos(100))

   //Venta Media y Total por tipo de pago
    def media (t: List[Transaccion]): Unit = {
      val b = t.groupBy(c => c.TipoPago).mapValues(_.map(_.Venta))
      val c = b.mapValues(list => list.sum/list.size)
      val d = b.mapValues(list => list.sum)
      println("Ventas medias por tipo pago: " + c)
      println("Ventas total por tipo pago: " + d)
    }
    media(datos)

    //Número de ventas totales y Media por día.
    def extrairDia(trans: Transaccion): String = {
      val partes = trans.Fecha.split(" ")(0).split("-")
      partes(1) + "-" + partes(2)
    }

    def perDay(): Unit = {
      val b = datos.groupBy(extrairDia(_))
      val c = b.mapValues(_.map(_.Venta).sum)
      val d = b.mapValues(_.map(_.Venta)).mapValues(list => list.sum/list.size)
      println("Ventas total por día: " + c)
      println("Ventas media por día: " + d)
    }

    perDay()


    //Venta Media del día 9/11
    def dia_9_11 (date: String): Unit = {
      val b = datos.groupBy(extrairDia(_).filter(_ == date)).mapValues(_.map(_.Venta)).mapValues(list => list.sum)
      println("Ventas total en el dia 9-11 es: " + b)
    }
    dia_9_11("9-11")

    //Número de Ventas y total de ventas en USA
    def totalIn(Pais: String): (Int, Double) = {

      val filtered = datos.filter(_.Pais == Pais)

      (filtered.size, filtered.map(_.Venta).sum)
    }

    val totalUSA = totalIn("USA")
    println("Total de Ventas en USA: " + totalUSA._1 + " y total de ventas en USA: " + totalUSA._2)

    //Número de Ventas y total de ventas fuera de USA
    def totalOut (Pais: String): (Int, Double) = {

      val filtered = datos.filter(_.Pais != Pais)

      (filtered.size, filtered.map(_.Venta).sum)
    }

    val totalOutUSA = totalOut("USA")
    println("Total de Ventas fuera de USA: " + totalOutUSA._1 + " y total de ventas fuera de USA: " + totalOutUSA._2)

    //Venta Media por día en Francia

    def paisFilter (Pais: String): Unit = {
      val b = datos.filter(_.Pais == Pais).groupBy(extrairDia(_)).mapValues(_.map(_.Venta).sum)
      println("Venta media por dia en "+ Pais+": "+b)
    }

    paisFilter("Francia")

    //Día Máxima Venta Media fuera de Rusia
    def maxOut (Pais: String):Unit = {
      val b = datos.filter(_.Pais != Pais).groupBy(extrairDia(_)).mapValues(_.map(_.Venta)).mapValues(list => list.sum/list.size).max
      println("Día máxima venta media fuera de " + Pais + ": " + b)
    }

    maxOut("Rusia")

    //Venta por tipo de pago en Italia.
    def sellPerType (Pais: String): Unit = {
      val b = datos.filter(_.Pais == Pais).groupBy(c => c.TipoPago).mapValues(_.map(_.Venta))
      println("Venta por tipo de pago en " + Pais + ": " + b)
    }

    sellPerType("Italia")

    //Venta de Visa en Italia
    def sellVisa (Pais: String, Type: String): Unit = {
      val b = datos.filter(_.Pais == Pais).filter(_.TipoPago == Type).groupBy(c => c.TipoPago).mapValues(_.map(_.Venta))
      println("Venta por tipo de pago en " + Pais + ": " + b)
    }

    sellVisa("Italia","Visa")

  }
}
