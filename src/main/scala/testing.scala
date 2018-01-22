import org.apache.log4j.Logger

sealed trait CommissionDisplay {
  def totalDisplayCommission: String={
    ""
  }
}

abstract class Commission(value: Int)

case class ClientSideCommission(value: Int) extends Commission(value)

case class StreetSideCommission(value: Int) extends Commission(value)

class TotalCommission extends CommissionDisplay{

  implicit def calculateCommission(list: List[Commission]): String = {

    def sum(list: List[Commission], s: Int): Int = {
      list match {
        case head :: tail => {
          head match {
            case ClientSideCommission(value) => sum(tail, s + value)
            case StreetSideCommission(value) => sum(tail, s + value)
          }
        }
        case _ => s
      }
    }

    val s = sum(list, 0)
    list match {
      case List(_: ClientSideCommission, _*) => s"the total client Commisiion $s"
      case List(_: StreetSideCommission, _*) => s"the total Street Commisiion $s"
      case List(_: Commission, _*) => s"the total Normal Commisiion $s"
      case _ => "wrong input"
    }
  }

  def getTotalCommission(list: List[Commission]): String = {
    list
  }

}

object Operations extends App {

  val log = Logger.getLogger(getClass)
  val num1 = 153
  val num2 = 50
  val num3 = 60
  val num4 = 120
  val num5 = 513
  val num6 = 256
  val cl1 = ClientSideCommission(num1)
  val cl2 = ClientSideCommission(num2)
  val cl3 = ClientSideCommission(num3)
  val sl1 = StreetSideCommission(num4)
  val sl2 = StreetSideCommission(num5)
  val sl3 = StreetSideCommission(num6)
  val tobj = new TotalCommission
  val lst5=List[Commission](cl1,cl2,sl1,sl2)
  val lst3 = List[ClientSideCommission](cl1, cl2, cl3)
  val lst4 = List[StreetSideCommission](sl1, sl2, sl3)
  log.info(tobj.getTotalCommission(lst4) + "\n")
  log.info(tobj.getTotalCommission(lst3) + "\n")
  log.info(tobj.getTotalCommission(lst5) + "\n")

}

