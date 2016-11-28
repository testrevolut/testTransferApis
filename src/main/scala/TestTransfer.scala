/**
  * Created by dmitri on 25/11/2016.
  */

import scala.collection.mutable.ListBuffer
import scala.util.Random
import org.json4s.jackson.JsonMethods._
import org.apache.http.client.methods.{HttpGet, HttpPost}
import org.apache.http.client.utils.URIBuilder
import org.apache.http.impl.client.{CloseableHttpClient, DefaultHttpClient, HttpClientBuilder, HttpClients}
import org.json4s._
import org.json4s.JsonDSL._
import org.apache.http.client.methods.HttpPost
import org.apache.http.entity.StringEntity

object TestTransfer extends App {
  override def main(args: Array[String]) = {
    var randomizer = scala.util.Random
    val types = List("debit","credit","premium")
    val currency = List("USD","EUR","RUB","GBP")
    var cls = sendMessage("clients","")
    var clients = parseClients(cls)
    if (clients.isEmpty) {
      println("no clients, lets add some")
      for (i <- 0 to 10 ){
        val end = i match {
          case 1 => "st"
          case 2 => "nd"
          case 3 => "rd"
          case _ => "th"
        }
        sendPostMessage("client",addNewUser("John", "Doe " + i + end))
      }
    }
    cls = sendMessage("clients", "")
    clients = parseClients(cls)
    println("These people are in the system now")
    for (client <- clients) {
      println(client._1 + " " + client._2 + ", accounts: " + client._3.mkString(","))
    }

    var accounts = parseAccounts(sendMessage("accounts",""))
    println("these are accounts")
    for (account <- accounts) {
      println("\t" + account)
    }

    if (accounts.length == clients.length){
      println("looks like there are only default (empty) accounts available, lets add more")
      for(i <- 0 to 10 ){
        val pers = randomizer.nextInt(clients.length)
        val cur = randomizer.nextInt(currency.length)
        val t = randomizer.nextInt(types.length)
        val json = ("name" -> clients(pers)._1) ~
          ("surname" -> clients(pers)._2) ~
          ("accounttype" -> types(t)) ~
          ("currency" -> currency(cur)) ~
          ("balance" -> randomizer.nextFloat().toDouble * 100) ~
          ("overdraft" -> 0.0)
        val addAccount = sendPostMessage("account",compact(render(json)))
      }
    }

    accounts =  parseAccounts(sendMessage("accounts",""))
    // here we pick an account and a person at random and try to transfer money and demostrate the result
    println("Lets try some transfers now:\n")
    for (x <- 1 to 13) {
      val pers = randomizer.nextInt(clients.length)
      val acc = randomizer.nextInt(accounts.length)
      println("This is the donor account:")
      var parsedDonor = parseAccounts(sendPostMessage("getaccount", compact(render("id" -> accounts(acc)._2))))
      println(
        "\t" + parsedDonor.head._2 + " " + parsedDonor.head._1 + " " + parsedDonor.head._3 + " " + parsedDonor.head._4
      )
      println(
        "we try to transfer from account " + accounts(acc)._2 + " to " + clients(pers)._1 + " " + clients(pers)._2
      )
      println("his accounts:")
      for (account <- clients(pers)._3){
        val acct = sendPostMessage("getaccount", compact(render("id" -> account)))
        val res = parseAccounts(acct)
        if (res.nonEmpty)
          println("\t" + res.head._2 + " " + res.head._1 + " " + " " + res.head._3 + " " + res.head._4)
      }
      val message: String = (x % 4) match {
        case 0 => makeTransferMessage(
          accounts(acc)._2,
          clients(pers)._1,
          clients(pers)._2,
          currency(randomizer.nextInt(currency.length)),
          randomizer.nextFloat().toDouble * 10
      )
        case 1 => makeGoodTransferMessage(accounts(acc), clients(pers))
        case 2 => makeBadTransferMessage(accounts(acc), clients(pers))
        case 3 => makeImpossibleTransferMessage(accounts(acc), clients(pers), currency)
      }

      println(message)
      val res = sendPostMessage("/transfer", message)
      println("STATUS: " + res)
      parsedDonor = parseAccounts(sendPostMessage("getaccount", compact(render("id" -> accounts(acc)._2))))
      println("What happens after:")
      if (parsedDonor.nonEmpty) {
        println("this is donor")
        println(
          "\t" + parsedDonor.head._2 + " " + parsedDonor.head._1 + " " + parsedDonor.head._3 + " " + parsedDonor.head._4
        )
      }
      println("these are receiver's accounts")
      for (account <- clients(pers)._3){
        val acct = sendPostMessage("getaccount", compact(render("id" -> account)))
        val res = parseAccounts(acct)
        if(res.nonEmpty)
          println("\t" + res.head._2 + " " + res.head._1 + " " + " " + res.head._3 + " " + res.head._4)
      }
      println("++++++++++")
    }
  }

  def sendMessage(api: String, query: String, header: (String,String) = ("","")): String = {
    val httpClient = HttpClients.createDefault()
    val request = new URIBuilder()
      .setScheme("http")
      .setHost("localhost")
      .setPort(9000)
      .setPath("/" + api)
      .setCustomQuery(query)

    val post = new HttpGet(request.build())
    if (header._1 != "")
      post.addHeader(header._1,header._2)
    val response = httpClient.execute(post)
    try {
      val status = response.getStatusLine.getStatusCode
      assert(
        status < 400, s"Api responded with error: ${response.getStatusLine.toString}\n"
          + org.apache.http.util.EntityUtils.toString(response.getEntity)
      )
      val ret = org.apache.http.util.EntityUtils.toString(response.getEntity)
      return ret
    } catch {
      case ex: Throwable =>
        println(ex.getMessage)
        ""
    } finally {
      httpClient.close()
    }
  }
  def sendPostMessage(api: String, query: String, header: (String,String) = ("","")): String = {
    val httpClient = HttpClients.createDefault()
    val request = new HttpPost("http://localhost:9000/" + api)
    val messageEntity = new StringEntity(query)
    request.addHeader("content-type", "application/json")
    request.setEntity(messageEntity)
    try {
      val response = httpClient.execute(request)
      try {
        val status = response.getStatusLine.getStatusCode
        assert(
          status < 400, s"Api responded with error: ${response.getStatusLine.toString}\n"
            + org.apache.http.util.EntityUtils.toString(response.getEntity)
        )
        val ret = org.apache.http.util.EntityUtils.toString(response.getEntity)
        return ret
      } catch {
        case ex: Throwable =>
          println(ex.getMessage)
          ""
      } finally {
        httpClient.close()
      }
    }
  }

  def addNewUser(name: String, surname: String): String = {
    val json = ("name" -> name ) ~ ("surname" -> surname)
    compact(render(json))
  }
  def addAccount(clients: List[(String,String)]): String = {
    var randomizer = scala.util.Random
    val types = List("debit","credit","premium")
    val currency = List("USD","EUR","RUB","GBP")

    val pers = randomizer.nextInt(clients.length)
    val cur = randomizer.nextInt(currency.length)
    val t = randomizer.nextInt(types.length)

    val json = ("name" -> clients(pers)._1) ~
      ("surname" -> clients(pers)._2) ~
      ("accounttype" -> types(t)) ~
    ("currency" -> currency(cur)) ~
    ("balance" -> randomizer.nextFloat().toDouble * 100) ~
    ("overdraft" -> 0.0)
    compact(render(json))
  }

  // sending a few roubles (everybody is guaranteed to have an account in rubles)
  def makeGoodTransferMessage(donor: (String,BigInt,String,Double), addressee: (String,String,List[BigInt])): String = {
    println("\nTrying to make good transfer")
    val amount = donor._4 / 2
    // everybody has account in rubles
    var currency = "RUB"
    for (account <- addressee._3){
        val acct = sendPostMessage("getaccount", compact(render("id" -> account)))
        val res = parseAccounts(acct)
      }
    makeTransferMessage(donor._2,addressee._1,addressee._2,currency,amount)
  }

  // trying to send more money than available in the account
  def makeBadTransferMessage(donor: (String,BigInt,String,Double), addressee: (String,String,List[BigInt])): String = {
    println("\nTrying to send more money than we have")
    val amount = donor._4 * 2
    var currency = donor._3
    makeTransferMessage(donor._2,addressee._1,addressee._2,currency,amount)
  }
  // sending money in currency that addressee cannot accept
  def makeImpossibleTransferMessage(
                                     donor: (String,BigInt,String,Double),
                                     addressee: (String,String,List[BigInt]),
                                     currencies: List[String]
                                   ): String = {
    println("\nTrying to send wrong currency")
    val amount = donor._4 * 0.05
    // everybody has account in rubles
    val availCurr = scala.collection.mutable.Map[String,Boolean]()
    var curr = "RUB"
    for (account <- addressee._3){
      val acct = sendPostMessage("getaccount", compact(render("id" -> account)))
      val res = parseAccounts(acct)
      if (res.nonEmpty){
        availCurr.+=(res.head._3 -> true)
      }
    }
    for (c <- currencies){
      if (! availCurr.contains(c))
        curr = c
    }
    makeTransferMessage(donor._2,addressee._1,addressee._2,curr,amount)
  }

  def makeTransferMessage(id: BigInt, name: String, surname: String, curr: String, amount: Double) = {
    val json = ("id" -> id) ~
      ("name" -> name) ~
      ("surname" -> surname) ~
      ("currency" -> curr) ~
      ("amount" -> amount)
    compact(render(json))
  }
  def parseAccounts(accountsStr: String): List[(String,BigInt,String,Double)] = {
    val json = parse(accountsStr )
    val res: List[(String, BigInt,String, Double)] = for {
      JArray(accounts) <- json \ "accounts"
      JObject(account) <- accounts
      JField("type", JString(typeOfAcc)) <- account
      JField("currency", JString(curr)) <- account
      JField("id", JInt(id)) <- account
      JField("balance", JDouble(balance)) <- account
    } yield (typeOfAcc, id, curr, balance)
    res
  }
  def parseClients(clientStr: String): List[(String,String,List[BigInt])] = {
    val json = parse(clientStr )
    val res: List[(String,String,List[BigInt])] = for {
      JArray(clients) <- json \ "clients"
      JObject(client) <- clients
      JField("name", JString(name)) <- client
      JField("surname", JString(surname)) <- client
      JField("accounts", JArray(account)) <- client
     // (JInt(id)) <- account
    } yield (name, surname, account.map(x => x.toSome match {case Some(y) => y.values.asInstanceOf[BigInt]}))
    res
  }
}
