package models

import com.google.inject.{Inject, Singleton}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.bson.ObjectId
import org.mongodb.scala.bson.codecs.Macros._
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.model.Updates._
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase}
import play.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

// adding encryption in password
import org.mindrot.jbcrypt.BCrypt

case class Address(addressLine1:String, addressLine2: String, city: String, pincode: String, state:String, country: String)

case class User(_id: ObjectId, firstName: String, lastName: String, email: String, password: String, phoneNumber: Option[String], address:Option[Address], shippingAddress: List[Address])

case class Product(_id: ObjectId, name:String, description: String, price: Double, stockQuantity: Int, category: String, imageURL: String)

case class CartItem(_id: ObjectId, userId: String, productId: String, quantity: Int)

case class OrderItem(_id: ObjectId, orderId: String, productId: String, quantity: Int)

case class Order(_id: ObjectId, userId: String, orderItems: List[OrderItem], shippingAddress: Address, date: String)

@Singleton
class model @Inject() (config: Configuration) {
  val codecRegistry = fromRegistries( fromProviders(classOf[User], classOf[Product], classOf[Address], classOf[CartItem], classOf[OrderItem], classOf[Order]),DEFAULT_CODEC_REGISTRY)
  val url: String = config.get[String]("Database.url")

  val mongoClient: Future[MongoClient] = Future{MongoClient(url)}
  val db: Future[MongoDatabase] = mongoClient.map(_.getDatabase("parlok").withCodecRegistry(codecRegistry))
  val user: Future[MongoCollection[User]] = db.map(_.getCollection("user"))
  val product: Future[MongoCollection[Product]] = db.map(_.getCollection("product"))
  val cart: Future[MongoCollection[CartItem]] =db.map(_.getCollection("cart"))

  def insertUser(user1: User): Future[Either[String,String]] = {
    user.flatMap{userCollection =>
      userCollection.find({equal("email",user1.email)}).headOption().flatMap{
        case Some(user) => Future.successful(Left(s"user exist with this email with name ${user.firstName +" " +user.lastName}"))
        case None => userCollection.insertOne{User(user1._id, user1.firstName, user1.lastName, user1.email,BCrypt.hashpw(user1.password,BCrypt.gensalt()) , user1.phoneNumber, user1.address, user1.shippingAddress)}
          .toFuture().map{result => Right(result.getInsertedId.asObjectId().getValue.toString)}
      }
    }
  }
  def getUser(userId: String): Future[Option[User]] = {
    user.flatMap{ userCollection =>
      userCollection.find({equal("_id", new ObjectId(userId))}).headOption()
    }
  }
  def findUserId(email: String, password: String): Future[Option[ObjectId]] = {
    user.flatMap{userCollection => {
      userCollection.find{equal("email", email)}.headOption().map{
        case Some(tempUser) if(BCrypt.checkpw(password,tempUser.password))=> Some(tempUser._id)
        case None => None
      }
    }
    }
  }
  def isUserExist(userId: String): Future[Option[String]] = {
   user.flatMap{ userCollection => userCollection.find({equal("_id", new ObjectId(userId))}).headOption().map{
        case Some(_) => Some(userId)
        case None => None
      }
    }
  }
  def addShippingAddress(userId: String)(addressLine1:String, addressLine2: String, city: String, pincode: String, state:String, country: String) = {
    user.flatMap{ userCollection => userCollection.updateOne({equal("_id",new ObjectId(userId))}, {addToSet("shippingAddress",Address(addressLine1, addressLine2, city, pincode, state, country))}).toFuture()}
  }

  def getAllProducts(): Future[List[Product]] = {
    product.flatMap{productCollection =>
      productCollection.find().toFuture().map(_.toList)
    }
  }
  def addItemInCart(productId: String, userId: String, quantity: Int) : Future[String] = {
    cart.flatMap{ cartCollection =>
      cartCollection.insertOne(CartItem(new ObjectId, userId, productId, quantity)).toFuture().map(result => result.getInsertedId.toString)
    }
  }
  def getProduct(productId: String): Future[Option[Product]] = {
    product.flatMap{ productCollection =>
      productCollection.find(equal("_id",new ObjectId(productId))).headOption()
    }
  }
  def getCartItems(userId: String): Future[List[(String, Product,Int)]] = {
    cart.flatMap{ cartCollection =>
      cartCollection.find(equal("userId",userId)).toFuture().map(_.toList).flatMap(
        itemList => Future.sequence(itemList.map{
          case CartItem(cartId, _, productId, quantity) => getProduct(productId).map{
            case Some(product) => Some(cartId.toString, product, quantity)
            case None => None
          }
        }).map(_.flatten)
      )
    }
  }
  def removeFromCart(cartItemId: String,userId: String) = {
    cart.flatMap{cartCollection =>
      cartCollection.deleteOne({and(equal("_id",new ObjectId(cartItemId)),equal("userId",userId))}).toFuture()
    }
  }
  def clearCart(userId: String) = {
    cart.flatMap{cartCollection =>
      println(userId)
      cartCollection.deleteMany({equal("userId",userId)}).toFuture()
    }
  }
}