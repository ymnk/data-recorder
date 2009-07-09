/*
 * Copyright 2009 ymnk, JCraft,Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package com.jcraft.lift.model
import javax.jdo.annotations._
import com.google.appengine.api.datastore.Key

import _root_.net.liftweb.http.{SessionVar}
import _root_.net.liftweb.util._

import _root_.org.scala_libs.jdo._
import _root_.org.scala_libs.jdo.criterion._
import _root_.com.jcraft.lift.model.Model._
import com.google.appengine.api.users.{User => GUser}
import com.google.appengine.api.users.{UserService, UserServiceFactory}

@PersistenceCapable{val identityType = IdentityType.APPLICATION,
                    val detachable="true"}
class User {

  @PrimaryKey
  @Persistent{val valueStrategy = IdGeneratorStrategy.IDENTITY}
  var id : Key = _

  @Persistent
  var email : String = ""

  @Persistent
  var name : String = ""

  @Persistent{val mappedBy = "user"}
  var data : java.util.List[Data] = new java.util.LinkedList[Data]

  @Persistent
  var twitter : String = ""

  def save() = {
    val user = this
    Model.withPM{ _.makePersistent(user) }
  }
}

object User{
  import _root_.net.liftweb.http.S
  import Model._

  object userVar extends SessionVar[Option[User]](None)

  def loggedIn_? = currentUser.isDefined
 
  def currentUser:Option[User] = {
    UserServiceFactory.getUserService.getCurrentUser match {
      case null =>
        userVar(None)
        None
      case u => userVar.is match{
        case _usr@Some(usr) => 
          // TODO if user.email != u.getEmail
	  _usr
        case None => 
          Model.withPM{ from(_, classOf[User])
                                   .where(eqC("email", u.getEmail))
                                   .findOne } match {
            case None =>
              val user = new User()
              Model.withPM{ pm =>
                user.email = u.getEmail
                user.name = u.getNickname
                pm.makePersistent(user)
              }
              S.redirectTo("/users/edit")
              userVar(Some(user))
            case user => 
              userVar(user)
          }
          userVar.is

      }
    }
  }

  def findAll() = Model.withPM{ from(_, classOf[User]).resultList } 
}
