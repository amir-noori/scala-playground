package code.playground.patterns.design

object DI {

  case class User(username: String, password: String)

  case class Account(accountId: String, user: User)

  trait UserRepositoryComponent {
    val userRepository = new UserRepository

    class UserRepository {
      def authenticate(user: User): User = {
        println("authenticating user: " + user)
        user
      }

      def create(user: User): User = {
        println("creating user: " + user)
        user
      }

      def delete(user: User) = println("deleting user: " + user)
    }
  }

  trait UserServiceComponent { this: UserRepositoryComponent =>
    val userService = new UserService

    class UserService {
      def authenticate(username: String, password: String): User =
        userRepository.authenticate(User(username, password))

      def create(username: String, password: String): User =
        userRepository.create(User(username, password))

      def delete(user: User) = userRepository.delete(user)
    }
  }

  trait AccountServiceComponent {
    val accountService = new AccountService

    class AccountService {
      def check(account: Account): Boolean = {
        println(s"checking account $account")
        true
      }
    }
  }

  case class UserServiceComponentMock() {
    val userService = new UserService

    class UserService {
      def authenticate(username: String, password: String): User = {
        println("just mocking authenticate!")
        User(username, password)
      }

      def create(username: String, password: String) = {
        println("just mocking create!")
        User(username, password)
      }

      def delete(user: User) = {
        println("just mocking delete!")
      }
    }
  }

  trait AccountUserComponent
    extends UserServiceComponent
      with UserRepositoryComponent with AccountServiceComponent {
  }

  trait UserApp { this: UserServiceComponent =>
    val user: User = userService.create("test", "123")
  }

  trait UserAccountApp extends AccountUserComponent {
    val user: User = userService.create("test", "123")
    accountService.check(Account("100", user))
  }

  trait UserAppTest { this: UserServiceComponentMock =>
    val user: User = userService.create("test", "123")
    userService.delete(user)
  }


}
